source("../common/regime_classify.R")

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggrepel')
library('gtExtras')
library('webshot2')
library('parallel')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")
source("/mnt/data/blog/common/theme.returns.common.r")

drag <- 0.2/100

# ---- Position-sizing rules ----
# Each takes vote_share in [0,1] and returns position in [0,1]
position_linear <- function(vs) 1 - vs

position_thresh_lin <- function(vs, t_low = 0.3, t_high = 0.7) {
  ifelse(vs < t_low, 1, ifelse(vs > t_high, 0, (t_high - vs) / (t_high - t_low)))
}

position_sigmoid <- function(vs, k = 10) 1 / (1 + exp(k * (vs - 0.5)))

position_step <- function(vs) ifelse(vs < 0.5, 1, 0)

# Named list for iteration
sizing_rules <- list(
  Linear     = position_linear,
  ThreshLin  = position_thresh_lin,
  Sigmoid    = position_sigmoid,
  Step       = position_step
)

print("connecting to norway...")
lcon <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    ldbserver, "StockViz", ldbuser, ldbpassword
  ),
  case = "nochange",
  believeNRows = TRUE
)

startDate <- as.Date("2005-04-01")
indices <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR")

# ---- Fetch prices ----
fileName <- "../common/prices_index.Rdata"
pXts <- NULL
if (file.exists(fileName)) {
  print("loading prices from cache...")
  load(fileName)
} else {
  print("loading prices from database...")
  syms <- c()
  for (iName in indices) {
    pDf <- sqlQuery(lcon, sprintf(
      "select px_close, time_stamp from bhav_index
       where index_name = '%s' and time_stamp >= '%s'",
      iName, startDate))
    if (nrow(pDf) == 0) next
    pXts <- merge.xts(pXts, xts(pDf$px_close, pDf$time_stamp))
  }
  names(pXts) <- indices
  save(pXts, file = fileName)
}

dSymXts <- do.call(merge.xts, lapply(indices, \(x) dailyReturn(pXts[, x])))
names(dSymXts) <- indices

# ---- Helper: compute strategy returns from position vector ----
# pos_xts: single-column xts of positions in [0,1]
# date_range: character e.g. "2016-01-01/2024-12-31"
# ret_xts: pre-computed daily returns xts
# Returns a single-column xts of strategy returns (post-friction)
compute_from_positions <- function(pos_xts, date_range, ret_xts, drag = 0.2/100) {
  retL1 <- stats::lag(ret_xts[date_range], -1)
  posSub  <- pos_xts[date_range]

  # IMPORTANT: merge by date to avoid positional-recycling bugs
  aligned <- merge(posSub, retL1, join = "left")
  pos_aligned <- aligned[, 1]
  ret_aligned <- aligned[, 2]

  gross_ret <- pos_aligned * ret_aligned
  pos_change <- pos_aligned - stats::lag(pos_aligned, 1)
  friction <- abs(pos_change) * drag
  net_ret <- gross_ret - friction
  na.omit(net_ret)
}

# ---- Phase 1: Regime classification (reuse cached from historical-index) ----
print("=== PHASE 1: REGIME CLASSIFICATION ===")
window_days <- 365 * 5
ncores <- 4

cache_file <- sprintf("%s/window-class-cache.Rdata", reportPath)
cache_is_shared <- FALSE

# Reuse cache from historical-index if available
if (!file.exists(cache_file)) {
  parent_cache <- "../historical-index/window-class-cache.Rdata"
  if (file.exists(parent_cache)) {
    cat(sprintf("  linking cache from %s\n", parent_cache))
    file.symlink(parent_cache, cache_file)
    cache_is_shared <- TRUE
  }
}

window_cache <- list()
if (file.exists(cache_file)) {
  load(cache_file)
  cat(sprintf("  loaded %d cached classifications%s\n",
              length(window_cache),
              if (cache_is_shared) " (shared, read-only)" else ""))
}

# Only run classification for keys not in cache
all_dates <- index(pXts)
n_total <- length(all_dates)
needed <- FALSE

for (iName in indices) {
  cat(sprintf("  %s\n", iName))
  tasks <- list()
  for (i in seq_along(all_dates)) {
    window_start <- all_dates[i] - window_days + 1
    window_end   <- all_dates[i]
    if (window_start < first(all_dates)) next
    cache_key <- sprintf("sliding_%s_%s_%s", iName, window_start, window_end)
    if (cache_key %in% names(window_cache)) next
    tasks[[length(tasks) + 1]] <- list(
      key = cache_key, start = window_start, end = window_end, name = iName
    )
  }

  if (length(tasks) == 0) {
    cat("      all cached, skipping\n")
    next
  }

  needed <- TRUE
  batch_size <- 500
  n_batches  <- ceiling(length(tasks) / batch_size)
  for (b in seq_len(n_batches)) {
    bs <- (b - 1) * batch_size + 1
    be <- min(b * batch_size, length(tasks))
    batch <- tasks[bs:be]

    results <- mclapply(batch, function(task) {
      window_range <- paste0(task$start, "/", task$end)
      window_ret   <- dSymXts[window_range, task$name]
      if (nrow(window_ret) < 100) return(NULL)
      window_class <- tryCatch({
        classify_regime(window_ret)
      }, error = function(e) NULL)
      if (!is.null(window_class)) {
        stats::setNames(list(window_class$regime_tbl), task$key)
      } else {
        NULL
      }
    }, mc.cores = ncores)

    for (res in results) {
      if (!is.null(res) && !is.null(names(res))) {
        window_cache[[names(res)]] <- res[[1]]
      }
    }

    print(paste(iName, batch[[1]]$start, "to", batch[[length(batch)]]$end,
                sprintf("[batch %d/%d]", b, n_batches)))
    if (!cache_is_shared) save(window_cache, file = cache_file)
  }
}

if (!needed) {
  cat("  all classifications cached, Phase 1 skipped\n")
}

# ---- Phase 2a: Sliding-window vote-share sizing ----
print("=== PHASE 2a: SLIDING WINDOW ===")
test_step_days <- 252L

process_sliding_index <- function(iName) {
  cache_keys <- grep(sprintf("^sliding_%s_", iName), names(window_cache), value = TRUE)
  if (length(cache_keys) == 0) return(NULL)

  all_windows <- tibble()
  for (ck in cache_keys) {
    tbl <- window_cache[[ck]]
    if (is.null(tbl) || nrow(tbl) == 0) next
    train_start <- min(tbl$Date)
    train_end   <- max(tbl$Date)
    all_windows <- rbind(all_windows, tibble(
      train_start = train_start, train_end = train_end
    ))
  }
  if (nrow(all_windows) == 0) return(NULL)

  all_windows <- all_windows |>
    arrange(train_end) |>
    distinct(train_end, .keep_all = TRUE) |>
    filter(row_number() %% test_step_days == 0L)

  results_rows <- list()
  strats       <- list()
  label        <- gsub(" TR$", "", iName)

  for (wi in seq_len(nrow(all_windows))) {
    train_end <- all_windows$train_end[wi]
    test_start_idx <- which(all_dates == train_end) + 1L
    if (is.na(test_start_idx) || test_start_idx > length(all_dates)) next
    test_end_idx <- min(test_start_idx + test_step_days - 1L, length(all_dates))
    test_start <- all_dates[test_start_idx]
    test_end   <- all_dates[test_end_idx]
    test_range <- paste0(test_start, "/", test_end)
    px_range   <- paste0(first(all_dates), "/", test_end)

    # Build daily vote-share vector for the test year
    vote_share_vec <- c()
    for (j in test_start_idx:test_end_idx) {
      d <- all_dates[j]
      ws <- d - window_days + 1
      ck <- sprintf("sliding_%s_%s_%s", iName, ws, d)
      tbl <- window_cache[[ck]]
      if (is.null(tbl)) { vote_share_vec <- c(vote_share_vec, NA_real_); next }
      row <- tbl |> filter(Date == d)
      if (nrow(row) == 0) row <- tail(tbl, 1)
      if (nrow(row) > 0) {
        vote_share_vec <- c(vote_share_vec,
                            row$N_Unstable[1] / pmax(row$N_Total[1], 1))
      } else {
        vote_share_vec <- c(vote_share_vec, NA_real_)
      }
    }

    vs_xts <- xts(vote_share_vec, order.by = all_dates[test_start_idx:test_end_idx])
    vs_xts <- na.locf(vs_xts, fromLast = FALSE)
    if (nrow(vs_xts) < 20) next

    # Compute returns for each sizing rule
    rule_rets <- list()
    for (rule_name in names(sizing_rules)) {
      rule_fn <- sizing_rules[[rule_name]]
      pos_vec <- rule_fn(coredata(vs_xts))
      pos_xts <- xts(pos_vec, order.by = index(vs_xts))

      strat_ret <- tryCatch({
        compute_from_positions(pos_xts, px_range, dSymXts[, iName], drag)
      }, error = function(e) NULL)

      if (!is.null(strat_ret) && nrow(strat_ret) >= 20) {
        rule_rets[[rule_name]] <- strat_ret[test_range]
      }
    }

    # B&H baseline
    bh_ret <- tryCatch({
      retL1 <- stats::lag(dSymXts[px_range, iName], -1)
      bh <- na.omit(retL1[test_range])
      if (nrow(bh) >= 20) bh else NULL
    }, error = function(e) NULL)

    if (length(rule_rets) == 0) next

    # Merge all rule returns + B&H
    all_rets <- rule_rets
    if (!is.null(bh_ret)) {
      names(bh_ret) <- "B&H"
      all_rets[["B&H"]] <- bh_ret
    }
    merged_rets <- do.call(merge.xts, unname(all_rets))
    names(merged_rets) <- names(all_rets)
    merged_rets <- na.omit(merged_rets)
    if (nrow(merged_rets) < 20) next

    strats[[sprintf("%s_%s_%s", label, test_start, test_end)]] <- merged_rets

    ann_ret <- Return.annualized(merged_rets)
    sharpe  <- SharpeRatio.annualized(merged_rets)
    dd      <- maxDrawdown(merged_rets)

    row <- tibble(Index = iName,
                  Window_Start = as.character(test_start),
                  Window_End   = as.character(test_end))
    for (cn in names(all_rets)) {
      cn_clean <- gsub("[^A-Za-z0-9_]", "_", cn)
      row[[paste0(cn_clean, "_Ret")]]    <- round(ann_ret[1, cn], 4)
      row[[paste0(cn_clean, "_Sharpe")]] <- round(sharpe[1, cn], 3)
      row[[paste0(cn_clean, "_DD")]]     <- round(-as.numeric(dd[1, cn]), 4)
    }
    results_rows[[length(results_rows) + 1]] <- row
  }

  list(results_rows = results_rows, strats = strats)
}

index_results <- mclapply(indices, process_sliding_index, mc.cores = ncores)

sliding_results <- tibble()
sliding_strats  <- list()
for (res in index_results) {
  if (is.null(res)) next
  for (row in res$results_rows) {
    sliding_results <- rbind(sliding_results, row)
  }
  for (nm in names(res$strats)) {
    sliding_strats[[nm]] <- res$strats[[nm]]
  }
}

# Identify all strategy columns (exclude Index, Window_Start, Window_End)
strat_cols <- setdiff(names(sliding_results),
                      c("Index", "Window_Start", "Window_End"))
ret_cols  <- grep("_Ret$",    strat_cols, value = TRUE)
sr_cols   <- grep("_Sharpe$", strat_cols, value = TRUE)
dd_cols   <- grep("_DD$",     strat_cols, value = TRUE)

sliding_summary <- sliding_results |>
  group_by(Index) |>
  summarise(
    Windows = n(),
    across(all_of(ret_cols), ~ round(mean(.x, na.rm = TRUE), 4)),
    across(all_of(sr_cols),  ~ round(mean(.x, na.rm = TRUE), 3)),
    across(all_of(dd_cols),  ~ round(mean(.x, na.rm = TRUE), 4)),
    .groups = "drop"
  )

print("Sliding-window summary:")
print(sliding_summary)

# ---- Phase 2b: Expanding-window vote-share sizing ----
print("=== PHASE 2b: EXPANDING WINDOW ===")
full_range <- paste0(first(all_dates), "/", last(all_dates))

process_expanding_index <- function(iName) {
  vote_share_vec <- rep(NA_real_, length(all_dates))

  for (i in seq_along(all_dates)) {
    d <- all_dates[i]
    ws <- d - window_days + 1
    if (ws < first(all_dates)) next

    cache_key <- sprintf("sliding_%s_%s_%s", iName, ws, d)
    if (!cache_key %in% names(window_cache)) next

    tbl <- window_cache[[cache_key]]
    row <- tbl |> filter(Date == d)
    if (nrow(row) == 0) row <- tail(tbl, 1)
    if (nrow(row) > 0) {
      vote_share_vec[i] <- row$N_Unstable[1] / pmax(row$N_Total[1], 1)
    }
  }

  vs_xts <- xts(vote_share_vec, order.by = all_dates)
  vs_xts <- na.locf(vs_xts, fromLast = FALSE)
  vs_xts <- na.omit(vs_xts)
  first_valid <- which(!is.na(coredata(vs_xts)))[1]
  vs_xts <- vs_xts[first_valid:nrow(vs_xts)]
  if (nrow(vs_xts) < 50) return(NULL)

  # Compute returns for each sizing rule
  rule_rets <- list()
  for (rule_name in names(sizing_rules)) {
    rule_fn <- sizing_rules[[rule_name]]
    pos_vec <- rule_fn(coredata(vs_xts))
    pos_xts <- xts(pos_vec, order.by = index(vs_xts))

    strat_ret <- tryCatch({
      compute_from_positions(pos_xts, full_range, dSymXts[, iName], drag)
    }, error = function(e) NULL)

    if (!is.null(strat_ret) && nrow(strat_ret) >= 50) {
      rule_rets[[rule_name]] <- strat_ret
    }
  }

  # B&H baseline
  retL1 <- stats::lag(dSymXts[full_range, iName], -1)
  bh <- na.omit(retL1)
  if (nrow(bh) >= 50) {
    names(bh) <- "B&H"
    rule_rets[["B&H"]] <- bh
  }

  if (length(rule_rets) == 0) return(NULL)

  merged_rets <- do.call(merge.xts, unname(rule_rets))
  names(merged_rets) <- names(rule_rets)
  merged_rets <- na.omit(merged_rets)
  if (nrow(merged_rets) < 50) return(NULL)

  ann_ret <- Return.annualized(merged_rets)
  sharpe  <- SharpeRatio.annualized(merged_rets)
  dd      <- maxDrawdown(merged_rets)

  row <- tibble(Index = iName)
  for (cn in names(rule_rets)) {
    cn_clean <- gsub("[^A-Za-z0-9_]", "_", cn)
    row[[paste0(cn_clean, "_Ret")]]    <- round(ann_ret[1, cn], 4)
    row[[paste0(cn_clean, "_Sharpe")]] <- round(sharpe[1, cn], 3)
    row[[paste0(cn_clean, "_DD")]]     <- round(-as.numeric(dd[1, cn]), 4)
  }

  list(name = iName, strat = merged_rets, row = row)
}

expanding_index_results <- mclapply(indices, process_expanding_index, mc.cores = ncores)

expanding_results <- tibble()
expanding_strats  <- list()
for (res in expanding_index_results) {
  if (is.null(res)) next
  expanding_results <- rbind(expanding_results, res$row)
  expanding_strats[[res$name]] <- res$strat
}

print("Expanding-window summary:")
print(expanding_results)

# ---- Build gt metric tables ----
build_metrics_table <- function(df, title, subtitle, df_ref) {
  # df_ref: original data frame for B&H beat masks
  d <- df_ref
  strat_cols <- setdiff(names(df), c("Index", "Windows"))
  ret_cols <- grep("_Ret$", strat_cols, value = TRUE)
  sr_cols  <- grep("_Sharpe$", strat_cols, value = TRUE)
  dd_cols  <- grep("_DD$", strat_cols, value = TRUE)

  # Identify B&H columns
  bh_ret_col <- grep("B_H_Ret|BH_Ret", ret_cols, value = TRUE)[1]
  bh_sr_col  <- grep("B_H_Sharpe|BH_Sharpe", sr_cols, value = TRUE)[1]
  bh_dd_col  <- grep("B_H_DD|BH_DD", dd_cols, value = TRUE)[1]

  # Build display column order
  display_cols <- c("Index")
  if ("Windows" %in% names(df)) display_cols <- c(display_cols, "Windows")
  display_cols <- c(display_cols, ret_cols, sr_cols, dd_cols)

  tbl <- df |>
    select(all_of(display_cols)) |>
    gt() |>
    tab_header(title = title, subtitle = subtitle) |>
    tab_spanner(label = "Annualized Return", columns = all_of(ret_cols)) |>
    tab_spanner(label = "Sharpe Ratio",     columns = all_of(sr_cols)) |>
    tab_spanner(label = "Max Drawdown",     columns = all_of(dd_cols)) |>
    fmt_percent(columns = all_of(ret_cols), decimals = 1) |>
    fmt_percent(columns = all_of(dd_cols), decimals = 1) |>
    fmt_number(columns = c(all_of(sr_cols)), decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = Index))

  if ("Windows" %in% names(df)) {
    tbl <- tbl |> fmt_number(columns = Windows, decimals = 0)
  }

  # B&H beat highlighting
  if (!is.na(bh_ret_col)) {
    for (col in setdiff(ret_cols, bh_ret_col)) {
      tbl <- tbl |> tab_style(
        style = cell_text(weight = "bold", color = "#1a6b1a"),
        locations = cells_body(columns = all_of(col),
                               rows = which(d[[col]] > d[[bh_ret_col]])))
    }
  }
  if (!is.na(bh_sr_col)) {
    for (col in setdiff(sr_cols, bh_sr_col)) {
      tbl <- tbl |> tab_style(
        style = cell_text(weight = "bold", color = "#1a6b1a"),
        locations = cells_body(columns = all_of(col),
                               rows = which(d[[col]] > d[[bh_sr_col]])))
    }
  }
  if (!is.na(bh_dd_col)) {
    for (col in setdiff(dd_cols, bh_dd_col)) {
      tbl <- tbl |> tab_style(
        style = cell_text(weight = "bold", color = "#1a6b1a"),
        locations = cells_body(columns = all_of(col),
                               rows = which(d[[col]] > d[[bh_dd_col]])))
    }
  }

  tbl
}

# Sliding-window table
if (nrow(sliding_summary) > 0) {
  stbl <- build_metrics_table(
    sliding_summary,
    "Vote-Share Sizing — Sliding Window",
    "Train: 5yr. Test: next 1yr. Mean across ~15 windows.",
    sliding_summary
  )
  gtsave(stbl, sprintf("%s/sliding-metrics.html", reportPath))
  webshot2::webshot(
    sprintf("%s/sliding-metrics.html", reportPath),
    sprintf("%s/sliding-metrics.png", reportPath),
    selector = "table.gt_table", expand = c(10, 10, 10, 10), vwidth = 2000
  )
}

# Expanding-window table
if (nrow(expanding_results) > 0) {
  etbl <- build_metrics_table(
    expanding_results,
    "Vote-Share Sizing — Expanding Window",
    "2005 → date; consolidated regime.",
    expanding_results
  )
  gtsave(etbl, sprintf("%s/expanding-metrics.html", reportPath))
  webshot2::webshot(
    sprintf("%s/expanding-metrics.html", reportPath),
    sprintf("%s/expanding-metrics.png", reportPath),
    selector = "table.gt_table", expand = c(10, 10, 10, 10), vwidth = 2000
  )
}

# Combined table (sliding + expanding)
combined_all <- NULL
if (exists("sliding_summary") && nrow(sliding_summary) > 0) {
  combined_all <- sliding_summary %>%
    mutate(Window = "Sliding Window (train/test, mean across windows)")
}
if (exists("expanding_results") && nrow(expanding_results) > 0) {
  exp_df <- expanding_results %>%
    mutate(Window = "Expanding Window (2005 → date)")
  combined_all <- if (is.null(combined_all)) exp_df else bind_rows(combined_all, exp_df)
}

if (!is.null(combined_all) && nrow(combined_all) > 0) {
  d <- combined_all
  strat_cols <- setdiff(names(d), c("Index", "Windows", "Window"))
  ret_cols <- grep("_Ret$", strat_cols, value = TRUE)
  sr_cols  <- grep("_Sharpe$", strat_cols, value = TRUE)
  dd_cols  <- grep("_DD$", strat_cols, value = TRUE)
  bh_ret_col <- grep("B_H_Ret|BH_Ret", ret_cols, value = TRUE)[1]
  bh_sr_col  <- grep("B_H_Sharpe|BH_Sharpe", sr_cols, value = TRUE)[1]
  bh_dd_col  <- grep("B_H_DD|BH_DD", dd_cols, value = TRUE)[1]

  display_cols <- c("Window", "Index")
  if ("Windows" %in% names(d)) display_cols <- c(display_cols, "Windows")
  display_cols <- c(display_cols, ret_cols, sr_cols, dd_cols)

  ctbl <- d |>
    select(all_of(display_cols)) |>
    gt(groupname_col = "Window") |>
    tab_header(
      title = "Vote-Share Position Sizing — Combined Metrics",
      subtitle = "Changepoint regime vote share mapped to position size"
    ) |>
    tab_spanner(label = "Annualized Return", columns = all_of(ret_cols)) |>
    tab_spanner(label = "Sharpe Ratio",     columns = all_of(sr_cols)) |>
    tab_spanner(label = "Max Drawdown",     columns = all_of(dd_cols)) |>
    fmt_percent(columns = all_of(ret_cols), decimals = 1) |>
    fmt_percent(columns = all_of(dd_cols), decimals = 1) |>
    fmt_number(columns = all_of(sr_cols), decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_row_groups()) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = Index))

  if ("Windows" %in% names(d)) {
    ctbl <- ctbl |> fmt_number(columns = Windows, decimals = 0)
  }

  if (!is.na(bh_ret_col)) {
    for (col in setdiff(ret_cols, bh_ret_col)) {
      ctbl <- ctbl |> tab_style(
        style = cell_text(weight = "bold", color = "#1a6b1a"),
        locations = cells_body(columns = all_of(col),
                               rows = which(d[[col]] > d[[bh_ret_col]])))
    }
  }
  if (!is.na(bh_sr_col)) {
    for (col in setdiff(sr_cols, bh_sr_col)) {
      ctbl <- ctbl |> tab_style(
        style = cell_text(weight = "bold", color = "#1a6b1a"),
        locations = cells_body(columns = all_of(col),
                               rows = which(d[[col]] > d[[bh_sr_col]])))
    }
  }
  if (!is.na(bh_dd_col)) {
    for (col in setdiff(dd_cols, bh_dd_col)) {
      ctbl <- ctbl |> tab_style(
        style = cell_text(weight = "bold", color = "#1a6b1a"),
        locations = cells_body(columns = all_of(col),
                               rows = which(d[[col]] > d[[bh_dd_col]])))
    }
  }

  gtsave(ctbl, sprintf("%s/combined-metrics.html", reportPath))
  webshot2::webshot(
    sprintf("%s/combined-metrics.html", reportPath),
    sprintf("%s/combined-metrics.png", reportPath),
    selector = "table.gt_table", expand = c(10, 10, 10, 10), vwidth = 2000
  )
}

# ---- Drawdown tables ----
print("  Drawdown tables...")
# Sliding
for (iName in indices) {
  label <- gsub(" TR$", "", iName)
  idx_strats <- sliding_strats[grepl(paste0("^", label, "_"), names(sliding_strats))]
  if (length(idx_strats) == 0) next
  merged <- do.call(rbind.xts, unname(idx_strats))
  merged <- na.omit(merged)
  if (nrow(merged) < 20) next

  ddown <- table.Drawdowns(merged)
  if (is.null(ddown) || nrow(ddown) == 0) next
  tbl <- as_tibble(ddown, rownames = "Strategy")
  tbl |>
    gt() |>
    tab_header(title = paste("Drawdowns —", iName, "(sliding)")) |>
    fmt_percent(columns = Depth, decimals = 1) |>
    fmt_number(columns = c(Length, "To Trough", Recovery), decimals = 0) |>
    gtsave(sprintf("%s/%s.sliding.drawdowns.html", reportPath, iName))
  webshot2::webshot(
    sprintf("%s/%s.sliding.drawdowns.html", reportPath, iName),
    sprintf("%s/%s.sliding.drawdowns.png", reportPath, iName),
    selector = "table.gt_table", expand = c(10, 10, 10, 10)
  )
}

# Expanding
for (iName in names(expanding_strats)) {
  strat <- expanding_strats[[iName]]
  ddTb <- tibble()
  for (j in 1:ncol(strat)) {
    tdd <- table.Drawdowns(strat[, j])
    tdd_df <- as_tibble(tdd)
    tdd_df$INDEX <- sprintf("%s (%s)", iName, names(strat)[j])
    ddTb <- rbind(ddTb, tdd_df)
  }
  ddTb |>
    gt(groupname_col = "INDEX") |>
    tab_header(
      title = "Drawdowns — Expanding Window",
      subtitle = sprintf("%s: %s → %s", iName,
                         format(start(strat), "%Y-%m-%d"),
                         format(end(strat), "%Y-%m-%d"))
    ) |>
    fmt_percent(columns = Depth, decimals = 1) |>
    fmt_number(columns = c(Length, "To Trough", Recovery), decimals = 0) |>
    sub_missing(missing_text = "") |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    gtsave(sprintf("%s/%s.expanding.drawdowns.html", reportPath, iName))
  webshot2::webshot(
    sprintf("%s/%s.expanding.drawdowns.html", reportPath, iName),
    sprintf("%s/%s.expanding.drawdowns.png", reportPath, iName),
    selector = "table.gt_table", expand = c(10, 10, 10, 10)
  )
}

# ---- Cumulative return charts ----
print("  Cumulative return charts...")
# Sliding
for (iName in indices) {
  label <- gsub(" TR$", "", iName)
  idx_strats <- sliding_strats[grepl(paste0("^", label, "_"), names(sliding_strats))]
  if (length(idx_strats) == 0) next
  merged <- do.call(rbind.xts, unname(idx_strats))
  merged <- na.omit(merged)
  if (nrow(merged) < 20) next
  Common.PlotCumReturns(merged, iName,
    sprintf("Sliding (merged): %s → %s",
            format(start(merged), "%Y-%m-%d"), format(end(merged), "%Y-%m-%d")),
    sprintf("%s/%s.sliding.cumret.png", reportPath, iName), NULL)
}

# Expanding
for (iName in names(expanding_strats)) {
  strat <- expanding_strats[[iName]]
  Common.PlotCumReturns(strat, iName,
    sprintf("Expanding Window: %s → %s",
            format(start(strat), "%Y-%m-%d"), format(end(strat), "%Y-%m-%d")),
    sprintf("%s/%s.expanding.cumret.png", reportPath, iName), NULL)
}

print("=== DONE ===")

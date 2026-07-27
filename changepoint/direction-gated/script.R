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

drag  <- 0.2/100
smaLb <- 50

# ---- Strategy names (display order) ----
# B&H: always long
# SMA: long when close > 50-day MA
# CP:  long when regime = STABLE
# SMA+CP: long when BOTH SMA and CP agree
# DG (Direction-Gated): exit only when UNSTABLE AND downtrend

print("connecting to norway...")
lcon <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    ldbserver, "StockViz", ldbuser, ldbpassword
  ),
  case = "nochange", believeNRows = TRUE
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

# ---- Helper: compute all five strategy returns + positions ----
# Returns list(rets = xts, positions = xts)
compute_strategies <- function(price_xts, regime_xts, date_range,
                               sma_lb = 50, drag = 0.2/100,
                               ret_xts = NULL) {
  if (is.null(ret_xts)) {
    retL1 <- stats::lag(dailyReturn(price_xts[date_range]), -1)
  } else {
    retL1 <- stats::lag(ret_xts[date_range], -1)
  }
  pxSubset <- price_xts[date_range]

  # IMPORTANT: date-align regime_xts onto retL1's index to avoid positional recycling
  classSubset <- merge(retL1, regime_xts, join = "left")[, 2]
  smaPx <- SMA(pxSubset, sma_lb)

  # --- SMA ---
  smaPos   <- ifelse(pxSubset > smaPx, 1, 0)
  smaGross <- smaPos * retL1
  trd      <- smaPos - stats::lag(smaPos, 1)
  smaNet   <- ifelse(trd != 0, smaGross - drag, smaGross)

  # --- CP ---
  cpPos   <- ifelse(classSubset == 1, 1, 0)
  cpGross <- cpPos * retL1
  trd     <- cpPos - stats::lag(cpPos, 1)
  cpNet   <- ifelse(trd != 0, cpGross - drag, cpGross)

  # --- SMA+CP ---
  smaCpPos   <- ifelse(pxSubset > smaPx & classSubset == 1, 1, 0)
  smaCpGross <- smaCpPos * retL1
  trd        <- smaCpPos - stats::lag(smaCpPos, 1)
  smaCpNet   <- ifelse(trd != 0, smaCpGross - drag, smaCpGross)

  # --- DG: exit only when UNSTABLE AND downtrend ---
  dgPos   <- ifelse(classSubset == 0 & pxSubset < smaPx, 0, 1)
  dgGross <- dgPos * retL1
  trd     <- dgPos - stats::lag(dgPos, 1)
  dgNet   <- ifelse(trd != 0, dgGross - drag, dgGross)

  # --- B&H ---
  bhPos <- xts(rep(1, nrow(retL1)), order.by = index(retL1))

  all_rets <- na.omit(merge(smaNet, cpNet, smaCpNet, dgNet, retL1))
  names(all_rets) <- c("SMA", "CP", "SMA_CP", "DG", "B_H")

  all_pos <- na.omit(merge(smaPos, cpPos, smaCpPos, dgPos, bhPos))
  all_pos <- all_pos[index(all_rets)]   # align to same dates as returns
  names(all_pos) <- names(all_rets)

  list(rets = all_rets, positions = all_pos)
}

# ---- Helper: compute metrics from returns + positions ----
compute_metrics <- function(strat_list) {
  R <- strat_list$rets
  P <- strat_list$positions

  ann_ret  <- Return.annualized(R)
  sharpe   <- SharpeRatio.annualized(R)
  dd       <- maxDrawdown(R)
  calmar   <- ann_ret / abs(dd)
  time_in  <- colMeans(coredata(P) > 0)
  turnover <- colMeans(abs(diff(P)), na.rm = TRUE)

  list(ann_ret = ann_ret, sharpe = sharpe, max_dd = dd,
       calmar = calmar, time_in = time_in, turnover = turnover)
}

# ---- Helper: build per-window result row ----
build_result_row <- function(strat_obj, iName, test_start, test_end) {
  strat_test <- strat_obj$rets
  m <- compute_metrics(strat_obj)
  row <- tibble(Index = iName,
                Window_Start = as.character(test_start),
                Window_End   = as.character(test_end))
  for (cn in names(strat_test)) {
    row[[paste0(cn, "_Ret")]]    <- round(m$ann_ret[1, cn], 4)
    row[[paste0(cn, "_Sharpe")]] <- round(m$sharpe[1, cn], 3)
    row[[paste0(cn, "_TimeIn")]] <- round(as.numeric(m$time_in[cn]), 3)
    row[[paste0(cn, "_Tvr")]]    <- round(as.numeric(m$turnover[cn]), 4)
  }
  row
}

# ---- Helper: aggregate summary from results tibble ----
summarise_results <- function(df) {
  df |>
    group_by(Index) |>
    summarise(
      Windows = n(),
      across(matches("_Ret$"),     ~ round(mean(.x, na.rm = TRUE), 4)),
      across(matches("_Sharpe$"),  ~ round(mean(.x, na.rm = TRUE), 3)),
      across(matches("_TimeIn$"),  ~ round(mean(.x, na.rm = TRUE), 3)),
      across(matches("_Tvr$"),     ~ round(mean(.x, na.rm = TRUE), 4)),
      .groups = "drop"
    )
}

# =========================================================================
# Phase 1: Regime classification (reuse cache from historical-index)
# =========================================================================
print("=== PHASE 1: REGIME CLASSIFICATION ===")
window_days <- 365 * 5
ncores <- 4

cache_file <- sprintf("%s/window-class-cache.Rdata", reportPath)
cache_is_shared <- FALSE

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

all_dates <- index(pXts)
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
      key = cache_key, start = window_start, end = window_end, name = iName)
  }

  if (length(tasks) == 0) { cat("      all cached, skipping\n"); next }

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
      } else NULL
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

if (!needed) cat("  all classifications cached, Phase 1 skipped\n")

# =========================================================================
# Phase 2a: Sliding window
# =========================================================================
print("=== PHASE 2a: SLIDING WINDOW ===")
test_step_days <- 252L

process_sliding_index <- function(iName) {
  cache_keys <- grep(sprintf("^sliding_%s_", iName), names(window_cache),
                     value = TRUE)
  if (length(cache_keys) == 0) return(NULL)

  all_windows <- tibble()
  for (ck in cache_keys) {
    tbl <- window_cache[[ck]]
    if (is.null(tbl) || nrow(tbl) == 0) next
    all_windows <- rbind(all_windows, tibble(
      train_start = min(tbl$Date), train_end = max(tbl$Date)))
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

    # Build daily regime vector for the test year
    regime_vec <- c()
    for (j in test_start_idx:test_end_idx) {
      d <- all_dates[j]
      ws <- d - window_days + 1
      ck <- sprintf("sliding_%s_%s_%s", iName, ws, d)
      tbl <- window_cache[[ck]]
      if (is.null(tbl)) { regime_vec <- c(regime_vec, NA_integer_); next }
      row <- tbl |> filter(Date == d)
      if (nrow(row) == 0) row <- tail(tbl, 1)
      regime_vec <- c(regime_vec,
        ifelse(nrow(row) > 0 && row$Regime[1] == "STABLE", 1L, 0L))
    }
    regime_xts <- xts(regime_vec, order.by = all_dates[test_start_idx:test_end_idx])
    regime_xts <- na.locf(regime_xts, fromLast = FALSE)
    if (nrow(regime_xts) < 20) next

    strat <- tryCatch({
      compute_strategies(pXts[, iName], regime_xts, px_range,
                         sma_lb = smaLb, drag = drag,
                         ret_xts = dSymXts[, iName])
    }, error = function(e) NULL)
    if (is.null(strat) || nrow(strat$rets) < 20) next

    # Subset to test period
    strat_test <- list(
      rets      = strat$rets[test_range],
      positions = strat$positions[test_range]
    )
    if (nrow(strat_test$rets) < 20) next

    strats[[sprintf("%s_%s_%s", label, test_start, test_end)]] <- strat_test
    results_rows[[length(results_rows) + 1]] <- build_result_row(
      strat_test, iName, test_start, test_end)
  }

  list(results_rows = results_rows, strats = strats)
}

index_results <- mclapply(indices, process_sliding_index, mc.cores = ncores)

sliding_results <- tibble()
sliding_strats  <- list()
for (res in index_results) {
  if (is.null(res)) next
  for (row in res$results_rows) sliding_results <- rbind(sliding_results, row)
  for (nm in names(res$strats)) sliding_strats[[nm]] <- res$strats[[nm]]
}

sliding_summary <- summarise_results(sliding_results)
print("Sliding-window summary:")
print(sliding_summary)

# Compute max DD and Calmar from merged (concatenated) test-window series
sliding_dd <- tibble()
for (iName in indices) {
  label <- gsub(" TR$", "", iName)
  idx_strats <- sliding_strats[grepl(paste0("^", label, "_"), names(sliding_strats))]
  if (length(idx_strats) == 0) next
  merged_rets <- do.call(rbind.xts, lapply(idx_strats, `[[`, "rets"))
  merged_rets <- na.omit(merged_rets)
  if (nrow(merged_rets) < 20) next
  dd <- maxDrawdown(merged_rets)
  calmar <- Return.annualized(merged_rets) / abs(dd)
  row <- tibble(Index = iName)
  for (cn in names(merged_rets)) {
    row[[paste0(cn, "_DD")]]     <- round(-as.numeric(dd[1, cn]), 4)
    row[[paste0(cn, "_Calmar")]] <- round(as.numeric(calmar[1, cn]), 3)
  }
  sliding_dd <- rbind(sliding_dd, row)
}
print("Sliding-window merged DD/Calmar:")
print(sliding_dd)

sliding_summary <- sliding_summary %>%
  left_join(sliding_dd, by = "Index")

# =========================================================================
# Phase 2b: Expanding window
# =========================================================================
print("=== PHASE 2b: EXPANDING WINDOW ===")
full_range <- paste0(first(all_dates), "/", last(all_dates))

process_expanding_index <- function(iName) {
  regime_vec <- rep(NA_integer_, length(all_dates))
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
      regime_vec[i] <- ifelse(row$Regime[1] == "STABLE", 1L, 0L)
    }
  }

  regime_xts <- xts(regime_vec, order.by = all_dates)
  regime_xts <- na.locf(regime_xts, fromLast = FALSE)
  regime_xts <- na.omit(regime_xts)
  first_valid <- which(!is.na(coredata(regime_xts)))[1]
  regime_xts <- regime_xts[first_valid:nrow(regime_xts)]
  if (nrow(regime_xts) < 50) return(NULL)

  strat <- tryCatch({
    compute_strategies(pXts[, iName], regime_xts, full_range,
                       sma_lb = smaLb, drag = drag,
                       ret_xts = dSymXts[, iName])
  }, error = function(e) NULL)
  if (is.null(strat) || nrow(strat$rets) < 50) return(NULL)

  row <- build_result_row(strat, iName, start(strat$rets), end(strat$rets))
  list(name = iName, strat = strat, row = row)
}

expanding_index_results <- mclapply(indices, process_expanding_index,
                                     mc.cores = ncores)

expanding_results <- tibble()
expanding_strats  <- list()
for (res in expanding_index_results) {
  if (is.null(res)) next
  expanding_results <- rbind(expanding_results, res$row)
  expanding_strats[[res$name]] <- res$strat
}

expanding_summary <- expanding_results  # single-row per index, no aggregation needed

# Expanding is a single consolidated run — compute DD/Calmar from full series
for (iName in names(expanding_strats)) {
  R <- expanding_strats[[iName]]$rets
  dd <- maxDrawdown(R)
  calmar <- Return.annualized(R) / abs(dd)
  for (cn in names(R)) {
    expanding_summary[expanding_summary$Index == iName, paste0(cn, "_DD")] <-
      round(-as.numeric(dd[1, cn]), 4)
    expanding_summary[expanding_summary$Index == iName, paste0(cn, "_Calmar")] <-
      round(as.numeric(calmar[1, cn]), 3)
  }
}
print("Expanding-window results:")
print(expanding_summary)

# =========================================================================
# Phase 2c: Frozen annual model
# =========================================================================
print("=== PHASE 2c: FROZEN ANNUAL MODEL ===")

frozen_results <- tibble()
frozen_strats  <- list()

for (iName in indices) {
  cat(sprintf("  %s\n", iName))
  label <- gsub(" TR$", "", iName)

  first_train_end <- first(all_dates) + window_days - 1
  if (first_train_end > last(all_dates)) next
  anchor_dates <- all_dates[all_dates >= first_train_end]
  anchor_dates <- anchor_dates[seq(1, length(anchor_dates), by = test_step_days)]

  for (i in seq_along(anchor_dates)) {
    ad <- anchor_dates[i]
    train_start <- ad - window_days + 1
    train_end   <- ad
    ck <- sprintf("sliding_%s_%s_%s", iName, train_start, train_end)
    if (!ck %in% names(window_cache)) next

    tbl <- window_cache[[ck]]
    row <- tbl |> filter(Date == train_end)
    if (nrow(row) == 0) row <- tail(tbl, 1)
    if (nrow(row) == 0) next
    regime_at_freeze <- ifelse(row$Regime[1] == "STABLE", 1L, 0L)

    px_at_freeze <- as.numeric(pXts[train_end, iName])
    sma_hist <- SMA(pXts[paste0(train_start, "/", train_end), iName], smaLb)
    sma_at_freeze <- as.numeric(tail(sma_hist, 1))
    downtrend_at_freeze <- !is.na(sma_at_freeze) && px_at_freeze < sma_at_freeze

    test_start_idx <- which(all_dates == train_end) + 1L
    if (is.na(test_start_idx) || test_start_idx > length(all_dates)) next
    test_end_idx <- min(test_start_idx + test_step_days - 1L, length(all_dates))
    test_start <- all_dates[test_start_idx]
    test_end   <- all_dates[test_end_idx]
    test_range <- paste0(test_start, "/", test_end)
    px_range   <- paste0(first(all_dates), "/", test_end)

    retL1   <- stats::lag(dSymXts[px_range, iName], -1)
    pxSub   <- pXts[px_range, iName]
    smaPx   <- SMA(pxSub, smaLb)

    # Frozen regime: same label for every test day
    regime_frozen <- xts(rep(regime_at_freeze, nrow(retL1)), order.by = index(retL1))
    classSubset <- merge(retL1, regime_frozen, join = "left")[, 2]

    # --- SMA position (always re-evaluated daily, unfrozen) ---
    smaPos   <- ifelse(pxSub > smaPx, 1, 0)
    smaGross <- smaPos * retL1
    trd      <- smaPos - stats::lag(smaPos, 1)
    smaNet   <- ifelse(trd != 0, smaGross - drag, smaGross)

    # --- CP (frozen regime) ---
    cpPos   <- ifelse(classSubset == 1, 1, 0)
    cpGross <- cpPos * retL1
    trd     <- cpPos - stats::lag(cpPos, 1)
    cpNet   <- ifelse(trd != 0, cpGross - drag, cpGross)

    # --- SMA+CP (frozen regime) ---
    smaCpPos   <- ifelse(pxSub > smaPx & classSubset == 1, 1, 0)
    smaCpGross <- smaCpPos * retL1
    trd        <- smaCpPos - stats::lag(smaCpPos, 1)
    smaCpNet   <- ifelse(trd != 0, smaCpGross - drag, smaCpGross)

    # --- DG (frozen regime + unfrozen SMA for downtrend check) ---
    # Downtrend still uses current SMA (direction check, not regime)
    dgPos   <- ifelse(classSubset == 0 & pxSub < smaPx, 0, 1)
    dgGross <- dgPos * retL1
    trd     <- dgPos - stats::lag(dgPos, 1)
    dgNet   <- ifelse(trd != 0, dgGross - drag, dgGross)

    bhPos <- xts(rep(1, nrow(retL1)), order.by = index(retL1))

    all_rets <- na.omit(merge(smaNet, cpNet, smaCpNet, dgNet, retL1))
    names(all_rets) <- c("SMA", "CP", "SMA_CP", "DG", "B_H")
    all_pos <- na.omit(merge(smaPos, cpPos, smaCpPos, dgPos, bhPos))
    all_pos <- all_pos[index(all_rets)]
    names(all_pos) <- names(all_rets)

    all_rets_test <- all_rets[test_range]
    all_pos_test  <- all_pos[test_range]
    if (nrow(all_rets_test) < 20) next

    # Entry friction: CP and DG positions are constant over full history
    # when regime is frozen, so trd never fires. Charge entry cost on the
    # first test day if the frozen position is non-zero.
    if (regime_at_freeze == 1L) {
      # CP is always invested when regime is STABLE
      if (!is.na(all_rets_test[1, "CP"])) {
        all_rets_test[1, "CP"] <- all_rets_test[1, "CP"] - drag
      }
      # DG is always invested when regime is STABLE (never exits)
      if (!is.na(all_rets_test[1, "DG"])) {
        all_rets_test[1, "DG"] <- all_rets_test[1, "DG"] - drag
      }
    }

    strat_obj <- list(rets = all_rets_test, positions = all_pos_test)
    frozen_strats[[sprintf("%s_frozen_%s_%s", label, test_start, test_end)]] <- strat_obj
    frozen_results <- rbind(frozen_results,
      build_result_row(strat_obj, iName, test_start, test_end))
  }
}

frozen_summary <- if (nrow(frozen_results) > 0) {
  summarise_results(frozen_results)
} else tibble()

# Compute merged DD/Calmar for frozen annual
if (nrow(frozen_summary) > 0) {
  frozen_dd <- tibble()
  for (iName in indices) {
    label <- gsub(" TR$", "", iName)
    f_strats <- frozen_strats[grepl(paste0("^", label, "_frozen_"), names(frozen_strats))]
    if (length(f_strats) == 0) next
    merged_rets <- do.call(rbind.xts, lapply(f_strats, `[[`, "rets"))
    merged_rets <- na.omit(merged_rets)
    if (nrow(merged_rets) < 20) next
    dd <- maxDrawdown(merged_rets)
    calmar <- Return.annualized(merged_rets) / abs(dd)
    row <- tibble(Index = iName)
    for (cn in names(merged_rets)) {
      row[[paste0(cn, "_DD")]]     <- round(-as.numeric(dd[1, cn]), 4)
      row[[paste0(cn, "_Calmar")]] <- round(as.numeric(calmar[1, cn]), 3)
    }
    frozen_dd <- rbind(frozen_dd, row)
  }
  frozen_summary <- frozen_summary %>%
    left_join(frozen_dd, by = "Index")
  print("Frozen annual merged DD/Calmar:")
  print(frozen_dd)
}

if (nrow(frozen_summary) > 0) {
  print("Frozen annual summary:")
  print(frozen_summary)
}

# =========================================================================
# gt metric tables
# =========================================================================
show_labels <- c(
  "Ret"    = "Annualized Return",
  "Sharpe" = "Sharpe Ratio",
  "DD"     = "Max Drawdown",
  "Calmar" = "Calmar Ratio",
  "TimeIn" = "Time in Market",
  "Tvr"    = "Turnover"
)
show_suffixes <- names(show_labels)

build_gt_table <- function(df, title, subtitle, df_ref = NULL) {
  if (is.null(df_ref)) df_ref <- df
  d <- df_ref

  all_cols <- names(df)
  display_cols <- "Index"
  if ("Windows" %in% all_cols) display_cols <- c(display_cols, "Windows")

  spanner_map <- list()
  for (suf in show_suffixes) {
    cols <- grep(paste0("_", suf, "$"), all_cols, value = TRUE)
    if (length(cols) > 0) {
      display_cols <- c(display_cols, cols)
      spanner_map[[show_labels[suf]]] <- cols
    }
  }

  tbl <- df |>
    select(all_of(display_cols)) |>
    gt() |>
    tab_header(title = title, subtitle = subtitle)

  for (nm in names(spanner_map)) {
    tbl <- tbl |> tab_spanner(label = nm, columns = all_of(spanner_map[[nm]]))
  }

  for (suf in show_suffixes) {
    cols <- grep(paste0("_", suf, "$"), all_cols, value = TRUE)
    if (length(cols) == 0) next
    if (suf %in% c("Ret", "DD")) {
      tbl <- tbl |> fmt_percent(columns = all_of(cols), decimals = 1)
    } else if (suf == "TimeIn") {
      tbl <- tbl |> fmt_percent(columns = all_of(cols), decimals = 0)
    } else {
      tbl <- tbl |> fmt_number(columns = all_of(cols), decimals = 2)
    }
  }
  if ("Windows" %in% all_cols) {
    tbl <- tbl |> fmt_number(columns = Windows, decimals = 0)
  }

  tbl <- tbl |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = Index))

  # B&H beat highlighting
  bh_ret <- "B_H_Ret"; bh_sr <- "B_H_Sharpe"; bh_dd <- "B_H_DD"
  for (col in grep("_Ret$", names(d), value = TRUE)) {
    if (col == bh_ret || !bh_ret %in% names(d)) next
    rows <- which(d[[col]] > d[[bh_ret]])
    if (length(rows) > 0) tbl <- tbl |> tab_style(
      style = cell_text(weight = "bold", color = "#1a6b1a"),
      locations = cells_body(columns = all_of(col), rows = rows))
  }
  for (col in grep("_Sharpe$", names(d), value = TRUE)) {
    if (col == bh_sr || !bh_sr %in% names(d)) next
    rows <- which(d[[col]] > d[[bh_sr]])
    if (length(rows) > 0) tbl <- tbl |> tab_style(
      style = cell_text(weight = "bold", color = "#1a6b1a"),
      locations = cells_body(columns = all_of(col), rows = rows))
  }
  for (col in grep("_DD$", names(d), value = TRUE)) {
    if (col == bh_dd || !bh_dd %in% names(d)) next
    rows <- which(d[[col]] > d[[bh_dd]])
    if (length(rows) > 0) tbl <- tbl |> tab_style(
      style = cell_text(weight = "bold", color = "#1a6b1a"),
      locations = cells_body(columns = all_of(col), rows = rows))
  }
  tbl
}

webshot_save <- function(html_path, png_path) {
  webshot2::webshot(html_path, png_path,
    selector = "table.gt_table", expand = c(10, 10, 10, 10),
    vwidth = 3200, vheight = 1600)
}

if (nrow(sliding_summary) > 0) {
  s <- build_gt_table(sliding_summary,
    "Direction-Gated CP — Sliding Window",
    "Train: 5yr. Test: next 1yr. Mean across ~15 windows.", sliding_summary)
  gtsave(s, sprintf("%s/sliding-metrics.html", reportPath))
  webshot_save(sprintf("%s/sliding-metrics.html", reportPath),
               sprintf("%s/sliding-metrics.png", reportPath))
}

if (nrow(expanding_summary) > 0) {
  e <- build_gt_table(expanding_summary,
    "Direction-Gated CP — Expanding Window",
    "2005 → date; consolidated regime.", expanding_summary)
  gtsave(e, sprintf("%s/expanding-metrics.html", reportPath))
  webshot_save(sprintf("%s/expanding-metrics.html", reportPath),
               sprintf("%s/expanding-metrics.png", reportPath))
}

if (nrow(frozen_summary) > 0) {
  f <- build_gt_table(frozen_summary,
    "Direction-Gated CP — Frozen Annual Model",
    "Regime + SMA direction frozen at train_end, held for entire test year.",
    frozen_summary)
  gtsave(f, sprintf("%s/frozen-metrics.html", reportPath))
  webshot_save(sprintf("%s/frozen-metrics.html", reportPath),
               sprintf("%s/frozen-metrics.png", reportPath))
}

# Combined table
combined_all <- NULL
for (src in list(
  list(df = sliding_summary,  label = "Sliding (train/test, mean across windows)"),
  list(df = expanding_summary, label = "Expanding (2005 → date)"),
  list(df = frozen_summary,    label = "Frozen Annual (train_end freeze)")
)) {
  df <- src$df
  if (is.data.frame(df) && nrow(df) > 0) {
    df <- df %>% mutate(Window = src$label)
    combined_all <- if (is.null(combined_all)) df else bind_rows(combined_all, df)
  }
}

if (!is.null(combined_all) && nrow(combined_all) > 0) {
  all_cols <- names(combined_all)
  display_cols <- c("Window", "Index")
  if ("Windows" %in% all_cols) display_cols <- c(display_cols, "Windows")
  spanner_map <- list()
  for (suf in show_suffixes) {
    cols <- grep(paste0("_", suf, "$"), all_cols, value = TRUE)
    if (length(cols) > 0) {
      display_cols <- c(display_cols, cols)
      spanner_map[[show_labels[suf]]] <- cols
    }
  }

  ctbl <- combined_all |>
    select(all_of(display_cols)) |>
    gt(groupname_col = "Window") |>
    tab_header(
      title = "Direction-Gated CP — Combined Metrics",
      subtitle = "Exit only when UNSTABLE AND downtrend. All three methodologies."
    )

  for (nm in names(spanner_map)) {
    ctbl <- ctbl |> tab_spanner(label = nm, columns = all_of(spanner_map[[nm]]))
  }

  for (suf in show_suffixes) {
    cols <- grep(paste0("_", suf, "$"), all_cols, value = TRUE)
    if (length(cols) == 0) next
    if (suf %in% c("Ret", "DD")) {
      ctbl <- ctbl |> fmt_percent(columns = all_of(cols), decimals = 1)
    } else if (suf == "TimeIn") {
      ctbl <- ctbl |> fmt_percent(columns = all_of(cols), decimals = 0)
    } else {
      ctbl <- ctbl |> fmt_number(columns = all_of(cols), decimals = 2)
    }
  }
  if ("Windows" %in% all_cols) {
    ctbl <- ctbl |> fmt_number(columns = Windows, decimals = 0)
  }

  ctbl <- ctbl |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_row_groups()) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = Index))

  d <- combined_all
  bh_ret <- "B_H_Ret"; bh_sr <- "B_H_Sharpe"; bh_dd <- "B_H_DD"
  for (col in grep("_Ret$", names(d), value = TRUE)) {
    if (col == bh_ret || !bh_ret %in% names(d)) next
    rows <- which(d[[col]] > d[[bh_ret]])
    if (length(rows) > 0) ctbl <- ctbl |> tab_style(
      style = cell_text(weight = "bold", color = "#1a6b1a"),
      locations = cells_body(columns = all_of(col), rows = rows))
  }
  for (col in grep("_Sharpe$", names(d), value = TRUE)) {
    if (col == bh_sr || !bh_sr %in% names(d)) next
    rows <- which(d[[col]] > d[[bh_sr]])
    if (length(rows) > 0) ctbl <- ctbl |> tab_style(
      style = cell_text(weight = "bold", color = "#1a6b1a"),
      locations = cells_body(columns = all_of(col), rows = rows))
  }
  for (col in grep("_DD$", names(d), value = TRUE)) {
    if (col == bh_dd || !bh_dd %in% names(d)) next
    rows <- which(d[[col]] > d[[bh_dd]])
    if (length(rows) > 0) ctbl <- ctbl |> tab_style(
      style = cell_text(weight = "bold", color = "#1a6b1a"),
      locations = cells_body(columns = all_of(col), rows = rows))
  }

  gtsave(ctbl, sprintf("%s/combined-metrics.html", reportPath))
  webshot_save(sprintf("%s/combined-metrics.html", reportPath),
               sprintf("%s/combined-metrics.png", reportPath))
}

# =========================================================================
# Drawdown tables
# =========================================================================
print("  Drawdown tables...")
for (iName in indices) {
  label <- gsub(" TR$", "", iName)

  # Sliding
  idx_strats <- sliding_strats[grepl(paste0("^", label, "_"), names(sliding_strats))]
  if (length(idx_strats) > 0) {
    merged <- do.call(rbind.xts, lapply(idx_strats, `[[`, "rets"))
    merged <- na.omit(merged)
    if (nrow(merged) >= 20) {
      ddown <- table.Drawdowns(merged)
      if (!is.null(ddown) && nrow(ddown) > 0) {
        as_tibble(ddown, rownames = "Strategy") |> gt() |>
          tab_header(title = paste("Drawdowns —", iName, "(sliding)")) |>
          fmt_percent(columns = Depth, decimals = 1) |>
          fmt_number(columns = c(Length, "To Trough", Recovery), decimals = 0) |>
          gtsave(sprintf("%s/%s.sliding.drawdowns.html", reportPath, iName))
        webshot_save(sprintf("%s/%s.sliding.drawdowns.html", reportPath, iName),
                     sprintf("%s/%s.sliding.drawdowns.png", reportPath, iName))
      }
    }
  }

  # Expanding
  if (iName %in% names(expanding_strats)) {
    R <- expanding_strats[[iName]]$rets
    ddTb <- tibble()
    for (j in 1:ncol(R)) {
      tdd <- table.Drawdowns(R[, j])
      tdd_df <- as_tibble(tdd)
      tdd_df$INDEX <- sprintf("%s (%s)", iName, names(R)[j])
      ddTb <- rbind(ddTb, tdd_df)
    }
    ddTb |> gt(groupname_col = "INDEX") |>
      tab_header(title = "Drawdowns — Expanding Window",
        subtitle = sprintf("%s: %s → %s", iName,
          format(start(R), "%Y-%m-%d"), format(end(R), "%Y-%m-%d"))) |>
      fmt_percent(columns = Depth, decimals = 1) |>
      fmt_number(columns = c(Length, "To Trough", Recovery), decimals = 0) |>
      sub_missing(missing_text = "") |>
      tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) |>
      tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
      gtsave(sprintf("%s/%s.expanding.drawdowns.html", reportPath, iName))
    webshot_save(sprintf("%s/%s.expanding.drawdowns.html", reportPath, iName),
                 sprintf("%s/%s.expanding.drawdowns.png", reportPath, iName))
  }

  # Frozen
  f_strats <- frozen_strats[grepl(paste0("^", label, "_frozen_"), names(frozen_strats))]
  if (length(f_strats) > 0) {
    merged <- do.call(rbind.xts, lapply(f_strats, `[[`, "rets"))
    merged <- na.omit(merged)
    if (nrow(merged) >= 20) {
      ddown <- table.Drawdowns(merged)
      if (!is.null(ddown) && nrow(ddown) > 0) {
        as_tibble(ddown, rownames = "Strategy") |> gt() |>
          tab_header(title = paste("Drawdowns —", iName, "(frozen annual)")) |>
          fmt_percent(columns = Depth, decimals = 1) |>
          fmt_number(columns = c(Length, "To Trough", Recovery), decimals = 0) |>
          gtsave(sprintf("%s/%s.frozen.drawdowns.html", reportPath, iName))
        webshot_save(sprintf("%s/%s.frozen.drawdowns.html", reportPath, iName),
                     sprintf("%s/%s.frozen.drawdowns.png", reportPath, iName))
      }
    }
  }
}

# =========================================================================
# Cumulative return charts
# =========================================================================
print("  Cumulative return charts...")
for (iName in indices) {
  label <- gsub(" TR$", "", iName)

  idx_strats <- sliding_strats[grepl(paste0("^", label, "_"), names(sliding_strats))]
  if (length(idx_strats) > 0) {
    merged <- do.call(rbind.xts, lapply(idx_strats, `[[`, "rets"))
    merged <- na.omit(merged)
    if (nrow(merged) >= 20) {
      Common.PlotCumReturns(merged, iName,
        sprintf("Sliding (merged): %s → %s",
                format(start(merged), "%Y-%m-%d"), format(end(merged), "%Y-%m-%d")),
        sprintf("%s/%s.sliding.cumret.png", reportPath, iName), NULL)
    }
  }

  if (iName %in% names(expanding_strats)) {
    R <- expanding_strats[[iName]]$rets
    Common.PlotCumReturns(R, iName,
      sprintf("Expanding: %s → %s",
              format(start(R), "%Y-%m-%d"), format(end(R), "%Y-%m-%d")),
      sprintf("%s/%s.expanding.cumret.png", reportPath, iName), NULL)
  }

  f_strats <- frozen_strats[grepl(paste0("^", label, "_frozen_"), names(frozen_strats))]
  if (length(f_strats) > 0) {
    merged <- do.call(rbind.xts, lapply(f_strats, `[[`, "rets"))
    merged <- na.omit(merged)
    if (nrow(merged) >= 20) {
      Common.PlotCumReturns(merged, iName,
        sprintf("Frozen Annual (merged): %s → %s",
                format(start(merged), "%Y-%m-%d"), format(end(merged), "%Y-%m-%d")),
        sprintf("%s/%s.frozen.cumret.png", reportPath, iName), NULL)
    }
  }
}

print("=== DONE ===")

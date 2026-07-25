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

print("connecting to norway...")
lcon <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    ldbserver,
    "StockViz",
    ldbuser,
    ldbpassword
  ),
  case = "nochange",
  believeNRows = TRUE
)

startDate <- as.Date("2005-04-01")
trainEndDt <- as.Date("2015-12-01")
testRange <- "2016-01-01/2024-12-31"

#define the universe
indices <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR")

#fetch the prices
fileName <- "../common/prices_index.Rdata"
pXts <- NULL
syms <- NULL
if(file.exists(fileName)){
  print("loading prices from cache...")
  load(fileName)
  syms <- names(pXts)
} else {
  print("loading prices from database...")
  syms <- c()
  for(iName in indices){
    pDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index
                      where index_name = '%s'
                      and time_stamp >= '%s'",
                      iName, startDate))
    
    if(nrow(pDf) == 0) next
    pXts <- merge.xts(pXts, xts(pDf$px_close, pDf$time_stamp))
  }
  names(pXts) <- indices
  
  save(pXts, file = fileName)
}

#calcuate daily returns
dSymXts <- do.call(merge.xts, lapply(indices, \(x) dailyReturn(pXts[,x])))
names(dSymXts) <- indices


# ---- Helper: compute four strategy returns for a given date range ----
# price_xts: single-column xts of prices
# regime_xts: single-column xts of regime (1=STABLE, 0=UNSTABLE)
# date_range: character e.g. "2016-01-01/2024-12-31"
# ret_xts: optional pre-computed daily returns xts (avoids re-computing)
compute_strategies <- function(price_xts, regime_xts, date_range,
                               sma_lb = 50, drag = 0.2/100,
                               ret_xts = NULL) {
  if (is.null(ret_xts)) {
    retL1 <- stats::lag(dailyReturn(price_xts[date_range]), -1)
  } else {
    retL1 <- stats::lag(ret_xts[date_range], -1)
  }
  pxSubset <- price_xts[date_range]
  classSubset <- regime_xts[date_range]
  smaPx <- SMA(pxSubset, sma_lb)

  smaGross <- ifelse(pxSubset > smaPx, retL1, 0)
  trd <- ifelse(pxSubset > smaPx, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  smaNet <- ifelse(trd != 0, smaGross - drag, smaGross)

  cpGross <- ifelse(classSubset == 1, retL1, 0)
  trd <- ifelse(classSubset == 1, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  cpNet <- ifelse(trd != 0, cpGross - drag, cpGross)

  smaCpGross <- ifelse(pxSubset > smaPx & classSubset == 1, retL1, 0)
  trd <- ifelse(pxSubset > smaPx & classSubset == 1, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  smaCpNet <- ifelse(trd != 0, smaCpGross - drag, smaCpGross)

  toPlot <- na.omit(merge(smaNet, cpNet, smaCpNet, retL1))
  names(toPlot) <- c("SMA", "CP", "SMA_CP", "B&H")
  toPlot
}


smaLb <- 50
drag <- 0.2/100

# Sliding-window analysis
# =========================================================================
# Window: 5 years ending on each date  |  Slide: 1 trading day
# Phase 1: compute all regime classifications (cached, parallelized)
# Phase 2: compute strategies from cached regimes

print("=== SLIDING WINDOW ===")
window_days <- 365 * 5
ncores <- 4

cache_file <- sprintf("%s/window-class-cache.Rdata", reportPath)
window_cache <- list()
if (file.exists(cache_file)) {
  load(cache_file)
}

all_dates <- index(pXts)
n_total <- length(all_dates)

# ---- Phase 1: classify regimes (windows ending on each date) ----
print("  Phase 1: classifying regimes (parallelized)...")
for (iName in indices) {
  cat(sprintf("    %s\n", iName))

  # Build task list: all uncached (start, end) pairs
  tasks <- list()
  for (i in seq_along(all_dates)) {
    window_start <- all_dates[i] - window_days + 1
    window_end   <- all_dates[i]
    if (window_start < first(all_dates)) next

    cache_key <- sprintf("sliding_%s_%s_%s", iName, window_start, window_end)
    if (cache_key %in% names(window_cache)) next

    tasks[[length(tasks) + 1]] <- list(
      key   = cache_key,
      start = window_start,
      end   = window_end,
      name  = iName
    )
  }

  if (length(tasks) == 0) {
    cat(sprintf("      all cached, skipping\n"))
    next
  }

  # Process in batches of 500, saving after each batch
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

    # Merge results back into the master cache
    for (res in results) {
      if (!is.null(res) && !is.null(names(res))) {
        window_cache[[names(res)]] <- res[[1]]
      }
    }

    print(paste(iName, batch[[1]]$start, "to", batch[[length(batch)]]$end,
                sprintf("[batch %d/%d]", b, n_batches)))
    save(window_cache, file = cache_file)
  }
}

# ---- Phase 2: train/test sliding window (non-overlapping, parallelized per-index) ----
# Train: 5-year window → classify regime → get final regime label
# Test:  next 1-year window → apply that regime, build strategies
# Windows: [2005-2010]→test 2011, [2006-2011]→test 2012, ...
print("  Phase 2: train/test sliding windows (parallelized)...")

test_step_days <- 252L  # ~1 year of trading days

process_sliding_index <- function(iName) {
  # Find all valid 5-year training windows from cache
  cache_keys <- grep(sprintf("^sliding_%s_", iName), names(window_cache), value = TRUE)
  if (length(cache_keys) == 0) return(NULL)

  # Find the latest date in each training window, determine test period
  all_windows <- tibble()
  for (ck in cache_keys) {
    tbl <- window_cache[[ck]]
    if (is.null(tbl) || nrow(tbl) == 0) next
    train_start <- min(tbl$Date)
    train_end   <- max(tbl$Date)
    final_regime <- tail(tbl$Regime, 1)
    all_windows <- rbind(all_windows, tibble(
      train_start = train_start, train_end = train_end,
      regime = final_regime
    ))
  }
  if (nrow(all_windows) == 0) return(NULL)

  all_windows <- all_windows |>
    arrange(train_end) |>
    distinct(train_end, .keep_all = TRUE) |>
    filter(row_number() %% test_step_days == 0L)

  results_rows <- list()
  strats       <- list()

  # For each train window, test on the following 1 year
  for (wi in seq_len(nrow(all_windows))) {
    train_start <- all_windows$train_start[wi]
    train_end   <- all_windows$train_end[wi]

    # Test period: after train_end, spanning ~1 year
    test_start_idx <- which(all_dates == train_end) + 1L
    if (is.na(test_start_idx) || test_start_idx > length(all_dates)) next
    test_end_idx <- min(test_start_idx + test_step_days - 1L, length(all_dates))
    test_start <- all_dates[test_start_idx]
    test_end   <- all_dates[test_end_idx]

    test_range <- paste0(test_start, "/", test_end)
    px_range <- paste0(first(all_dates), "/", test_end)

    # Build regime vector: daily labels from 5-year lookback ending on each test date
    regime_vec <- c()
    for (j in test_start_idx:test_end_idx) {
      d <- all_dates[j]
      ws <- d - window_days + 1
      ck <- sprintf("sliding_%s_%s_%s", iName, ws, d)
      tbl <- window_cache[[ck]]
      if (is.null(tbl)) { regime_vec <- c(regime_vec, NA_integer_); next }
      row <- tbl |> filter(Date == d)
      if (nrow(row) == 0) row <- tail(tbl, 1)
      regime_vec <- c(regime_vec, ifelse(row$Regime[1] == 'STABLE', 1L, 0L))
    }
    regime_xts <- xts(regime_vec, order.by = all_dates[test_start_idx:test_end_idx])
    regime_xts <- na.locf(regime_xts, fromLast = FALSE)

    if (nrow(regime_xts) < 20) next

    strat <- tryCatch({
      compute_strategies(pXts[, iName], regime_xts, px_range,
                         sma_lb = smaLb, drag = drag,
                         ret_xts = dSymXts[, iName])
    }, error = function(e) NULL)

    if (is.null(strat) || nrow(strat) < 20) next

    # Subset to test period only
    strat_test <- strat[test_range]
    if (nrow(strat_test) < 20) next

    strats[[sprintf("%s_%s_%s", iName, test_start, test_end)]] <- strat_test

    ann_ret <- Return.annualized(strat_test)
    sharpe <- SharpeRatio.annualized(strat_test)

    results_rows[[length(results_rows) + 1]] <- tibble(
      Index         = iName,
      Window_Start  = as.character(test_start),
      Window_End    = as.character(test_end),
      SMA_Ret       = round(ann_ret[1, "SMA"], 4),
      CP_Ret        = round(ann_ret[1, "CP"], 4),
      SMA_CP_Ret    = round(ann_ret[1, "SMA_CP"], 4),
      BH_Ret        = round(ann_ret[1, "B&H"], 4),
      SMA_Sharpe    = round(sharpe[1, "SMA"], 3),
      CP_Sharpe     = round(sharpe[1, "CP"], 3),
      SMA_CP_Sharpe = round(sharpe[1, "SMA_CP"], 3),
      BH_Sharpe     = round(sharpe[1, "B&H"], 3)
    )
  }

  list(results_rows = results_rows, strats = strats)
}

index_results <- mclapply(indices, process_sliding_index, mc.cores = ncores)

# Merge results from all workers
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

# Aggregate: mean metrics per index across all test windows
sliding_summary <- sliding_results |>
  group_by(Index) |>
  summarise(
    Windows = n(),
    across(ends_with("_Ret"), ~ round(mean(.x, na.rm = TRUE), 4)),
    across(ends_with("_Sharpe"), ~ round(mean(.x, na.rm = TRUE), 3)),
    .groups = "drop"
  )

print("Sliding-window Sharpe ratios (mean across test windows):")
print(sliding_summary)

# gt table
if (nrow(sliding_summary) > 0) {
  sliding_summary |>
    gt() |>
    tab_header(
      title = "Sliding Window (train/test split)",
      subtitle = "Train: 5yr → classify regime. Test: next 1yr → apply regime. Mean across windows."
    ) |>
    tab_spanner(label = "Annualized Return", columns = ends_with("_Ret")) |>
    tab_spanner(label = "Sharpe Ratio", columns = ends_with("_Sharpe")) |>
    fmt_percent(columns = ends_with("_Ret"), decimals = 2) |>
    fmt_number(columns = c(ends_with("_Sharpe"), Windows), decimals = 2) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = Index)
    ) |>
    cols_label(
      SMA_Ret = "SMA", CP_Ret = "CP", SMA_CP_Ret = "SMA+CP", BH_Ret = "B&H",
      SMA_Sharpe = "SMA", CP_Sharpe = "CP", SMA_CP_Sharpe = "SMA+CP", BH_Sharpe = "B&H"
    ) |>
    gtsave(sprintf("%s/sliding-window-sharpe.html", reportPath))
}

webshot2::webshot(
  sprintf("%s/sliding-window-sharpe.html", reportPath),
  sprintf("%s/sliding-window-sharpe.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10)
)

# Drawdown tables: merge all test windows per index, one table per index
print("  Sliding-window drawdowns...")
for (iName in indices) {
  idx_strats <- sliding_strats[grepl(paste0("^", iName, "_"), names(sliding_strats))]
  if (length(idx_strats) == 0) next
  merged <- do.call(rbind.xts, unname(idx_strats))
  merged <- na.omit(merged)
  if (nrow(merged) < 20) next
  names(merged) <- c("SMA", "CP", "SMA_CP", "B&H")
  ddown <- table.Drawdowns(merged)
  if (is.null(ddown) || nrow(ddown) == 0) next
  tbl <- ddown |> as_tibble(rownames = "Strategy")
  tbl |>
    gt() |>
    tab_header(title = paste("Drawdowns —", iName, "(sliding test windows)")) |>
    fmt_percent(columns = -Strategy, decimals = 2) |>
    gtsave(sprintf("%s/%s.sliding.drawdowns.html", reportPath, iName))
  webshot2::webshot(
    sprintf("%s/%s.sliding.drawdowns.html", reportPath, iName),
    sprintf("%s/%s.sliding.drawdowns.png", reportPath, iName),
    selector = "table.gt_table", expand = c(10, 10, 10, 10)
  )
}

# Cumulative return charts: one merged chart per index across all test windows
print("  Sliding-window cumulative charts...")
for (iName in indices) {
  idx_strats <- sliding_strats[grepl(paste0("^", iName, "_"), names(sliding_strats))]
  if (length(idx_strats) == 0) next
  merged <- do.call(rbind.xts, unname(idx_strats))
  merged <- na.omit(merged)
  if (nrow(merged) < 20) next
  names(merged) <- c("SMA", "CP", "SMA_CP", "B&H")
  
  cum_merged <- cumprod(1 + merged)
  cum_df <- fortify(cum_merged, melt = TRUE)
  names(cum_df) <- c("Date", "Strategy", "Value")
  
  p <- ggplot(cum_df, aes(x = Date, y = Value, color = Strategy)) +
    geom_line(linewidth = 0.8) +
    scale_color_viridis_d() +
    labs(title = paste(iName, "— Sliding Test Windows (merged)"),
         x = "", y = "Cumulative Return", caption = "@StockViz") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  ggsave(sprintf("%s/%s.sliding.cumulative.png", reportPath, iName),
         p, width = 10, height = 6, dpi = 120)
}



# ---- Expanding-window analysis (parallelized per-index) ----
# Each date's regime comes from a 5-year lookback ending on that date.
# This is properly walk-forward with no lookahead bias.
print("=== EXPANDING WINDOW ===")

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

  if (is.null(strat) || nrow(strat) < 50) return(NULL)

  ann_ret <- Return.annualized(strat)
  sharpe  <- SharpeRatio.annualized(strat)

  list(
    name  = iName,
    strat = strat,
    row   = tibble(
      Index         = iName,
      SMA_Ret       = round(ann_ret[1, "SMA"], 4),
      CP_Ret        = round(ann_ret[1, "CP"], 4),
      SMA_CP_Ret    = round(ann_ret[1, "SMA_CP"], 4),
      BH_Ret        = round(ann_ret[1, "B&H"], 4),
      SMA_Sharpe    = round(sharpe[1, "SMA"], 3),
      CP_Sharpe     = round(sharpe[1, "CP"], 3),
      SMA_CP_Sharpe = round(sharpe[1, "SMA_CP"], 3),
      BH_Sharpe     = round(sharpe[1, "B&H"], 3)
    )
  )
}

expanding_index_results <- mclapply(indices, process_expanding_index, mc.cores = ncores)

# Merge results from all workers
expanding_results <- tibble()
expanding_strats  <- list()
for (res in expanding_index_results) {
  if (is.null(res)) next
  expanding_results <- rbind(expanding_results, res$row)
  expanding_strats[[res$name]] <- res$strat
}

print("Expanding-window Sharpe ratios:")
print(expanding_results)

# gt table
if (nrow(expanding_results) > 0) {
  expanding_results |>
  gt() |>
  tab_header(
    title = "Expanding Window",
    subtitle = "2005 → date; consolidated regime; annualized returns + Sharpe"
  ) |>
  tab_spanner(label = "Annualized Return", columns = ends_with("_Ret")) |>
  tab_spanner(label = "Sharpe Ratio", columns = ends_with("_Sharpe")) |>
  fmt_percent(columns = ends_with("_Ret"), decimals = 2) |>
  fmt_number(columns = ends_with("_Sharpe"), decimals = 2) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Index)
  ) |>
  cols_label(
    SMA_Ret = "SMA", CP_Ret = "CP", SMA_CP_Ret = "SMA+CP", BH_Ret = "B&H",
    SMA_Sharpe = "SMA", CP_Sharpe = "CP", SMA_CP_Sharpe = "SMA+CP", BH_Sharpe = "B&H"
  ) |>
  gtsave(sprintf("%s/expanding-window-sharpe.html", reportPath))

webshot2::webshot(
  sprintf("%s/expanding-window-sharpe.html", reportPath),
  sprintf("%s/expanding-window-sharpe.png", reportPath),
  selector = "table.gt_table",
  expand = c(10, 10, 10, 10)
)
}

# expanding-window drawdowns
print("  Expanding-window drawdowns...")
for (iName in names(expanding_strats)) {
  strat <- expanding_strats[[iName]]
  ddTb <- tibble()
  for (j in 1:ncol(strat)) {
    tdd <- table.Drawdowns(strat[, j])
    tdd[, 4] <- format(round(100 * tdd[, 4], 2), nsmall = 2)
    tdd$INDEX <- sprintf("%s (%s)", iName, names(strat)[j])
    ddTb <- rbind(ddTb, tdd)
  }

  ddHtml <- sprintf("%s/%s.expanding.drawdowns.html", reportPath, iName)
  ddImg  <- sprintf("%s/%s.expanding.drawdowns.png", reportPath, iName)

  ddTb |>
    gt(groupname_col = "INDEX") |>
    tab_header(
      title = "Drawdowns — Expanding Window",
      subtitle = sprintf("%s: %s", iName,
                         paste(format(range(index(strat))), collapse = " → "))
    ) |>
    sub_missing(missing_text = "") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    gtsave(ddHtml)

  webshot2::webshot(ddHtml, ddImg,
                    selector = "table.gt_table",
                    expand = c(10, 10, 10, 10))
}

# expanding-window cumulative returns
for (iName in names(expanding_strats)) {
  strat <- expanding_strats[[iName]]
  Common.PlotCumReturns(strat, iName,
    sprintf("Expanding Window: %s → %s",
            format(start(strat), "%Y-%m-%d"), format(end(strat), "%Y-%m-%d")),
    sprintf("%s/%s.expanding.cumret.png", reportPath, iName), NULL)
}

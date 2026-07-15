# ============================================================================
# Drawdown Recovery — Momentum Lookback Period Comparison
# Trains on data through 2019-12-31, validates on 2020-2021, tests 2022+
# Compares 1/3/6/9/12-month lookbacks during drawdown recovery periods
# ============================================================================

suppressPackageStartupMessages({
  library('RODBC')
  library('RPostgres')
  library('quantmod')
  library('PerformanceAnalytics')
  library('xts')
  library('tidyverse')
  library('lubridate')
  library('gt')
  library('webshot2')
  library('viridis')
})

options("scipen" = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "/mnt/data/blog/momentum/drawdown-formation"
BENCHMARK      <- "NIFTY500 MOMENTUM 50 TR"
SKIP_TO_PHASE4 <- file.exists(sprintf("%s/checkpoint_portfolios.rds", reportPath))
source("/mnt/hollandC/StockViz/R/config.r")

# ── Parameters (available in both checkpoint and full mode) ──
TOP_MCAP_PCT   <- 0.50
TOP_N_MOM      <- 20L
LOOKBACKS      <- c(1L, 3L, 6L, 9L, 12L)
TRANSACTION_COST <- 0.005

TRAIN_END    <- "2019-12-31"
VALID_START  <- "2020-01-01"
VALID_END    <- "2021-12-31"
TEST_START   <- "2022-01-01"

if (!SKIP_TO_PHASE4) {

lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockViz", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

pcon <- dbConnect(RPostgres::Postgres(),
  host = ldbserver2, user = ldbuser2, password = ldbpassword2, dbname = ldbname2)

# ============================================================================
# PHASE 1 — DATA FETCH
# ============================================================================

cat("\n=== PHASE 1: DATA FETCH ===\n")

cat("Fetching market caps...\n")
mcapDf <- sqlQuery(lcon, "select SYMBOL, FF_MKT_CAP_CR, TIME_STAMP from equity_misc_info
  where FF_MKT_CAP_CR is not null and TIME_STAMP >= '2005-01-01' order by TIME_STAMP")
cat(sprintf("  %d market cap rows\n", nrow(mcapDf)))
mcapDf$TIME_STAMP <- as.Date(mcapDf$TIME_STAMP)

cat(sprintf("Fetching benchmark: %s...\n", BENCHMARK))
benchDf <- sqlQuery(lcon, sprintf(
  "select TIME_STAMP, PX_CLOSE from bhav_index where INDEX_NAME='%s' order by TIME_STAMP", BENCHMARK))
benchXts <- xts(benchDf$PX_CLOSE, as.Date(benchDf$TIME_STAMP))
benchDaily <- dailyReturn(benchXts, type = "log")
cat(sprintf("  %s: %s → %s, %d rows\n", BENCHMARK,
            first(index(benchDaily)), last(index(benchDaily)), nrow(benchDaily)))

allDates <- sort(unique(mcapDf$TIME_STAMP))
minDate <- min(allDates); maxDate <- max(allDates)

pxDf <- dbGetQuery(pcon, sprintf(
  "select ticker, date_stamp, c from eod_adjusted_nse
   where date_stamp >= '%s' and date_stamp <= '%s' order by ticker, date_stamp",
  minDate, maxDate))
cat(sprintf("  %d price rows\n", nrow(pxDf)))
pxDf$date_stamp <- as.Date(pxDf$date_stamp)

cat("Fetching EQ series symbols...\n")
eqDf <- sqlQuery(lcon, sprintf(
  "select SYMBOL, TIME_STAMP from px_history
   where SERIES='EQ' and TIME_STAMP >= '%s' and TIME_STAMP <= '%s' order by TIME_STAMP",
  minDate, maxDate))
cat(sprintf("  %d EQ-series rows\n", nrow(eqDf)))
eqDf$TIME_STAMP <- as.Date(eqDf$TIME_STAMP)

odbcClose(lcon); dbDisconnect(pcon)

# Match tickers
mcap_symbols <- unique(mcapDf$SYMBOL)
px_tickers   <- unique(pxDf$ticker)
common <- intersect(mcap_symbols, px_tickers)
cat(sprintf("  MCAP: %d, PX: %d, Common: %d\n", length(mcap_symbols), length(px_tickers), length(common)))
mcapDf <- mcapDf |> filter(SYMBOL %in% common)

# ============================================================================
# PHASE 2 — BUILD DAILY RETURNS
# ============================================================================

cat("\n=== PHASE 2: BUILD DAILY RETURNS ===\n")

symbols_all <- sort(unique(mcapDf$SYMBOL))
pxLookup <- pxDf |> select(ticker, date_stamp, c) |> arrange(ticker, date_stamp)

totalRetList  <- list()
ticker_count <- 0L
for (tkr in symbols_all) {
  ticker_count <- ticker_count + 1L
  if (ticker_count %% 200 == 0) cat(sprintf("  %d/%d...\n", ticker_count, length(symbols_all)))
  tkrPx <- pxLookup |> filter(ticker == tkr) |> arrange(date_stamp)
  if (nrow(tkrPx) < 260) next
  pClose <- xts(tkrPx$c, tkrPx$date_stamp)
  totalRet <- dailyReturn(pClose, type = "log")
  totalRet <- na.omit(totalRet)
  if (nrow(totalRet) < 260) next
  totalRetList[[tkr]] <- totalRet
}
cat(sprintf("  Valid tickers: %d\n", length(totalRetList)))

# ── Save checkpoint: returns ──
saveRDS(list(mcapDf = mcapDf, eqDf = eqDf, benchDaily = benchDaily,
             totalRetList = totalRetList),
        sprintf("%s/checkpoint_returns.rds", reportPath))

# ============================================================================
# PHASE 3 — MONTHLY MOMENTUM BACKTEST (all lookbacks)
# ============================================================================

cat("\n=== PHASE 3: BACKTEST ===\n")

valid_tickers <- names(totalRetList)
# Filter to stocks with ≥500 trading days (avoids "newly listed" skips for all lookbacks)
minBars <- 500L
valid_tickers <- valid_tickers[sapply(totalRetList[valid_tickers], nrow) >= minBars]
cat(sprintf("  Filtered to ≥%d days: %d tickers\n", minBars, length(valid_tickers)))
retDates <- index(totalRetList[[valid_tickers[1]]])

monthEnds <- unique(floor_date(retDates, "month") + months(1) - days(1))
monthEnds <- monthEnds[monthEnds >= retDates[1] & monthEnds <= last(retDates)]
monthEndsActual <- sapply(monthEnds, function(d) {
  idx <- which(retDates <= d); if (length(idx) == 0) NA else retDates[max(idx)]
})
monthEndsActual <- as.Date(unique(monthEndsActual[!is.na(monthEndsActual)]))
cat(sprintf("  Month-ends: %d\n", length(monthEndsActual)))

# Build cumulative return cache for fast lookback
cat("Building cumulative return cache...\n")
cumCache <- list()
for (tkr in valid_tickers) {
  r <- totalRetList[[tkr]]
  cumCache[[tkr]] <- list(cr = exp(cumsum(coredata(r))), di = index(r))
}
cat(sprintf("  Cached: %d tickers\n", length(cumCache)))

# Fast momentum lookup using benchmark trading-day reference
momFast <- function(tkr, cache, sigDate, sd, minBars) {
  e <- cache[[tkr]]; if (is.null(e)) return(NA_real_)
  ie <- findInterval(as.numeric(sigDate), as.numeric(e$di))
  is <- findInterval(as.numeric(sd), as.numeric(e$di))
  if (length(ie) == 0L || length(is) == 0L) return(NA_real_)
  if (is.na(ie) || is.na(is)) return(NA_real_)
  if (is < 1L || ie < 1L || ie - is < minBars) return(NA_real_)
  e$cr[ie] / e$cr[is] - 1
}

# Run backtest for each lookback period
runBacktest <- function(lbMonths, label) {
  cat(sprintf("\n  --- Lookback: %d month(s) ---\n", lbMonths))
  portDaily <- NULL
  monthLog <- list()  # diagnostic
  nSkipped <- c(universe = 0L, momentum = 0L, build = 0L)

  # Start from first month-end that has enough lookback history
  firstValid <- which(monthEndsActual %m-% months(lbMonths) >= retDates[1])[1]
  if (is.na(firstValid)) return(NULL)
  for (mi in seq(firstValid, length(monthEndsActual) - 1)) {
    sigDate <- monthEndsActual[mi]
    trdDate <- monthEndsActual[mi + 1]

    # 1. Compute momentum on all valid tickers FIRST
    # Pre-compute lookback start date and min bars from benchmark
    momStart <- sigDate %m-% months(lbMonths)
    benchSub <- benchDaily[paste0(as.character(momStart), "/", as.character(sigDate))]
    minBars <- max(10L, floor(nrow(benchSub) * 0.95))  # allow 5% fewer days
    if (nrow(benchSub) < 20 && nSkipped["momentum"] <= 3)
      cat(sprintf("  DEBUG %s lb=%d: benchSub=%d rows, minBars=%d\n",
        sigDate, lbMonths, nrow(benchSub), minBars))

    momRets <- sapply(valid_tickers, momFast, cache = cumCache,
                      sigDate = sigDate, sd = momStart, minBars = minBars, simplify = TRUE)
    names(momRets) <- valid_tickers
    momRets <- momRets[!is.na(momRets)]
    if (length(momRets) < TOP_N_MOM) {
      nSkipped["momentum"] <- nSkipped["momentum"] + 1L
      if (nSkipped["momentum"] <= 5 || nSkipped["momentum"] %% 10 == 0)
        cat(sprintf("  MOM-SKIP %s: validMom=%d/%d\n", sigDate, length(momRets), length(valid_tickers)))
      next
    }

    # 2. Filter to top N% by free-float market cap, THEN pick top 20 by momentum
    mcapNow <- mcapDf |>
      filter(TIME_STAMP <= sigDate, SYMBOL %in% names(momRets)) |>
      group_by(SYMBOL) |> slice_max(TIME_STAMP, n = 1) |> ungroup() |>
      arrange(desc(FF_MKT_CAP_CR))

    eqNow <- eqDf |>
      filter(TIME_STAMP <= sigDate, SYMBOL %in% mcapNow$SYMBOL) |>
      group_by(SYMBOL) |> slice_max(TIME_STAMP, n = 1) |> ungroup() |> pull(SYMBOL)

    mcapNow <- mcapNow |> filter(SYMBOL %in% eqNow)
    nTake <- max(TOP_N_MOM, floor(nrow(mcapNow) * TOP_MCAP_PCT))
    topMcap <- head(mcapNow$SYMBOL, nTake)
    if (length(topMcap) < TOP_N_MOM) { nSkipped["universe"] <- nSkipped["universe"] + 1L; next }

    # Pick top 20 by momentum from the market-cap-filtered set
    momFiltered <- momRets[names(momRets) %in% topMcap]
    if (length(momFiltered) < TOP_N_MOM) { nSkipped["momentum"] <- nSkipped["momentum"] + 1L; next }
    top20 <- names(sort(momFiltered, decreasing = TRUE))[1:TOP_N_MOM]

    # Build portfolio
    port <- NULL
    for (tkr in top20) {
      r <- totalRetList[[tkr]]; if (is.null(r)) next
      sub <- r[paste0(as.character(sigDate + 1), "/", as.character(trdDate))]
      if (nrow(sub) < 5) next
      if (is.null(port)) { port <- sub }
      else { cd <- intersect(index(port), index(sub)); if (length(cd) < 5) next; port <- cbind(port[cd], sub[cd]) }
    }
    if (is.null(port) || ncol(port) < min(5L, TOP_N_MOM)) {
      nSkipped["build"] <- nSkipped["build"] + 1L
      monthLog[[as.character(sigDate)]] <- sprintf("SKIP(build): %d stocks in port",
                                                     if(is.null(port)) 0L else ncol(port))
      next
    }
    ew <- xts(rowMeans(coredata(port), na.rm = TRUE), order.by = index(port))
    ew[1] <- ew[1] - TRANSACTION_COST
    portDaily <- rbind(portDaily, ew)

    # Log the portfolio
    monthLog[[as.character(sigDate)]] <- sprintf("%s | OK: %d days | %s",
      sigDate, nrow(ew), paste(top20, collapse = ", "))
  }

  # Write diagnostics
  cat(sprintf("  Skipped: universe=%d momentum=%d build=%d traded=%d\n",
              nSkipped["universe"], nSkipped["momentum"], nSkipped["build"],
              length(monthLog) - sum(nSkipped)))
  writeLines(unlist(monthLog), sprintf("%s/portfolio_log_%s.txt", reportPath, label))
  portDaily
}

# Run all lookbacks
allPortfolios <- list()
for (lb in LOOKBACKS) {
  lbl <- paste0("M", lb)
  allPortfolios[[lbl]] <- runBacktest(lb, lbl)
  cat(sprintf("  %s: %d days\n", lbl, nrow(allPortfolios[[lbl]])))
}

# ── Save checkpoint: portfolios ──
saveRDS(list(allPortfolios = allPortfolios, cumCache = cumCache,
             benchDaily = benchDaily, combinedMerged = NULL,
             valid_tickers = valid_tickers,
             monthEndsActual = monthEndsActual),
        sprintf("%s/checkpoint_portfolios.rds", reportPath))

} else {
  # ── Load from checkpoint ──
  cat("\nLoading from checkpoint...\n")
  cp <- readRDS(sprintf("%s/checkpoint_portfolios.rds", reportPath))
  allPortfolios <- cp$allPortfolios
  cumCache      <- cp$cumCache
  benchDaily    <- cp$benchDaily
  valid_tickers <- cp$valid_tickers
  rm(cp)
  cat(sprintf("  Loaded %d lookbacks\n", length(allPortfolios)))
}

# ============================================================================
# PHASE 4 — TRAIN / VALIDATION / TEST SPLIT
# ============================================================================

cat("\n=== PHASE 4: TRAIN / VALIDATION / TEST ===\n")

splitData <- function(portXts, benchXts) {
  # Build per-split metrics from available data (handle NAs from outer join)
  cd <- intersect(index(portXts), index(benchXts))
  port <- portXts[cd]; bench <- benchXts[cd]
  merged <- merge(bench, port)
  colnames(merged) <- c("Bench", "Port")

  list(
    train = merged[paste0("/", TRAIN_END)],
    valid = merged[paste0(VALID_START, "/", VALID_END)],
    test  = merged[paste0(TEST_START, "/")]
  )
}

# Merge all using outer join (not intersect — different lookbacks have different start dates)
cdAll <- Reduce(union, lapply(allPortfolios, index))
cdAll <- intersect(cdAll, index(benchDaily))
combinedMerged <- benchDaily[cdAll]
for (nm in names(allPortfolios)) {
  p <- allPortfolios[[nm]]
  pd <- intersect(cdAll, index(p))
  if (length(pd) > 0) combinedMerged <- merge(combinedMerged, p[pd])
}
colnames(combinedMerged) <- c(BENCHMARK, names(allPortfolios))
combinedMerged <- combinedMerged[, c(BENCHMARK, names(allPortfolios)), drop = FALSE]

# ============================================================================
# PHASE 5 — METRICS & DRAWDOWN RECOVERY
# ============================================================================

cat("\n=== PHASE 5: METRICS ===\n")

computeMetrics <- function(rets_xts) {
  m <- list()
  for (col in colnames(rets_xts)) {
    r <- rets_xts[, col]; rv <- as.numeric(coredata(r))
    m[[col]] <- c(CAGR = Return.annualized(r)[1,1],
      Vol = sd(rv, na.rm = TRUE) * sqrt(252),
      Sharpe = SharpeRatio.annualized(r)[1,1],
      MaxDD = maxDrawdown(r),
      Calmar = Return.annualized(r)[1,1] / maxDrawdown(r))
  }
  do.call(cbind, m)
}

fm <- computeMetrics(combinedMerged)
cat("\nFull Sample:\n"); print(round(fm, 4))

# Split metrics
splits <- c("train", "valid", "test")
splitMetrics <- list()
for (s in splits) {
  # Build combined xts for this split from the full merged xts
  sub <- combinedMerged[switch(s,
    train = paste0("/", TRAIN_END),
    valid = paste0(VALID_START, "/", VALID_END),
    test  = paste0(TEST_START, "/")
  )]
  if (is.null(sub) || nrow(sub) < 60) { cat(sprintf("  %s: insufficient data\n", s)); next }

  # Compute metrics per column (na.omit each column independently)
  sm <- computeMetrics(sub)  # computeMetrics already handles NAs via na.rm=TRUE in sd
  splitMetrics[[s]] <- sm
  cat(sprintf("\n  --- %s (%d days) ---\n", s, nrow(sub)))
  print(round(sm, 4))
}

# ── Save monthly returns to CSV ──
cat("\nSaving monthly returns...\n")
saveMonthly <- function(dailyXts, name) {
  if (is.null(dailyXts) || nrow(dailyXts) < 20) return()
  monthly <- apply.monthly(dailyXts, Return.cumulative)
  index(monthly) <- as.Date(format(index(monthly), "%Y-%m-01"))
  df <- data.frame(Date = index(monthly), Return = as.numeric(coredata(monthly)))
  write.csv(df, sprintf("%s/monthly_%s.csv", reportPath, name), row.names = FALSE)
  cat(sprintf("  monthly_%s.csv (%d rows)\n", name, nrow(df)))
}
for (nm in names(allPortfolios)) {
  saveMonthly(allPortfolios[[nm]], nm)
}
saveMonthly(benchDaily, "benchmark")

# ── Also save combined daily returns ──
dailyDf <- fortify(combinedMerged, melt = FALSE)
write.csv(dailyDf, sprintf("%s/daily_all.csv", reportPath), row.names = FALSE)
cat(sprintf("  daily_all.csv (%d rows)\n", nrow(dailyDf)))
cat("\n--- Drawdown Recovery Analysis ---\n")

analyzeRecovery <- function(rets_xts, label) {
  eq <- tryCatch(cumprod(1 + coredata(rets_xts)), error = function(e) NULL)
  if (is.null(eq)) return(NULL)
  eq_xts <- xts(eq, index(rets_xts))

  dds <- tryCatch(table.Drawdowns(rets_xts), error = function(e) NULL)
  if (is.null(dds) || nrow(dds) == 0) return(NULL)

  cat(sprintf("\n  %s — Top 5 Drawdowns:\n", label))
  for (i in 1:min(5, nrow(dds))) {
    depth <- tryCatch(as.numeric(dds[i, "Depth"]), error = function(e) NA)
    if (is.na(depth)) next

    from_str <- tryCatch(as.character(dds[i, "From"]), error = function(e) "")
    to_str   <- tryCatch(as.character(dds[i, "To"]),   error = function(e) "")
    from <- tryCatch(as.Date(from_str), error = function(e) NA)
    to   <- tryCatch(as.Date(to_str),   error = function(e) NA)
    if (is.na(from) || is.na(to)) next
    if (!(from_str %in% index(eq_xts))) next

    # Recovery days
    peak_val <- as.numeric(eq_xts[from_str])
    recovery_days <- NA_real_
    for (d in index(eq_xts)) {
      d <- as.Date(d)
      if (d <= to) next
      v <- tryCatch(as.numeric(eq_xts[as.character(d)])[1], error = function(e) NA)
      if (length(v) == 0 || any(is.na(v))) next
      if (v >= peak_val) { recovery_days <- as.numeric(d - as.Date(to)); break }
    }

    recovery_slope <- if (!is.na(recovery_days) && recovery_days > 0)
      (1 + abs(depth))^(252 / recovery_days) - 1 else NA_real_

    cat(sprintf("    %s → %s  depth=%.1f%%  recovery=%s  slope=%s\n",
                from_str, to_str, depth * 100,
                if (is.na(recovery_days)) "N/A" else sprintf("%dd", recovery_days),
                if (is.na(recovery_slope)) "N/A" else sprintf("%.1f%%/yr", recovery_slope * 100)))
  }
}

for (nm in names(allPortfolios)) {
  analyzeRecovery(allPortfolios[[nm]], nm)
}

# Also benchmark
analyzeRecovery(benchDaily, BENCHMARK)

# ── Fastest Recovery Summary ──
cat("\n--- Fastest Recovery Summary ---\n")
recoveryScores <- sapply(names(allPortfolios), function(nm) {
  port <- allPortfolios[[nm]]
  dds <- tryCatch(table.Drawdowns(port), error = function(e) NULL)
  if (is.null(dds) || nrow(dds) < 1) return(NA_real_)
  # Use the Recovery column from table.Drawdowns
  rec <- dds$Recovery
  rec <- rec[!is.na(rec) & rec > 0]
  if (length(rec) > 0) mean(rec) else NA_real_  # lower = faster
})
names(recoveryScores) <- names(allPortfolios)

cat(sprintf("%-6s %12s %12s\n", "LB", "Avg Rec Days", "Score"))
for (nm in names(allPortfolios)) {
  s <- recoveryScores[nm]
  port <- allPortfolios[[nm]]
  dds <- tryCatch(table.Drawdowns(port), error = function(e) NULL)
  avgDays <- if (!is.null(dds)) {
    r <- dds$Recovery; r <- r[!is.na(r) & r > 0]
    if(length(r) > 0) round(mean(r)) else NA
  } else NA
  cat(sprintf("%-6s %12s %12s\n", nm,
    if(is.na(avgDays)) "N/A" else sprintf("%dd", avgDays),
    if(is.na(s)) "N/A" else sprintf("%.4f", s)))
}
cat(sprintf("\nFastest recovery (fewest days): %s\n\n",
  names(which.min(recoveryScores))))

# ── Per-split recovery table (gt) ──
cat("\n--- Per-Split Recovery ---\n")
splitRanges <- list(
  train = paste0("/", TRAIN_END),
  valid = paste0(VALID_START, "/", VALID_END),
  test  = paste0(TEST_START, "/")
)
allNames <- c(names(allPortfolios), "Bench")
allSeries <- c(allPortfolios, list(Bench = benchDaily))

recDf <- data.frame(Lookback = allNames, Train = NA_character_, Valid = NA_character_, Test = NA_character_,
                    stringsAsFactors = FALSE)
for (i in seq_along(allNames)) {
  for (s in names(splitRanges)) {
    port <- allSeries[[allNames[i]]][splitRanges[[s]]]
    if (is.null(port) || nrow(port) < 60) next
    dds <- tryCatch(table.Drawdowns(port), error = function(e) NULL)
    if (is.null(dds)) next
    rec <- dds$Recovery; rec <- rec[!is.na(rec) & rec > 0]
    if (length(rec) > 0) recDf[i, tools::toTitleCase(s)] <- sprintf("%dd", round(mean(rec)))
  }
}
recDf[is.na(recDf)] <- "—"

recGt <- recDf |> gt() |>
  tab_header(title = "Average Recovery Days by Period",
             subtitle = sprintf("%s → %s | top %.0f%% mcap",
                                first(index(combinedMerged)), last(index(combinedMerged)),
                                TOP_MCAP_PCT * 100)) |>
  tab_source_note("@StockViz") |>
  tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
  tab_style(cell_fill("#f0fff0"), cells_body(rows = !grepl("Bench", recDf$Lookback))) |>
  tab_style(cell_fill("#fff8e1"), cells_body(rows = grepl("Bench", recDf$Lookback)))

gtsave(recGt, sprintf("%s/recovery_metrics.html", reportPath))
webshot(sprintf("%s/recovery_metrics.html", reportPath),
        sprintf("%s/recovery_metrics.png", reportPath),
        selector = "table.gt_table", expand = c(10, 10, 10, 10))

# Also print text version
cat(sprintf("%-8s %10s %10s %10s\n", "Lookback", "Train", "Valid", "Test"))
cat(sprintf("%-8s %10s %10s %10s\n", "--------", "-----", "-----", "----"))
for (i in 1:nrow(recDf))
  cat(sprintf("%-8s %10s %10s %10s\n", recDf$Lookback[i], recDf$Train[i], recDf$Valid[i], recDf$Test[i]))

# ============================================================================
# PHASE 6 — CHARTS & TABLES
# ============================================================================

cat("\n=== PHASE 6: CHARTS ===\n")

source("/mnt/hollandC/StockViz/R/plot.common.r")

# Combined cumulative chart
srC <- sapply(colnames(combinedMerged), function(nm)
  round(SharpeRatio.annualized(combinedMerged[,nm])[1,1], 2))
Common.PlotCumReturns(combinedMerged,
  sprintf("Momentum Lookback Comparison (1–12mo)"),
  sprintf("%s → %s | SR: %s", first(index(combinedMerged)), last(index(combinedMerged)),
          paste0(colnames(combinedMerged), "=", srC, collapse = ", ")),
  sprintf("%s/cumulative_all.png", reportPath), NULL)

# ── Per-split cumulative return charts ──
for (s in names(splitRanges)) {
  sr <- splitRanges[[s]]
  sub <- combinedMerged[sr]
  if (nrow(sub) < 60) next
  lbl <- if (s == "train") paste0("≤ ", TRAIN_END)
         else if (s == "valid") paste0(VALID_START, " – ", VALID_END)
         else paste0("≥ ", TEST_START)
  srs <- sapply(colnames(sub), function(nm) round(SharpeRatio.annualized(sub[,nm])[1,1], 2))
  Common.PlotCumReturns(sub,
    sprintf("Momentum Lookbacks — %s", tools::toTitleCase(s)),
    sprintf("%s | SR: %s", lbl, paste0(colnames(sub), "=", srs, collapse = ", ")),
    sprintf("%s/cumulative_%s.png", reportPath, s), NULL)
}

# Annual returns bar chart
annAll <- apply.yearly(combinedMerged, Return.cumulative)
annDf <- fortify(annAll, melt = TRUE)
names(annDf) <- c("Year", "Lookback", "Return")
annDf$Year <- as.numeric(format(annDf$Year, "%Y"))

pAnn <- ggplot(annDf, aes(x = factor(Year), y = Return * 100, fill = Lookback)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  labs(title = "Annual Returns by Momentum Lookback",
       subtitle = sprintf("%s → %s | top %.0f%% mcap | %s",
                          first(index(combinedMerged)), last(index(combinedMerged)),
                          TOP_MCAP_PCT * 100, BENCHMARK),
       x = "", y = "Return (%)", caption = "@StockViz") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
ggsave(sprintf("%s/annual_returns.png", reportPath), pAnn, width = 12, height = 6, dpi = 120)

# Individual charts per lookback vs benchmark
for (lb in LOOKBACKS) {
  nm <- paste0("M", lb)
  port <- allPortfolios[[nm]]
  cd <- intersect(index(port), index(benchDaily))
  combo <- na.omit(merge(port[cd], benchDaily[cd]))
  colnames(combo) <- c(nm, BENCHMARK)
  sr <- sapply(colnames(combo), function(cn) round(SharpeRatio.annualized(combo[,cn])[1,1], 2))
  Common.PlotCumReturns(combo,
    sprintf("Momentum — %d-Month Lookback", lb),
    sprintf("%s → %s | SR: %s", first(index(combo)), last(index(combo)),
            paste0(colnames(combo), "=", sr, collapse = ", ")),
    sprintf("%s/cumulative_m%d.png", reportPath, lb), NULL)
}

# ── Per-split annual returns bar charts ──
for (s in names(splitRanges)) {
  sr <- splitRanges[[s]]
  sub <- combinedMerged[sr]
  if (nrow(sub) < 60) next
  annSub <- apply.yearly(sub, Return.cumulative)
  annDfSub <- fortify(annSub, melt = TRUE)
  names(annDfSub) <- c("Year", "Lookback", "Return")
  annDfSub$Year <- as.numeric(format(annDfSub$Year, "%Y"))
  lbl <- if (s == "train") paste0("≤ ", TRAIN_END)
         else if (s == "valid") paste0(VALID_START, " – ", VALID_END)
         else paste0("≥ ", TEST_START)
  pAnnSub <- ggplot(annDfSub, aes(x = factor(Year), y = Return * 100, fill = Lookback)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_viridis_d(option = "D", end = 0.9) +
    labs(title = sprintf("Annual Returns — %s", tools::toTitleCase(s)),
         subtitle = sprintf("%s | top %.0f%% mcap | %s", lbl, TOP_MCAP_PCT * 100, BENCHMARK),
         x = "", y = "Return (%)", caption = "@StockViz") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
  ggsave(sprintf("%s/annual_%s.png", reportPath, s), pAnnSub, width = 12, height = 6, dpi = 120)
}

# ── Per-split drawdown recovery analysis ──
cat("\n--- Per-Split Drawdown Recovery ---\n")
for (s in names(splitRanges)) {
  cat(sprintf("\n  === %s ===\n", tools::toTitleCase(s)))
  for (nm in names(allPortfolios)) {
    port <- allPortfolios[[nm]][splitRanges[[s]]]
    if (nrow(port) < 60) next
    analyzeRecovery(port, paste0(nm, " (", s, ")"))
  }
  bSub <- benchDaily[splitRanges[[s]]]
  if (nrow(bSub) >= 60) analyzeRecovery(bSub, paste0(BENCHMARK, " (", s, ")"))
}

# GT metrics table
metrics_to_gt <- function(fm, title, subtitle, fileBase) {
  tbl <- as.data.frame(t(fm))
  tbl$Strategy <- rownames(tbl)
  tbl <- tbl |> select(Strategy, everything())
  rownames(tbl) <- NULL
  gt_tbl <- tbl |> gt() |>
    tab_header(title = title, subtitle = subtitle) |>
    fmt_percent(columns = c(CAGR, Vol, MaxDD), decimals = 2) |>
    fmt_number(columns = c(Sharpe, Calmar), decimals = 2) |>
    tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
    tab_source_note("@StockViz") |>
    tab_style(cell_text(align = "right"), cells_source_notes())
  benchRows <- which(tbl$Strategy == BENCHMARK)
  if (length(benchRows) > 0)
    gt_tbl <- gt_tbl |> tab_style(cell_fill("#fff8e1"), cells_body(rows = benchRows))
  gt_tbl |> gtsave(sprintf("%s/%s.html", reportPath, fileBase))
}

metrics_to_gt(fm, "Momentum Lookback Comparison",
  sprintf("%s → %s | top %.0f%% mcap", first(index(combinedMerged)), last(index(combinedMerged)), TOP_MCAP_PCT * 100),
  "metrics_combined")
webshot(sprintf("%s/metrics_combined.html", reportPath),
        sprintf("%s/metrics_combined.png", reportPath),
        selector = "table.gt_table", expand = c(10, 10, 10, 10))

# Split metrics tables
for (s in names(splitMetrics)) {
  sm <- splitMetrics[[s]]
  lbl <- if (s == "train") paste0("≤ ", TRAIN_END)
         else if (s == "valid") paste0(VALID_START, " – ", VALID_END)
         else paste0("≥ ", TEST_START)
  metrics_to_gt(sm, sprintf("Momentum Lookbacks — %s", tools::toTitleCase(s)),
    sprintf("%s | top %.0f%% mcap", lbl, TOP_MCAP_PCT * 100),
    sprintf("metrics_%s", s))
  webshot(sprintf("%s/metrics_%s.html", reportPath, s),
          sprintf("%s/metrics_%s.png", reportPath, s),
          selector = "table.gt_table", expand = c(10, 10, 10, 10))
}

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n===== SUMMARY =====\n")
cat(sprintf("Data: %s → %s\n", first(index(combinedMerged)), last(index(combinedMerged))))
cat(sprintf("Lookbacks: %s months\n", paste(LOOKBACKS, collapse = ", ")))
cat(sprintf("Train: ≤ %s  |  Valid: %s – %s  |  Test: ≥ %s\n",
            TRAIN_END, VALID_START, VALID_END, TEST_START))
cat("\nFull Sample Metrics:\n")
print(round(fm, 4))
cat("\n===== END =====\n")

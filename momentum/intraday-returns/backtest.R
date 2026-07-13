# ============================================================================
# Intraday vs. Overnight Cross-Sectional Momentum
# Universe: top 50% by free-float market cap (by count)
# Signal: 12-month total return, raw ranking at month-end
# Portfolio: top 20 equal-weight, held for next month
# Returns: intraday (open→close) vs. total (close→close)
# Benchmark: NIFTY500 MOMENTUM 50 TR
# Transaction cost: 0.5%
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
})

options("scipen" = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "/mnt/data/blog/momentum/intraday-returns"
source("/mnt/hollandC/StockViz/R/config.r")

# ── DB connections ──
lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockViz", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

pcon <- dbConnect(RPostgres::Postgres(),
  host = ldbserver2, user = ldbuser2, password = ldbpassword2,
  dbname = ldbname2)

# ── Parameters ──
TOP_N_MCAP     <- 0.50   # top 50% by free-float market cap (by count)
TOP_N_MOM      <- 20L
MOM_LOOKBACK   <- 12L    # months
TRANSACTION_COST <- 0.005 # 50 bps
BENCHMARK      <- "NIFTY500 MOMENTUM 50 TR"

# ============================================================================
# PHASE 1 — DATA FETCH
# ============================================================================

cat("\n=== PHASE 1: DATA FETCH ===\n")

# ── 1a. Free-float market cap universe (SQL Server) ──
cat("Fetching market caps...\n")
mcapDf <- sqlQuery(lcon, "select SYMBOL, FF_MKT_CAP_CR, TIME_STAMP from equity_misc_info
  where FF_MKT_CAP_CR is not null and TIME_STAMP >= '2005-01-01' order by TIME_STAMP")
cat(sprintf("  %d market cap rows\n", nrow(mcapDf)))
mcapDf$TIME_STAMP <- as.Date(mcapDf$TIME_STAMP)

# ── 1b. Benchmark index (SQL Server) ──
cat(sprintf("Fetching benchmark: %s...\n", BENCHMARK))
benchDf <- sqlQuery(lcon, sprintf(
  "select TIME_STAMP, PX_CLOSE from bhav_index where INDEX_NAME='%s' order by TIME_STAMP", BENCHMARK))
benchXts <- xts(benchDf$PX_CLOSE, as.Date(benchDf$TIME_STAMP))
benchDaily <- dailyReturn(benchXts, type = "log")
cat(sprintf("  %s: %s → %s, %d rows\n", BENCHMARK,
            first(index(benchDaily)), last(index(benchDaily)), nrow(benchDaily)))

# ── 1c. Daily OHLC prices (PostgreSQL) ──
cat("Fetching daily OHLC from eod_adjusted_nse...\n")
allDates <- sort(unique(mcapDf$TIME_STAMP))
minDate <- min(allDates); maxDate <- max(allDates)
cat(sprintf("  Date range: %s → %s\n", minDate, maxDate))

pxDf <- dbGetQuery(pcon, sprintf(
  "select ticker, date_stamp, o, c from eod_adjusted_nse
   where date_stamp >= '%s' and date_stamp <= '%s'
   order by ticker, date_stamp", minDate, maxDate))
cat(sprintf("  %d price rows\n", nrow(pxDf)))
pxDf$date_stamp <- as.Date(pxDf$date_stamp)

# ── 1d. EQ series filter (px_history, SQL Server) ──
cat("Fetching EQ series symbols from px_history...\n")
eqDf <- sqlQuery(lcon, sprintf(
  "select SYMBOL, TIME_STAMP from px_history
   where SERIES='EQ' and TIME_STAMP >= '%s' and TIME_STAMP <= '%s'
   order by TIME_STAMP", minDate, maxDate))
cat(sprintf("  %d EQ-series rows\n", nrow(eqDf)))
eqDf$TIME_STAMP <- as.Date(eqDf$TIME_STAMP)

odbcClose(lcon)
dbDisconnect(pcon)

# ── 1d. Match tickers between mcap and prices ──
mcap_symbols <- unique(mcapDf$SYMBOL)
px_tickers   <- unique(pxDf$ticker)
common <- intersect(mcap_symbols, px_tickers)
cat(sprintf("  MCAP symbols: %d, Price tickers: %d, Common: %d\n",
            length(mcap_symbols), length(px_tickers), length(common)))

# Filter market cap data to common symbols only
mcapDf <- mcapDf |> filter(SYMBOL %in% common)

# ============================================================================
# PHASE 2 — BUILD DAILY RETURNS UNIVERSE
# ============================================================================

cat("\n=== PHASE 2: BUILD DAILY RETURNS ===\n")

# Pivot prices to wide xts: one column per ticker
# Build both intraday (o→c) and total (c→c) returns

# Get unique tickers and dates
symbols_all <- sort(unique(mcapDf$SYMBOL))
all_dates   <- sort(unique(pxDf$date_stamp))

cat(sprintf("  %d symbols, %d dates\n", length(symbols_all), length(all_dates)))

# Pre-compute returns per ticker efficiently
# We'll store both intraday and total returns in a list for each ticker
# then merge at the end

# First, create a lookup for prices
pxLookup <- pxDf |>
  select(ticker, date_stamp, o, c) |>
  arrange(ticker, date_stamp)

# Compute daily returns per ticker
intraRetList  <- list()
totalRetList  <- list()
overnightRetList <- list()
pxCache       <- list()

ticker_count <- 0L
for (tkr in symbols_all) {
  ticker_count <- ticker_count + 1L
  if (ticker_count %% 200 == 0) cat(sprintf("  Processing ticker %d/%d...\n", ticker_count, length(symbols_all)))

  tkrPx <- pxLookup |> filter(ticker == tkr) |> arrange(date_stamp)
  if (nrow(tkrPx) < 260) next  # need at least 1 year

  # Build price xts
  pOpen  <- xts(tkrPx$o, tkrPx$date_stamp)
  pClose <- xts(tkrPx$c, tkrPx$date_stamp)

  # Intraday: open → close
  intraRet <- (pClose / pOpen - 1)
  # Overnight: close → next open
  overnightRet <- (pOpen / stats::lag(pClose, 1) - 1)
  # Total: close → close
  totalRet <- dailyReturn(pClose, type = "log")

  # Merge on common dates
  common_ret <- na.omit(merge(intraRet, overnightRet, totalRet))
  if (nrow(common_ret) < 260) next

  intraRetList[[tkr]]      <- common_ret[, 1]
  overnightRetList[[tkr]]  <- common_ret[, 2]
  totalRetList[[tkr]]      <- common_ret[, 3]
  pxCache[[tkr]]           <- pClose
}

cat(sprintf("  Valid tickers with ≥260 days: %d\n", length(intraRetList)))

# ============================================================================
# PHASE 3 — MONTHLY MOMENTUM SELECTION + PORTFOLIO CONSTRUCTION
# ============================================================================

cat("\n=== PHASE 3: MONTHLY MOMENTUM BACKTEST ===\n")

# Build month-end date sequence
valid_tickers <- names(intraRetList)
retDates <- index(intraRetList[[valid_tickers[1]]])

monthEnds <- unique(floor_date(retDates, "month") + months(1) - days(1))
monthEnds <- monthEnds[monthEnds >= retDates[1] & monthEnds <= last(retDates)]
# Snap to nearest existing trading day
monthEndsActual <- sapply(monthEnds, function(d) {
  idx <- which(retDates <= d)
  if (length(idx) == 0) return(NA)
  retDates[max(idx)]
})
monthEndsActual <- as.Date(monthEndsActual[!is.na(monthEndsActual)])
monthEndsActual <- unique(monthEndsActual)

cat(sprintf("  Month-ends: %d\n", length(monthEndsActual)))

# For each month-end:
# 1. Get top 50% by latest available FF_MKT_CAP_CR (by count)
# 2. Compute 12-month total return per stock
# 3. Rank by raw return across universe
# 4. Pick top 20
# 5. Record next-month portfolio returns (intraday + total)

# Precompute cumulative returns for fast 12-month lookback
cat("Building cumulative return cache...\n")
cumCache <- function(retList) {
  cache <- list()
  for (tkr in names(retList)) {
    r <- retList[[tkr]]
    if (is.null(r) || nrow(r) < 200) next
    cache[[tkr]] <- list(cr = cumprod(1 + coredata(r)), di = index(r))
  }
  cache
}
ccIntra     <- cumCache(intraRetList)
ccOvernight <- cumCache(overnightRetList)
ccTotal     <- cumCache(totalRetList)
cat(sprintf("  Cached: intra=%d overnight=%d total=%d\n",
            length(ccIntra), length(ccOvernight), length(ccTotal)))

# Fast lookup: 12-month return = cr[end] / cr[start-12mo] - 1
mom12m <- function(tkr, cache, sigDate) {
  e <- cache[[tkr]]; if (is.null(e)) return(NA_real_)
  sd <- sigDate - months(MOM_LOOKBACK)
  ie <- findInterval(as.numeric(sigDate), as.numeric(e$di))
  is <- findInterval(as.numeric(sd), as.numeric(e$di))
  if (length(ie) == 0L || length(is) == 0L) return(NA_real_)
  if (is.na(ie) || is.na(is)) return(NA_real_)
  if (is < 1L || ie < 1L || ie - is < 200L) return(NA_real_)
  e$cr[ie] / e$cr[is] - 1
}
intraPortDaily <- NULL
totalPortDaily <- NULL
overnightPortDaily <- NULL
# Also track benchmark on same dates
benchAligned <- NULL

# For each month-end (skip first MOM_LOOKBACK for warm-up)
for (mi in seq(MOM_LOOKBACK + 1, length(monthEndsActual) - 1)) {
  sigDate <- monthEndsActual[mi]       # signal at month t end
  trdDate <- monthEndsActual[mi + 1]    # trade month t+1 start

  # ── Universe selection: top 50% by cumulative free-float market cap ──
  mcapNow <- mcapDf |>
    filter(TIME_STAMP <= sigDate, SYMBOL %in% valid_tickers) |>
    group_by(SYMBOL) |>
    slice_max(TIME_STAMP, n = 1) |>
    ungroup() |>
    arrange(desc(FF_MKT_CAP_CR))

  # ── EQ series filter: px_history SERIES='EQ' at sigDate ──
  eqNow <- eqDf |>
    filter(TIME_STAMP <= sigDate, SYMBOL %in% mcapNow$SYMBOL) |>
    group_by(SYMBOL) |>
    slice_max(TIME_STAMP, n = 1) |>
    ungroup() |>
    pull(SYMBOL)

  mcapNow <- mcapNow |> filter(SYMBOL %in% eqNow)

  # Take top 50% by count (not cumulative mcap)
  nTake <- max(TOP_N_MOM, floor(nrow(mcapNow) * TOP_N_MCAP))
  topMcap <- head(mcapNow$SYMBOL, nTake)
  if (length(topMcap) < TOP_N_MOM) {
    cat(sprintf("  %s: only %d stocks in top %.0f%% mcap — skipping\n", sigDate, length(topMcap), TOP_N_MCAP*100))
    next
  }

  # ── Compute 12-month returns for each type (fast cache lookup) ──
  momIntra     <- sapply(topMcap, mom12m, cache = ccIntra, sigDate = sigDate, simplify = TRUE)
  momOvernight <- sapply(topMcap, mom12m, cache = ccOvernight, sigDate = sigDate, simplify = TRUE)
  momTotal     <- sapply(topMcap, mom12m, cache = ccTotal, sigDate = sigDate, simplify = TRUE)
  names(momIntra) <- names(momOvernight) <- names(momTotal) <- topMcap

  momIntra     <- momIntra[!is.na(momIntra)]
  momOvernight <- momOvernight[!is.na(momOvernight)]
  momTotal     <- momTotal[!is.na(momTotal)]

  top20Intra     <- if (length(momIntra)     >= TOP_N_MOM) names(sort(momIntra, decreasing = TRUE))[1:TOP_N_MOM]     else character(0)
  top20Overnight <- if (length(momOvernight) >= TOP_N_MOM) names(sort(momOvernight, decreasing = TRUE))[1:TOP_N_MOM] else character(0)
  top20Total     <- if (length(momTotal)     >= TOP_N_MOM) names(sort(momTotal, decreasing = TRUE))[1:TOP_N_MOM]     else character(0)

  # ── Next-month returns ──
  nextMonthStart <- sigDate + 1
  nextMonthEnd   <- trdDate

  # Build each portfolio independently
  buildPort <- function(topList, retList) {
    port <- NULL
    for (tkr in topList) {
      r <- retList[[tkr]]; if (is.null(r)) next
      sub <- r[paste0(as.character(nextMonthStart), "/", as.character(nextMonthEnd))]
      if (nrow(sub) < 5) next
      if (is.null(port)) { port <- sub }
      else { cd <- intersect(index(port), index(sub)); if (length(cd) < 5) next; port <- cbind(port[cd], sub[cd]) }
    }
    if (is.null(port) || ncol(port) < min(5L, TOP_N_MOM)) return(NULL)
    ew <- xts(rowMeans(coredata(port), na.rm = TRUE), order.by = index(port))
    ew[1] <- ew[1] - TRANSACTION_COST
    ew
  }

  ewI <- buildPort(top20Intra, totalRetList)
  ewO <- buildPort(top20Overnight, totalRetList)
  ewT <- buildPort(top20Total, totalRetList)

  if (!is.null(ewI)) intraPortDaily <- rbind(intraPortDaily, ewI)
  if (!is.null(ewO)) overnightPortDaily <- rbind(overnightPortDaily, ewO)
  if (!is.null(ewT)) totalPortDaily <- rbind(totalPortDaily, ewT)

  if (mi %% 12 == 0)
    cat(sprintf("  %s: universe=%d stocks, top20 mom=%.1f%%..%.1f%%\n",
                sigDate, length(topMcap),
                min(momTotal[top20Total])*100, max(momTotal[top20Total])*100))
}

cat(sprintf("  Portfolio: %d intraday days, %d total days\n",
            nrow(intraPortDaily), nrow(totalPortDaily)))

# ============================================================================
# PHASE 4 — PERFORMANCE METRICS
# ============================================================================

cat("\n=== PHASE 4: PERFORMANCE METRICS ===\n")

# Align benchmark to common dates
cdIntra  <- intersect(index(intraPortDaily), index(benchDaily))
cdTotal  <- intersect(index(totalPortDaily), index(benchDaily))
cdOvernight <- intersect(index(overnightPortDaily), index(benchDaily))

intraAligned      <- intraPortDaily[cdIntra]
totalAligned      <- totalPortDaily[cdTotal]
overnightAligned  <- overnightPortDaily[cdOvernight]
benchI       <- benchDaily[cdIntra]
benchT       <- benchDaily[cdTotal]

allI  <- na.omit(merge(intraAligned, benchI))
allO  <- na.omit(merge(overnightAligned, benchDaily[cdOvernight]))
allT  <- na.omit(merge(totalAligned, benchT))
colnames(allI) <- c("Ranked by Intra",     BENCHMARK)
colnames(allO) <- c("Ranked by Overnight", BENCHMARK)
colnames(allT) <- c("Ranked by Total",     BENCHMARK)

# Combined for comparison (4 columns)
cdAll <- Reduce(intersect, list(index(allI), index(allT), index(allO)))
allCombined <- na.omit(merge(allI[cdAll, 1], allO[cdAll, 1],
                              allT[cdAll, 1], benchDaily[cdAll]))
colnames(allCombined) <- c("Ranked by Intra", "Ranked by Overnight",
                            "Ranked by Total", BENCHMARK)

compute_metrics <- function(rets_xts) {
  m <- list()
  for (col in colnames(rets_xts)) {
    r <- rets_xts[, col]; rv <- as.numeric(coredata(r))
    m[[col]] <- c(
      CAGR      = Return.annualized(r)[1,1],
      Vol       = sd(rv, na.rm = TRUE) * sqrt(252),
      Sharpe    = SharpeRatio.annualized(r)[1,1],
      MaxDD     = maxDrawdown(r),
      Calmar    = Return.annualized(r)[1,1] / maxDrawdown(r)
    )
  }
  do.call(cbind, m)
}

fmI <- compute_metrics(allI)
fmT <- compute_metrics(allT)
fmO <- compute_metrics(allO)
fmC <- compute_metrics(allCombined)

cat("\n--- Ranked by Intra vs Benchmark ---\n")
print(round(fmI, 4))
cat("\n--- Ranked by Overnight vs Benchmark ---\n")
print(round(fmO, 4))
cat("\n--- Ranked by Total vs Benchmark ---\n")
print(round(fmT, 4))
cat("\n--- Combined (common window) ---\n")
print(round(fmC, 4))

# Annual returns comparison (common window, all 5)
annCombined <- apply.yearly(allCombined, Return.cumulative)
cat("\n--- Annual Returns (all strategies, common window) ---\n")
print(round(annCombined * 100, 2))

# ============================================================================
# PHASE 5 — REPORTING
# ============================================================================

cat("\n=== PHASE 5: CHARTS & TABLES ===\n")

source("/mnt/hollandC/StockViz/R/plot.common.r")

# Cumulative chart
srC <- sapply(colnames(allCombined), function(nm) round(SharpeRatio.annualized(allCombined[,nm])[1,1], 2))
Common.PlotCumReturns(allCombined,
  "Momentum — Intra vs Overnight vs Total Returns",
  sprintf("%s → %s | SR: %s", first(index(allCombined)), last(index(allCombined)),
          paste0(colnames(allCombined), "=", srC, collapse = ", ")),
  sprintf("%s/cumulative_all.png", reportPath), NULL)

# Individual charts
srI <- sapply(colnames(allI), function(nm) round(SharpeRatio.annualized(allI[,nm])[1,1], 2))
Common.PlotCumReturns(allI,
  "Signal: Ranked by Intra | Portfolio: Total (c→c)",
  sprintf("%s → %s | SR: %s", first(index(allI)), last(index(allI)),
          paste0(colnames(allI), "=", srI, collapse = ", ")),
  sprintf("%s/cumulative_intra.png", reportPath), NULL)

srT <- sapply(colnames(allT), function(nm) round(SharpeRatio.annualized(allT[,nm])[1,1], 2))
Common.PlotCumReturns(allT,
  "Signal: Ranked by Total | Portfolio: Total (c→c)",
  sprintf("%s → %s | SR: %s", first(index(allT)), last(index(allT)),
          paste0(colnames(allT), "=", srT, collapse = ", ")),
  sprintf("%s/cumulative_total.png", reportPath), NULL)

# ── Annual returns bar chart ──
annCombined <- apply.yearly(allCombined, Return.cumulative)
annDf <- fortify(annCombined, melt = TRUE)
names(annDf) <- c("Year", "Strategy", "Return")
annDf$Year <- as.numeric(format(annDf$Year, "%Y"))

library('viridis')
pAnn <- ggplot(annDf, aes(x = factor(Year), y = Return * 100, fill = Strategy)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_viridis_d(option = "D", end = 0.85) +
  labs(title = "Annual Returns — Signal Type Comparison",
       subtitle = sprintf("%s → %s | top %.0f%% mcap | 12mo raw return | %s",
                          first(index(allCombined)), last(index(allCombined)),
                          TOP_N_MCAP*100, BENCHMARK),
       x = "", y = "Return (%)",
       caption = "@StockViz") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
ggsave(sprintf("%s/annual_returns.png", reportPath), pAnn,
       width = 12, height = 6, dpi = 120)

# ── GT tables ──
metrics_to_gt <- function(fm, title, subtitle, fileBase) {
  tbl <- as.data.frame(t(fm))
  tbl$Strategy <- rownames(tbl)
  tbl <- tbl |> select(Strategy, everything())
  rownames(tbl) <- NULL

  gt_tbl <- tbl |> gt() |>
    tab_header(title = title, subtitle = subtitle) |>
    fmt_percent(columns = c(CAGR, Vol, MaxDD), decimals = 2) |>
    fmt_number(columns = c(Sharpe, Calmar), decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

  # Color: strategy rows green, benchmark yellow
  stratRows <- which(tbl$Strategy != BENCHMARK)
  benchRows <- which(tbl$Strategy == BENCHMARK)
  if (length(stratRows) > 0)
    gt_tbl <- gt_tbl |> tab_style(style = cell_fill(color = "#f0fff0"),
                                   locations = cells_body(rows = stratRows))
  if (length(benchRows) > 0)
    gt_tbl <- gt_tbl |> tab_style(style = cell_fill(color = "#fff8e1"),
                                   locations = cells_body(rows = benchRows))

  gt_tbl |> gtsave(sprintf("%s/%s.html", reportPath, fileBase))
}

metrics_to_gt(fmC, "Momentum — Intra vs Overnight vs Total Returns",
  sprintf("%s → %s | top %.0f%% mcap | 12mo raw return | %s",
          first(index(allCombined)), last(index(allCombined)),
          TOP_N_MCAP*100, BENCHMARK), "metrics_combined")

# PNG from HTML
webshot(sprintf("%s/metrics_combined.html", reportPath),
        sprintf("%s/metrics_combined.png", reportPath),
        selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n===== SUMMARY =====\n")
cat(sprintf("Data: %s → %s\n", first(index(allCombined)), last(index(allCombined))))
cat(sprintf("Universe: top %.0f%% by FF mcap (EQ only), top %d by 12mo raw return\n", TOP_N_MCAP*100, TOP_N_MOM))
cat(sprintf("Transaction cost: %.1f%%, Benchmark: %s\n", TRANSACTION_COST * 100, BENCHMARK))
cat("\n")

cat(sprintf("Ranked by Intra:  CAGR=%.2f%% Sharpe=%.2f MaxDD=%.2f%%\n",
            fmC["CAGR",1]*100, fmC["Sharpe",1], fmC["MaxDD",1]*100))
cat(sprintf("Ranked by Overnight: CAGR=%.2f%% Sharpe=%.2f MaxDD=%.2f%%\n",
            fmC["CAGR",2]*100, fmC["Sharpe",2], fmC["MaxDD",2]*100))
cat(sprintf("Ranked by Total:     CAGR=%.2f%% Sharpe=%.2f MaxDD=%.2f%%\n",
            fmC["CAGR",3]*100, fmC["Sharpe",3], fmC["MaxDD",3]*100))
cat(sprintf("\nBenchmark (%s): CAGR=%.2f%% Sharpe=%.2f MaxDD=%.2f%%\n",
            BENCHMARK, fmC["CAGR",BENCHMARK]*100, fmC["Sharpe",BENCHMARK], fmC["MaxDD",BENCHMARK]*100))
cat("\n===== END =====\n")

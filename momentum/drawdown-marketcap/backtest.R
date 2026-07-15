# ============================================================================
# Drawdown-Marketcap Analysis — Self-Contained
# Free-float filter recovery comparison + DD-switching backtest
# No dependencies on other scripts. Reads from StockViz databases directly.
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

reportPath <- "/mnt/data/blog/momentum/drawdown-marketcap"
dir.create(reportPath, showWarnings = FALSE, recursive = TRUE)
source("/mnt/hollandC/StockViz/R/config.r")

# ── Parameters ──
BENCHMARK   <- "NIFTY500 MOMENTUM 50 TR"
LOOKBACK    <- 12L
TOP_N_MOM   <- 20L
TRANSACTION_COST <- 0.005
MCAP_FILTERS <- c(0.10, 0.20, 0.30, 0.40, 0.50)
TRAIN_END   <- "2019-12-31"
VALID_START <- "2020-01-01"
VALID_END   <- "2021-12-31"
TEST_START  <- "2022-01-01"
CHK_FILE    <- sprintf("%s/checkpoint.rds", reportPath)

# ═══════════════════════════════════════════════════════════════
# PHASE 1 — DATA FETCH (skip if checkpoint exists)
# ═══════════════════════════════════════════════════════════════

if (file.exists(CHK_FILE)) {
  cat("Loading from checkpoint...\n")
  chk <- readRDS(CHK_FILE)
  mcapDf <- chk$mcapDf; eqDf <- chk$eqDf
  benchDaily <- chk$benchDaily; totalRetList <- chk$totalRetList
  cumCache <- chk$cumCache; valid_tickers <- chk$valid_tickers
  monthEndsActual <- chk$monthEndsActual
  rm(chk)
  cat(sprintf("  Tickers: %d, Month-ends: %d\n", length(valid_tickers), length(monthEndsActual)))

} else {
  cat("=== DATA FETCH ===\n")

  lcon <- odbcDriverConnect(
    sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            ldbserver, "StockViz", ldbuser, ldbpassword),
    case = "nochange", believeNRows = TRUE)
  pcon <- dbConnect(RPostgres::Postgres(),
    host = ldbserver2, user = ldbuser2, password = ldbpassword2, dbname = ldbname2)

  cat("Market caps...\n")
  mcapDf <- sqlQuery(lcon, "select SYMBOL, FF_MKT_CAP_CR, TIME_STAMP from equity_misc_info
    where FF_MKT_CAP_CR is not null and TIME_STAMP >= '2005-01-01' order by TIME_STAMP")
  cat(sprintf("  %d rows\n", nrow(mcapDf)))
  mcapDf$TIME_STAMP <- as.Date(mcapDf$TIME_STAMP)

  cat(sprintf("Benchmark: %s...\n", BENCHMARK))
  benchDf <- sqlQuery(lcon, sprintf(
    "select TIME_STAMP, PX_CLOSE from bhav_index where INDEX_NAME='%s' order by TIME_STAMP", BENCHMARK))
  benchDaily <- dailyReturn(xts(benchDf$PX_CLOSE, as.Date(benchDf$TIME_STAMP)), type = "log")

  allDates <- sort(unique(mcapDf$TIME_STAMP))
  minDate <- min(allDates); maxDate <- max(allDates)

  pxDf <- dbGetQuery(pcon, sprintf(
    "select ticker, date_stamp, c from eod_adjusted_nse
     where date_stamp >= '%s' and date_stamp <= '%s' order by ticker, date_stamp", minDate, maxDate))
  cat(sprintf("  %d price rows\n", nrow(pxDf)))
  pxDf$date_stamp <- as.Date(pxDf$date_stamp)

  cat("EQ series...\n")
  eqDf <- sqlQuery(lcon, sprintf(
    "select SYMBOL, TIME_STAMP from px_history
     where SERIES='EQ' and TIME_STAMP >= '%s' and TIME_STAMP <= '%s' order by TIME_STAMP", minDate, maxDate))
  cat(sprintf("  %d rows\n", nrow(eqDf)))
  eqDf$TIME_STAMP <- as.Date(eqDf$TIME_STAMP)

  odbcClose(lcon); dbDisconnect(pcon)

  # Filter common symbols
  common <- intersect(unique(mcapDf$SYMBOL), unique(pxDf$ticker))
  mcapDf <- mcapDf |> filter(SYMBOL %in% common)

  # ── Build daily returns ──
  cat("\nBuilding returns...\n")
  symbols_all <- sort(unique(mcapDf$SYMBOL))
  pxLookup <- pxDf |> select(ticker, date_stamp, c) |> arrange(ticker, date_stamp)

  totalRetList <- list()
  ticker_count <- 0L
  for (tkr in symbols_all) {
    ticker_count <- ticker_count + 1L
    if (ticker_count %% 200 == 0) cat(sprintf("  %d/%d...\n", ticker_count, length(symbols_all)))
    tkrPx <- pxLookup |> filter(ticker == tkr) |> arrange(date_stamp)
    if (nrow(tkrPx) < 260) next
    pClose <- xts(tkrPx$c, tkrPx$date_stamp)
    totalRet <- na.omit(dailyReturn(pClose, type = "log"))
    if (nrow(totalRet) < 260) next
    totalRetList[[tkr]] <- totalRet
  }
  cat(sprintf("  Valid tickers: %d\n", length(totalRetList)))

  # Filter and build cache
  valid_tickers <- names(totalRetList)
  valid_tickers <- valid_tickers[sapply(totalRetList[valid_tickers], nrow) >= 500L]
  cat(sprintf("  ≥500 days: %d\n", length(valid_tickers)))

  retDates <- index(totalRetList[[valid_tickers[1]]])
  monthEnds <- unique(floor_date(retDates, "month") + months(1) - days(1))
  monthEnds <- monthEnds[monthEnds >= retDates[1] & monthEnds <= last(retDates)]
  monthEndsActual <- sapply(monthEnds, function(d) {
    idx <- which(retDates <= d); if (length(idx) == 0) NA else retDates[max(idx)]
  })
  monthEndsActual <- as.Date(unique(monthEndsActual[!is.na(monthEndsActual)]))

  cat("Building cumulative cache...\n")
  cumCache <- list()
  for (tkr in valid_tickers) {
    r <- totalRetList[[tkr]]
    cumCache[[tkr]] <- list(cr = exp(cumsum(coredata(r))), di = index(r))
  }
  cat(sprintf("  Cached: %d tickers, %d month-ends\n", length(cumCache), length(monthEndsActual)))

  saveRDS(list(mcapDf=mcapDf, eqDf=eqDf, benchDaily=benchDaily,
               totalRetList=totalRetList, cumCache=cumCache,
               valid_tickers=valid_tickers, monthEndsActual=monthEndsActual), CHK_FILE)
  cat(sprintf("  Checkpoint saved: %s\n", CHK_FILE))
}

# ═══════════════════════════════════════════════════════════════
# Momentum helper
# ═══════════════════════════════════════════════════════════════

momFast <- function(tkr, sigDate, lb) {
  e <- cumCache[[tkr]]
  if (is.null(e)) return(NA_real_)
  sd <- sigDate %m-% months(lb)
  ie <- findInterval(as.numeric(sigDate), as.numeric(e$di))
  is_val <- findInterval(as.numeric(sd), as.numeric(e$di))
  if (is_val < 1L || ie < 1L || ie - is_val < 10L) return(NA_real_)
  e$cr[ie] / e$cr[is_val] - 1
}

# ═══════════════════════════════════════════════════════════════
# Portfolio builder (reusable)
# ═══════════════════════════════════════════════════════════════

buildPortfolio <- function(mcapPct, lb, label) {
  cat(sprintf("  Building %s...\n", label))
  portDaily <- NULL
  for (mi in seq_along(monthEndsActual)) {
    sigDate <- monthEndsActual[mi]
    if (mi == length(monthEndsActual)) break
    trdDate <- monthEndsActual[mi + 1]

    benchSub <- benchDaily[paste0(as.character(sigDate %m-% months(lb)), "/", as.character(sigDate))]
    if (nrow(benchSub) < 60) next

    momRets <- sapply(valid_tickers, momFast, sigDate = sigDate, lb = lb, simplify = TRUE)
    names(momRets) <- valid_tickers; momRets <- momRets[!is.na(momRets)]
    if (length(momRets) < TOP_N_MOM) next

    mcapNow <- mcapDf |>
      filter(TIME_STAMP <= sigDate, SYMBOL %in% names(momRets)) |>
      group_by(SYMBOL) |> slice_max(TIME_STAMP, n = 1) |> ungroup() |>
      arrange(desc(FF_MKT_CAP_CR))

    eqNow <- eqDf |>
      filter(TIME_STAMP <= sigDate, SYMBOL %in% mcapNow$SYMBOL) |>
      group_by(SYMBOL) |> slice_max(TIME_STAMP, n = 1) |> ungroup() |> pull(SYMBOL)

    mcapNow <- mcapNow |> filter(SYMBOL %in% eqNow)
    nTake <- max(TOP_N_MOM, floor(nrow(mcapNow) * mcapPct))
    topMcap <- head(mcapNow$SYMBOL, nTake)
    if (length(topMcap) < TOP_N_MOM) next

    momFiltered <- momRets[names(momRets) %in% topMcap]
    if (length(momFiltered) < TOP_N_MOM) next
    top20 <- names(sort(momFiltered, decreasing = TRUE))[1:TOP_N_MOM]

    port <- NULL
    for (tkr in top20) {
      r <- totalRetList[[tkr]]
      if (is.null(r)) next
      sub <- r[paste0(as.character(sigDate + 1), "/", as.character(trdDate))]
      if (nrow(sub) < 5) next
      if (is.null(port)) { port <- sub }
      else { cd <- intersect(index(port), index(sub)); if (length(cd) < 5) next; port <- cbind(port[cd], sub[cd]) }
    }
    if (is.null(port) || ncol(port) < 5) next
    ew <- xts(rowMeans(coredata(port), na.rm = TRUE), order.by = index(port))
    ew[1] <- ew[1] - TRANSACTION_COST
    portDaily <- rbind(portDaily, ew)
  }
  cat(sprintf("    %s: %d days\n", label, nrow(portDaily)))
  portDaily
}

# ═══════════════════════════════════════════════════════════════
# ANALYSIS 1: Recovery by depth for each mcap filter
# ═══════════════════════════════════════════════════════════════

cat("\n=== ANALYSIS 1: Recovery by Depth per MCap Filter ===\n")

allPortfolios <- list()
for (pct in MCAP_FILTERS) {
  nm <- sprintf("M12_%.0fpct", pct * 100)
  allPortfolios[[nm]] <- buildPortfolio(pct, LOOKBACK, nm)
}

bucketRecovery <- function(rets) {
  eq <- cumprod(1 + coredata(rets)); n <- length(eq); events <- list()
  peak <- eq[1]; peakIdx <- 1
  for (i in 2:n) {
    if (eq[i] >= peak) {
      if (i - 1 > peakIdx) {
        troughIdx <- which.min(eq[peakIdx:(i-1)]) + peakIdx - 1
        depth <- eq[troughIdx] / peak - 1
        if (depth < -0.05)
          events[[length(events)+1]] <- c(depth=depth, recovery=i - troughIdx)
      }
      peak <- eq[i]; peakIdx <- i
    }
  }
  if (length(events) == 0) return(NULL)
  ev <- do.call(rbind, events)
  buckets <- c("≥10%", "≥15%", "≥20%", ">25%")
  res <- setNames(rep(NA_real_, 4), buckets)
  for (j in 1:4) {
    idx <- which(ev[,"depth"] <= -(0.10 + (j-1)*0.05))
    if (j == 4) idx <- which(ev[,"depth"] < -0.25)
    if (length(idx) > 0) res[j] <- round(mean(ev[idx,"recovery"]))
  }
  res
}

tv_end <- as.Date(VALID_END)
cat(sprintf("\n%-12s %8s %8s %8s %8s\n", "Filter", "≥10%", "≥15%", "≥20%", ">25%"))
cat(sprintf("%-12s %8s %8s %8s %8s\n", "------", "----", "----", "----", "----"))

for (nm in names(allPortfolios)) {
  r <- allPortfolios[[nm]]; r <- r[index(r) <= tv_end]; r <- na.omit(r)
  if (nrow(r) < 260) next
  rec <- bucketRecovery(r)
  if (is.null(rec)) next
  cat(sprintf("%-12s %8s %8s %8s %8s\n", nm,
    if(is.na(rec[1])) "—" else sprintf("%.0fd", rec[1]),
    if(is.na(rec[2])) "—" else sprintf("%.0fd", rec[2]),
    if(is.na(rec[3])) "—" else sprintf("%.0fd", rec[3]),
    if(is.na(rec[4])) "—" else sprintf("%.0fd", rec[4])))
}

b <- benchDaily[index(benchDaily) <= tv_end]; b <- na.omit(b)
if (nrow(b) >= 260) {
  brec <- bucketRecovery(b)
  cat(sprintf("%-12s %8s %8s %8s %8s\n", "Benchmark",
    if(is.na(brec[1])) "—" else sprintf("%.0fd", brec[1]),
    if(is.na(brec[2])) "—" else sprintf("%.0fd", brec[2]),
    if(is.na(brec[3])) "—" else sprintf("%.0fd", brec[3]),
    if(is.na(brec[4])) "—" else sprintf("%.0fd", brec[4])))
}

# ═══════════════════════════════════════════════════════════════
# ANALYSIS 2: DD-Switching 50% ⇄ 10% at 15% DD
# ═══════════════════════════════════════════════════════════════

cat("\n=== ANALYSIS 2: DD-Switching 50% ⇄ 10% ===\n")

m12_50 <- allPortfolios[["M12_50pct"]]
m12_10 <- allPortfolios[["M12_10pct"]]

cd <- Reduce(intersect, list(index(m12_50), index(m12_10), index(benchDaily)))
n <- length(cd)

switchDaily <- numeric(n)
eqSwitch <- 1.0; runningPeak <- 1.0; in50 <- logical(n)

for (i in seq_len(n)) {
  runningPeak <- max(runningPeak, eqSwitch)
  use50 <- eqSwitch / runningPeak - 1 > -0.15
  r <- if (use50) as.numeric(coredata(m12_50[cd[i]])) else as.numeric(coredata(m12_10[cd[i]]))
  eqSwitch <- eqSwitch * (1 + r)
  switchDaily[i] <- r; in50[i] <- use50
}

cat(sprintf("  50%% universe: %.1f%% of days\n", mean(in50) * 100))

switchXts <- xts(switchDaily, cd)
combined <- na.omit(merge(switchXts, m12_50[cd], m12_10[cd], benchDaily[cd]))
colnames(combined) <- c("DD Switch (50%⇄10%)", "M12_50pct", "M12_10pct", BENCHMARK)

computeMetrics <- function(rets_xts) {
  m <- list()
  for (col in colnames(rets_xts)) {
    r <- rets_xts[, col]; rv <- as.numeric(coredata(r))
    m[[col]] <- c(CAGR=Return.annualized(r)[1,1], Vol=sd(rv,na.rm=TRUE)*sqrt(252),
                  Sharpe=SharpeRatio.annualized(r)[1,1], MaxDD=maxDrawdown(r),
                  Calmar=Return.annualized(r)[1,1]/maxDrawdown(r))
  }
  do.call(cbind, m)
}

fm <- computeMetrics(combined)
cat("\nFull Sample:\n"); print(round(fm, 4))

for (s in c("train", "valid", "test")) {
  sr <- switch(s,
    train = paste0("/", TRAIN_END),
    valid = paste0(VALID_START, "/", VALID_END),
    test  = paste0(TEST_START, "/"))
  sub <- combined[sr]
  if (nrow(sub) < 60) next
  sm <- computeMetrics(sub)
  cat(sprintf("\n--- %s (%d days) ---\n", s, nrow(sub)))
  print(round(sm, 4))
}

# ═══════════════════════════════════════════════════════════════
# CHARTS
# ═══════════════════════════════════════════════════════════════

cat("\n=== CHARTS ===\n")
source("/mnt/hollandC/StockViz/R/plot.common.r")

srAll <- sapply(colnames(combined), function(nm)
  round(SharpeRatio.annualized(combined[,nm])[1,1], 2))
Common.PlotCumReturns(combined,
  "DD-Switching M12 Universe (50% ⇄ 10% at 15% DD)",
  sprintf("%s → %s | SR: %s", first(index(combined)), last(index(combined)),
          paste0(colnames(combined), "=", srAll, collapse = ", ")),
  sprintf("%s/cumulative_all.png", reportPath), NULL)

for (s in c("train", "valid", "test")) {
  sr <- switch(s,
    train = paste0("/", TRAIN_END),
    valid = paste0(VALID_START, "/", VALID_END),
    test  = paste0(TEST_START, "/"))
  sub <- combined[sr]
  if (nrow(sub) < 60) next
  lbl <- switch(s, train=paste0("≤ ",TRAIN_END),
                valid=paste0(VALID_START," – ",VALID_END),
                test=paste0("≥ ",TEST_START))
  srs <- sapply(colnames(sub), function(nm) round(SharpeRatio.annualized(sub[,nm])[1,1], 2))
  Common.PlotCumReturns(sub,
    sprintf("DD Switch MCap — %s", tools::toTitleCase(s)),
    sprintf("%s | SR: %s", lbl, paste0(colnames(sub), "=", srs, collapse = ", ")),
    sprintf("%s/cumulative_%s.png", reportPath, s), NULL)
}

annAll <- apply.yearly(combined, Return.cumulative)
annDf <- fortify(annAll, melt = TRUE)
names(annDf) <- c("Year", "Strategy", "Return")
annDf$Year <- as.numeric(format(annDf$Year, "%Y"))
pAnn <- ggplot(annDf, aes(x=factor(Year), y=Return*100, fill=Strategy)) +
  geom_col(position="dodge", width=0.7) + scale_fill_viridis_d(option="D", end=0.9) +
  labs(title="Annual Returns — DD-Switching Universe",
       subtitle=sprintf("%s → %s | M12, 50%%⇄10%% at 15%% DD",
                        first(index(combined)), last(index(combined))),
       x="", y="Return (%)", caption="@StockViz") +
  theme_minimal(base_size=12) +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.position="bottom",
        plot.caption=element_text(hjust=0, size=8, color="grey50"))
ggsave(sprintf("%s/annual_returns.png", reportPath), pAnn, width=12, height=6, dpi=120)

# GT metrics table
tbl <- as.data.frame(t(fm)); tbl$Strategy <- rownames(tbl)
tbl <- tbl |> select(Strategy, everything()); rownames(tbl) <- NULL
gt_tbl <- tbl |> gt() |>
  tab_header(title="DD-Switching M12 — Universe Filter Switching",
             subtitle=sprintf("%s → %s | 50%%⇄10%% at 15%% DD",
                              first(index(combined)), last(index(combined)))) |>
  fmt_percent(columns=c(CAGR,Vol,MaxDD), decimals=2) |>
  fmt_number(columns=c(Sharpe,Calmar), decimals=2) |>
  tab_style(cell_text(weight="bold"), cells_column_labels()) |>
  tab_source_note("@StockViz") |>
  tab_style(cell_fill("#f0fff0"), cells_body(rows=grepl("DD Switch", tbl$Strategy))) |>
  tab_style(cell_fill("#fff8e1"), cells_body(rows=grepl(BENCHMARK, tbl$Strategy)))
gtsave(gt_tbl, sprintf("%s/metrics.html", reportPath))
webshot(sprintf("%s/metrics.html", reportPath), sprintf("%s/metrics.png", reportPath),
        selector="table.gt_table", expand=c(10,10,10,10))

# ═══════════════════════════════════════════════════════════════

cat("\n===== SUMMARY =====\n")
cat(sprintf("DD Switch:  CAGR=%.2f%%  Sharpe=%.2f  MaxDD=%.1f%%\n",
            fm["CAGR",1]*100, fm["Sharpe",1], fm["MaxDD",1]*100))
cat(sprintf("M12 50%%:    CAGR=%.2f%%  Sharpe=%.2f  MaxDD=%.1f%%\n",
            fm["CAGR",2]*100, fm["Sharpe",2], fm["MaxDD",2]*100))
cat(sprintf("M12 10%%:    CAGR=%.2f%%  Sharpe=%.2f  MaxDD=%.1f%%\n",
            fm["CAGR",3]*100, fm["Sharpe",3], fm["MaxDD",3]*100))
cat(sprintf("Benchmark:   CAGR=%.2f%%  Sharpe=%.2f  MaxDD=%.1f%%\n",
            fm["CAGR",4]*100, fm["Sharpe",4], fm["MaxDD",4]*100))
cat("\n===== END =====\n")

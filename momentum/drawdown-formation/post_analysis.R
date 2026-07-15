# ============================================================================
# Post-Analysis: Test Set Lookback Rotation
# Reads checkpoint from backtest.R, tests strategies on the test set
# ============================================================================

suppressPackageStartupMessages({
  library('xts')
  library('PerformanceAnalytics')
  library('tidyverse')
  library('lubridate')
  library('viridis')
})

options("scipen" = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "/mnt/data/blog/momentum/drawdown-formation"

# ── Parameters ──
BENCHMARK      <- "NIFTY500 MOMENTUM 50 TR"
TRAIN_END      <- "2019-12-31"
VALID_START    <- "2020-01-01"
VALID_END      <- "2021-12-31"
TEST_START     <- "2022-01-01"
LOOKBACKS      <- c(1L, 3L, 6L, 9L, 12L)

# ── Load checkpoint ──
cat("Loading checkpoint...\n")
cp <- readRDS(sprintf("%s/checkpoint_portfolios.rds", reportPath))
allPortfolios <- cp$allPortfolios
benchDaily    <- cp$benchDaily
cumCache      <- cp$cumCache
rm(cp)
cat(sprintf("  Loaded %d lookbacks\n", length(allPortfolios)))

# ── Build combined xts for test set ──
cdAll <- Reduce(union, lapply(allPortfolios, index))
cdAll <- intersect(cdAll, index(benchDaily))
combined <- benchDaily[cdAll]
for (nm in names(allPortfolios)) {
  p <- allPortfolios[[nm]]
  pd <- intersect(cdAll, index(p))
  if (length(pd) > 0) combined <- merge(combined, p[pd])
}
colnames(combined) <- c(BENCHMARK, names(allPortfolios))

testCombined <- combined[paste0(TEST_START, "/")]
cat(sprintf("Test set: %s → %s, %d days\n",
            first(index(testCombined)), last(index(testCombined)), nrow(testCombined)))

# ═══════════════════════════════════════════════════════════════
# 1. Monthly Returns Chart
# ═══════════════════════════════════════════════════════════════
cat("\n=== Monthly Returns Chart ===\n")
monTest <- apply.monthly(testCombined, Return.cumulative)
monDf <- fortify(monTest, melt = TRUE)
names(monDf) <- c("Month", "Lookback", "Return")
monDf$Month <- as.Date(monDf$Month)

pMon <- ggplot(monDf, aes(x = Month, y = Return * 100, fill = Lookback)) +
  geom_col(position = "dodge", width = 20) +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  labs(title = "Monthly Returns — Test Set",
       subtitle = sprintf("≥ %s | M1 vs M3 vs M6 vs M9 vs M12 vs %s", TEST_START, BENCHMARK),
       x = "", y = "Return (%)", caption = "@StockViz") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
ggsave(sprintf("%s/monthly_returns_test.png", reportPath), pMon, width = 14, height = 6, dpi = 120)

# ═══════════════════════════════════════════════════════════════
# 2. Lookback Rotation Strategy
#    At month-end t: pick lookback with best return in month t
#    Invest in that lookback for month t+1
# ═══════════════════════════════════════════════════════════════
cat("\n=== Rotation Strategy ===\n")

monthlyRets <- monTest  # monthly returns for each lookback + benchmark
monthlyDates <- index(monthlyRets)

# Build rotation signal: for each month, pick the best lookback (excluding benchmark)
lookbackNames <- names(allPortfolios)
bestLB <- character(length(monthlyDates) - 1)
for (i in seq_len(length(monthlyDates) - 1)) {
  rets <- as.numeric(coredata(monthlyRets[i, lookbackNames]))
  bestLB[i] <- lookbackNames[which.max(rets)]
}

# Build rotation portfolio: month[i]'s return → use best from month[i-1]
rotationDaily <- NULL
for (i in seq_len(length(monthlyDates) - 1)) {
  lb <- bestLB[i]
  # Month t's best lookback is invested in month t+1
  nextMonthStart <- monthlyDates[i]
  nextMonthEnd   <- monthlyDates[i + 1]
  port <- allPortfolios[[lb]]
  sub <- port[paste0(as.character(nextMonthStart), "/", as.character(nextMonthEnd))]
  if (is.null(sub) || nrow(sub) < 5) next
  rotationDaily <- rbind(rotationDaily, sub)
}

# Also compute equally-weighted blend for comparison
blendDaily <- NULL
for (nm in lookbackNames) {
  port <- allPortfolios[[nm]]
  cd <- intersect(index(port), index(combined))
  if (is.null(blendDaily)) {
    blendDaily <- port[cd]
  } else {
    blendDaily <- merge(blendDaily, port[cd])
  }
}
blendDaily <- na.omit(blendDaily)
colnames(blendDaily) <- lookbackNames
blendEW <- xts(rowMeans(coredata(blendDaily), na.rm = TRUE), order.by = index(blendDaily))
blendEW <- blendEW[paste0(TEST_START, "/")]

# ── Merge with benchmark ──
cdRot  <- intersect(index(rotationDaily), index(benchDaily))
cdBlend <- intersect(index(blendEW), index(benchDaily))
cdAll2  <- intersect(cdRot, cdBlend)

testFinal <- na.omit(merge(
  rotationDaily[cdAll2],
  blendEW[cdAll2],
  benchDaily[cdAll2]
))
colnames(testFinal) <- c("Best Prior Month", "Equal Blend", BENCHMARK)

# Metrics
computeMetrics <- function(rets_xts) {
  m <- list()
  for (col in colnames(rets_xts)) {
    r <- rets_xts[, col]; rv <- as.numeric(coredata(r))
    m[[col]] <- c(CAGR = Return.annualized(r)[1,1],
      Vol = sd(rv, na.rm = TRUE) * sqrt(252),
      Sharpe = SharpeRatio.annualized(r)[1,1],
      MaxDD = maxDrawdown(r))
  }
  do.call(cbind, m)
}

fmRot <- computeMetrics(testFinal)
cat("\nRotation Strategy (Test Set):\n")
print(round(fmRot, 4))

# Cumulative chart
source("/mnt/hollandC/StockViz/R/plot.common.r")
srRot <- sapply(colnames(testFinal), function(nm)
  round(SharpeRatio.annualized(testFinal[,nm])[1,1], 2))
Common.PlotCumReturns(testFinal,
  "Lookback Rotation — Best Prior Month",
  sprintf("%s → %s | SR: %s", first(index(testFinal)), last(index(testFinal)),
          paste0(colnames(testFinal), "=", srRot, collapse = ", ")),
  sprintf("%s/rotation_cumulative.png", reportPath), NULL)

# ── Also show annual returns for rotation ──
annRot <- apply.yearly(testFinal, Return.cumulative)
annRotDf <- fortify(annRot, melt = TRUE)
names(annRotDf) <- c("Year", "Strategy", "Return")
annRotDf$Year <- as.numeric(format(annRotDf$Year, "%Y"))
pAnnRot <- ggplot(annRotDf, aes(x = factor(Year), y = Return * 100, fill = Strategy)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  labs(title = "Annual Returns — Rotation Strategy (Test Set)",
       subtitle = sprintf("≥ %s | Best prior month lookback", TEST_START),
       x = "", y = "Return (%)", caption = "@StockViz") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
ggsave(sprintf("%s/rotation_annual.png", reportPath), pAnnRot, width = 10, height = 6, dpi = 120)

# ── How often was each lookback picked? ──
cat("\nLookback Selection Frequency:\n")
lbFreq <- table(factor(bestLB, levels = lookbackNames))
print(lbFreq)
cat(sprintf("Total months: %d\n", length(bestLB)))

# ═══════════════════════════════════════════════════════════════
# 3. Scatter: median momentum signal of ALL qualifying stocks vs next-month return
# ═══════════════════════════════════════════════════════════════
cat("\n=== Scatter: Universe Median Signal vs Forward Return ===\n")

for (lb in LOOKBACKS) {
  nm <- paste0("M", lb)
  logFile <- sprintf("%s/portfolio_log_%s.txt", reportPath, nm)
  if (!file.exists(logFile)) { cat(sprintf("  %s: log not found\n", nm)); next }

  lines <- readLines(logFile)
  monthlyRets <- apply.monthly(allPortfolios[[nm]][paste0(TEST_START, "/")], Return.cumulative)
  monIdx <- as.character(index(monthlyRets))

  # Collect all unique signal dates from log
  sigDates <- c()
  for (line in lines) {
    if (grepl("^\\d{4}-\\d{2}-\\d{2} \\| OK:", line))
      sigDates <- c(sigDates, sub(" \\|.*", "", line))
  }
  sigDates <- unique(as.Date(sigDates))
  cat(sprintf("  %s: %d signal dates\n", nm, length(sigDates)))

  # Precompute median momentum for all dates at once (sample 500 stocks)
  cat(sprintf("  %s: %d signal dates, sampling 500 stocks...\n", nm, length(sigDates)))
  sampleTickers <- sample(names(cumCache), min(500L, length(cumCache)))
  medMomByDate <- list()
  for (sd_idx in seq_along(sigDates)) {
    sd <- sigDates[sd_idx]
    moms <- sapply(sampleTickers, function(tkr) {
      e <- cumCache[[tkr]]
      if (is.null(e)) return(NA)
      startDate <- sd %m-% months(lb)
      ie <- findInterval(as.numeric(sd), as.numeric(e$di))
      is_val <- findInterval(as.numeric(startDate), as.numeric(e$di))
      if (is_val < 1L || ie < 1L || ie - is_val < 10L) return(NA)
      e$cr[ie] / e$cr[is_val] - 1
    }, simplify = TRUE)
    medMomByDate[[as.character(sd)]] <- median(moms, na.rm = TRUE)
  }

  # Build scatter data
  xVals <- c(); yVals <- c()
  for (line in lines) {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2} \\| OK:", line)) next
    sigDate <- as.character(as.Date(sub(" \\|.*", "", line)))
    medMom <- medMomByDate[[sigDate]]
    if (is.null(medMom) || is.na(medMom)) next

    nextMon <- as.character(as.Date(sigDate) %m+% months(1))
    fwdIdx <- which(monIdx >= nextMon)[1]
    if (is.na(fwdIdx)) next

    xVals <- c(xVals, medMom)
    yVals <- c(yVals, as.numeric(coredata(monthlyRets[fwdIdx])))
  }

  if (length(xVals) < 5) { cat(sprintf("  %s: insufficient data\n", nm)); next }
  df <- data.frame(MedMom = xVals * 100, FwdRet = yVals * 100)
  r2 <- summary(lm(FwdRet ~ MedMom, data = df))$r.squared

  p <- ggplot(df, aes(x = MedMom, y = FwdRet)) +
    geom_point(alpha = 0.7, size = 2.5, color = viridis(1, option = "D")) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    labs(title = sprintf("Universe Median Signal → %s Forward Return (Test Set)", nm),
         subtitle = sprintf("R² = %.3f | %d months | ≥ %s", r2, nrow(df), TEST_START),
         x = sprintf("Median %d-Month Signal of All Qualifying Stocks (%%)", lb),
         y = sprintf("%s Next-Month Return (%%)", nm),
         caption = "@StockViz") +
    theme_minimal(base_size = 12)
  ggsave(sprintf("%s/scatter_%s.png", reportPath, tolower(nm)), p, width = 7, height = 6, dpi = 120)
  cat(sprintf("  %-6s R²=%.3f  n=%d\n", nm, r2, nrow(df)))
}

# ═══════════════════════════════════════════════════════════════
# 4. Scatter: % positive stocks in universe vs next-month return
# ═══════════════════════════════════════════════════════════════
cat("\n=== Scatter: % Positive Stocks vs Forward Return ===\n")

for (lb in LOOKBACKS) {
  nm <- paste0("M", lb)
  logFile <- sprintf("%s/portfolio_log_%s.txt", reportPath, nm)
  if (!file.exists(logFile)) next
  lines <- readLines(logFile)
  monthlyRets <- apply.monthly(allPortfolios[[nm]][paste0(TEST_START, "/")], Return.cumulative)
  monIdx <- as.character(index(monthlyRets))

  sigDates <- unique(as.Date(sub(" \\|.*", "", grep("^\\d{4}-\\d{2}-\\d{2} \\| OK:", lines, value=TRUE))))
  if (length(sigDates) == 0) next
  sampleTickers <- sample(names(cumCache), min(500L, length(cumCache)))

  pctPosByDate <- list()
  for (i in seq_along(sigDates)) {
    sd <- sigDates[i]
    moms <- sapply(sampleTickers, function(tkr) {
      e <- cumCache[[tkr]]
      if (is.null(e)) return(NA)
      startDate <- sd %m-% months(lb)
      ie <- findInterval(as.numeric(sd), as.numeric(e$di))
      is_val <- findInterval(as.numeric(startDate), as.numeric(e$di))
      if (is_val < 1L || ie < 1L || ie - is_val < 10L) return(NA)
      e$cr[ie] / e$cr[is_val] - 1
    }, simplify = TRUE)
    pctPosByDate[[as.character(sd)]] <- mean(moms > 0, na.rm = TRUE) * 100
  }

  xVals <- c(); yVals <- c()
  for (line in lines) {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2} \\| OK:", line)) next
    sigDate <- as.character(as.Date(sub(" \\|.*", "", line)))
    pctPos <- pctPosByDate[[sigDate]]
    if (is.null(pctPos) || is.na(pctPos)) next
    nextMon <- as.character(as.Date(sigDate) %m+% months(1))
    fwdIdx <- which(monIdx >= nextMon)[1]
    if (is.na(fwdIdx)) next
    xVals <- c(xVals, pctPos)
    yVals <- c(yVals, as.numeric(coredata(monthlyRets[fwdIdx])) * 100)
  }

  if (length(xVals) < 5) next
  df <- data.frame(PctPos = xVals, FwdRet = yVals)
  r2 <- summary(lm(FwdRet ~ PctPos, data = df))$r.squared

  p <- ggplot(df, aes(x = PctPos, y = FwdRet)) +
    geom_point(alpha = 0.7, size = 2.5, color = viridis(1, option = "D")) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(title = sprintf("%% Positive Stocks → %s Forward Return (Test Set)", nm),
         subtitle = sprintf("R² = %.3f | %d months | ≥ %s", r2, nrow(df), TEST_START),
         x = sprintf("%% of Stocks with Positive %d-Month Return", lb),
         y = sprintf("%s Next-Month Return (%%)", nm),
         caption = "@StockViz") +
    theme_minimal(base_size = 12)
  ggsave(sprintf("%s/scatter_pctpos_%s.png", reportPath, tolower(nm)), p, width = 7, height = 6, dpi = 120)
  cat(sprintf("  %-6s R²=%.3f  n=%d\n", nm, r2, nrow(df)))
}

# ═══════════════════════════════════════════════════════════════
# 5. Deep drawdown recovery by depth bucket (Train + Validation)
# ═══════════════════════════════════════════════════════════════
cat("\n=== Drawdown Recovery by Depth (Train + Validation) ===\n")

trainValid <- combined[paste0("/", VALID_END)]
allNames <- c(names(allPortfolios), "Bench")
allSeries <- c(allPortfolios, list(Bench = benchDaily))

bucketRecovery <- function(rets, label) {
  eq <- cumprod(1 + coredata(rets))
  n <- length(eq)
  events <- list()
  peak <- eq[1]; peakIdx <- 1

  for (i in 2:n) {
    if (eq[i] >= peak) {
      # New peak — check if there was a drawdown to close
      if (i - 1 > peakIdx) {
        troughIdx <- which.min(eq[peakIdx:(i-1)]) + peakIdx - 1
        depth <- eq[troughIdx] / peak - 1
        if (depth < -0.05) {  # at least 5% drawdown
          recoveryDays <- i - troughIdx
          events[[length(events)+1]] <- c(depth=depth, recovery=recoveryDays)
        }
      }
      peak <- eq[i]; peakIdx <- i
    }
  }
  if (length(events) == 0) return(NULL)

  ev <- do.call(rbind, events)
  buckets <- c("≥10%", "≥15%", "≥20%", ">25%")
  thresholds <- c(-0.10, -0.15, -0.20, -0.25)
  result <- setNames(rep(NA_real_, length(buckets)), buckets)

  for (j in seq_along(buckets)) {
    idx <- which(ev[, "depth"] <= thresholds[j])
    if (length(idx) > 0)
      result[j] <- mean(ev[idx, "recovery"])
  }
  result
}

cat(sprintf("%-8s %8s %8s %8s %8s\n", "LB", "≥10%", "≥15%", "≥20%", ">25%"))
cat(sprintf("%-8s %8s %8s %8s %8s\n", "--------", "----", "----", "----", "----"))
for (nm in allNames) {
  r <- allSeries[[nm]][index(trainValid)]
  r <- na.omit(r)
  if (nrow(r) < 260) next
  rec <- bucketRecovery(r, nm)
  if (is.null(rec)) next
  cat(sprintf("%-8s %8s %8s %8s %8s\n", nm,
    if(is.na(rec[1])) "—" else sprintf("%.0fd", rec[1]),
    if(is.na(rec[2])) "—" else sprintf("%.0fd", rec[2]),
    if(is.na(rec[3])) "—" else sprintf("%.0fd", rec[3]),
    if(is.na(rec[4])) "—" else sprintf("%.0fd", rec[4])))
}

cat("\n===== END =====\n")

# ============================================================================
# backtest.R — Q1 LIQC Long-Only Portfolio (Phase 2)
# ============================================================================
suppressPackageStartupMessages({
  library('RODBC'); library('RPostgres'); library('quantmod')
  library('PerformanceAnalytics'); library('xts'); library('tidyverse')
  library('lubridate'); library('gt'); library('webshot2'); library('viridis')
  library('ggthemes'); library('scales')
})

pdf(NULL); options("scipen" = 100); options(stringsAsFactors = FALSE)
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")
source("/mnt/data/blog/momentum/liquidity-improvement/liqim-common.R")
source("/mnt/data/blog/momentum/liquidity-improvement/liqim-config.R")

reportPath <- "/mnt/data/blog/momentum/liquidity-improvement"
chk <- readRDS(sprintf("%s/checkpoint.rds", reportPath))
priceVol <- lapply(chk$priceVol, function(df) df[order(df$date_stamp), ])
monthEnds <- chk$monthEnds; benchXts <- chk$benchXts
liqcCache <- chk$liqcCache; universeCache <- chk$universeCache; rm(chk)

# Q1 portfolio
cat("=== PORTFOLIO ===\n")
q1Rets <- makePortfolio(pickQ(1L, CFG$TOP_N), liqcCache, universeCache, monthEnds, priceVol,
  "Q1_LIQC", CFG$TOP_N, CFG$HOLDING_K, CFG$SKIP_MONTH, CFG$DRAG)

# Benchmark
benchRets <- na.omit(dailyReturn(benchXts, type = "arithmetic"))
benchRets <- xts(coredata(benchRets), as.Date(index(benchRets)))
colnames(benchRets) <- "NIFTY500_MOM50_TR"

combined <- na.omit(do.call(merge.xts, list(benchRets, q1Rets)))

# Metrics
cat(sprintf("\n=== METRICS (%s → %s) ===\n", first(index(combined)), last(index(combined))))
fm <- sapply(colnames(combined), function(cn) computeMetrics(combined[, cn]))
print(round(fm, 4))

# Charts + table
cat("\n=== CHARTS ===\n")
makeCumretChart(combined, "LIQIM Q1 Long-Only (top 60%)",
  sprintf("%s/cumret.png", reportPath))
makeAnnualChart(combined, "LIQIM Q1 Long-Only (top 60%)",
  sprintf("%s/annual.png", reportPath))
cat("\n=== TABLE ===\n")
makeGtTable(fm, "LIQIM Q1 Long-Only (top 60%, 1-mo LIQC)",
  sprintf("%s/metrics.png", reportPath), reportPath)

# CSVs
cat("\n=== CSVs ===\n")
makeMonthlyCsv(combined, "liqc", reportPath)

cat("\n===== DONE =====\n")

# Save returns for consolidated.R
saveRDS(list(q1 = q1Rets, bench = benchRets), sprintf("%s/q1_liqc.rds", reportPath))
cat("Saved: q1_liqc.rds\n")

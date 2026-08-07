# ============================================================================
# momentum.R — Momentum vs Momentum-minus-Q5 (Phase 3)
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
liqcCache <- chk$liqcCache; universeCache <- chk$universeCache
illiqCache <- chk$illiqCache; rm(chk)

# Momentum cache
cat("=== MOMENTUM ===\n")
momCache <- buildMomentumCache(monthEnds, universeCache, priceVol, CFG$MOM_LB)

# Q5 exclusion sets: 1-month LIQC
q5_1m <- buildQ5Exclude(monthEnds, liqcCache, universeCache)

# Q5 exclusion sets: 12-month LIQC
cat("  12-month LIQC...\n")
liqc_12m <- computeLIQC(illiqCache, monthEnds, 12L)
q5_12m <- buildQ5Exclude(monthEnds, liqc_12m, universeCache)

# Raw momentum
momRaw <- makePortfolio(pickMomentum(momCache, NULL, CFG$TOP_N, FALSE),
  NULL, universeCache, monthEnds, priceVol, "Momentum",
  CFG$TOP_N, CFG$HOLDING_K, CFG$SKIP_MONTH, CFG$DRAG,
  momCache = momCache, warmupCache = momCache)

# Momentum ex-Q5 (1-month LIQC)
momFilt1 <- makePortfolio(pickMomentum(momCache, q5_1m, CFG$TOP_N, TRUE),
  NULL, universeCache, monthEnds, priceVol, "Mom_exQ5_1m",
  CFG$TOP_N, CFG$HOLDING_K, CFG$SKIP_MONTH, CFG$DRAG,
  momCache = momCache, warmupCache = momCache)

# Momentum ex-Q5 (12-month LIQC)
momFilt12 <- makePortfolio(pickMomentum(momCache, q5_12m, CFG$TOP_N, TRUE),
  NULL, universeCache, monthEnds, priceVol, "Mom_exQ5_12m",
  CFG$TOP_N, CFG$HOLDING_K, CFG$SKIP_MONTH, CFG$DRAG,
  momCache = momCache, warmupCache = momCache)

# Benchmark
benchRets <- na.omit(dailyReturn(benchXts, type = "arithmetic"))
benchRets <- xts(coredata(benchRets), as.Date(index(benchRets)))
colnames(benchRets) <- "NIFTY500_MOM50_TR"

combined <- na.omit(do.call(merge.xts, list(benchRets, momRaw, momFilt1, momFilt12)))

# Metrics
cat(sprintf("\n=== METRICS (%s → %s) ===\n", first(index(combined)), last(index(combined))))
fm <- sapply(colnames(combined), function(cn) computeMetrics(combined[, cn]))
print(round(fm, 4))

# Charts + table
cat("\n=== CHARTS ===\n")
makeCumretChart(combined, "Momentum vs Mom-ex-Q5 (top 60%)",
  sprintf("%s/mom_cumret.png", reportPath))
cat("\n=== TABLE ===\n")
makeGtTable(fm, "Momentum vs Mom-ex-Q5 (top 60%, 12-mo Mom)",
  sprintf("%s/mom_metrics.png", reportPath), reportPath)

cat("\n===== DONE =====\n")

saveRDS(list(raw = momRaw, filt1 = momFilt1, filt12 = momFilt12, bench = benchRets),
  sprintf("%s/momentum.rds", reportPath))
cat("Saved: momentum.rds\n")

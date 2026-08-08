# ============================================================================
# momentum.R — Momentum vs Mom+Skew vs Mom+Skew+LIQC (Skewness Project)
# 12-month lookback, 0 skip, top 60% FF-mcap universe
# All sources local to this directory — fully self-contained.
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

# ── All local sources (self-contained) ──
reportPath <- "/mnt/data/blog/momentum/skewness"
source(sprintf("%s/skew-config.R", reportPath))
source(sprintf("%s/liq-common.R", reportPath))
source(sprintf("%s/backtest-common.R", reportPath))
source(sprintf("%s/skew-common.R", reportPath))

CHK_FILE <- sprintf("%s/checkpoint.rds", reportPath)

chk <- readRDS(CHK_FILE)
priceVol <- lapply(chk$priceVol, function(df) df[order(df$date_stamp), ])
monthEnds <- chk$monthEnds; benchXts <- chk$benchXts
liqcCache <- chk$liqcCache; universeCache <- chk$universeCache
illiqCache <- chk$illiqCache; rm(chk)

cat(sprintf("Checkpoint: %d month-ends, %d symbols, %d LIQC months, %d universe months\n",
    length(monthEnds), length(priceVol),
    sum(!sapply(liqcCache, is.null)), sum(!sapply(universeCache, is.null))))

# ═══════════════════════════════════════════════════════════════
# Momentum cache
# ═══════════════════════════════════════════════════════════════

cat("=== MOMENTUM CACHE ===\n")
momCache <- buildMomentumCache(monthEnds, universeCache, priceVol, CFG$MOM_LB)

# ═══════════════════════════════════════════════════════════════
# LIQC exclusion sets (Q5 = bottom liquidity quintile)
# ═══════════════════════════════════════════════════════════════

cat("=== LIQC EXCLUSION ===\n")
q5Exclude <- buildQ5Exclude(monthEnds, liqcCache, universeCache)

# ═══════════════════════════════════════════════════════════════
# Monthly realized skewness & volatility
# ═══════════════════════════════════════════════════════════════

cat("=== MONTHLY STATS (RS, RV, prior returns) ===\n")
monthlyStats <- computeMonthlyStats(priceVol, monthEnds, universeCache, CFG$MIN_DAILY)
rsCache    <- monthlyStats$rs
rvCache    <- monthlyStats$rv
priorCache <- monthlyStats$prior
rm(monthlyStats)

# ═══════════════════════════════════════════════════════════════
# Size terciles + industry cache
# ═══════════════════════════════════════════════════════════════

cat("=== SIZE TERCILES ===\n")
sizeTercileCache <- buildSizeTerciles(monthEnds, universeCache)

cat("=== INDUSTRY CACHE ===\n")
industryCache <- buildIndustryCache(monthEnds, universeCache, priceVol)

# ═══════════════════════════════════════════════════════════════
# Expected skewness forecast (monthly cross-sectional)
# ═══════════════════════════════════════════════════════════════

cat("=== EXPECTED SKEWNESS FORECAST ===\n")
expRsCache <- forecastExpectedSkewness(rsCache, rvCache, priorCache, momCache,
                                        sizeTercileCache, industryCache, monthEnds)

# ═══════════════════════════════════════════════════════════════
# Portfolios
# ═══════════════════════════════════════════════════════════════

cat("=== PORTFOLIOS ===\n")
SKEW_TOP_N <- 20L   # same count as baseline momentum for fair comparison

# 1. Baseline momentum (equal-weight top-N)
momRaw <- makePortfolio(pickMomentum(momCache, NULL, CFG$TOP_N, FALSE),
  NULL, universeCache, monthEnds, priceVol, "Momentum",
  CFG$TOP_N, CFG$HOLDING_K, CFG$SKIP_MONTH, CFG$DRAG,
  momCache = momCache, warmupCache = momCache)

# 2. Momentum + expected skewness overlay (sequential sort)
momSkew <- makePortfolio(
  pickMomentumSkew(momCache, expRsCache, excludeCache = NULL,
    momTopPct = 0.10, skewTopPct = 0.33, topN = SKEW_TOP_N),
  NULL, universeCache, monthEnds, priceVol, "Mom+Skew",
  SKEW_TOP_N, CFG$HOLDING_K, CFG$SKIP_MONTH, CFG$DRAG,
  warmupCache = expRsCache)

# 3. Momentum + skewness + LIQC filter (exclude Q5, then sequential sort)
momSkewLiq <- makePortfolio(
  pickMomentumSkew(momCache, expRsCache, excludeCache = q5Exclude,
    momTopPct = 0.10, skewTopPct = 0.33, topN = SKEW_TOP_N),
  NULL, universeCache, monthEnds, priceVol, "Mom+Skew+LIQC",
  SKEW_TOP_N, CFG$HOLDING_K, CFG$SKIP_MONTH, CFG$DRAG,
  warmupCache = expRsCache)

# ═══════════════════════════════════════════════════════════════
# Benchmark
# ═══════════════════════════════════════════════════════════════

benchRets <- na.omit(dailyReturn(benchXts, type = "arithmetic"))
benchRets <- xts(coredata(benchRets), as.Date(index(benchRets)))
colnames(benchRets) <- "NIFTY500_MOM50_TR"

combined <- na.omit(do.call(merge.xts,
  list(benchRets, momRaw, momSkew, momSkewLiq)))

# ═══════════════════════════════════════════════════════════════
# Metrics
# ═══════════════════════════════════════════════════════════════

cat(sprintf("\n=== METRICS (%s → %s) ===\n",
    first(index(combined)), last(index(combined))))
fm <- sapply(colnames(combined), function(cn) computeMetrics(combined[, cn]))
print(round(fm, 4))

# Incremental analysis
if (!is.null(momSkew) && !is.null(momRaw)) {
  incRets <- momSkew - momRaw
  cat("\n=== INCREMENTAL (Mom+Skew - Momentum) ===\n")
  incMetrics <- computeMetrics(incRets)
  print(round(incMetrics, 4))
}
if (!is.null(momSkewLiq) && !is.null(momSkew)) {
  incLiqRets <- momSkewLiq - momSkew
  cat("\n=== INCREMENTAL (Mom+Skew+LIQC - Mom+Skew) ===\n")
  incLiqMetrics <- computeMetrics(incLiqRets)
  print(round(incLiqMetrics, 4))
}

# ═══════════════════════════════════════════════════════════════
# Charts + table
# ═══════════════════════════════════════════════════════════════

cat("\n=== CHARTS ===\n")
makeCumretChart(combined,
  "Momentum vs Mom+Skew vs Mom+Skew+LIQC (12-mo, top 60% FF-mcap)",
  sprintf("%s/mom_cumret.png", reportPath))
makeAnnualChart(combined,
  "Momentum vs Mom+Skew vs Mom+Skew+LIQC — Annual Returns",
  sprintf("%s/mom_annual.png", reportPath))

cat("\n=== TABLE ===\n")
makeGtTable(fm,
  "Momentum Baseline vs Skewness Overlay vs Skewness+LIQC",
  sprintf("%s/mom_metrics.png", reportPath), reportPath)

cat("\n=== CSV ===\n")
makeMonthlyCsv(combined, "momentum", reportPath)

# ═══════════════════════════════════════════════════════════════
# Post-2020-05-01 subset
# ═══════════════════════════════════════════════════════════════

POST_CUTOFF <- "2020-05-01"
postCombined <- combined[paste0(POST_CUTOFF, "/")]

if (nrow(postCombined) > 60) {
  cat(sprintf("\n=== POST-%s METRICS (%s → %s) ===\n",
      POST_CUTOFF, first(index(postCombined)), last(index(postCombined))))
  fmPost <- sapply(colnames(postCombined),
    function(cn) computeMetrics(postCombined[, cn]))
  print(round(fmPost, 4))

  if (!is.null(momSkew) && !is.null(momRaw)) {
    msCol <- grep("Mom.*Skew", colnames(postCombined), value=TRUE)
    msCol <- msCol[!grepl("LIQC", msCol)][1]
    momCol <- grep("^Momentum$", colnames(postCombined), value=TRUE)[1]
    incPost <- postCombined[, msCol] - postCombined[, momCol]
    cat(sprintf("\n=== POST-%s INCREMENTAL (Mom+Skew - Momentum) ===\n", POST_CUTOFF))
    print(round(computeMetrics(incPost), 4))
  }

  cat(sprintf("\n=== POST-%s CHARTS ===\n", POST_CUTOFF))
  makeCumretChart(postCombined,
    sprintf("Momentum vs Mom+Skew vs Mom+Skew+LIQC (post %s)", POST_CUTOFF),
    sprintf("%s/mom_cumret_post2020.png", reportPath))
  makeAnnualChart(postCombined,
    sprintf("Momentum vs Mom+Skew vs Mom+Skew+LIQC — Annual (post %s)", POST_CUTOFF),
    sprintf("%s/mom_annual_post2020.png", reportPath))

  cat(sprintf("\n=== POST-%s TABLE ===\n", POST_CUTOFF))
  makeGtTable(fmPost,
    sprintf("Momentum Baseline vs Skewness Overlay (post %s)", POST_CUTOFF),
    sprintf("%s/mom_metrics_post2020.png", reportPath), reportPath)

  cat(sprintf("\n=== POST-%s CSV ===\n", POST_CUTOFF))
  makeMonthlyCsv(postCombined, "momentum_post2020", reportPath)
}

# ═══════════════════════════════════════════════════════════════
# Save
# ═══════════════════════════════════════════════════════════════

saveRDS(list(momRaw = momRaw, momSkew = momSkew, momSkewLiq = momSkewLiq,
             benchRets = benchRets, expRsCache = expRsCache,
             momCache = momCache, q5Exclude = q5Exclude),
  sprintf("%s/momentum.rds", reportPath))
cat("Saved: momentum.rds\n")

cat("\n===== DONE =====\n")

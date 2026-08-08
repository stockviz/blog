# ============================================================================
# consolidated.R — All scenarios in one place
# Momentum, Mom+Skew, Mom+Skew+LIQC, OmegaMom, Omega+Skew, Omega+Skew+LIQC
# Splits: full period, pre-2019-12-31, post-2020-05-01
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

reportPath <- "/mnt/data/blog/momentum/skewness"
source(sprintf("%s/skew-config.R", reportPath))
source(sprintf("%s/liq-common.R", reportPath))
source(sprintf("%s/backtest-common.R", reportPath))
source(sprintf("%s/skew-common.R", reportPath))

chk <- readRDS(sprintf("%s/checkpoint.rds", reportPath))
priceVol <- lapply(chk$priceVol, function(df) df[order(df$date_stamp), ])
monthEnds <- chk$monthEnds; benchXts <- chk$benchXts
liqcCache <- chk$liqcCache; universeCache <- chk$universeCache
illiqCache <- chk$illiqCache; rm(chk)

cat(sprintf("Checkpoint: %d month-ends, %d symbols\n",
    length(monthEnds), length(priceVol)))

TOP_N <- 20L

# ═══════════════════════════════════════════════════════════════
# All caches (built once)
# ═══════════════════════════════════════════════════════════════

cat("=== MOMENTUM CACHE ===\n")
momCache <- buildMomentumCache(monthEnds, universeCache, priceVol, CFG$MOM_LB)

# Omega ratio cache
buildOmegaCache <- function(monthEnds, universeCache, priceVol, momLb = 12L) {
  omegaCache <- vector("list", length(monthEnds))
  for (mi in seq(momLb + 1L, length(monthEnds))) {
    sigDate <- monthEnds[mi]; momEnd <- sigDate
    momStart <- momEnd %m-% months(momLb)
    syms <- universeCache[[mi]]
    if (is.null(syms) || length(syms) == 0) next
    omegaVals <- vapply(syms, function(tkr) {
      df <- priceVol[[tkr]]
      if (is.null(df) || nrow(df) < 260) return(NA_real_)
      sub <- df[df$date_stamp >= momStart & df$date_stamp <= momEnd, , drop=FALSE]
      if (nrow(sub) < 230) return(NA_real_)
      n <- nrow(sub); rets <- diff(sub$c) / sub$c[-n]
      posRets <- rets[rets > 0]; negRets <- rets[rets < 0]
      if (length(posRets) == 0 || length(negRets) == 0) return(NA_real_)
      sum(posRets) / sum(abs(negRets))
    }, double(1))
    names(omegaVals) <- syms; omegaVals <- omegaVals[!is.na(omegaVals)]
    if (length(omegaVals) > 0)
      omegaCache[[mi]] <- sort(omegaVals, decreasing = TRUE)
  }
  cat(sprintf("  Omega ratio: %d months cached\n",
      sum(!sapply(omegaCache, is.null))))
  omegaCache
}

cat("=== OMEGA CACHE ===\n")
omegaCache <- buildOmegaCache(monthEnds, universeCache, priceVol, CFG$MOM_LB)

cat("=== LIQC EXCLUSION ===\n")
q5Exclude <- buildQ5Exclude(monthEnds, liqcCache, universeCache)

cat("=== SKEWNESS CACHES ===\n")
stats <- computeMonthlyStats(priceVol, monthEnds, universeCache, CFG$MIN_DAILY)
rsCache <- stats$rs; rvCache <- stats$rv; priorCache <- stats$prior; rm(stats)
sizeTercileCache <- buildSizeTerciles(monthEnds, universeCache)
industryCache <- buildIndustryCache(monthEnds, universeCache, priceVol)
expRsCache <- forecastExpectedSkewness(rsCache, rvCache, priorCache, momCache,
                                        sizeTercileCache, industryCache, monthEnds)

# ═══════════════════════════════════════════════════════════════
# Pickers
# ═══════════════════════════════════════════════════════════════

# Raw momentum
pickMom     <- pickMomentum(momCache, NULL, TOP_N, FALSE)
pickMomSkew <- pickMomentumSkew(momCache, expRsCache, NULL,
                  momTopPct=0.10, skewTopPct=0.33, topN=TOP_N)
pickMomSkewL <- pickMomentumSkew(momCache, expRsCache, q5Exclude,
                  momTopPct=0.10, skewTopPct=0.33, topN=TOP_N)

# Omega-based
pickOmega     <- pickMomentum(omegaCache, NULL, TOP_N, FALSE)
pickOmegaSkew <- pickMomentumSkew(omegaCache, expRsCache, NULL,
                   momTopPct=0.10, skewTopPct=0.33, topN=TOP_N)
pickOmegaSkewL <- pickMomentumSkew(omegaCache, expRsCache, q5Exclude,
                   momTopPct=0.10, skewTopPct=0.33, topN=TOP_N)

# ═══════════════════════════════════════════════════════════════
# Portfolios
# ═══════════════════════════════════════════════════════════════

cat("=== PORTFOLIOS ===\n")

momRaw <- makePortfolio(pickMom, NULL, universeCache, monthEnds, priceVol,
  "Momentum", TOP_N, CFG$HOLDING_K, CFG$SKIP_MONTH, CFG$DRAG,
  momCache=momCache, warmupCache=momCache)

momSkew <- makePortfolio(pickMomSkew, NULL, universeCache, monthEnds, priceVol,
  "Mom+Skew", TOP_N, CFG$HOLDING_K, CFG$SKIP_MONTH, CFG$DRAG,
  warmupCache=expRsCache)

momSkewLiq <- makePortfolio(pickMomSkewL, NULL, universeCache, monthEnds, priceVol,
  "Mom+Skew+LIQC", TOP_N, CFG$HOLDING_K, CFG$SKIP_MONTH, CFG$DRAG,
  warmupCache=expRsCache)

omegaRaw <- makePortfolio(pickOmega, NULL, universeCache, monthEnds, priceVol,
  "OmegaMom", TOP_N, CFG$HOLDING_K, CFG$SKIP_MONTH, CFG$DRAG,
  momCache=omegaCache, warmupCache=omegaCache)

omegaSkew <- makePortfolio(pickOmegaSkew, NULL, universeCache, monthEnds, priceVol,
  "Omega+Skew", TOP_N, CFG$HOLDING_K, CFG$SKIP_MONTH, CFG$DRAG,
  warmupCache=expRsCache)

omegaSkewLiq <- makePortfolio(pickOmegaSkewL, NULL, universeCache, monthEnds, priceVol,
  "Omega+Skew+LIQC", TOP_N, CFG$HOLDING_K, CFG$SKIP_MONTH, CFG$DRAG,
  warmupCache=expRsCache)

# ═══════════════════════════════════════════════════════════════
# Benchmark + merge
# ═══════════════════════════════════════════════════════════════

benchRets <- na.omit(dailyReturn(benchXts, type="arithmetic"))
benchRets <- xts(coredata(benchRets), as.Date(index(benchRets)))
colnames(benchRets) <- "NIFTY500_MOM50_TR"

combined <- na.omit(do.call(merge.xts,
  list(benchRets, momRaw, momSkew, momSkewLiq,
       omegaRaw, omegaSkew, omegaSkewLiq)))

# ═══════════════════════════════════════════════════════════════
# Full-period metrics + table
# ═══════════════════════════════════════════════════════════════

cat(sprintf("\n=== FULL PERIOD (%s → %s) ===\n",
    first(index(combined)), last(index(combined))))
fm <- sapply(colnames(combined), function(cn) computeMetrics(combined[, cn]))
print(round(fm, 4))

cat("\n=== FULL PERIOD TABLE ===\n")
makeGtTable(fm, "All Scenarios — Full Period",
  sprintf("%s/consolidated_metrics.png", reportPath), reportPath)

cat("\n=== FULL PERIOD CHARTS ===\n")
makeCumretChart(combined, "All Scenarios — Cumulative Returns",
  sprintf("%s/consolidated_cumret.png", reportPath))

makeMonthlyCsv(combined, "consolidated", reportPath)

# ═══════════════════════════════════════════════════════════════
# Pre-2019-12-31 subset
# ═══════════════════════════════════════════════════════════════

PRE_CUTOFF <- "2019-12-31"
preCombined <- combined[paste0("/", PRE_CUTOFF)]

if (nrow(preCombined) > 60) {
  cat(sprintf("\n=== PRE-%s (%s → %s) ===\n",
      PRE_CUTOFF, first(index(preCombined)), last(index(preCombined))))
  fmPre <- sapply(colnames(preCombined),
    function(cn) computeMetrics(preCombined[, cn]))
  print(round(fmPre, 4))

  cat(sprintf("\n=== PRE-%s TABLE ===\n", PRE_CUTOFF))
  makeGtTable(fmPre,
    sprintf("All Scenarios — Pre-%s", PRE_CUTOFF),
    sprintf("%s/consolidated_metrics_pre2019.png", reportPath), reportPath)

  cat(sprintf("\n=== PRE-%s CHARTS ===\n", PRE_CUTOFF))
  makeCumretChart(preCombined,
    sprintf("All Scenarios — Cumulative Returns (pre-%s)", PRE_CUTOFF),
    sprintf("%s/consolidated_cumret_pre2019.png", reportPath))

  makeMonthlyCsv(preCombined, "consolidated_pre2019", reportPath)
}

# ═══════════════════════════════════════════════════════════════
# Post-2020-05-01 subset
# ═══════════════════════════════════════════════════════════════

POST_CUTOFF <- "2020-05-01"
postCombined <- combined[paste0(POST_CUTOFF, "/")]

if (nrow(postCombined) > 60) {
  cat(sprintf("\n=== POST-%s (%s → %s) ===\n",
      POST_CUTOFF, first(index(postCombined)), last(index(postCombined))))
  fmPost <- sapply(colnames(postCombined),
    function(cn) computeMetrics(postCombined[, cn]))
  print(round(fmPost, 4))

  cat(sprintf("\n=== POST-%s TABLE ===\n", POST_CUTOFF))
  makeGtTable(fmPost,
    sprintf("All Scenarios — Post-%s", POST_CUTOFF),
    sprintf("%s/consolidated_metrics_post2020.png", reportPath), reportPath)

  cat(sprintf("\n=== POST-%s CHARTS ===\n", POST_CUTOFF))
  makeCumretChart(postCombined,
    sprintf("All Scenarios — Cumulative Returns (post-%s)", POST_CUTOFF),
    sprintf("%s/consolidated_cumret_post2020.png", reportPath))

  makeMonthlyCsv(postCombined, "consolidated_post2020", reportPath)
}

# ═══════════════════════════════════════════════════════════════
# Save
# ═══════════════════════════════════════════════════════════════

saveRDS(list(combined=combined, fm=fm, fmPre=fmPre, fmPost=fmPost),
  sprintf("%s/consolidated.rds", reportPath))
cat("Saved: consolidated.rds\n")
cat("\n===== DONE =====\n")

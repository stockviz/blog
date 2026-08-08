# ============================================================================
# omega-momentum.R — Omega Ratio vs Raw Momentum Ranking
# Compares: raw Momentum, Omega-Mom, Omega+Skew, Omega+Skew+LIQC
# Also produces post-2020-05-01 subset charts & metrics.
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

# ═══════════════════════════════════════════════════════════════
# Caches
# ═══════════════════════════════════════════════════════════════

cat("=== MOMENTUM CACHE ===\n")
momCache <- buildMomentumCache(monthEnds, universeCache, priceVol, CFG$MOM_LB)

# ── Omega ratio cache (gains / losses over lookback period) ──
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

TOP_N <- 20L

# Raw momentum rank
pickMom    <- pickMomentum(momCache,   NULL, TOP_N, FALSE)

# Omega ratio rank
pickOmega  <- pickMomentum(omegaCache, NULL, TOP_N, FALSE)

# Omega + skewness overlay (same skewness model, omega-based momentum selection)
pickOmegaSkew  <- pickMomentumSkew(omegaCache, expRsCache, NULL,
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
# Benchmark
# ═══════════════════════════════════════════════════════════════

benchRets <- na.omit(dailyReturn(benchXts, type="arithmetic"))
benchRets <- xts(coredata(benchRets), as.Date(index(benchRets)))
colnames(benchRets) <- "NIFTY500_MOM50_TR"

combined <- na.omit(do.call(merge.xts,
  list(benchRets, momRaw, omegaRaw, omegaSkew, omegaSkewLiq)))

# ═══════════════════════════════════════════════════════════════
# Full-period metrics
# ═══════════════════════════════════════════════════════════════

cat(sprintf("\n=== FULL PERIOD METRICS (%s → %s) ===\n",
    first(index(combined)), last(index(combined))))
fm <- sapply(colnames(combined), function(cn) computeMetrics(combined[, cn]))
print(round(fm, 4))

# Incremental: Omega vs raw Momentum
if (!is.null(omegaRaw) && !is.null(momRaw)) {
  incOmg <- omegaRaw - momRaw
  cat("\n=== INCREMENTAL (OmegaMom - Momentum) ===\n")
  print(round(computeMetrics(incOmg), 4))
}
if (!is.null(omegaSkew) && !is.null(omegaRaw)) {
  incOmgS <- omegaSkew - omegaRaw
  cat("\n=== INCREMENTAL (Omega+Skew - OmegaMom) ===\n")
  print(round(computeMetrics(incOmgS), 4))
}

cat("\n=== FULL PERIOD CHARTS ===\n")
makeCumretChart(combined, "Momentum vs Omega-Momentum (12-mo, top 60% FF-mcap)",
  sprintf("%s/omega_cumret.png", reportPath))
makeAnnualChart(combined, "Momentum vs Omega-Momentum — Annual Returns",
  sprintf("%s/omega_annual.png", reportPath))

cat("\n=== FULL PERIOD TABLE ===\n")
makeGtTable(fm, "Raw Momentum vs Omega-Based Momentum",
  sprintf("%s/omega_metrics.png", reportPath), reportPath)

cat("\n=== FULL PERIOD CSV ===\n")
makeMonthlyCsv(combined, "omega", reportPath)

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

  cat(sprintf("\n=== POST-%s CHARTS ===\n", POST_CUTOFF))
  makeCumretChart(postCombined,
    sprintf("Momentum vs Omega-Momentum (post %s)", POST_CUTOFF),
    sprintf("%s/omega_cumret_post2020.png", reportPath))
  makeAnnualChart(postCombined,
    sprintf("Momentum vs Omega-Momentum — Annual Returns (post %s)", POST_CUTOFF),
    sprintf("%s/omega_annual_post2020.png", reportPath))

  cat(sprintf("\n=== POST-%s TABLE ===\n", POST_CUTOFF))
  makeGtTable(fmPost,
    sprintf("Raw vs Omega Momentum (post %s)", POST_CUTOFF),
    sprintf("%s/omega_metrics_post2020.png", reportPath), reportPath)

  cat(sprintf("\n=== POST-%s CSV ===\n", POST_CUTOFF))
  makeMonthlyCsv(postCombined, "omega_post2020", reportPath)

  # Post-2020 incremental analysis
  omCol  <- grep("OmegaMom", colnames(postCombined), value=TRUE)[1]
  momCol <- grep("^Momentum$", colnames(postCombined), value=TRUE)[1]
  osCol  <- grep("Omega.*Skew", colnames(postCombined), value=TRUE)
  osCol  <- osCol[!grepl("LIQC", osCol)][1]   # Omega+Skew (not LIQC variant)
  if (!is.na(omCol) && !is.na(momCol)) {
    incOmgPost <- postCombined[, omCol] - postCombined[, momCol]
    cat(sprintf("\n=== POST-%s INCREMENTAL (OmegaMom - Momentum) ===\n", POST_CUTOFF))
    print(round(computeMetrics(incOmgPost), 4))
  }
  if (!is.na(osCol) && !is.na(omCol)) {
    incOmgSPost <- postCombined[, osCol] - postCombined[, omCol]
    cat(sprintf("\n=== POST-%s INCREMENTAL (Omega+Skew - OmegaMom) ===\n", POST_CUTOFF))
    print(round(computeMetrics(incOmgSPost), 4))
  }
}

# ═══════════════════════════════════════════════════════════════
# Save
# ═══════════════════════════════════════════════════════════════

saveRDS(list(momRaw=momRaw, omegaRaw=omegaRaw,
             omegaSkew=omegaSkew, omegaSkewLiq=omegaSkewLiq,
             benchRets=benchRets, omegaCache=omegaCache, momCache=momCache),
  sprintf("%s/omega-momentum.rds", reportPath))
cat("Saved: omega-momentum.rds\n")

cat("\n===== DONE =====\n")

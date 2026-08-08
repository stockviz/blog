# ============================================================================
# rank-persistence.R — Portfolio membership persistence by market regime
# Compares Momentum (top 20) vs Mom+Skew (top 20) month-to-month overlap.
# Also includes Spearman rank correlation of momentum scores.
# Outputs rank-persistence.md
# ============================================================================
suppressPackageStartupMessages({
  library('RODBC'); library('RPostgres'); library('quantmod')
  library('PerformanceAnalytics'); library('xts'); library('tidyverse')
  library('lubridate')
})

pdf(NULL); options("scipen" = 100); options(stringsAsFactors = FALSE)
source("/mnt/hollandC/StockViz/R/config.r")

reportPath <- "/mnt/data/blog/momentum/skewness"
source(sprintf("%s/skew-config.R", reportPath))
source(sprintf("%s/liq-common.R", reportPath))
source(sprintf("%s/backtest-common.R", reportPath))
source(sprintf("%s/skew-common.R", reportPath))

chk <- readRDS(sprintf("%s/checkpoint.rds", reportPath))
priceVol <- lapply(chk$priceVol, function(df) df[order(df$date_stamp), ])
monthEnds <- chk$monthEnds; benchXts <- chk$benchXts
universeCache <- chk$universeCache; rm(chk)

cat(sprintf("Checkpoint: %d month-ends, %d symbols, %d universe months\n",
    length(monthEnds), length(priceVol),
    sum(!sapply(universeCache, is.null))))

# ═══════════════════════════════════════════════════════════════
# Caches
# ═══════════════════════════════════════════════════════════════

cat("=== MOMENTUM CACHE ===\n")
momCache <- buildMomentumCache(monthEnds, universeCache, priceVol, CFG$MOM_LB)

cat("=== SKEWNESS CACHES ===\n")
stats <- computeMonthlyStats(priceVol, monthEnds, universeCache, CFG$MIN_DAILY)
rsCache <- stats$rs; rvCache <- stats$rv; priorCache <- stats$prior; rm(stats)
sizeTercileCache <- buildSizeTerciles(monthEnds, universeCache)
industryCache <- buildIndustryCache(monthEnds, universeCache, priceVol)
expRsCache <- forecastExpectedSkewness(rsCache, rvCache, priorCache, momCache,
                                        sizeTercileCache, industryCache, monthEnds)

# ═══════════════════════════════════════════════════════════════
# Portfolio pickers (both top 20)
# ═══════════════════════════════════════════════════════════════

TOP_N <- 20L
pickMom  <- pickMomentum(momCache, NULL, TOP_N, FALSE)
pickSkew <- pickMomentumSkew(momCache, expRsCache, NULL,
                momTopPct=0.10, skewTopPct=0.33, topN=TOP_N)

# ═══════════════════════════════════════════════════════════════
# Benchmark drawdowns → regime classification
# ═══════════════════════════════════════════════════════════════

benchRets <- na.omit(dailyReturn(benchXts, type = "arithmetic"))
benchCum  <- cumprod(1 + benchRets)
benchMonthly <- apply.monthly(benchRets, function(r) compoundReturn(r))
benchMonthly <- xts(coredata(benchMonthly), as.Date(index(benchMonthly)))

runningMax <- cummax(benchCum)
ddSeries   <- benchCum / runningMax - 1
ddMonthly  <- ddSeries[monthEnds[monthEnds >= first(index(ddSeries))]]
ddMonthly  <- na.locf(ddMonthly)

DD_DEEP <- -0.10
DD_MILD <- -0.05

regimes <- data.frame(
  date     = index(ddMonthly),
  drawdown = as.numeric(coredata(ddMonthly)),
  stringsAsFactors = FALSE
)
bmMon <- data.frame(date = index(benchMonthly),
                     bmRet = as.numeric(coredata(benchMonthly)),
                     stringsAsFactors = FALSE)
regimes <- merge(regimes, bmMon, by = "date", all.x = TRUE)
regimes$bmRet[is.na(regimes$bmRet)] <- 0
regimes$regime <- with(regimes, ifelse(
  drawdown > DD_MILD,                            "Normal",
  ifelse(drawdown <= DD_DEEP,                    "Drawdown",
  ifelse(drawdown > DD_DEEP & bmRet > 0,         "Recovery",
                                                  "Other"))))

cat(sprintf("Regimes: Normal=%d  Drawdown=%d  Recovery=%d  Other=%d\n",
    sum(regimes$regime == "Normal"),   sum(regimes$regime == "Drawdown"),
    sum(regimes$regime == "Recovery"), sum(regimes$regime == "Other")))

# ═══════════════════════════════════════════════════════════════
# Monthly persistence: Spearman corr + portfolio membership overlap
# ═══════════════════════════════════════════════════════════════

cat("=== MONTHLY PERSISTENCE ===\n")
MIN_COMMON <- 30L
firstMI <- which(!sapply(momCache, is.null))[1]

persistRows <- list()
for (mi in seq(firstMI, length(monthEnds) - 1L)) {

  # ── Spearman rank correlation of momentum scores ──
  mom_t   <- momCache[[mi]]
  mom_t1  <- momCache[[mi + 1L]]
  if (is.null(mom_t) || is.null(mom_t1)) next
  common <- intersect(names(mom_t), names(mom_t1))
  if (length(common) < MIN_COMMON) next
  rho <- cor(mom_t[common], mom_t1[common], method = "spearman")
  if (is.na(rho)) next

  # ── Portfolio membership overlap: Momentum (top 20) ──
  momStocks_t  <- pickMom(mi,     monthEnds[mi],     NULL, universeCache, TOP_N)
  momStocks_t1 <- pickMom(mi + 1L, monthEnds[mi + 1L], NULL, universeCache, TOP_N)
  momOverlap <- if (is.null(momStocks_t) || is.null(momStocks_t1)) NA_real_
                else length(intersect(momStocks_t, momStocks_t1)) / TOP_N

  # ── Portfolio membership overlap: Mom+Skew (top 20) ──
  warmupSkew <- which(!sapply(expRsCache, is.null))[1]
  skewOverlap <- NA_real_
  if (mi >= warmupSkew) {
    skewStocks_t  <- pickSkew(mi,     monthEnds[mi],     NULL, universeCache, TOP_N)
    skewStocks_t1 <- pickSkew(mi + 1L, monthEnds[mi + 1L], NULL, universeCache, TOP_N)
    if (!is.null(skewStocks_t) && !is.null(skewStocks_t1))
      skewOverlap <- length(intersect(skewStocks_t, skewStocks_t1)) / TOP_N
  }

  # ── Regime ──
  sigDate <- monthEnds[mi]
  regimeRow <- which(regimes$date == sigDate)
  regime <- if (length(regimeRow) == 1) regimes$regime[regimeRow] else "Other"
  dd     <- if (length(regimeRow) == 1) regimes$drawdown[regimeRow] else NA_real_

  persistRows[[length(persistRows) + 1L]] <- data.frame(
    date        = sigDate,
    regime      = regime,
    drawdown    = dd,
    spearman    = rho,
    momOverlap  = momOverlap,
    skewOverlap = skewOverlap,
    nStocks     = length(common),
    stringsAsFactors = FALSE
  )
}
persistDf <- do.call(rbind, persistRows)

# ═══════════════════════════════════════════════════════════════
# Summary by regime
# ═══════════════════════════════════════════════════════════════

statsFn <- function(x) c(
  N      = length(x),
  Mean   = mean(x, na.rm = TRUE),
  Median = median(x, na.rm = TRUE),
  SD     = sd(x, na.rm = TRUE),
  Min    = min(x, na.rm = TRUE),
  Max    = max(x, na.rm = TRUE)
)

cat("\n=== PERSISTENCE BY REGIME ===\n")
for (metric in c("spearman", "momOverlap", "skewOverlap")) {
  cat(sprintf("\n  --- %s ---\n", metric))
  for (r in c("Normal", "Drawdown", "Recovery")) {
    sub <- persistDf[[metric]][persistDf$regime == r]
    s <- statsFn(sub)
    cat(sprintf("  %s (n=%d): mean=%.4f  median=%.4f  sd=%.4f  [%.4f, %.4f]\n",
        r, s["N"], s["Mean"], s["Median"], s["SD"], s["Min"], s["Max"]))
  }
}

# ═══════════════════════════════════════════════════════════════
# Output rank-persistence.md
# ═══════════════════════════════════════════════════════════════

mdPath <- sprintf("%s/rank-persistence.md", reportPath)
sink(mdPath)

hdr <- paste0(
  "# Momentum Rank & Portfolio Persistence by Market Regime\n\n",
  "**Period:** ", first(persistDf$date), " → ", last(persistDf$date), "  \n",
  "**Momentum definition:** ", CFG$MOM_LB, "-month lookback, no skip, top 60% FF-mcap universe  \n",
  "**Portfolios:** Momentum = top ", TOP_N, " by 12-mo return; ",
  "Mom+Skew = top decile momentum → top tercile expected skewness → top ", TOP_N, "  \n",
  "**Persistence metrics:**  \n",
  "  1. **Spearman ρ:** Corr(MOM_t, MOM_{t+1}) — score-level rank persistence  \n",
  "  2. **Mom Overlap:** fraction of Momentum top-", TOP_N, " stocks retained from t → t+1  \n",
  "  3. **Skew Overlap:** fraction of Mom+Skew top-", TOP_N, " stocks retained from t → t+1  \n\n",
  "**Regime definitions (NIFTY500 MOMENTUM 50 TR drawdown from running peak):**\n\n",
  "- **Normal:** drawdown > ", as.integer(DD_MILD * 100), "%\n",
  "- **Drawdown:** drawdown ≤ ", as.integer(DD_DEEP * 100), "%\n",
  "- **Recovery:** drawdown between ", as.integer(DD_DEEP * 100), "% and ",
  as.integer(DD_MILD * 100), "%, with positive monthly return\n\n"
)
cat(hdr)

# ── Spearman table ──
cat("---\n\n## 1. Momentum Score Persistence (Spearman ρ)\n\n")
cat("| Regime | N | Mean ρ | Median ρ | SD | Min | Max |\n")
cat("|--------|---|--------|----------|-----|-----|-----|\n")
for (r in c("Normal", "Drawdown", "Recovery")) {
  sub <- persistDf$spearman[persistDf$regime == r]
  s <- statsFn(sub)
  cat(sprintf("| %s | %d | %.4f | %.4f | %.4f | %.4f | %.4f |\n",
      r, s["N"], s["Mean"], s["Median"], s["SD"], s["Min"], s["Max"]))
}
cat("\n")

# ── Portfolio overlap table ──
cat("---\n\n## 2. Portfolio Membership Persistence (fraction retained t → t+1)\n\n")
cat("| Regime | N | Mom Mean | Mom Median | Skew Mean | Skew Median | Skew − Mom |\n")
cat("|--------|---|----------|------------|-----------|-------------|------------|\n")
for (r in c("Normal", "Drawdown", "Recovery")) {
  mSub <- persistDf$momOverlap[persistDf$regime == r]
  sSub <- persistDf$skewOverlap[persistDf$regime == r]
  ms <- statsFn(mSub); ss <- statsFn(sSub)
  delta <- mean(sSub, na.rm = TRUE) - mean(mSub, na.rm = TRUE)
  cat(sprintf("| %s | %d | %.3f | %.3f | %.3f | %.3f | %+.3f |\n",
      r, ms["N"], ms["Mean"], ms["Median"], ss["Mean"], ss["Median"], delta))
}
cat("\n")

# ── Interpretation ──
cat("---\n\n## 3. Interpretation\n\n")

momAll  <- persistDf$momOverlap[!is.na(persistDf$momOverlap)]
skewAll <- persistDf$skewOverlap[!is.na(persistDf$skewOverlap)]
deltaOverall <- mean(skewAll) - mean(momAll)
cat(paste0(
  "**Overall:** Momentum retains **", round(mean(momAll) * 100), "%** of stocks month-to-month (mean). ",
  "Mom+Skew retains **", round(mean(skewAll) * 100), "%** — the skewness overlay ",
  if (deltaOverall > 0) "reduces" else "increases",
  " turnover by ", round(abs(deltaOverall) * 100), " pp.\n\n"))

for (r in c("Normal", "Drawdown", "Recovery")) {
  mSub <- persistDf$momOverlap[persistDf$regime == r]
  sSub <- persistDf$skewOverlap[persistDf$regime == r]
  if (length(mSub) < 3 || length(sSub) < 3) next
  delta <- mean(sSub, na.rm = TRUE) - mean(mSub, na.rm = TRUE)
  direction <- if (delta > 0) "higher" else "lower"
  interpret <- if (abs(delta) < 0.03) {
    "The difference is small."
  } else if (delta > 0) {
    "The skewness overlay stabilizes holdings."
  } else {
    "The skewness overlay increases turnover."
  }
  cat(paste0("- **", r, ":** Mom+Skew retention is **", round(abs(delta), 3), " ", direction,
      "** than Momentum (", round(mean(sSub, na.rm = TRUE), 3), " vs ",
      round(mean(mSub, na.rm = TRUE), 3), "). ", interpret, "\n"))
}

# Spearman interpretation
spearmanNorm <- mean(persistDf$spearman[persistDf$regime == "Normal"], na.rm = TRUE)
spearmanDD   <- mean(persistDf$spearman[persistDf$regime == "Drawdown"], na.rm = TRUE)
cat(paste0(
  "\n**Momentum score persistence (Spearman rho):** mean ", round(mean(persistDf$spearman, na.rm = TRUE), 3), " overall. ",
  "Drawdown months (", round(spearmanDD, 3), ") vs Normal (", round(spearmanNorm, 3), ") — difference is negligible. ",
  "Momentum ranks are highly sticky regardless of regime.\n"))

# ── Monthly detail ──
cat("\n---\n\n## 4. Monthly Detail\n\n")
cat("| Date | Regime | DD% | Spearman ρ | Mom Overlap | Skew Overlap |\n")
cat("|------|--------|-----|------------|-------------|-------------|\n")
for (i in seq_len(nrow(persistDf))) {
  cat(sprintf("| %s | %s | %.1f%% | %.3f | %.2f | %.2f |\n",
      persistDf$date[i], persistDf$regime[i],
      persistDf$drawdown[i] * 100,
      persistDf$spearman[i],
      persistDf$momOverlap[i],
      persistDf$skewOverlap[i]))
}

# ── Lowest / Highest turnover months ──
cat("\n---\n\n## 5. Highest Turnover Months (lowest Mom+Skew overlap)\n\n")
cat("| Date | Regime | DD% | Mom Overlap | Skew Overlap |\n")
cat("|------|--------|-----|-------------|-------------|\n")
low10 <- head(persistDf[order(persistDf$skewOverlap), ], 10)
for (i in seq_len(nrow(low10))) {
  cat(sprintf("| %s | %s | %.1f%% | %.2f | %.2f |\n",
      low10$date[i], low10$regime[i], low10$drawdown[i] * 100,
      low10$momOverlap[i], low10$skewOverlap[i]))
}

cat("\n---\n\n## 6. Lowest Turnover Months (highest Mom+Skew overlap)\n\n")
cat("| Date | Regime | DD% | Mom Overlap | Skew Overlap |\n")
cat("|------|--------|-----|-------------|-------------|\n")
high10 <- head(persistDf[order(-persistDf$skewOverlap), ], 10)
for (i in seq_len(nrow(high10))) {
  cat(sprintf("| %s | %s | %.1f%% | %.2f | %.2f |\n",
      high10$date[i], high10$regime[i], high10$drawdown[i] * 100,
      high10$momOverlap[i], high10$skewOverlap[i]))
}

# ── Post-2020-05-01 subset ──
POST_CUTOFF <- as.Date("2020-05-01")
postDf <- persistDf[persistDf$date >= POST_CUTOFF, ]
if (nrow(postDf) > 10) {
  cat("\n---\n\n## 7. Post-2020-05-01 Subset\n\n")

  cat("### Spearman ρ (post-2020)\n\n")
  cat("| Regime | N | Mean ρ | Median ρ |\n")
  cat("|--------|---|--------|----------|\n")
  for (r in c("Normal","Drawdown","Recovery")) {
    sub <- postDf$spearman[postDf$regime == r]
    if (length(sub) < 2) next
    s <- statsFn(sub)
    cat(sprintf("| %s | %d | %.4f | %.4f |\n", r, s["N"], s["Mean"], s["Median"]))
  }
  cat("\n")

  cat("### Portfolio Overlap (post-2020)\n\n")
  cat("| Regime | N | Mom Mean | Mom Median | Skew Mean | Skew Median |\n")
  cat("|--------|---|----------|------------|-----------|-------------|\n")
  for (r in c("Normal","Drawdown","Recovery")) {
    mSub <- postDf$momOverlap[postDf$regime == r]
    sSub <- postDf$skewOverlap[postDf$regime == r]
    if (length(mSub) < 2) next
    ms <- statsFn(mSub); ss <- statsFn(sSub)
    cat(sprintf("| %s | %d | %.3f | %.3f | %.3f | %.3f |\n",
        r, ms["N"], ms["Mean"], ms["Median"], ss["Mean"], ss["Median"]))
  }
  cat("\n")

  cat(paste0(
    "**Post-2020 summary:** Momentum scores remain highly persistent (Spearman ~",
    round(mean(postDf$spearman, na.rm=TRUE), 3), "). ",
    "Portfolio overlap: Mom ~", round(mean(postDf$momOverlap, na.rm=TRUE)*100, 0),
    "%, Skew ~", round(mean(postDf$skewOverlap, na.rm=TRUE)*100, 0),
    "% — the high-churn pattern persists in the recent regime.\n"))
}

sink()
cat(sprintf("\nSaved: %s\n", mdPath))
cat("\n===== DONE =====\n")

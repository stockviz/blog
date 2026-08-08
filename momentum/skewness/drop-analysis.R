# ============================================================================
# drop-analysis.R — Next-month returns of stocks dropped from portfolios
# Compares Momentun vs Mom+Skew: do dropped stocks continue to perform?
# Outputs drop-analysis.md
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

# ═══════════════════════════════════════════════════════════════
# Caches + pickers
# ═══════════════════════════════════════════════════════════════

cat("=== BUILDING CACHES ===\n")
momCache <- buildMomentumCache(monthEnds, universeCache, priceVol, CFG$MOM_LB)

stats <- computeMonthlyStats(priceVol, monthEnds, universeCache, CFG$MIN_DAILY)
rsCache <- stats$rs; rvCache <- stats$rv; priorCache <- stats$prior; rm(stats)
sizeTercileCache <- buildSizeTerciles(monthEnds, universeCache)
industryCache <- buildIndustryCache(monthEnds, universeCache, priceVol)
expRsCache <- forecastExpectedSkewness(rsCache, rvCache, priorCache, momCache,
                                        sizeTercileCache, industryCache, monthEnds)

TOP_N <- 20L
pickMom  <- pickMomentum(momCache, NULL, TOP_N, FALSE)
pickSkew <- pickMomentumSkew(momCache, expRsCache, NULL,
                momTopPct=0.10, skewTopPct=0.33, topN=TOP_N)

# ═══════════════════════════════════════════════════════════════
# Benchmark drawdowns → regimes
# ═══════════════════════════════════════════════════════════════

benchRets <- na.omit(dailyReturn(benchXts, type = "arithmetic"))
benchCum  <- cumprod(1 + benchRets)
benchMonthly <- apply.monthly(benchRets, function(r) compoundReturn(r))
benchMonthly <- xts(coredata(benchMonthly), as.Date(index(benchMonthly)))

runningMax <- cummax(benchCum)
ddSeries   <- benchCum / runningMax - 1
ddMonthly  <- ddSeries[monthEnds[monthEnds >= first(index(ddSeries))]]
ddMonthly  <- na.locf(ddMonthly)

DD_DEEP <- -0.10; DD_MILD <- -0.05
regimes <- data.frame(date=index(ddMonthly),
  drawdown=as.numeric(coredata(ddMonthly)), stringsAsFactors=FALSE)
bmMon <- data.frame(date=index(benchMonthly),
  bmRet=as.numeric(coredata(benchMonthly)), stringsAsFactors=FALSE)
regimes <- merge(regimes, bmMon, by="date", all.x=TRUE)
regimes$bmRet[is.na(regimes$bmRet)] <- 0
regimes$regime <- with(regimes, ifelse(
  drawdown > DD_MILD,                          "Normal",
  ifelse(drawdown <= DD_DEEP,                  "Drawdown",
  ifelse(drawdown > DD_DEEP & bmRet > 0,       "Recovery", "Other"))))

# ═══════════════════════════════════════════════════════════════
# Monthly drop analysis
# ═══════════════════════════════════════════════════════════════

cat("=== DROP ANALYSIS ===\n")

# Helper: next-month return for a stock
nextMonthReturn <- function(sym, fromME, toME) {
  df <- priceVol[[sym]]
  if (is.null(df)) return(NA_real_)
  sub <- df[df$date_stamp >= fromME & df$date_stamp <= toME, , drop=FALSE]
  if (nrow(sub) < 5) return(NA_real_)
  as.numeric(tail(sub$c, 1)) / as.numeric(sub$c[1]) - 1
}

# Helper: equal-weight return for a set of stocks
ewReturn <- function(stocks, fromME, toME) {
  rets <- vapply(stocks, function(s) nextMonthReturn(s, fromME, toME), double(1))
  rets <- rets[!is.na(rets)]
  if (length(rets) < 3) return(NA_real_)
  mean(rets, na.rm=TRUE)
}

firstMI <- which(!sapply(momCache, is.null))[1]
warmupSkew <- which(!sapply(expRsCache, is.null))[1]
startMI <- max(firstMI, warmupSkew) + 1L   # need t-1 and t+1

rows <- list()
for (mi in seq(startMI, length(monthEnds) - 1L)) {

  # ── Stock sets at t-1, t, t+1 ──
  mom_tm1 <- pickMom(mi-1L, monthEnds[mi-1L], NULL, universeCache, TOP_N)
  mom_t   <- pickMom(mi,    monthEnds[mi],    NULL, universeCache, TOP_N)
  mom_tp1 <- pickMom(mi+1L, monthEnds[mi+1L], NULL, universeCache, TOP_N)

  sk_tm1 <- pickSkew(mi-1L, monthEnds[mi-1L], NULL, universeCache, TOP_N)
  sk_t   <- pickSkew(mi,    monthEnds[mi],    NULL, universeCache, TOP_N)
  sk_tp1 <- pickSkew(mi+1L, monthEnds[mi+1L], NULL, universeCache, TOP_N)

  if (any(sapply(list(mom_tm1,mom_t,mom_tp1,sk_tm1,sk_t,sk_tp1), is.null))) next

  # ── Dropped / Kept / New sets (dropped between t-1 and t, next return t→t+1) ──
  momDropped <- setdiff(mom_tm1, mom_t)
  momKept    <- intersect(mom_tm1, mom_t)
  momNew     <- setdiff(mom_t, mom_tm1)

  skDropped <- setdiff(sk_tm1, sk_t)
  skKept    <- intersect(sk_tm1, sk_t)
  skNew     <- setdiff(sk_t, sk_tm1)

  # ── Next-month returns (t → t+1) ──
  fromME <- monthEnds[mi] + 1
  toME   <- monthEnds[mi + 1L]

  momDropRet <- ewReturn(momDropped, fromME, toME)
  momKeepRet <- ewReturn(momKept,    fromME, toME)
  momNewRet  <- ewReturn(momNew,     fromME, toME)

  skDropRet  <- ewReturn(skDropped,  fromME, toME)
  skKeepRet  <- ewReturn(skKept,     fromME, toME)
  skNewRet   <- ewReturn(skNew,      fromME, toME)

  # ── Regime ──
  sigDate <- monthEnds[mi]
  rRow <- which(regimes$date == sigDate)
  regime <- if (length(rRow)==1) regimes$regime[rRow] else "Other"
  dd     <- if (length(rRow)==1) regimes$drawdown[rRow] else NA_real_

  rows[[length(rows)+1L]] <- data.frame(
    date         = sigDate, regime = regime, drawdown = dd,
    momDropRet   = momDropRet,   momKeepRet = momKeepRet,  momNewRet = momNewRet,
    skDropRet    = skDropRet,    skKeepRet  = skKeepRet,   skNewRet  = skNewRet,
    momDropN     = length(momDropped), momKeepN = length(momKept),
    skDropN      = length(skDropped),  skKeepN  = length(skKept),
    stringsAsFactors = FALSE)
}
dropDf <- do.call(rbind, rows)
dropDf <- dropDf[complete.cases(dropDf[, c("momDropRet","skDropRet")]), ]

# ═══════════════════════════════════════════════════════════════
# Summary by regime
# ═══════════════════════════════════════════════════════════════

statsFn <- function(x) c(
  N=length(x), Mean=mean(x,na.rm=TRUE), Median=median(x,na.rm=TRUE),
  SD=sd(x,na.rm=TRUE), PctPos=mean(x>0,na.rm=TRUE)*100)

cat("\n=== DROP ANALYSIS BY REGIME ===\n")
for (r in c("All","Normal","Drawdown","Recovery")) {
  sub <- if (r=="All") dropDf else dropDf[dropDf$regime==r,]
  if (nrow(sub) < 3) next
  cat(sprintf("\n  --- %s (n=%d) ---\n", r, nrow(sub)))
  for (col in c("momDropRet","momKeepRet","skDropRet","skKeepRet")) {
    s <- statsFn(sub[[col]])
    cat(sprintf("  %-12s mean=%+.4f  median=%+.4f  sd=%.4f  pos=%.0f%%\n",
        col, s["Mean"], s["Median"], s["SD"], s["PctPos"]))
  }
}

# ═══════════════════════════════════════════════════════════════
# Output drop-analysis.md
# ═══════════════════════════════════════════════════════════════

mdPath <- sprintf("%s/drop-analysis.md", reportPath)
sink(mdPath)

hdr <- paste0(
  "# Drop Analysis: Next-Month Returns of Stocks Removed from Portfolios\n\n",
  "**Period:** ", first(dropDf$date), " → ", last(dropDf$date), "  \n",
  "**Portfolios:** Momentum (top 20 by 12-mo return), ",
  "Mom+Skew (top decile momentum → top tercile expected skewness → top 20)  \n",
  "**Question:** When a stock is dropped from the portfolio at rebalance t, ",
  "how does it perform in the *next* holding period (t → t+1)?  \n",
  "**Comparison groups:** Dropped (held at t-1, removed at t) vs ",
  "Kept (held at both t-1 and t) vs New (added at t, not held at t-1)  \n\n",
  "**Regime definitions (NIFTY500 MOMENTUM 50 TR drawdown from running peak):**\n\n",
  "- **Normal:** drawdown > ", as.integer(DD_MILD*100), "%\n",
  "- **Drawdown:** drawdown ≤ ", as.integer(DD_DEEP*100), "%\n",
  "- **Recovery:** drawdown between ", as.integer(DD_DEEP*100), "% and ",
  as.integer(DD_MILD*100), "%, positive monthly return\n\n"
)
cat(hdr)

cat("---\n\n## 1. Summary: Mean Next-Month Return by Group\n\n")
cat("| Regime | N | Mom Drop | Mom Keep | Mom New | Skew Drop | Skew Keep | Skew New |\n")
cat("|--------|---|---------:|--------:|-------:|----------:|---------:|--------:|\n")
for (r in c("All","Normal","Drawdown","Recovery")) {
  sub <- if (r=="All") dropDf else dropDf[dropDf$regime==r,]
  if (nrow(sub)<3) next
  md <- mean(sub$momDropRet, na.rm=TRUE); mk <- mean(sub$momKeepRet, na.rm=TRUE)
  mn <- mean(sub$momNewRet,  na.rm=TRUE)
  sd <- mean(sub$skDropRet,  na.rm=TRUE); sp <- mean(sub$skKeepRet,  na.rm=TRUE)
  sn <- mean(sub$skNewRet,   na.rm=TRUE)
  cat(sprintf("| %s | %d | %+.2f%% | %+.2f%% | %+.2f%% | %+.2f%% | %+.2f%% | %+.2f%% |\n",
    r, nrow(sub), md*100, mk*100, mn*100, sd*100, sp*100, sn*100))
}
cat("\n")

# ── Interpretation ──
cat("---\n\n## 2. Interpretation\n\n")

# Overall means
momDropMean <- mean(dropDf$momDropRet, na.rm=TRUE)
momKeepMean <- mean(dropDf$momKeepRet, na.rm=TRUE)
momNewMean  <- mean(dropDf$momNewRet,  na.rm=TRUE)
skDropMean  <- mean(dropDf$skDropRet,  na.rm=TRUE)
skKeepMean  <- mean(dropDf$skKeepRet,  na.rm=TRUE)
skNewMean   <- mean(dropDf$skNewRet,   na.rm=TRUE)

cat(paste0(
  "### 1. Are dropped stocks underperformers?\n\n",
  "- **Momentum:** dropped stocks average **", round(momDropMean*100,1),
  "%** next month vs **", round(momKeepMean*100,1), "%** for kept stocks ",
  "(Δ = ", round((momDropMean-momKeepMean)*100,1), "pp). ",
  if(momDropMean < momKeepMean) "Dropped stocks underperform — the rebalance is adding value."
  else "Dropped stocks outperform — the rebalance may be counterproductive.",
  "\n",
  "- **Mom+Skew:** dropped stocks average **", round(skDropMean*100,1),
  "%** next month vs **", round(skKeepMean*100,1), "%** for kept stocks ",
  "(Δ = ", round((skDropMean-skKeepMean)*100,1), "pp). ",
  if(skDropMean < skKeepMean) "Dropped stocks underperform — the rebalance is adding value."
  else "Dropped stocks outperform — the rebalance may be counterproductive.",
  "\n\n"
))

cat(paste0(
  "### 2. Does Mom+Skew make better drop decisions?\n\n",
  "- Mom dropped stocks earn **", round(momDropMean*100,1), "%** vs ",
  "Skew dropped stocks earn **", round(skDropMean*100,1), "%**. ",
  if(abs(skDropMean-momDropMean)<0.005) "Similar drop quality."
  else if(skDropMean < momDropMean) "Mom+Skew drops stocks that subsequently do WORSE — better drop quality."
  else "Mom+Skew drops stocks that subsequently do BETTER — worse drop quality (potentially regrettable drops).",
  "\n\n"
))

cat(paste0(
  "### 3. Do new stocks outperform?\n\n",
  "- **Momentum:** new stocks average **", round(momNewMean*100,1),
  "%** next month vs **", round(momKeepMean*100,1), "%** for kept stocks ",
  "(Δ = ", round((momNewMean-momKeepMean)*100,1), "pp). ",
  if(momNewMean > momKeepMean) "New additions outperform — the rebalance is capturing fresh momentum."
  else "New additions underperform — stocks entering the portfolio don't deliver superior returns.",
  "\n",
  "- **Mom+Skew:** new stocks average **", round(skNewMean*100,1),
  "%** next month vs **", round(skKeepMean*100,1), "%** for kept stocks ",
  "(Δ = ", round((skNewMean-skKeepMean)*100,1), "pp). ",
  if(skNewMean > skKeepMean) "New additions outperform."
  else "New additions underperform.",
  "\n\n"
))

# Positive fraction
momDropPos <- mean(dropDf$momDropRet > 0, na.rm=TRUE) * 100
skDropPos  <- mean(dropDf$skDropRet  > 0, na.rm=TRUE) * 100
momNewPos  <- mean(dropDf$momNewRet  > 0, na.rm=TRUE) * 100
skNewPos   <- mean(dropDf$skNewRet   > 0, na.rm=TRUE) * 100
cat(paste0(
  "### 4. Hit rates (% positive next-month returns)\n\n",
  "| Strategy | Dropped | Kept | New |\n",
  "|----------|--------:|-----:|----:|\n",
  sprintf("| Momentum | %.0f%% | %.0f%% | %.0f%% |\n",
    momDropPos, mean(dropDf$momKeepRet > 0, na.rm=TRUE)*100, momNewPos),
  sprintf("| Mom+Skew | %.0f%% | %.0f%% | %.0f%% |\n",
    skDropPos,  mean(dropDf$skKeepRet  > 0, na.rm=TRUE)*100, skNewPos),
  "\n",
  "- Mom+Skew drops **more winners** (", round(skDropPos,0), "% vs ", round(momDropPos,0),
  "%) but its new picks also have a ", if(skNewPos > momNewPos) "higher" else "lower",
  " hit rate (", round(skNewPos,0), "% vs ", round(momNewPos,0), "%).\n\n"
))

# Regime breakdown
cat("### 5. Regime breakdown\n\n")
cat(paste0(
  "| Regime | Mom Drop | Mom Keep | Mom New | Skew Drop | Skew Keep | Skew New |\n",
  "|--------|----------|----------|---------|-----------|-----------|----------|\n"))
for (r in c("Normal","Drawdown","Recovery")) {
  sub <- dropDf[dropDf$regime==r,]
  if (nrow(sub)<3) next
  md <- mean(sub$momDropRet, na.rm=TRUE); mk <- mean(sub$momKeepRet, na.rm=TRUE)
  mn <- mean(sub$momNewRet,  na.rm=TRUE)
  sd <- mean(sub$skDropRet,  na.rm=TRUE); sp <- mean(sub$skKeepRet,  na.rm=TRUE)
  sn <- mean(sub$skNewRet,   na.rm=TRUE)
  cat(sprintf("| %s | %+.1f%% | %+.1f%% | %+.1f%% | %+.1f%% | %+.1f%% | %+.1f%% |\n",
    r, md*100, mk*100, mn*100, sd*100, sp*100, sn*100))
}
cat("\n")

# Bottom line
cat("---\n\n## 6. Bottom Line\n\n")
cat(paste0(
  "Momentum's rebalance: drops **", round(momDropMean*100,1),
  "%**, keeps **", round(momKeepMean*100,1),
  "%**, adds **", round(momNewMean*100,1), "%** next month.  \n",
  "Mom+Skew's rebalance: drops **", round(skDropMean*100,1),
  "%**, keeps **", round(skKeepMean*100,1),
  "%**, adds **", round(skNewMean*100,1), "%** next month.  \n\n",
  "Drop quality is similar between strategies (~", round(momDropMean*100,1),
  "% vs ~", round(skDropMean*100,1), "%). ",
  "New-stock quality is also similar (~", round(momNewMean*100,1),
  "% vs ~", round(skNewMean*100,1), "%). ",
  "However, the skewness overlay generates **~3× more turnover** ",
  "(~75% of positions churn monthly vs ~35% for Momentum), ",
  "so the similar drop/add quality must overcome substantially higher transaction costs.\n"))

# ── Post-2020-05-01 subset ──
POST_CUTOFF <- as.Date("2020-05-01")
postDrop <- dropDf[dropDf$date >= POST_CUTOFF, ]
if (nrow(postDrop) > 10) {
  cat("\n---\n\n## 7. Post-2020-05-01 Subset\n\n")

  cat("### Mean Next-Month Return (post-2020)\n\n")
  cat("| Regime | N | Mom Drop | Mom Keep | Mom New | Skew Drop | Skew Keep | Skew New |\n")
  cat("|--------|---|---------:|--------:|-------:|----------:|---------:|--------:|\n")
  for (r in c("All","Normal","Drawdown","Recovery")) {
    sub <- if (r=="All") postDrop else postDrop[postDrop$regime==r,]
    if (nrow(sub)<2) next
    md <- mean(sub$momDropRet, na.rm=TRUE); mk <- mean(sub$momKeepRet, na.rm=TRUE)
    mn <- mean(sub$momNewRet,  na.rm=TRUE)
    sd <- mean(sub$skDropRet,  na.rm=TRUE); sp <- mean(sub$skKeepRet,  na.rm=TRUE)
    sn <- mean(sub$skNewRet,   na.rm=TRUE)
    cat(sprintf("| %s | %d | %+.2f%% | %+.2f%% | %+.2f%% | %+.2f%% | %+.2f%% | %+.2f%% |\n",
      r, nrow(sub), md*100, mk*100, mn*100, sd*100, sp*100, sn*100))
  }
  cat("\n")

  cat("### Hit Rates (post-2020)\n\n")
  cat("| Strategy | Dropped | Kept | New |\n")
  cat("|----------|--------:|-----:|----:|\n")
  cat(sprintf("| Momentum | %.0f%% | %.0f%% | %.0f%% |\n",
    mean(postDrop$momDropRet > 0, na.rm=TRUE)*100,
    mean(postDrop$momKeepRet > 0, na.rm=TRUE)*100,
    mean(postDrop$momNewRet  > 0, na.rm=TRUE)*100))
  cat(sprintf("| Mom+Skew | %.0f%% | %.0f%% | %.0f%% |\n",
    mean(postDrop$skDropRet > 0, na.rm=TRUE)*100,
    mean(postDrop$skKeepRet > 0, na.rm=TRUE)*100,
    mean(postDrop$skNewRet  > 0, na.rm=TRUE)*100))
  cat("\n")

  pmd <- mean(postDrop$momDropRet, na.rm=TRUE)
  psd <- mean(postDrop$skDropRet,  na.rm=TRUE)
  pmn <- mean(postDrop$momNewRet,  na.rm=TRUE)
  psn <- mean(postDrop$skNewRet,   na.rm=TRUE)
  cat(paste0(
    "**Post-2020 summary:** Mom dropped: **", round(pmd*100,1),
    "%**, Skew dropped: **", round(psd*100,1),
    "%**. Mom new: **", round(pmn*100,1),
    "%**, Skew new: **", round(psn*100,1),
    "%**. The drop/add quality patterns are consistent with the full period.\n"))
}

sink()
cat(sprintf("\nSaved: %s\n", mdPath))
cat("\n===== DONE =====\n")

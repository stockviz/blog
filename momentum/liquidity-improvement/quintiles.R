# ============================================================================
# quintiles.R — LIQC quintile × lookback analysis
# ============================================================================
# Loads checkpoint. Computes 1/3/6/12-month LIQC, then next-month return
# statistics by quintile. No portfolio construction — pure signal analysis.
# ============================================================================
suppressPackageStartupMessages({
  library('quantmod'); library('PerformanceAnalytics'); library('xts')
  library('tidyverse'); library('lubridate')
})

source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/momentum/liquidity-improvement/liqim-common.R")
source("/mnt/data/blog/momentum/liquidity-improvement/liqim-config.R")

reportPath <- "/mnt/data/blog/momentum/liquidity-improvement"
chk <- readRDS(sprintf("%s/checkpoint.rds", reportPath))
priceVol <- lapply(chk$priceVol, function(df) df[order(df$date_stamp), ])
monthEnds <- chk$monthEnds; illiqCache <- chk$illiqCache
universeCache <- chk$universeCache; rm(chk)

# Next-month return for a single stock
nextMonthReturn <- function(tkr, sigDate) {
  holdStart <- sigDate + 1
  nextMe <- monthEnds[which(monthEnds > sigDate)[1]]
  if (is.na(nextMe)) return(NA_real_)
  rets <- stockReturns(priceVol[[tkr]], holdStart, nextMe)
  if (is.null(rets)) return(NA_real_)
  compoundReturn(coredata(rets))
}

lookbacks <- c(1L, 3L, 6L, 12L)

for (lb in lookbacks) {
  cat(sprintf("\n========== %d-month LIQC ==========\n", lb))

  liqcCache <- computeLIQC(illiqCache, monthEnds, lb)
  cat(sprintf("LIQC months: %d\n", sum(!sapply(liqcCache, is.null))))

  warmupMI <- which(!sapply(liqcCache, is.null))[1]
  if (is.na(warmupMI)) { cat("  No LIQC data\n"); next }

  allRows <- list()

  for (mi in seq(warmupMI, length(monthEnds))) {
    sigDate <- monthEnds[mi]; if (mi >= length(monthEnds)) break
    liqcVals <- liqcCache[[mi]]; if (is.null(liqcVals) || length(liqcVals) == 0) next
    u <- universeCache[[mi]]; if (is.null(u) || length(u) == 0) next
    lf <- liqcVals[names(liqcVals) %in% u]
    if (length(lf) < 50) next

    n <- length(lf); qSize <- floor(n / 5)

    for (q in 1:5) {
      iStart <- (q - 1) * qSize + 1
      iEnd   <- if (q < 5) q * qSize else n
      stocks <- names(lf)[iStart:iEnd]

      stockRets <- vapply(stocks, function(tkr) nextMonthReturn(tkr, sigDate), double(1))
      stockRets <- stockRets[!is.na(stockRets)]
      if (length(stockRets) < 10) next

      allRows[[length(allRows) + 1L]] <- data.frame(
        date = sigDate, quintile = q, n_stocks = length(stockRets),
        n_universe = n, mean_ret = mean(stockRets),
        median_ret = median(stockRets), sd_ret = sd(stockRets),
        pct_positive = mean(stockRets > 0) * 100,
        min_ret = min(stockRets), max_ret = max(stockRets),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(allRows) == 0) { cat("  No qualifying months\n"); next }

  df <- do.call(rbind, allRows)

  for (q in 1:5) {
    sub <- df[df$quintile == q, , drop = FALSE]
    if (nrow(sub) == 0) next
    cat(sprintf("\n--- Q%d (%d months) ---\n", q, nrow(sub)))
    cat(sprintf("  Mean: %.4f (%.2f%%), Median: %.4f\n",
        mean(sub$mean_ret), mean(sub$mean_ret) * 100, median(sub$mean_ret)))
    cat(sprintf("  %% up: %.1f%%, stock %% pos: %.1f%%\n",
        mean(sub$mean_ret > 0) * 100, mean(sub$pct_positive)))
    cat(sprintf("  Cumulative: %.4f (%.2f%%)\n",
        compoundReturn(sub$mean_ret), compoundReturn(sub$mean_ret) * 100))
    cat(sprintf("  Sharpe: %.3f\n", mean(sub$mean_ret) / sd(sub$mean_ret)))
  }

  csv <- sprintf("%s/quintile_lb%d.csv", reportPath, lb)
  write.csv(df, csv, row.names = FALSE)
  cat(sprintf("\nSaved: %s (%d rows)\n", csv, nrow(df)))
}

cat("\n===== DONE =====\n")

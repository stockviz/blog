# ============================================================================
# liq-common.R — LIQC/liquidity helpers for the Skewness project
# ============================================================================

winsorize <- function(x, lo = 0.01, hi = 0.99) {
  if (length(x) < 10) return(x)
  q <- quantile(x, c(lo, hi), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

# ── Compute LIQC (liquidity improvement) from ILLIQ cache ──────────────────
# LIQC = -(ILLIQ_now - mean(ILLIQ_past)), higher = more liquid improvement

computeLIQC <- function(illiqCache, monthEnds, lookback = 1L) {
  liqcCache <- vector("list", length(monthEnds))
  warmupMI <- lookback + 1L
  for (mi in seq(warmupMI + lookback, length(monthEnds))) {
    illiqNow <- illiqCache[[mi]]
    if (is.null(illiqNow)) next

    pastMonths <- (mi - lookback):(mi - 1L)
    pastMonths <- pastMonths[pastMonths >= 1L]
    pastList <- lapply(pastMonths, function(pm) illiqCache[[pm]])
    pastList <- pastList[!sapply(pastList, is.null)]
    if (length(pastList) < lookback) next

    commonSyms <- Reduce(intersect, c(list(names(illiqNow)), lapply(pastList, names)))
    if (length(commonSyms) < 20) next

    pastMeans <- vapply(commonSyms, function(sym) {
      mean(sapply(pastList, function(pl)
        if (sym %in% names(pl)) pl[[sym]] else NA_real_), na.rm = TRUE)
    }, double(1))
    names(pastMeans) <- commonSyms

    nowVals  <- illiqNow[commonSyms]
    liqcVals <- -(nowVals - pastMeans)
    liqcVals <- liqcVals[!is.na(liqcVals) & is.finite(liqcVals)]

    if (length(liqcVals) > 0) {
      liqcVals <- winsorize(liqcVals)
      liqcVals <- sort(liqcVals, decreasing = TRUE)
      liqcCache[[mi]] <- liqcVals
    }
  }
  liqcCache
}

# ── Build Q5 (bottom liquidity quintile) exclusion sets ─────────────────────

buildQ5Exclude <- function(monthEnds, liqcCache, universeCache) {
  q5Exclude <- vector("list", length(monthEnds))
  for (mi in seq_len(length(monthEnds))) {
    lq <- liqcCache[[mi]]
    if (is.null(lq)) next

    u <- universeCache[[mi]]
    if (is.null(u)) next

    lf <- lq[names(lq) %in% u]
    if (length(lf) < 50) next

    n     <- length(lf)
    qSize <- floor(n / 5)
    q5Exclude[[mi]] <- names(lf)[(4 * qSize + 1):n]
  }
  cat(sprintf("  Q5 exclude: %d months\n", sum(!sapply(q5Exclude, is.null))))
  q5Exclude
}

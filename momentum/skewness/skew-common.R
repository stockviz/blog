# ============================================================================
# skew-common.R — Shared helpers for the Skewness project pipeline
# ============================================================================
# Sourced by momentum.R and future build.R, backtest.R, consolidated.R

# ── Monthly realized skewness & volatility from daily returns ──────────────
# Returns list(rs=rsCache, rv=rvCache, prior=priorCache) each indexed by month

computeMonthlyStats <- function(priceVol, monthEnds, universeCache, minDaily = 15L) {
  rsCache    <- vector("list", length(monthEnds))
  rvCache    <- vector("list", length(monthEnds))
  priorCache <- vector("list", length(monthEnds))

  for (mi in seq(2L, length(monthEnds))) {
    me <- monthEnds[mi]
    ms <- floor_date(me, "month")

    syms <- universeCache[[mi]]
    if (is.null(syms) || length(syms) == 0) next

    rsVals    <- double(0)
    rvVals    <- double(0)
    priorVals <- double(0)

    for (sym in syms) {
      df <- priceVol[[sym]]
      if (is.null(df)) next

      sub <- df[df$date_stamp >= ms & df$date_stamp <= me, , drop = FALSE]
      if (nrow(sub) < minDaily + 1L) next   # need N+1 rows for N returns

      n    <- nrow(sub)
      rets <- diff(sub$c) / sub$c[-n]        # daily arithmetic returns

      if (length(rets) < minDaily) next

      N  <- length(rets)
      rv <- sqrt(mean(rets^2, na.rm = TRUE))

      # Paper's raw third-moment skewness: mean(r^3) / RV^3
      rs <- if (rv > 1e-12) mean(rets^3, na.rm = TRUE) / (rv^3) else NA_real_

      # Prior month return from first to last close
      prior <- as.numeric(tail(sub$c, 1)) / as.numeric(sub$c[1]) - 1

      if (!is.na(rs) && is.finite(rs) && !is.na(rv) && is.finite(rv)) {
        rsVals    <- c(rsVals,    setNames(rs,    sym))
        rvVals    <- c(rvVals,    setNames(rv,    sym))
        priorVals <- c(priorVals, setNames(prior, sym))
      }
    }

    if (length(rsVals) > 0) {
      rsCache[[mi]]    <- rsVals
      rvCache[[mi]]    <- rvVals
      priorCache[[mi]] <- priorVals
    }
  }

  cat(sprintf("  Monthly stats: %d RS months, %d RV months, %d prior months\n",
      sum(!sapply(rsCache, is.null)),
      sum(!sapply(rvCache, is.null)),
      sum(!sapply(priorCache, is.null))))

  list(rs = rsCache, rv = rvCache, prior = priorCache)
}

# ── Size terciles within universe (universeCache already sorted FF-mcap desc) ──

buildSizeTerciles <- function(monthEnds, universeCache) {
  sizeCache <- vector("list", length(monthEnds))

  for (mi in seq_len(length(monthEnds))) {
    u <- universeCache[[mi]]
    if (is.null(u) || length(u) < 30L) next

    n  <- length(u)
    t1 <- floor(n / 3)
    t2 <- floor(2 * n / 3)

    tercile              <- rep("LARGE", n)
    tercile[(t1 + 1):t2] <- "MEDIUM"
    tercile[(t2 + 1):n]  <- "SMALL"
    names(tercile)       <- u

    # Store as factor with LARGE = reference
    sizeCache[[mi]] <- factor(tercile, levels = c("LARGE", "MEDIUM", "SMALL"))
  }

  cat(sprintf("  Size terciles: %d months\n", sum(!sapply(sizeCache, is.null))))
  sizeCache
}

# ── Industry cache (per-month, per-symbol) ──────────────────────────────────
# Uses nse_industry table (same as getIMeta in industry-rotation/common.R).
# Stocks without an industry record are marked "UNK".

buildIndustryCache <- function(monthEnds, universeCache, priceVol) {
  lcon <- odbcDriverConnect(
    sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            ldbserver, "StockViz", ldbuser, ldbpassword),
    case = "nochange", believeNRows = TRUE)

  allSymbols <- names(priceVol)
  # Bulk-fetch all industry data in one query; split + findInterval per month
  indDf <- sqlQuery(lcon, sprintf(
    "select symbol, basic_industry, time_stamp from nse_industry
     where symbol in (%s) order by symbol, time_stamp",
    paste(sprintf("'%s'", allSymbols), collapse = ",")))
  indDf$time_stamp <- as.Date(indDf$time_stamp)
  odbcClose(lcon)

  indBySym <- split(indDf, indDf$symbol)

  indCache <- vector("list", length(monthEnds))
  for (mi in seq_len(length(monthEnds))) {
    sigNum <- as.numeric(monthEnds[mi])
    syms <- universeCache[[mi]]
    if (is.null(syms) || length(syms) == 0) next

    inds <- vapply(syms, function(sym) {
      rows <- indBySym[[sym]]
      if (is.null(rows) || nrow(rows) == 0) return("UNK")
      idx <- findInterval(sigNum, as.numeric(rows$time_stamp))
      if (idx < 1L) return("UNK")
      rows$basic_industry[idx]
    }, character(1))
    names(inds) <- syms
    indCache[[mi]] <- inds
  }

  cat(sprintf("  Industry cache: %d months, %d symbols with industry data\n",
      sum(!sapply(indCache, is.null)), length(indBySym)))
  indCache
}

# ── Cross-sectional expected-skewness forecast ─────────────────────────────
# Each month t:  RS(t) ~ RV(t-1) + RS(t-1) + MOM(t-1) + PRIOR(t-1) + SIZE(t-1) + INDUSTRY(t-1)
# Then forecast:  ExpectedRS(t+1) = alpha_hat + beta_hat * X(t)
# expRsCache[[mi]] is the forecast available at signal date mi (for holding mi→mi+1)

forecastExpectedSkewness <- function(rsCache, rvCache, priorCache, momCache,
                                      sizeTercileCache, industryCache,
                                      monthEnds, minStocks = 50L) {
  expRsCache <- vector("list", length(monthEnds))

  for (mi in seq(3L, length(monthEnds) - 1L)) {

    # ── RS at month mi (DV for estimation) ──
    rs_t <- rsCache[[mi]]
    if (is.null(rs_t) || length(rs_t) < minStocks) next

    # ── Lagged predictors at month mi-1 ──
    rv_t1    <- rvCache[[mi - 1L]]
    rs_t1    <- rsCache[[mi - 1L]]
    mom_t1   <- momCache[[mi - 1L]]
    prior_t1 <- priorCache[[mi - 1L]]
    size_t1  <- sizeTercileCache[[mi - 1L]]
    ind_t1   <- industryCache[[mi - 1L]]

    if (is.null(rv_t1) || is.null(rs_t1) || is.null(mom_t1) ||
        is.null(prior_t1) || is.null(size_t1) || is.null(ind_t1)) next

    # ── Common symbols for the cross-sectional regression ──
    common <- Reduce(intersect, list(
      names(rs_t), names(rv_t1), names(rs_t1),
      names(mom_t1), names(prior_t1), names(size_t1), names(ind_t1)))
    if (length(common) < minStocks) next

    # Pool industries with < 5 stocks into "OTHER" for stable FE
    indTab <- table(ind_t1[common])
    rareInds <- names(indTab)[indTab < 5L]
    indClean <- ind_t1
    toReplace <- common[indClean[common] %in% rareInds]
    indClean[toReplace] <- "OTHER"

    indVec <- indClean[common]
    nInd <- length(unique(indVec))
    useInd <- nInd >= 2L   # need 2+ levels for contrast coding

    regDf <- data.frame(
      RS        = rs_t[common],
      RV_lag    = rv_t1[common],
      RS_lag    = rs_t1[common],
      MOM_lag   = mom_t1[common],
      PRIOR_lag = prior_t1[common],
      SIZE      = size_t1[common],
      stringsAsFactors = FALSE
    )
    if (useInd) regDf$INDUSTRY <- indVec

    # ── Estimate monthly cross-sectional regression ──
    fmla <- if (useInd)
      RS ~ RV_lag + RS_lag + MOM_lag + PRIOR_lag + SIZE + INDUSTRY
    else
      RS ~ RV_lag + RS_lag + MOM_lag + PRIOR_lag + SIZE

    fit <- tryCatch(lm(fmla, data = regDf), error = function(e) NULL)
    if (is.null(fit)) next

    coefs <- coef(fit)

    # ── Forecast for month mi+1 using current (mi) predictors ──
    rv_now    <- rvCache[[mi]]
    rs_now    <- rsCache[[mi]]
    mom_now   <- momCache[[mi]]
    prior_now <- priorCache[[mi]]
    size_now  <- sizeTercileCache[[mi]]
    ind_now   <- industryCache[[mi]]

    if (is.null(rv_now) || is.null(rs_now) || is.null(mom_now) ||
        is.null(prior_now) || is.null(size_now) || is.null(ind_now)) next

    forecastSyms <- Reduce(intersect, list(
      names(rv_now), names(rs_now), names(mom_now),
      names(prior_now), names(size_now), names(ind_now)))
    if (length(forecastSyms) < 10L) next

    # Base forecast from continuous predictors
    expRs <- rep(coefs["(Intercept)"], length(forecastSyms))
    names(expRs) <- forecastSyms

    if ("RV_lag" %in% names(coefs))
      expRs <- expRs + coefs["RV_lag"] * rv_now[forecastSyms]
    if ("RS_lag" %in% names(coefs))
      expRs <- expRs + coefs["RS_lag"] * rs_now[forecastSyms]
    if ("MOM_lag" %in% names(coefs))
      expRs <- expRs + coefs["MOM_lag"] * mom_now[forecastSyms]
    if ("PRIOR_lag" %in% names(coefs))
      expRs <- expRs + coefs["PRIOR_lag"] * prior_now[forecastSyms]

    # Add size dummies
    sz <- as.character(size_now[forecastSyms])
    if ("SIZEMEDIUM" %in% names(coefs))
      expRs[sz == "MEDIUM"] <- expRs[sz == "MEDIUM"] + coefs["SIZEMEDIUM"]
    if ("SIZESMALL" %in% names(coefs))
      expRs[sz == "SMALL"]  <- expRs[sz == "SMALL"]  + coefs["SIZESMALL"]

    # Add industry fixed effects (coef names like "INDUSTRYPharmaceuticals")
    indCoefNames <- grep("^INDUSTRY", names(coefs), value = TRUE)
    for (icn in indCoefNames) {
      indName <- sub("^INDUSTRY", "", icn)
      mask <- ind_now[forecastSyms] == indName
      if (any(mask)) expRs[mask] <- expRs[mask] + coefs[icn]
    }

    expRs <- expRs[!is.na(expRs) & is.finite(expRs)]
    if (length(expRs) > 0) {
      expRs <- sort(expRs, decreasing = TRUE)
      expRsCache[[mi]] <- expRs    # forecast available at signal date mi
    }
  }

  cat(sprintf("  Expected RS forecast: %d months\n",
      sum(!sapply(expRsCache, is.null))))
  expRsCache
}

# ── Sequential sort picker: top momentum → top expected skewness ────────────

pickMomentumSkew <- function(momCache, expRsCache, excludeCache = NULL,
                              momTopPct = 0.10, skewTopPct = 0.33,
                              topN = 20L, minStocks = 5L) {
  function(mi, sigDate, liqcCache, universeCache, topN, ...) {

    mr <- momCache[[mi]]
    if (is.null(mr) || length(mr) < minStocks) return(NULL)

    u <- universeCache[[mi]]
    if (is.null(u) || length(u) < minStocks) return(NULL)

    mf <- mr[names(mr) %in% u]

    # Optional Q5 exclusion (LIQC filter)
    if (!is.null(excludeCache)) {
      excl <- excludeCache[[mi]]
      if (!is.null(excl) && length(excl) > 0)
        mf <- mf[!names(mf) %in% excl]
    }
    if (length(mf) < minStocks) return(NULL)

    # Step 1: top momentum decile (or momTopPct)
    nMom <- max(minStocks, floor(length(mf) * momTopPct))
    momWinners <- names(mf)[1:nMom]

    # Step 2: within momentum winners, rank by expected skewness
    er <- expRsCache[[mi]]
    if (is.null(er) || length(er) == 0) return(NULL)

    erSub <- er[names(er) %in% momWinners]
    if (length(erSub) < minStocks) return(NULL)

    # Step 3: top skewness tercile (or skewTopPct) within momentum winners
    nSkew <- max(minStocks, floor(length(erSub) * skewTopPct))
    # If tercile filter yields fewer than topN, fill from remaining momentum winners
    if (nSkew < topN && length(erSub) > nSkew)
      nSkew <- min(length(erSub), topN)
    stocks <- names(erSub)[1:nSkew]

    if (length(stocks) < minStocks) return(NULL)

    # Return up to topN (for compatibility with makePortfolio)
    stocks[1:min(length(stocks), topN)]
  }
}

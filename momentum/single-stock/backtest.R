# ============================================================================
# Single-Stock Winner / NIFTY-Short Momentum Backtest
# ============================================================================
# Ammann, Moellenbeck & Schmid (2010) — Feasible Momentum Strategies
#
# Long:  top-N stocks by J-month momentum (top 60% FF mcap, EQ only)
# Short: NIFTY futures (near-month contract, equal notional)
#
# Two configs:
#   paper_best: J=6, K=3, N=1 (the paper's best cell)
#   search_*:   best config found by training-set Sharpe sweep over J×K×N
# ============================================================================

suppressPackageStartupMessages({
  library('RODBC')
  library('RPostgres')
  library('quantmod')
  library('PerformanceAnalytics')
  library('xts')
  library('tidyverse')
  library('lubridate')
  library('gt')
  library('webshot2')
  library('viridis')
})

options("scipen" = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "/mnt/data/blog/momentum/single-stock"
dir.create(reportPath, showWarnings = FALSE, recursive = TRUE)
source("/mnt/hollandC/StockViz/R/config.r")

# ═══════════════════════════════════════════════════════════════
# Parameters
# ═══════════════════════════════════════════════════════════════

MCAP_PCT     <- 0.60        # top N% by cumulative free-float mcap
DRAG         <- 0.005       # 0.5% per trade
ANNUAL_REBAL <- TRUE        # annual redistribution across strands
SKIP_MONTH   <- TRUE        # 1-month skip between formation and holding

PAPER_J <- 6L; PAPER_K <- 3L; PAPER_N <- 1L   # paper's best cell

# Parameter grid for search
J_GRID <- c(3L, 6L, 12L)
K_GRID <- c(3L, 6L, 12L)
N_GRID <- c(1L, 3L)  # 1 and 3 stocks (5 and 10 omitted — too slow for sweep)

CHK_FILE <- sprintf("%s/checkpoint.rds", reportPath)

# ═══════════════════════════════════════════════════════════════
# PHASE 1 — DATA FETCH (skip if checkpoint exists)
# ═══════════════════════════════════════════════════════════════

if (file.exists(CHK_FILE)) {
  cat("Loading from checkpoint...\n")
  chk <- readRDS(CHK_FILE)
  if (!is.null(chk$mcapBySymbol)) {
    mcapBySymbol <- chk$mcapBySymbol
    eqTimestamps <- chk$eqTimestamps
  } else {
    mcapDf <- chk$mcapDf; eqDf <- chk$eqDf
  }
  niftyFutRets <- chk$niftyFutRets; niftyXts <- chk$niftyXts
  n500momXts <- chk$n500momXts
  totalRetList <- chk$totalRetList; cumCache <- chk$cumCache
  valid_tickers <- chk$valid_tickers; monthEndsActual <- chk$monthEndsActual
  rm(chk)
  cat(sprintf("  Tickers: %d, Month-ends: %d\n", length(valid_tickers), length(monthEndsActual)))

  if (exists("mcapDf") && exists("eqDf")) {
    cat("Building symbol indexes...\n")
    mcapBySymbol <- split(mcapDf, mcapDf$SYMBOL)
    mcapBySymbol <- lapply(mcapBySymbol, function(df) {
      df$TIME_STAMP_NUM <- as.numeric(df$TIME_STAMP); df
    })
    eqTimestamps <- split(as.numeric(eqDf$TIME_STAMP), eqDf$SYMBOL)
    cat(sprintf("  %d mcap, %d eq\n", length(mcapBySymbol), length(eqTimestamps)))
    rm(mcapDf, eqDf); gc()
  }
} else {
  cat("=== DATA FETCH ===\n")

  lcon <- odbcDriverConnect(
    sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            ldbserver, "StockViz", ldbuser, ldbpassword),
    case = "nochange", believeNRows = TRUE)
  pcon <- dbConnect(RPostgres::Postgres(),
    host = ldbserver2, user = ldbuser2, password = ldbpassword2, dbname = ldbname2)

  cat("Market caps (equity_misc_info)...\n")
  mcapDf <- sqlQuery(lcon, "select SYMBOL, FF_MKT_CAP_CR, TIME_STAMP
    from equity_misc_info where FF_MKT_CAP_CR is not null
    and TIME_STAMP >= '2005-01-01'")
  mcapDf$TIME_STAMP <- as.Date(mcapDf$TIME_STAMP)
  mcapDf <- mcapDf |> arrange(TIME_STAMP)
  cat(sprintf("  %d rows, %d symbols\n", nrow(mcapDf), length(unique(mcapDf$SYMBOL))))

  allDates <- sort(unique(mcapDf$TIME_STAMP))
  minDate <- min(allDates); maxDate <- max(allDates)

  cat(sprintf("Stock prices (eod_adjusted_nse) %s → %s...\n", minDate, maxDate))
  pxDf <- dbGetQuery(pcon, sprintf(
    "select ticker, date_stamp, c from eod_adjusted_nse
     where date_stamp >= '%s' and date_stamp <= '%s' order by ticker, date_stamp",
    minDate, maxDate))
  pxDf$date_stamp <- as.Date(pxDf$date_stamp)
  cat(sprintf("  %d rows, %d tickers\n", nrow(pxDf), length(unique(pxDf$ticker))))

  cat("EQ series (px_history)...\n")
  eqDf <- sqlQuery(lcon, sprintf(
    "select SYMBOL, TIME_STAMP from px_history
     where SERIES='EQ' and TIME_STAMP >= '%s' and TIME_STAMP <= '%s'", minDate, maxDate))
  eqDf$TIME_STAMP <- as.Date(eqDf$TIME_STAMP)
  eqDf <- eqDf |> arrange(TIME_STAMP)
  cat(sprintf("  %d rows\n", nrow(eqDf)))

  cat("NIFTY 50 index...\n")
  niftyIdxDf <- sqlQuery(lcon, sprintf(
    "select px_close, time_stamp from bhav_index
     where index_name='NIFTY 50' and time_stamp >= '%s' order by time_stamp", minDate))
  niftyXts <- xts(niftyIdxDf$px_close, as.Date(niftyIdxDf$time_stamp))
  cat(sprintf("  %d rows\n", nrow(niftyIdxDf)))

  cat("NIFTY500 MOMENTUM 50 TR...\n")
  n500momDf <- sqlQuery(lcon, sprintf(
    "select px_close, time_stamp from bhav_index
     where index_name='NIFTY500 MOMENTUM 50 TR' and time_stamp >= '%s' order by time_stamp",
    minDate))
  n500momXts <- xts(n500momDf$px_close, as.Date(n500momDf$time_stamp))
  cat(sprintf("  %d rows\n", nrow(n500momDf)))

  cat("NIFTY futures (BHAV_EQ_FUT)...\n")
  futDf <- sqlQuery(lcon, sprintf(
    "select SYMBOL, TIME_STAMP, EXPIRY_DT, PX_CLOSE from BHAV_EQ_FUT
     where SYMBOL='NIFTY' and TIME_STAMP >= '%s' order by TIME_STAMP, EXPIRY_DT", minDate))
  futDf$TIME_STAMP <- as.Date(futDf$TIME_STAMP)
  futDf$EXPIRY_DT  <- as.Date(futDf$EXPIRY_DT)
  cat(sprintf("  %d rows\n", nrow(futDf)))

  cat("  Building near-month NIFTY futures returns...\n")
  futDates <- sort(unique(futDf$TIME_STAMP))
  niftyFutPx <- sapply(futDates, function(d) {
    rows <- futDf[futDf$TIME_STAMP == d, ]
    rows$PX_CLOSE[which.min(rows$EXPIRY_DT)]
  })
  niftyFutXts <- xts(niftyFutPx, order.by = futDates)
  niftyFutRets <- dailyReturn(niftyFutXts)
  colnames(niftyFutRets) <- "NIFTY_FUT"
  cat(sprintf("  %d trading days, %s → %s\n", length(futDates),
              as.character(futDates[1]), as.character(futDates[length(futDates)])))

  odbcClose(lcon); dbDisconnect(pcon)

  common <- intersect(unique(mcapDf$SYMBOL), unique(pxDf$ticker))
  mcapDf <- mcapDf |> filter(SYMBOL %in% common)
  cat(sprintf("  Common symbols (mcap ∩ prices): %d\n", length(common)))

  cat("\nBuilding returns...\n")
  symbols_all <- sort(unique(mcapDf$SYMBOL))
  pxLookup <- pxDf |> select(ticker, date_stamp, c) |> arrange(ticker, date_stamp)

  totalRetList <- list()
  ticker_count <- 0L
  for (tkr in symbols_all) {
    ticker_count <- ticker_count + 1L
    if (ticker_count %% 300 == 0) cat(sprintf("  %d/%d...\n", ticker_count, length(symbols_all)))
    tkrPx <- pxLookup |> filter(ticker == tkr) |> arrange(date_stamp)
    if (nrow(tkrPx) < 260) next
    pClose <- xts(tkrPx$c, tkrPx$date_stamp)
    totalRet <- na.omit(dailyReturn(pClose, type = "log"))
    if (nrow(totalRet) < 260) next
    totalRetList[[tkr]] <- totalRet
  }
  cat(sprintf("  Valid tickers: %d\n", length(totalRetList)))

  valid_tickers <- names(totalRetList)
  valid_tickers <- valid_tickers[sapply(totalRetList[valid_tickers], nrow) >= 500L]
  cat(sprintf("  ≥500 days: %d\n", length(valid_tickers)))

  retDates <- index(totalRetList[[valid_tickers[1]]])
  monthEnds <- unique(floor_date(retDates, "month") + months(1) - days(1))
  monthEnds <- monthEnds[monthEnds >= retDates[1] & monthEnds <= last(retDates)]
  monthEndsActual <- sapply(monthEnds, function(d) {
    idx <- which(retDates <= d); if (length(idx) == 0) NA else retDates[max(idx)]
  })
  monthEndsActual <- as.Date(unique(monthEndsActual[!is.na(monthEndsActual)]))
  cat(sprintf("  Month-ends: %d (%s → %s)\n", length(monthEndsActual),
              monthEndsActual[1], monthEndsActual[length(monthEndsActual)]))

  cat("Building cumulative cache...\n")
  cumCache <- list()
  for (tkr in valid_tickers) {
    r <- totalRetList[[tkr]]
    cumCache[[tkr]] <- list(cr = exp(cumsum(coredata(r))),
                            di_num = as.numeric(index(r)))
  }
  cat(sprintf("  Cached: %d tickers\n", length(cumCache)))

  cat("Indexing mcap by symbol...\n")
  mcapBySymbol <- split(mcapDf, mcapDf$SYMBOL)
  mcapBySymbol <- lapply(mcapBySymbol, function(df) {
    df$TIME_STAMP_NUM <- as.numeric(df$TIME_STAMP); df
  })
  cat(sprintf("  %d symbols\n", length(mcapBySymbol)))

  cat("Indexing EQ by symbol...\n")
  eqTimestamps <- split(as.numeric(eqDf$TIME_STAMP), eqDf$SYMBOL)
  cat(sprintf("  %d symbols\n", length(eqTimestamps)))

  saveRDS(list(mcapBySymbol=mcapBySymbol, eqTimestamps=eqTimestamps,
               niftyFutRets=niftyFutRets, niftyXts=niftyXts,
               n500momXts=n500momXts,
               totalRetList=totalRetList, cumCache=cumCache,
               valid_tickers=valid_tickers, monthEndsActual=monthEndsActual), CHK_FILE)
  cat(sprintf("  Checkpoint saved: %s\n", CHK_FILE))
  rm(mcapDf, eqDf, futDf, pxDf, pxLookup, niftyFutPx, niftyFutXts); gc()
}

# ═══════════════════════════════════════════════════════════════
# PHASE 2 — Shared helpers
# ═══════════════════════════════════════════════════════════════

momFast <- function(tkr, momEndNum, momStartNum) {
  e <- cumCache[[tkr]]
  if (is.null(e)) return(NA_real_)
  ie <- findInterval(momEndNum, e$di_num)
  is_val <- findInterval(momStartNum, e$di_num)
  if (is_val < 1L || ie < 1L || ie - is_val < 10L) return(NA_real_)
  e$cr[ie] / e$cr[is_val] - 1
}

getUniverse <- function(sigDate, candidateTickers, mcapPct) {
  sigNum <- as.numeric(sigDate)
  mcaps <- vapply(candidateTickers, function(sym) {
    rows <- mcapBySymbol[[sym]]
    if (is.null(rows)) return(NA_real_)
    idx <- findInterval(sigNum, rows$TIME_STAMP_NUM)
    if (idx < 1L) return(NA_real_)
    rows$FF_MKT_CAP_CR[idx]
  }, FUN.VALUE = double(1))
  names(mcaps) <- candidateTickers

  eqOk <- vapply(candidateTickers, function(sym) {
    dates <- eqTimestamps[[sym]]
    if (is.null(dates)) return(FALSE)
    findInterval(sigNum, dates) >= 1L
  }, FUN.VALUE = logical(1))

  valid <- !is.na(mcaps) & eqOk
  if (!any(valid)) return(character(0))

  syms <- candidateTickers[valid]
  vals <- mcaps[valid]
  ord <- order(vals, decreasing = TRUE)
  syms <- syms[ord]; vals <- vals[ord]

  totalMcap <- sum(vals, na.rm = TRUE)
  cumPct <- cumsum(vals) / totalMcap
  syms[cumPct <= mcapPct]
}

# NAV-weighted aggregation with capital-neutral strand entry/exit
aggregateStrands <- function(strandRecs, annualRebal) {
  if (length(strandRecs) == 0) return(xts(double(0), order.by = as.Date(character(0))))
  allStrandXts <- do.call(merge.xts, strandRecs)
  colnames(allStrandXts) <- paste0("S", seq_len(ncol(allStrandXts)))
  nDays <- nrow(allStrandXts); nStrands <- ncol(allStrandXts)
  allDates <- index(allStrandXts); retMat <- coredata(allStrandXts)

  strandNAV <- matrix(NA_real_, nrow = nDays, ncol = nStrands)
  firstActive <- which(!is.na(retMat[1, ]))
  if (length(firstActive) > 0L) strandNAV[1, firstActive] <- 1.0 / length(firstActive)

  portDailyVec <- numeric(nDays)
  for (d in seq_len(nDays)) {
    dDate <- allDates[d]
    if (d > 1L) strandNAV[d, ] <- strandNAV[d - 1L, ]

    if (d > 1L) {
      newStrands <- which(!is.na(retMat[d, ]) & is.na(retMat[d - 1L, ]))
    } else {
      newStrands <- which(!is.na(retMat[d, ]))
    }
    newStrands <- setdiff(newStrands, firstActive)

    if (d > 1L) {
      exitStrands <- which(is.na(retMat[d, ]) & !is.na(retMat[d - 1L, ]))
    } else {
      exitStrands <- integer(0)
    }

    if (length(exitStrands) > 0L) {
      exitNAV <- sum(strandNAV[d, exitStrands], na.rm = TRUE)
      strandNAV[d, exitStrands] <- NA_real_
      continuing <- which(!is.na(strandNAV[d, ]))
      if (length(continuing) > 0L && exitNAV > 0) {
        totalCont <- sum(strandNAV[d, continuing], na.rm = TRUE)
        if (totalCont > 0) {
          for (s in continuing) {
            strandNAV[d, s] <- strandNAV[d, s] * (1.0 + exitNAV / totalCont)
          }
        }
      }
    }

    if (length(newStrands) > 0L && d > 1L) {
      existing <- which(!is.na(strandNAV[d, ]))
      if (length(existing) > 0L) {
        totalExisting <- sum(strandNAV[d, existing], na.rm = TRUE)
        eqNAV <- totalExisting / (length(existing) + length(newStrands))
        for (s in existing) strandNAV[d, s] <- eqNAV
        for (s in newStrands) strandNAV[d, s] <- eqNAV
      } else {
        strandNAV[d, newStrands] <- 1.0 / length(newStrands)
      }
    }

    isJan <- annualRebal && format(dDate, "%m") == "01" &&
      (d == 1L || format(allDates[d - 1L], "%Y") != format(dDate, "%Y"))
    if (isJan) {
      activeNow <- which(!is.na(strandNAV[d, ]))
      if (length(activeNow) > 0L) {
        totalNAV <- sum(strandNAV[d, activeNow], na.rm = TRUE)
        strandNAV[d, activeNow] <- totalNAV / length(activeNow)
      }
    }

    for (s in seq_len(nStrands)) {
      if (!is.na(retMat[d, s]) && !is.na(strandNAV[d, s]))
        strandNAV[d, s] <- strandNAV[d, s] * (1.0 + retMat[d, s])
    }

    activeNow <- which(!is.na(retMat[d, ]) & !is.na(strandNAV[d, ]))
    if (length(activeNow) > 0L) {
      weights <- strandNAV[d, activeNow] / sum(strandNAV[d, activeNow], na.rm = TRUE)
      portDailyVec[d] <- sum(retMat[d, activeNow] * weights, na.rm = TRUE)
    } else {
      portDailyVec[d] <- NA_real_
    }
  }
  xts(portDailyVec, order.by = allDates)
}

computeMetrics <- function(rets) {
  if (nrow(rets) < 60) return(c(CAGR=NA_real_, Vol=NA_real_, Sharpe=NA_real_, MaxDD=NA_real_, Calmar=NA_real_))
  annRet <- Return.annualized(rets)[1, 1]
  annVol <- sd(coredata(rets), na.rm = TRUE) * sqrt(252)
  sharpe <- tryCatch(SharpeRatio.annualized(rets)[1, 1], error = function(e) NA_real_)
  maxDD  <- maxDrawdown(rets)
  calmar <- if (!is.na(maxDD) && maxDD > 0) annRet / maxDD else NA_real_
  c(CAGR = annRet, Vol = annVol, Sharpe = sharpe, MaxDD = maxDD, Calmar = calmar)
}

# ═══════════════════════════════════════════════════════════════
# PHASE 3 — Pre-compute momentum for all J values (once)
# ═══════════════════════════════════════════════════════════════

cat("\n=== PRE-COMPUTING MOMENTUM ===\n")
momCache <- list()  # momCache[[as.character(J)]][[as.character(mi)]] = named vector
allJ <- unique(c(J_GRID, PAPER_J))
for (j in allJ) {
  cat(sprintf("  J=%d...\n", j))
  jMom <- vector("list", length(monthEndsActual))
  warmupJ <- j + 3L
  for (mi in seq(warmupJ, length(monthEndsActual))) {
    sigDate <- monthEndsActual[mi]
    if (SKIP_MONTH) {
      momEnd <- sigDate %m-% months(1)
    } else {
      momEnd <- sigDate
    }
    momStart <- momEnd %m-% months(j)
    momEndNum <- as.numeric(momEnd); momStartNum <- as.numeric(momStart)
    momRets <- vapply(valid_tickers, momFast,
                      momEndNum = momEndNum, momStartNum = momStartNum,
                      FUN.VALUE = double(1))
    names(momRets) <- valid_tickers
    momRets <- momRets[!is.na(momRets)]
    jMom[[mi]] <- momRets
  }
  momCache[[as.character(j)]] <- jMom
  cat(sprintf("    %d months cached\n", sum(!sapply(jMom, is.null))))
}

# Pre-compute universe (EQ + top-60% mcap) for all months
cat("  Universe...\n")
universeCache <- vector("list", length(monthEndsActual))
for (mi in seq_len(length(monthEndsActual))) {
  sigDate <- monthEndsActual[mi]
  # Use a representative J (6) to get candidate tickers; universe is mcap-based, not J-dependent
  momSample <- momCache[["6"]][[mi]]
  if (is.null(momSample)) { universeCache[[mi]] <- character(0); next }
  universeCache[[mi]] <- getUniverse(sigDate, names(momSample), MCAP_PCT)
}
nUniv <- sum(sapply(universeCache, length) > 0)
cat(sprintf("    %d months cached\n", nUniv))

# ═══════════════════════════════════════════════════════════════
# PHASE 4 — Parameterized backtest engine
# ═══════════════════════════════════════════════════════════════

runBacktest <- function(J, K, N, trainEnd = NULL, quiet = FALSE, fast = FALSE, monthlyOnly = FALSE) {
  niftyIdxRets <- dailyReturn(niftyXts)
  colnames(niftyIdxRets) <- "NIFTY"
  allDays <- index(niftyFutRets)

  warmupMonths <- max(J, K) + 3L
  firstIdx <- warmupMonths

  # If trainEnd given, find the last month-end index ≤ trainEnd
  if (!is.null(trainEnd)) {
    endIdx <- findInterval(as.numeric(as.Date(trainEnd)), as.numeric(monthEndsActual))
    if (endIdx < firstIdx) endIdx <- firstIdx
  } else {
    endIdx <- length(monthEndsActual)
  }

  strandRecords <- list()
  strandRecordsLong <- list()
  nTraded <- 0L; nSkipped <- 0L

  if (monthlyOnly) {
    # Ultra-fast: compute monthly returns directly, no daily tracking or merge.xts
    monthlyRets <- numeric(endIdx - firstIdx + 1L)
    monthlyDates <- as.Date(character(endIdx - firstIdx + 1L))
    idx <- 1L

    for (mi in seq(firstIdx, endIdx)) {
      sigDate <- monthEndsActual[mi]
      if (mi >= length(monthEndsActual)) break
      momRets <- momCache[[as.character(J)]][[mi]]
      if (is.null(momRets) || length(momRets) < N) { nSkipped <- nSkipped + 1L; next }
      universeSyms <- universeCache[[mi]]
      if (length(universeSyms) < N) { nSkipped <- nSkipped + 1L; next }
      momFiltered <- momRets[names(momRets) %in% universeSyms]
      if (length(momFiltered) < N) { nSkipped <- nSkipped + 1L; next }
      topStocks <- names(sort(momFiltered, decreasing = TRUE))[1:N]

      holdingEndIdx <- min(mi + K, length(monthEndsActual))
      holdingEnd <- monthEndsActual[holdingEndIdx]
      holdDays <- allDays[allDays >= (sigDate + 1) & allDays <= holdingEnd]
      if (length(holdDays) < 5) { nSkipped <- nSkipped + 1L; next }

      monthCombined <- 0; nValid <- 0L
      for (tkr in topStocks) {
        r <- totalRetList[[tkr]]
        if (is.null(r)) next
        stockSub <- r[as.character(holdDays)]
        niftySub <- niftyFutRets[as.character(holdDays)]
        cd <- intersect(index(stockSub), index(niftySub))
        if (length(cd) < 5) next
        combined <- 0.5 * as.numeric(coredata(stockSub[cd])) - 0.5 * as.numeric(coredata(niftySub[cd]))
        combined[1] <- combined[1] - DRAG
        monthCombined <- monthCombined + sum(combined)
        nValid <- nValid + 1L
      }
      if (nValid > 0L) {
        nTraded <- nTraded + 1L
        monthlyRets[idx] <- monthCombined / nValid
        monthlyDates[idx] <- sigDate
        idx <- idx + 1L
      }
    }
    monthlyRets <- monthlyRets[1:(idx - 1L)]
    monthlyDates <- monthlyDates[1:(idx - 1L)]
    portStrat <- xts(monthlyRets, order.by = monthlyDates)
    colnames(portStrat) <- "Strategy"
    portLong <- portStrat
    colnames(portLong) <- "Long Only"

    if (!quiet) cat(sprintf("  J=%d K=%d N=%d: %d traded, %d skipped (monthly)\n",
                            J, K, N, nTraded, nSkipped))
    return(list(strat = portStrat, long = portLong, nTraded = nTraded, nSkipped = nSkipped))
  }

  # Full daily tracking path
  for (mi in seq(firstIdx, endIdx)) {
    sigDate <- monthEndsActual[mi]
    if (mi >= length(monthEndsActual)) break

    holdingEndIdx <- min(mi + K, length(monthEndsActual))
    holdingEnd <- monthEndsActual[holdingEndIdx]

    holdStart <- sigDate + 1
    holdDays <- allDays[allDays >= holdStart & allDays <= holdingEnd]
    if (length(holdDays) < 5) { nSkipped <- nSkipped + 1L; next }

    # Use pre-computed momentum and universe from caches
    momRets <- momCache[[as.character(J)]][[mi]]
    if (is.null(momRets) || length(momRets) < N) { nSkipped <- nSkipped + 1L; next }

    universeSyms <- universeCache[[mi]]
    if (length(universeSyms) < N) { nSkipped <- nSkipped + 1L; next }

    momFiltered <- momRets[names(momRets) %in% universeSyms]
    if (length(momFiltered) < N) { nSkipped <- nSkipped + 1L; next }
    topStocks <- names(sort(momFiltered, decreasing = TRUE))[1:N]

    for (tkr in topStocks) {
      r <- totalRetList[[tkr]]
      if (is.null(r)) next
      stockSub <- r[as.character(holdDays)]
      niftySub <- niftyFutRets[as.character(holdDays)]
      cd <- intersect(index(stockSub), index(niftySub))
      if (length(cd) < 5) next

      strandCombined <- 0.5 * as.numeric(coredata(stockSub[cd])) -
                        0.5 * as.numeric(coredata(niftySub[cd]))
      strandCombined[1] <- strandCombined[1] - DRAG
      strandRecords[[length(strandRecords) + 1L]] <- xts(strandCombined, cd)

      strandLong <- as.numeric(coredata(stockSub[cd]))
      strandRecordsLong[[length(strandRecordsLong) + 1L]] <- xts(strandLong, cd)
    }
    nTraded <- nTraded + 1L
  }

  if (!quiet) cat(sprintf("  J=%d K=%d N=%d: %d traded, %d skipped, %d strands\n",
                          J, K, N, nTraded, nSkipped, length(strandRecords)))

  if (fast) {
    # Fast equal-weight aggregation for sweep (avoids O(n²) NAV loop)
    mat <- do.call(merge.xts, strandRecords)
    portStrat <- xts(rowMeans(coredata(mat), na.rm = TRUE), order.by = index(mat))
    colnames(portStrat) <- "Strategy"
    if (length(strandRecordsLong) > 0) {
      matL <- do.call(merge.xts, strandRecordsLong)
      portLong <- xts(rowMeans(coredata(matL), na.rm = TRUE), order.by = index(matL))
    } else {
      portLong <- portStrat
    }
    colnames(portLong) <- "Long Only"
    portStrat <- na.omit(portStrat)
    portLong  <- na.omit(portLong)
  } else {
    portStrat <- na.omit(aggregateStrands(strandRecords, ANNUAL_REBAL))
    portLong  <- na.omit(aggregateStrands(strandRecordsLong, ANNUAL_REBAL))
    colnames(portStrat) <- "Strategy"
    colnames(portLong)  <- "Long Only"
  }

  list(strat = portStrat, long = portLong, nTraded = nTraded, nSkipped = nSkipped)
}

# ═══════════════════════════════════════════════════════════════
# PHASE 5 — Paper-best config (J=6, K=3, N=1)
# ═══════════════════════════════════════════════════════════════

cat("\n=== PAPER BEST (J=6, K=3, N=1) ===\n")
paperRes <- runBacktest(PAPER_J, PAPER_K, PAPER_N)

# ═══════════════════════════════════════════════════════════════
# PHASE 6 — Training-set parameter sweep
# ═══════════════════════════════════════════════════════════════

cat("\n=== TRAINING SET SWEEP ===\n")
TRAIN_END <- "2019-12-31"

sweepResults <- data.frame(
  J = integer(0), K = integer(0), N = integer(0),
  CAGR = double(0), Vol = double(0), Sharpe = double(0),
  MaxDD = double(0), nTraded = integer(0), nSkipped = integer(0),
  stringsAsFactors = FALSE
)

nTotal <- length(J_GRID) * length(K_GRID) * length(N_GRID)
nConfig <- 0L
for (j in J_GRID) {
  for (k in K_GRID) {
    for (n in N_GRID) {
      nConfig <- nConfig + 1L
      if (nConfig %% 6 == 0) cat(sprintf("  [%d/%d] J=%d K=%d N=%d...\n", nConfig, nTotal, j, k, n))
      res <- runBacktest(j, k, n, trainEnd = TRAIN_END, quiet = TRUE, monthlyOnly = TRUE)
      if (nrow(res$strat) < 12) next
      # Monthly returns: annualized Sharpe with scale=12
      m <- c(
        CAGR = Return.annualized(res$strat, scale = 12)[1, 1],
        Vol  = sd(coredata(res$strat), na.rm = TRUE) * sqrt(12),
        Sharpe = tryCatch(SharpeRatio.annualized(res$strat, scale = 12)[1, 1], error = function(e) NA_real_),
        MaxDD = maxDrawdown(res$strat),
        Calmar = NA_real_
      )
      sweepResults <- rbind(sweepResults, data.frame(
        J = j, K = k, N = n,
        CAGR = m["CAGR"], Vol = m["Vol"], Sharpe = m["Sharpe"],
        MaxDD = m["MaxDD"],
        nTraded = res$nTraded, nSkipped = res$nSkipped,
        stringsAsFactors = FALSE
      ))
    }
  }
}

cat(sprintf("\nTraining-set sweep complete: %d configs\n", nrow(sweepResults)))

# Pick best by Sharpe
sweepResults <- sweepResults |> arrange(desc(Sharpe))
bestRow <- sweepResults[1, ]
SEARCH_J <- bestRow$J; SEARCH_K <- bestRow$K; SEARCH_N <- bestRow$N
cat(sprintf("Best config: J=%d K=%d N=%d  Sharpe=%.3f  CAGR=%.2f%%\n",
            SEARCH_J, SEARCH_K, SEARCH_N, bestRow$Sharpe, bestRow$CAGR * 100))

# Print training sweep table
cat("\nTraining sweep (sorted by Sharpe):\n")
print(sweepResults, row.names = FALSE)

# ═══════════════════════════════════════════════════════════════
# PHASE 7 — Search-best config (full sample)
# ═══════════════════════════════════════════════════════════════

cat(sprintf("\n=== SEARCH BEST (J=%d, K=%d, N=%d) ===\n", SEARCH_J, SEARCH_K, SEARCH_N))
searchRes <- runBacktest(SEARCH_J, SEARCH_K, SEARCH_N)

# ═══════════════════════════════════════════════════════════════
# PHASE 8 — Output for both configs
# ═══════════════════════════════════════════════════════════════

source("/mnt/hollandC/StockViz/R/plot.common.r")

# Benchmark daily returns
niftyIdxRets <- dailyReturn(niftyXts)
colnames(niftyIdxRets) <- "NIFTY"
n500momRets <- dailyReturn(n500momXts)
colnames(n500momRets) <- "NIFTY500_MOM50_TR"

outputConfig <- function(res, prefix, jVal, kVal, nVal) {
  cat(sprintf("\n=== OUTPUT: %s ===\n", prefix))

  # Align: Strategy, Long Only, NIFTY 50, NIFTY500 Mom TR
  cd <- Reduce(intersect, list(index(res$strat), index(res$long),
                                index(niftyIdxRets), index(n500momRets)))
  combined <- na.omit(merge(res$strat[cd], res$long[cd],
                            niftyIdxRets[cd], n500momRets[cd]))

  # Metrics
  fm <- sapply(colnames(combined), function(cn) computeMetrics(combined[, cn]))
  cat("Full Sample:\n"); print(round(fm, 4))

  trainSub <- combined[paste0("/", TRAIN_END)]
  testSub  <- combined[paste0(TRAIN_END, "/")]
  if (nrow(trainSub) >= 60) {
    tm <- sapply(colnames(trainSub), function(cn) computeMetrics(trainSub[, cn]))
    cat(sprintf("\nTrain (≤%s, %d days):\n", TRAIN_END, nrow(trainSub)))
    print(round(tm, 4))
  }
  if (nrow(testSub) >= 60) {
    tm2 <- sapply(colnames(testSub), function(cn) computeMetrics(testSub[, cn]))
    cat(sprintf("\nTest (≥%s, %d days):\n", TRAIN_END, nrow(testSub)))
    print(round(tm2, 4))
  }

  # Cumulative return charts
  srAll <- sapply(colnames(combined), function(nm)
    round(SharpeRatio.annualized(combined[, nm])[1, 1], 2))
  Common.PlotCumReturns(combined,
    sprintf("%s (J=%d K=%d N=%d)", prefix, jVal, kVal, nVal),
    sprintf("%s → %s | SR: %s",
            as.character(as.Date(first(index(combined)))),
            as.character(as.Date(last(index(combined)))),
            paste0(colnames(combined), "=", srAll, collapse = ", ")),
    sprintf("%s/%s_cumulative_all.png", reportPath, prefix), NULL)

  for (s in c("train", "test")) {
    sr <- if (s == "train") paste0("/", TRAIN_END) else paste0(TRAIN_END, "/")
    sub <- combined[sr]
    if (nrow(sub) < 60) next
    srs <- sapply(colnames(sub), function(nm) round(SharpeRatio.annualized(sub[, nm])[1, 1], 2))
    Common.PlotCumReturns(sub,
      sprintf("%s (J=%d K=%d N=%d) — %s", prefix, jVal, kVal, nVal, tools::toTitleCase(s)),
      sprintf("%s | SR: %s", tools::toTitleCase(s),
              paste0(colnames(sub), "=", srs, collapse = ", ")),
      sprintf("%s/%s_cumulative_%s.png", reportPath, prefix, s), NULL)
  }

  # GT metrics table
  tbl <- as.data.frame(t(fm))
  tbl$Strategy <- rownames(tbl)
  tbl <- tbl |> select(Strategy, everything())
  rownames(tbl) <- NULL

  gtTbl <- tbl |>
    gt() |>
    tab_header(
      title = sprintf("%s (J=%d K=%d N=%d)", prefix, jVal, kVal, nVal),
      subtitle = sprintf("Top %.0f%% mcap, EQ-only | %s → %s",
                         MCAP_PCT * 100,
                         as.character(as.Date(first(index(combined)))),
                         as.character(as.Date(last(index(combined)))))) |>
    fmt_percent(columns = c(CAGR, Vol, MaxDD), decimals = 2) |>
    fmt_number(columns = c(Sharpe, Calmar), decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

  for (col in c("CAGR", "Sharpe", "Calmar")) {
    neg_rows <- which(tbl[[col]] < 0)
    if (length(neg_rows) > 0)
      gtTbl <- gtTbl |> tab_style(style = cell_text(color = "#8B0000"),
        locations = cells_body(columns = all_of(col), rows = neg_rows))
  }
  gtTbl <- gtTbl |>
    tab_style(style = cell_fill(color = "#f0fff0"),
              locations = cells_body(rows = tbl$Strategy == "Strategy")) |>
    tab_style(style = cell_fill(color = "#fff8e1"),
              locations = cells_body(rows = tbl$Strategy == "NIFTY.50"))

  gtsave(gtTbl, sprintf("%s/%s_metrics.html", reportPath, prefix))
  webshot2::webshot(
    paste0("file://", reportPath, "/", prefix, "_metrics.html"),
    sprintf("%s/%s_metrics.png", reportPath, prefix),
    selector = "table", expand = c(10, 10, 10, 10))
  cat(sprintf("  %s_metrics.png\n", prefix))

  # Annual returns bar chart
  annCombined <- apply.yearly(combined, Return.cumulative)
  annDf <- fortify(annCombined, melt = TRUE)
  names(annDf) <- c("Year", "Strategy", "Return")
  annDf$Year <- as.numeric(format(annDf$Year, "%Y"))

  pAnn <- ggplot(annDf, aes(x = factor(Year), y = Return * 100, fill = Strategy)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_vline(xintercept = which(levels(factor(annDf$Year)) == "2020") - 0.5,
               linetype = "dashed", color = "grey50", linewidth = 0.6) +
    scale_fill_viridis_d(option = "D", end = 0.85) +
    labs(title = sprintf("%s Annual Returns (J=%d K=%d N=%d)", prefix, jVal, kVal, nVal),
         subtitle = sprintf("Top %.0f%% mcap, EQ-only | %s → %s",
                            MCAP_PCT * 100,
                            as.character(as.Date(first(index(combined)))),
                            as.character(as.Date(last(index(combined))))),
         x = "", y = "Return (%)", caption = "@StockViz") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
  ggsave(sprintf("%s/%s_annual_returns.png", reportPath, prefix),
         pAnn, width = 14, height = 6, dpi = 120)
  cat(sprintf("  %s_annual_returns.png\n", prefix))
}

# Run output for both configs
outputConfig(paperRes,  "paper_best", PAPER_J, PAPER_K, PAPER_N)
outputConfig(searchRes, "search",     SEARCH_J, SEARCH_K, SEARCH_N)

# ── Sweep results table (all scenarios) ──
cat("\n=== SWEEP TABLE ===\n")
if (nrow(sweepResults) > 0) {
  sweepTbl <- sweepResults |>
    mutate(
      CAGR   = CAGR * 100,   # convert to percentage for display
      Vol    = Vol * 100,
      MaxDD  = MaxDD * 100,
      Config = sprintf("J=%d K=%d N=%d", J, K, N)
    ) |>
    select(Config, CAGR, Vol, Sharpe, MaxDD, nTraded, nSkipped)

  gtSweep <- sweepTbl |>
    gt() |>
    tab_header(
      title    = "Training-Set Parameter Sweep",
      subtitle = sprintf("All J×K×N configs | Train ≤ %s | Sorted by Sharpe", TRAIN_END)) |>
    fmt_number(columns = c(CAGR, Vol, MaxDD), decimals = 1) |>
    fmt_number(columns = Sharpe, decimals = 3) |>
    fmt_number(columns = c(nTraded, nSkipped), decimals = 0) |>
    cols_label(
      CAGR   = "CAGR (%)",
      Vol    = "Vol (%)",
      MaxDD  = "MaxDD (%)",
      nTraded = "Traded",
      nSkipped = "Skipped"
    ) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes()) |>
    tab_style(style = cell_fill(color = "#f0fff0"),
              locations = cells_body(rows = 1)) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(rows = 1)) |>
    tab_style(style = cell_fill(color = "#f5f5f5"),
              locations = cells_body(
                rows = sweepTbl$Config == sprintf("J=%d K=%d N=%d", PAPER_J, PAPER_K, PAPER_N)))

  # Color negative returns red
  negRows <- which(sweepTbl$CAGR < 0)
  if (length(negRows) > 0)
    gtSweep <- gtSweep |> tab_style(style = cell_text(color = "#8B0000"),
      locations = cells_body(columns = CAGR, rows = negRows))

  gtsave(gtSweep, sprintf("%s/search_sweep_metrics.html", reportPath))
  webshot2::webshot(
    paste0("file://", reportPath, "/search_sweep_metrics.html"),
    sprintf("%s/search_sweep_metrics.png", reportPath),
    selector = "table", expand = c(10, 10, 10, 10))
  cat("  search_sweep_metrics.png\n")
}

# ═══════════════════════════════════════════════════════════════
# SUMMARY
# ═══════════════════════════════════════════════════════════════

cat("\n===== SUMMARY =====\n")
cat(sprintf("paper_best (J=%d K=%d N=%d): %d traded, %d skipped\n",
            PAPER_J, PAPER_K, PAPER_N, paperRes$nTraded, paperRes$nSkipped))
cat(sprintf("search     (J=%d K=%d N=%d): %d traded, %d skipped  (best train Sharpe=%.3f)\n",
            SEARCH_J, SEARCH_K, SEARCH_N, searchRes$nTraded, searchRes$nSkipped,
            bestRow$Sharpe))
cat(sprintf("Output: %s/\n", reportPath))
cat("===== END =====\n")

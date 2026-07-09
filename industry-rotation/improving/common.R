# ═══════════════════════════════════════════════════════════════════════════
#  Common infrastructure — Improving quadrant (RS-Ratio < 100, RS-Mom > 100)
# ═══════════════════════════════════════════════════════════════════════════

library('RODBC')
library('tidyverse')
library('quantmod')
library('PerformanceAnalytics')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggrepel')
library('gtExtras')
library('webshot2')

options("scipen" = 100)
options(stringsAsFactors = FALSE)

source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

# ── config (override before sourcing if needed) ──
if (!exists("reportPath")) reportPath <- "/mnt/data/blog/industry-rotation/improving"
if (!exists("DRAG"))       DRAG <- 0.5/100

BENCHMARKS <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR")

lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockViz", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

# ── benchmark returns ──
if (!exists("startDate")) startDate <- "2023-01-01"
bmRets <- list()
for (bm in BENCHMARKS) {
  bmDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index 
                                 where index_name='%s' and time_stamp >= '%s'",
                                 bm, startDate))
  bmRets[[bm]] <- dailyReturn(xts(bmDf[,1], as.Date(bmDf[,2])))
}
indexRet <- bmRets[["NIFTY 50 TR"]]
n50Px <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index 
                                where index_name='NIFTY 50 TR' 
                                and time_stamp >= '%s'", startDate))
indexPx <- xts(n50Px[,1], as.Date(n50Px[,2]))
lastDayOfWeek <- index(to.period(indexPx, period='weeks', OHLC = FALSE))

# ── RRG parameters ──
RS_WMA_N    <- 10L
MOM_PERIODS <- 4L
TOP_N       <- 5L

# ── first valid week-end date ──
FIRST_DATE <- as.Date("2025-07-25")
firstIdx   <- which(as.Date(lastDayOfWeek) >= FIRST_DATE)[1L]
lastIdx    <- length(lastDayOfWeek) - 1L

# ── helpers ──

getIMeta <- function(dt) {
  dtStr <- format(dt, "%Y-%m-%d")
  indDf <- sqlQuery(lcon, sprintf(
    "select a.symbol, a.basic_industry, a.time_stamp from nse_industry a
     join (select b.symbol, max(b.time_stamp) mt
           from nse_industry b where time_stamp <= '%s' group by symbol) t
     on a.symbol = t.symbol and a.time_stamp = t.mt", dtStr))
  indDf |>
    group_by(basic_industry) |>
    summarise(CNT = n(), .groups = "drop") |>
    filter(CNT >= 10L)
}

wma <- function(x, n) {
  if (length(x) < n) return(rep(NA_real_, length(x)))
  wts <- rev(seq_len(n))
  as.numeric(stats::filter(x, wts / sum(wts), sides = 1))
}

# ── industry return cache ──
lastEow <- as.Date(last(lastDayOfWeek))
allMeta <- getIMeta(lastEow)
allInds <- allMeta$basic_industry
lastEowStr <- format(lastEow, "%Y-%m-%d")

capCache <- list()
eqCache  <- list()
for (iName in allInds) {
  rDf <- sqlQuery(lcon, sprintf(
    "select TIME_STAMP, RET_EQ_WT, RET_CAP_WT from BHAV_INDUSTRY
     where INDUSTRY = '%s' and time_stamp <= '%s' order by time_stamp",
    iName, lastEowStr))
  rDf <- rDf[!duplicated(rDf$TIME_STAMP), ]
  capCache[[iName]] <- xts(rDf$RET_CAP_WT, as.Date(rDf$TIME_STAMP))
  eqCache[[iName]]  <- xts(rDf$RET_EQ_WT, as.Date(rDf$TIME_STAMP))
}

# ── RRG snapshot ──

getRrgSnapshot <- function(cache, curMeta, rbDate) {
  rbStr <- as.character(rbDate)
  res <- data.frame()
  for (iName in curMeta$basic_industry) {
    full <- cache[[iName]]
    if (is.null(full)) next
    sub <- full[paste0("/", rbStr)]
    if (NROW(sub) < 60L) next
    cd  <- intersect(index(sub), index(indexRet))
    if (length(cd) < 60L) next

    iVals  <- coredata(sub[cd])
    bVals  <- coredata(indexRet[cd])
    excess <- (1 + iVals) / (1 + bVals) - 1
    rs     <- cumprod(na.fill(1 + excess, 1)) * 100
    rsXts  <- xts(rs, cd)
    rsWk   <- na.omit(rsXts[lastDayOfWeek])
    rsVec  <- coredata(rsWk)
    if (length(rsVec) < RS_WMA_N * 2L + MOM_PERIODS) next

    rsSmooth <- wma(rsVec, RS_WMA_N)
    rsBench  <- wma(rsSmooth, RS_WMA_N)
    rsRatio  <- (rsSmooth / rsBench) * 100
    rsMom    <- c(rep(NA_real_, MOM_PERIODS),
                  rsRatio[(MOM_PERIODS + 1L):length(rsRatio)] /
                  rsRatio[1L:(length(rsRatio) - MOM_PERIODS)] * 100)

    res <- rbind(res, data.frame(
      industry   = iName,
      rsRatio    = tail(rsRatio, 1L),
      rsMomentum = tail(rsMom,   1L),
      stringsAsFactors = FALSE))
  }
  res
}

# ── Improving quadrant: RS-Ratio < 100, RS-Momentum > 100 ──
# Sort by RS-Momentum descending (strongest upward momentum)
selectTop <- function(rrg) {
  rrg |>
    filter(rsRatio < 100, rsMomentum > 100) |>
    slice_max(rsMomentum, n = TOP_N) |>
    pull(industry)
}

weekReturns <- function(cache, inds, wkRange, applyDrag) {
  wkXts <- NULL
  for (nm in inds) {
    sub <- cache[[nm]][wkRange]
    if (NROW(sub) > 0L) {
      if (is.null(wkXts)) wkXts <- sub
      else wkXts <- merge.xts(wkXts, sub)
    }
  }
  if (is.null(wkXts) || NROW(wkXts) == 0L) return(NULL)
  port <- xts(rowMeans(coredata(wkXts), na.rm = TRUE), index(wkXts))
  if (applyDrag) port[1L] <- port[1L] - DRAG
  port
}

# ── run a single backtest ──
runBacktest <- function(freqWeeks = 4L, offset = 0L, label = "") {
  stratCapDaily <- NULL
  stratEqDaily  <- NULL
  topCap <- character(0L)
  topEq  <- character(0L)
  startI <- firstIdx + offset

  for (i in firstIdx:lastIdx) {
    rbDate <- as.Date(lastDayOfWeek[i])
    nextRb <- as.Date(lastDayOfWeek[i + 1L])
    wkRange <- paste0(as.character(rbDate + 1L), "/", as.character(nextRb))
    isRb <- (i >= startI && (i - startI) %% freqWeeks == 0L)

    if (isRb) {
      curMeta <- getIMeta(rbDate)
      rrgCap  <- getRrgSnapshot(capCache, curMeta, rbDate)
      rrgEq   <- getRrgSnapshot(eqCache,  curMeta, rbDate)
      topCap  <- selectTop(rrgCap)
      topEq   <- selectTop(rrgEq)

      if (i %% 10L == 0L || i == startI)
        cat(sprintf("  [%s] %s: cap=%d eq=%d\n", label,
                    as.character(rbDate), length(topCap), length(topEq)))
    }

    portCap <- weekReturns(capCache, topCap, wkRange, isRb && length(topCap) > 0L)
    portEq  <- weekReturns(eqCache,  topEq,  wkRange, isRb && length(topEq)  > 0L)

    if (!is.null(portCap)) stratCapDaily <- rbind(stratCapDaily, portCap)
    if (!is.null(portEq))  stratEqDaily  <- rbind(stratEqDaily,  portEq)
  }

  list(cap = stratCapDaily, eq = stratEqDaily)
}

# ── merge strategy returns with benchmarks ──
mergeWithBenchmarks <- function(stratList) {
  firstStrat <- stratList[[1]]
  res <- do.call(merge,
    c(stratList, lapply(bmRets, function(x) x[index(firstStrat)])))
  res <- na.omit(res)
  names(res) <- c(names(stratList), BENCHMARKS)
  res
}

# ── performance stats ──
perfStats <- function(dailyXts) {
  sr <- sapply(names(dailyXts), function(nm)
    round(SharpeRatio.annualized(dailyXts[, nm])[1L, 1L], 2))
  ar <- sapply(names(dailyXts), function(nm)
    round(Return.annualized(dailyXts[, nm])[1L, 1L] * 100, 2))
  list(ar = ar, sr = sr)
}

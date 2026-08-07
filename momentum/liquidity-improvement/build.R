# ============================================================================
# build.R — LIQIM Signal Builder (Phase 1)
# ============================================================================
# Fetches data, computes ILLIQ/LIQC/universe cache, quintile stats, saves checkpoint.
# Single tier: top 60% FF-mcap. 1-month LIQC only.
# ============================================================================
suppressPackageStartupMessages({
  library('RODBC'); library('RPostgres'); library('quantmod')
  library('PerformanceAnalytics'); library('xts'); library('tidyverse'); library('lubridate')
})

source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/momentum/liquidity-improvement/liqim-common.R")
source("/mnt/data/blog/momentum/liquidity-improvement/liqim-config.R")

reportPath <- "/mnt/data/blog/momentum/liquidity-improvement"
CHK_FILE <- sprintf("%s/checkpoint.rds", reportPath)

MCAP_PCT <- CFG$MCAP_PCT; MIN_PRICE <- CFG$MIN_PRICE; MIN_DVOL <- CFG$MIN_DVOL

PARAM_FP <- list(MCAP_PCT=MCAP_PCT, MIN_PRICE=MIN_PRICE, MIN_DVOL=MIN_DVOL,
                 WINSOR_LO=CFG$WINSOR_LO, WINSOR_HI=CFG$WINSOR_HI, ILLIQ_LB=CFG$ILLIQ_LB)

# ═══════════════════════════════════════════════════════════════
# Data fetch / checkpoint load
# ═══════════════════════════════════════════════════════════════

if (file.exists(CHK_FILE)) {
  chk <- readRDS(CHK_FILE)
  if (!is.null(chk$params) && identical(chk$params, PARAM_FP)) {
    priceVol    <- chk$priceVol; monthEnds <- chk$monthEnds
    benchXts    <- chk$benchXts;  illiqCache <- chk$illiqCache
    priceVol    <- lapply(priceVol, function(df) df[order(df$date_stamp), ])
    benchDates  <- index(benchXts)
    cat(sprintf("Loaded: %d month-ends, %d ILLIQ months\n",
        length(monthEnds), sum(!sapply(illiqCache,is.null))))
  } else {
    cat("Stale — re-fetching\n"); rm(chk); gc(); file.remove(CHK_FILE)
  }
}

if (!exists("illiqCache")) {
  cat("=== DATA FETCH ===\n")
  lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver,"StockViz",ldbuser,ldbpassword), case="nochange", believeNRows=TRUE)
  pcon <- dbConnect(RPostgres::Postgres(), host=ldbserver2, user=ldbuser2, password=ldbpassword2, dbname=ldbname2)

  mcapDf <- sqlQuery(lcon, "SELECT SYMBOL,FF_MKT_CAP_CR,TIME_STAMP FROM equity_misc_info WHERE FF_MKT_CAP_CR IS NOT NULL AND TIME_STAMP>='2005-01-01' ORDER BY SYMBOL,TIME_STAMP")
  mcapDf$TIME_STAMP <- as.Date(mcapDf$TIME_STAMP)
  eqDf <- sqlQuery(lcon, "SELECT SYMBOL,TIME_STAMP FROM px_history WHERE SERIES='EQ' ORDER BY SYMBOL,TIME_STAMP")
  eqDf$TIME_STAMP <- as.Date(eqDf$TIME_STAMP)
  benchDf <- sqlQuery(lcon, "SELECT px_close,time_stamp FROM bhav_index WHERE index_name='NIFTY500 MOMENTUM 50 TR' ORDER BY time_stamp")
  benchDf$time_stamp <- as.Date(benchDf$time_stamp); benchXts <- xts(benchDf$px_close, benchDf$time_stamp)

  minDate <- min(benchDf$time_stamp)-365; maxDate <- max(benchDf$time_stamp)
  pxDf <- dbGetQuery(pcon, sprintf("SELECT ticker,date_stamp,c,v FROM eod_adjusted_nse WHERE date_stamp>='%s' AND date_stamp<='%s' ORDER BY ticker,date_stamp", minDate, maxDate))
  pxDf$date_stamp <- as.Date(pxDf$date_stamp)
  odbcClose(lcon); dbDisconnect(pcon)

  allSymbols <- sort(Reduce(intersect, list(unique(pxDf$ticker), unique(eqDf$SYMBOL), unique(mcapDf$SYMBOL))))
  cat(sprintf("  Common symbols: %d\n", length(allSymbols)))

  pxF <- pxDf |> dplyr::filter(ticker %in% allSymbols, c > 0)
  eqF <- eqDf |> dplyr::filter(SYMBOL %in% allSymbols)
  mcF <- mcapDf |> dplyr::filter(SYMBOL %in% allSymbols)

  priceVol <- lapply(split(pxF, pxF$ticker), function(df) df[order(df$date_stamp),])
  eqBySym  <- lapply(split(eqF, eqF$SYMBOL),  function(df) df[order(df$TIME_STAMP),])
  mcapBySym<- lapply(split(mcF, mcF$SYMBOL), function(df) df[order(df$TIME_STAMP),])

  benchDates <- index(benchXts)
  monthEnds <- unique(floor_date(benchDates,"month")+months(1)-days(1))
  monthEnds <- monthEnds[monthEnds>=benchDates[1] & monthEnds<=last(benchDates)]
  monthEnds <- sapply(monthEnds, function(d){idx<-which(benchDates<=d);if(length(idx)==0)NA else benchDates[max(idx)]})
  monthEnds <- as.Date(unique(monthEnds[!is.na(monthEnds)]))
  cat(sprintf("  Month-ends: %d (%s → %s)\n", length(monthEnds), monthEnds[1], monthEnds[length(monthEnds)]))

  illiqCache <- vector("list", length(monthEnds))
  saveRDS(list(params=PARAM_FP, priceVol=priceVol, monthEnds=monthEnds, allSymbols=allSymbols,
               eqBySymbol=eqBySym, mcapBySymbol=mcapBySym, benchXts=benchXts,
               illiqCache=illiqCache), CHK_FILE)
  cat("  Checkpoint saved\n"); rm(pxDf,eqDf,mcapDf,benchDf,pxF,eqF,mcF); gc()
}

# ═══════════════════════════════════════════════════════════════
# Universe helper
# ═══════════════════════════════════════════════════════════════

getUniverse <- function(sigDate) {
  sigNum <- as.numeric(sigDate)
  allSymbols <- names(priceVol)
  mcaps <- vapply(allSymbols, function(sym){
    rows <- mcapBySym[[sym]]; if(is.null(rows)) return(NA_real_)
    idx <- findInterval(sigNum, as.numeric(rows$TIME_STAMP)); if(idx<1L) return(NA_real_); rows$FF_MKT_CAP_CR[idx]
  }, double(1)); names(mcaps) <- allSymbols
  eqOk <- vapply(allSymbols, function(sym){
    df <- eqBySym[[sym]]; if(is.null(df)) return(FALSE)
    idx <- findInterval(sigNum, as.numeric(df$TIME_STAMP))
    if(idx < 1L) return(FALSE)
    lastDate <- df$TIME_STAMP[max(idx, 1L)]
    as.numeric(sigDate - lastDate) <= 90  # traded within 90 days
  }, logical(1))
  valid <- !is.na(mcaps) & eqOk
  if(!any(valid)) return(character(0))
  syms <- allSymbols[valid]; vals <- mcaps[valid]
  syms <- syms[order(vals, decreasing=TRUE)]
  n <- length(syms); hiIdx <- floor(n * MCAP_PCT)
  if(hiIdx < 1) return(character(0))
  syms[1:hiIdx]
}

# ═══════════════════════════════════════════════════════════════
# ILLIQ computation
# ═══════════════════════════════════════════════════════════════

if (all(sapply(illiqCache, is.null))) {
  cat("=== COMPUTING ILLIQ ===\n")
  for (mi in seq(2L, length(monthEnds))) {
    if(mi %% 12 == 0) cat(sprintf("  ILLIQ %d/%d...\n", mi, length(monthEnds)))
    me <- monthEnds[mi]; ms <- floor_date(me,"month")
    syms <- getUniverse(me); if(length(syms)==0) next
    vals <- vapply(syms, function(sym){
      df <- priceVol[[sym]]; if(is.null(df)) return(NA_real_)
      df <- df[df$date_stamp>=ms & df$date_stamp<=me, , drop=FALSE]
      if(nrow(df)<15) return(NA_real_)
      if(median(df$c,na.rm=TRUE)<MIN_PRICE) return(NA_real_)
      dv <- df$c*df$v; if(median(dv,na.rm=TRUE)<MIN_DVOL) return(NA_real_)
      n <- nrow(df); rets <- diff(df$c)/df$c[-n]
      dollarVol <- df$c[-n]*df$v[-n]
      ok <- dollarVol>0 & !is.na(dollarVol) & !is.na(rets)
      if(sum(ok)<10) return(NA_real_)
      mean(1e6*abs(rets[ok])/dollarVol[ok], na.rm=TRUE)
    }, double(1))
    vals <- vals[!is.na(vals)]
    if(length(vals)>0){ vals<-winsorize(vals); illiqCache[[mi]]<-vals }
  }
  saveRDS(list(params=PARAM_FP, priceVol=priceVol, monthEnds=monthEnds, allSymbols=allSymbols,
               eqBySymbol=eqBySym, mcapBySymbol=mcapBySym, benchXts=benchXts,
               illiqCache=illiqCache), CHK_FILE)
  cat(sprintf("  %d ILLIQ months\n", sum(!sapply(illiqCache,is.null)))); gc()
}

# ═══════════════════════════════════════════════════════════════
# LIQC + universe cache + quintile stats
# ═══════════════════════════════════════════════════════════════

cat("=== LIQC + UNIVERSE ===\n")
liqcCache <- computeLIQC(illiqCache, monthEnds, CFG$ILLIQ_LB)
cat(sprintf("  %d LIQC months\n", sum(!sapply(liqcCache,is.null))))

universeCache <- vector("list", length(monthEnds))
for(mi in seq_len(length(monthEnds))){
  u <- getUniverse(monthEnds[mi]); if(length(u)>0) universeCache[[mi]] <- u
}
cat(sprintf("  %d universe months\n", sum(!sapply(universeCache,is.null))))

# Quintile stats
cat("=== QUINTILE STATS ===\n")
warmupMI <- which(!sapply(liqcCache,is.null))[1]
allRows <- list()

for(mi in seq(warmupMI, length(monthEnds))){
  sigDate <- monthEnds[mi]; if(mi>=length(monthEnds)) break
  liqcVals <- liqcCache[[mi]]; if(is.null(liqcVals)||length(liqcVals)==0) next
  universeSyms <- universeCache[[mi]]; if(is.null(universeSyms)||length(universeSyms)==0) next
  lf <- liqcVals[names(liqcVals) %in% universeSyms]
  if(length(lf) < 50) next

  n <- length(lf); qSize <- floor(n/5)
  for(q in 1:5){
    iStart <- (q-1)*qSize+1; iEnd <- if(q<5) q*qSize else n
    stocks <- names(lf)[iStart:iEnd]
    holdStart <- sigDate+1; nextMe <- monthEnds[which(monthEnds>sigDate)[1]]
    if(is.na(nextMe)) next
    stockRets <- vapply(stocks, function(tkr){
      rets <- stockReturns(priceVol[[tkr]], holdStart, nextMe)
      if(is.null(rets)) NA_real_ else compoundReturn(coredata(rets))
    }, double(1))
    stockRets <- stockRets[!is.na(stockRets)]
    if(length(stockRets)<10) next
    allRows[[length(allRows)+1L]] <- data.frame(
      date=sigDate, quintile=q, n_stocks=length(stockRets), n_universe=n,
      mean_ret=mean(stockRets), median_ret=median(stockRets), sd_ret=sd(stockRets),
      pct_positive=mean(stockRets>0)*100, min_ret=min(stockRets), max_ret=max(stockRets),
      stringsAsFactors=FALSE)
  }
}

df <- do.call(rbind, allRows)
for(q in 1:5){
  sub <- df[df$quintile==q,,drop=FALSE]
  cat(sprintf("\n--- Q%d (%d months) ---\n", q, nrow(sub)))
  cat(sprintf("  Mean: %.4f (%.2f%%), Median: %.4f\n", mean(sub$mean_ret), mean(sub$mean_ret)*100, median(sub$mean_ret)))
  cat(sprintf("  %% up: %.1f%%, stock %% pos: %.1f%%\n", mean(sub$mean_ret>0)*100, mean(sub$pct_positive)))
  cat(sprintf("  Cumulative: %.4f (%.2f%%)\n", compoundReturn(sub$mean_ret), compoundReturn(sub$mean_ret)*100))
  cat(sprintf("  Sharpe: %.3f\n", mean(sub$mean_ret)/sd(sub$mean_ret)))
}
csv <- sprintf("%s/quintile_stats.csv", reportPath); write.csv(df, csv, row.names=FALSE)
cat(sprintf("\nSaved: %s\n", csv))

# ═══════════════════════════════════════════════════════════════
# Save final checkpoint
# ═══════════════════════════════════════════════════════════════

saveRDS(list(params=PARAM_FP, priceVol=priceVol, monthEnds=monthEnds,
             benchXts=benchXts, illiqCache=illiqCache, liqcCache=liqcCache,
             universeCache=universeCache), CHK_FILE)
cat(sprintf("Checkpoint: %s\n", CHK_FILE))
cat("\n===== DONE =====\n")

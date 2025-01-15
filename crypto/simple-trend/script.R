library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
#library('ggrepel')
#library('gtExtras')
#library('webshot2')

options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)

tickers <- c('btcusd', 'ethusd')
smaLbs <- c(5, 10, 20, 50)
drag <- 0.05/100

tickerTs <- dbGetQuery(pgCon, "select curr_code, min(time_stamp) st, max(time_stamp) et from tiingo_crypto_usd_5min_ts group by curr_code")

startDate <- as.Date(max(tickerTs$st)) + 1
endDate <- as.Date(min(tickerTs$et)) - 1

allPx <- list()
for(i in 1:length(tickers)){
  pDf <- dbGetQuery(pgCon, "select time_stamp, px_open Open, px_high High, px_low Low, px_close Close from tiingo_crypto_usd_5min_ts 
             where curr_code = $1 and time_stamp >= $2 and time_stamp <= $3",
             params = list(tickers[i], startDate, endDate))
  
  pXts <- xts(pDf[,-1], pDf[,1])
  allPx[[tickers[i]]] <- pXts
}

allDailySmaRet <- NULL
for(i in 1:length(tickers)){
  pXts <- allPx[[tickers[i]]]  
  hourlyXts <- to.period(pXts$close, period='hours')
  allSmaRets <- NULL 
  for(hr in 0:23){
    hsliceXts <- hourlyXts[sprintf("T%02d:00/T%02d:59", hr, hr), 4]
    index(hsliceXts) <- as.Date(index(hsliceXts))
    smaXts <- do.call(merge.xts, lapply(smaLbs, \(x) SMA(hsliceXts, x)))
    rets <- dailyReturn(hsliceXts)
    allslice <- na.omit(merge(hsliceXts, stats::lag(rets, -1), smaXts))
    smaRets <- do.call(merge.xts, lapply(1:length(smaLbs), \(x){
      rret <- ifelse(allslice[,1] > allslice[,2 + x], allslice[,2], 0)
      rret$T <- ifelse(allslice[,1] > allslice[,2 + x], 1, 0)
      rret$T1 <-  rret$T - stats::lag(rret$T, 1)
      dret <- ifelse(rret$T1 != 0, rret[,1]-drag, rret[,1])
      return(dret)
    }))
    sliceSmaRet <- xts(rowMeans(smaRets, na.rm = TRUE), index(smaRets))
    allSmaRets <- merge.xts(allSmaRets, sliceSmaRet)
  }
  dailySmaRet <- xts(rowMeans(allSmaRets, na.rm = TRUE), index(allSmaRets))
  allDailySmaRet <- merge.xts(allDailySmaRet, dailySmaRet)
}

avgSmaRets <- xts(rowMeans(allDailySmaRet, na.rm = TRUE), index(allDailySmaRet))
names(avgSmaRets) <- c('AVG_SMA')

allDRets <- NULL
for(i in 1:length(tickers)){
  pXts <- allPx[[tickers[i]]] 
  dXts <- dailyReturn(pXts) 
  index(dXts) <- as.Date(index(dXts))
  
  allDRets <- merge.xts(allDRets, dXts)
  
  tXts <- allDailySmaRet[,i]
  index(tXts) <- as.Date(index(tXts))
  
  singleRet <- na.omit(merge(tXts, dXts))
  names(singleRet) <- c(paste0(tickers[i], '_SMA'), paste0(tickers[i], '_BH'))
  
  sharpe <- SharpeRatio.annualized(singleRet)
  Common.PlotCumReturns(singleRet, sprintf("%s SMA vs. B&H", tickers[i]), sprintf("SR: %s", paste(round(sharpe,2), collapse="/")), 
                        sprintf("%s/sma.%s.all.png", reportPath, tickers[i]), NULL)
  
}

avgBhRets <- xts(rowMeans(allDRets, na.rm = TRUE), index(allDRets))
index(avgSmaRets) <- as.Date(index(avgSmaRets))

avgRets <- merge(avgSmaRets, avgBhRets)
names(avgRets) <- c('AVG_SMA', 'AVG_BH')

sharpe <- SharpeRatio.annualized(avgRets)
Common.PlotCumReturns(singleRet, "Avg. SMA vs. B&H", sprintf("SR: %s", paste(round(sharpe,2), collapse="/")), 
                      sprintf("%s/sma.all.png", reportPath), NULL)

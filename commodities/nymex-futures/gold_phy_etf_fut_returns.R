library('RODBC')
library("RSQLite")
library("DBI") 
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", 
                                     ldbserver, 'StockVizUs2', ldbuser, ldbpassword), 
                             case = "nochange", 
                             believeNRows = TRUE)


wmCon <- dbConnect(RSQLite::SQLite(), "/mnt/siberia/data/westmetall.db", flags = RSQLite::SQLITE_RO)
wmGoldDf <-  dbGetQuery(wmCon, "select TIME_STAMP, PX from PRICE_HISTORY where NAME = 'GOLD_LONDON_FIXING'")
dbDisconnect(wmCon)

goldWmXts <- xts(wmGoldDf$PX, as.Date(strptime(wmGoldDf$TIME_STAMP, "%Y%m%d")))
goldWmRet <- dailyReturn(goldWmXts)
goldWmXts <- merge(goldWmXts, stats::lag(goldWmRet, -1))
goldWmXts[abs(goldWmXts[, 2]) > 0.95, 1] <- NA #remove outliers

goldWmXts <- goldWmXts[,1]
goldWmXts <- na.locf(goldWmXts)
goldWmRet <- dailyReturn(goldWmXts)

gldDf <- sqlQuery(lconUs2, "select c, time_stamp from bhav_eq_td where symbol='GLD'")
gldXts <- xts(gldDf[,1], gldDf[,2])
gldRet <- dailyReturn(gldXts)

wmCon <- dbConnect(RSQLite::SQLite(), "/mnt/siberia/data/yahoo_nymex_futures.db", flags = RSQLite::SQLITE_RO)
vDf <- dbGetQuery(wmCon, "select date time_stamp, expiry_date expiry, close val
                                 from futures_eod 
                                 where root = 'GC'
                                 and expiry_date not null
                                 order by date, expiry_date")
dbDisconnect(wmCon)

vDf0 <- vDf |> 
  group_by(time_stamp) |> 
  mutate(time_stamp = as.Date(time_stamp), expiry_series = rank(expiry)) |>
  ungroup() |>
  filter(expiry_series == 1) |>
  select(val, time_stamp)

#todo: calculate the drag of rolling futures contracts

gcXts <- xts(vDf0$val, vDf0$time_stamp)
gcXts[gcXts == 0] <- NA
gcRet <- dailyReturn(gcXts)

toPlot <- merge(goldWmRet, gldRet, gcRet)
names(toPlot) <- c("PHYSICAL", "GLD_ETF", "FRONT_MTH_FUT")
toPlot <- na.omit(toPlot)

Common.PlotCumReturns(toPlot, "Buy & Hold Gold Returns", "",
                      sprintf("%s/gold.phy-etf-fut.cum.png", reportPath))






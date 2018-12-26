library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('schoRsch')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

lookback<-220*10 #10-years

startDate<-as.Date("1970-01-01")
endDate<-as.Date("2018-11-30")
index1<-'^GSPC'

usdDf1<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO 
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and symbol='%s'", startDate, endDate, index1))

usdXts1<-xts(usdDf1$ac, as.Date(usdDf1$time_stamp))

usdXts1<-merge(usdXts1, SMA(usdXts1[,1], 50))
usdXts1<-merge(usdXts1, SMA(usdXts1[,1], 100))
usdXts1<-merge(usdXts1, SMA(usdXts1[,1], 200))

pXts<-na.omit(usdXts1)
names(pXts)<-c('INDEX', 'S50', 'S100', 'S200')

pXts$DAILY_RET_LAG_1<-stats::lag(dailyReturn(pXts[,1]), -1)

pXts$D50<-100*(pXts$S50/pXts$INDEX-1)
pXts$D100<-100*(pXts$S100/pXts$INDEX-1)
pXts$D200<-100*(pXts$S200/pXts$INDEX-1)

pXts<-na.omit(pXts)

d50Tiles<-rollapply(pXts$D50, lookback, function(X) last(ntiles(data.frame(X), dv=1, bins=5)))
d100Tiles<-rollapply(pXts$D100, lookback, function(X) last(ntiles(data.frame(X), dv=1, bins=5)))
d200Tiles<-rollapply(pXts$D200, lookback, function(X) last(ntiles(data.frame(X), dv=1, bins=5)))

dSet<-na.omit(merge(d50Tiles, d100Tiles, d200Tiles, pXts$DAILY_RET_LAG_1))

dSet$L1<-ifelse(dSet[,1] == 1 | dSet[,2] == 1, dSet[,4] , 0)
dSet$L2<-ifelse((dSet[,1] == 1 | dSet[,2] == 1) & dSet[,3] != 1, dSet[,4], 0)

Common.PlotCumReturns(dSet[, c('DAILY_RET_LAG_1', 'L1','L2')], "SP500 SMA Distance", sprintf("%s/SP500.cumulative.all.png", reportPath))
Common.PlotCumReturns(dSet["2006/", c('DAILY_RET_LAG_1', 'L1','L2')], "SP500 SMA Distance", sprintf("%s/SP500.cumulative.2006-.png", reportPath))


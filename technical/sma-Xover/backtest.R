library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('reshape2')

library('ggplot2')
library('extrafont')
library('ggthemes')

reportPath <- "."
options("scipen"=100)
options(stringsAsFactors = FALSE)
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

smaLbs <- c(10, 20, 50, 100, 200)
refIndices <- c('NIFTY 50', 'NIFTY MIDCAP 100', 'NIFTY SMLCAP 100')
endDate<-as.Date('2019-01-31')

for(ri in refIndices){
	iDf<-sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s'", ri))
	iXts<-xts(iDf$px_close, as.Date(iDf$time_stamp))
	iXts<-merge(iXts, stats::lag(dailyReturn(iXts), -1))
	names(iXts)<-c('INDEX', 'RET')
	for(lb in smaLbs){
		oldNames<-names(iXts)
		smaXts<-SMA(iXts[,1], lb)
		smaXtsSmall<-SMA(iXts[,1], round(lb/4))
		smaXtsLag<-stats::lag(smaXts, lb/2)
		iXts<-merge(iXts, smaXts, smaXtsSmall, smaXtsLag)
		
		names(iXts)<-c(oldNames, sprintf("SMA_%d", lb), sprintf("SMA_%d_X", lb), sprintf("SMA_%d_L", lb))
	}
	iXts<-na.omit(iXts)
	
	for(lb in smaLbs){
		oldNames<-names(iXts)
		iXts<-merge(iXts, ifelse(iXts[, sprintf("SMA_%d_X", lb)] > iXts[, sprintf("SMA_%d", lb)], iXts$RET, 0))
		iXts<-merge(iXts, ifelse(iXts[, sprintf("SMA_%d_X", lb)] > iXts[, sprintf("SMA_%d", lb)] & iXts[, sprintf("SMA_%d", lb)] > iXts[, sprintf("SMA_%d_L", lb)], iXts$RET, 0))
		names(iXts)<-c(oldNames, sprintf("RET_%d_X", lb), sprintf("RET_%d_XL", lb))
	}
	
	Common.PlotCumReturns(iXts[, c('RET', sapply(smaLbs, function (X) sprintf("RET_%d_X", X)))], sprintf("%s Tactical", ri), sprintf("%s/%s.index.X.cumulative.all.png", reportPath, ri))
	Common.PlotCumReturns(iXts[, c('RET', sapply(smaLbs, function (X) sprintf("RET_%d_XL", X)))], sprintf("%s Tactical", ri), sprintf("%s/%s.index.XL.cumulative.all.png", reportPath, ri))
}
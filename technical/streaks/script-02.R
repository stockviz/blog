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

btFn<-function(ndf, nameFix){
	ndx<-xts(ndf$px_close, as.Date(ndf$time_stamp))

	dRet<-monthlyReturn(ndx)
	names(dRet)<-c('RET') #current month return
	dRet$RET_LAG_1p<-stats::lag(dRet$RET, 1) #previous month return
	dRet$RET_LAG_1n<-stats::lag(dRet$RET, -1) #next month return

	dRet$L2<-ifelse(dRet$RET_LAG_1p < 0 & dRet$RET < 0, dRet$RET_LAG_1n, 0) #if previous month and current month returns are negative, go long next month

	Common.PlotCumReturns(na.omit(dRet[, c('RET_LAG_1n', 'L2')]), sprintf("%s Streak", nameFix), sprintf("%s/%s.streak.cumulative.all.png", reportPath, nameFix))
	Common.PlotCumReturns(na.omit(dRet["2005/", c('RET_LAG_1n', 'L2')]), sprintf("%s Streak", nameFix), sprintf("%s/%s.streak.cumulative.2005-.png", reportPath, nameFix))
}

ndf<-sqlQuery(lcon, "select px_close, time_stamp from bhav_index where index_name='nifty 50' and time_stamp >= '1991-01-01' and time_stamp <= '2018-12-31'")
btFn(ndf, "NIFTY50")

ndf<-sqlQuery(lcon, "select px_close, time_stamp from bhav_index where index_name='nifty midcap 100' and time_stamp >= '2001-01-01' and time_stamp <= '2018-12-31'")
btFn(ndf, "MIDCAP100")
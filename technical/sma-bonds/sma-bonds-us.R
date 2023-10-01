source("d:/stockviz/r/config.r")
source("d:/stockviz/r/plot.common.r")

reportPath<-"."

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('reshape2')
library('tidyverse')
library('ggthemes')

pdf(NULL)
options(stringsAsFactors = FALSE)
options("scipen"=100)

smaLbs <- c(10, 20, 50, 100, 200, 500)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

etfs <- c('TLT', 'IEF', 'SHY', 'AGG')

pXts <- NULL
for(ticker in etfs){
	indexDf <- sqlQuery(lconUs2, sprintf("select time_stamp, c from BHAV_EQ_TD where symbol = '%s'", ticker))
	pXts <- merge.xts(pXts, xts(indexDf[,-1], indexDf[,1]))
}
names(pXts) <- etfs

for(ticker in etfs){
	etfName <- sqlQuery(lcon, sprintf("select FUND from ETF_META where symbol = '%s'", ticker))[[1]]
	
	print(paste("num n/as: ", sum(ifelse(is.na(pXts[, ticker]), 1, 0))))
	px <- na.omit(pXts[, ticker])
	
	smaXts <- do.call(merge.xts, lapply(smaLbs, function(X) SMA(px, X)))
	pAll <- merge(px, dailyReturn(px))
	pAll <- merge(pAll, stats::lag(pAll[,2], -1))

	pRets <- do.call(merge.xts, lapply(1:length(smaLbs), function(X) ifelse(pAll[,1] > smaXts[,X], pAll[,3], 0)))
	pRets <- merge(pRets, pAll[,3])

	names(pRets) <- c(sapply(smaLbs, function(X) paste0('SMA_', X)), 'BH')
	pRets <- na.omit(pRets)

	Common.PlotCumReturns(pRets, sprintf("%s - %s", ticker, etfName), "USD", sprintf("%s/%s.png", reportPath, ticker))
}
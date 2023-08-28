library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
COUNTRY_ETF_CSV <- "../COUNTRY_NQ.csv"

startDate<-as.Date("2004-03-31")
endDate<-as.Date("2018-12-31")

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indices<-read.csv(COUNTRY_ETF_CSV)
indices$CODE<-sort(indices$CODE)

mRet<-xts()

for(i in 1:nrow(indices)){
	ticker<-indices$CODE[i]
	metaData<-sqlQuery(lconUs2, sprintf("select ID from QUANDL_META_V3 where DATASET_CODE='%s'", ticker))
	iId<-metaData$ID[1]
	
	px<-sqlQuery(lconUs2, sprintf("select trade_date, index_value from QUANDL_DATA_V3 where id=%d and trade_date >='%s' and trade_date <='%s'", iId, startDate, endDate))
	pXts<-xts(px$index_value, as.Date(px$trade_date))
	mXts<-monthlyReturn(pXts)
	mRet<-merge(mRet, mXts)
}

mRet<-mRet[-1,]
names(mRet)<-indices$CODE

Common.PlotCumReturns(mRet, sprintf("NASDAQOMX Country TR Index Cumulative Returns [%s:%s]", startDate, endDate), sprintf("%s/NASDAQOMX.cumulative.%s.%s.png", reportPath, startDate, endDate))
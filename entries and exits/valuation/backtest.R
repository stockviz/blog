library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")


options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."
indexName1<-"NIFTY 50"
indexName2<-"NIFTY MIDCAP 50"
startDate<-as.Date("2008-06-01")
endDate<-as.Date("2020-09-30")
ratioName<-"PB"

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

nDf1<-sqlQuery(lcon, sprintf("select TIME_STAMP, %s from INDEX_NSE_VALUATION where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", ratioName, indexName1, startDate, endDate))
nXts1<-xts(nDf1[,2], as.Date(nDf1[,1]))

nDf2<-sqlQuery(lcon, sprintf("select TIME_STAMP, %s from INDEX_NSE_VALUATION where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", ratioName, indexName2, startDate, endDate))
nXts2<-xts(nDf2[,2], as.Date(nDf2[,1]))

allXts<-merge(nXts1, nXts2)
names(allXts)<-c(indexName1, indexName2)
relXts<-nXts2/nXts1
names(relXts)<-c('RELATIVE')

relVal<-to.period(relXts$RELATIVE, "months")[,4]

nDf1<-sqlQuery(lcon, sprintf("select TIME_STAMP, px_close from bhav_index where index_name='%s TR' and time_stamp >= '%s' and time_stamp <= '%s'", indexName1, startDate, endDate))
nXts1<-monthlyReturn(xts(nDf1[,2], as.Date(nDf1[,1])))

nDf2<-sqlQuery(lcon, sprintf("select TIME_STAMP, px_close from bhav_index where index_name='%s TR' and time_stamp >= '%s' and time_stamp <= '%s'", indexName2, startDate, endDate))
nXts2<-monthlyReturn(xts(nDf2[,2], as.Date(nDf2[,1])))

allXts<-merge(relVal, stats::lag(nXts1, -1), stats::lag(nXts2, -1))
names(allXts)<-c('R', indexName1, indexName2)

allXts$S1<-allXts[,1]*allXts[,2] + (1-allXts[,1])*allXts[,3]
allXts$EQL<- allXts[,2]/2 + allXts[,3]/2

Common.PlotCumReturns(allXts[, -1], sprintf("%s/%s %s Weights", indexName1, indexName2, ratioName), "gross/monthly", sprintf("%s/%s.%s.%s-weights.png", reportPath, indexName1, indexName2, ratioName), NULL)
library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('viridis')

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")

reportPath <- "D:/StockViz/public/blog/entries and exits/valuation"

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."
indexName1<-"NIFTY 50"
indexName2<-"NIFTY MIDCAP 50"
startDate<-as.Date("2004-01-01")
endDate<-as.Date("2020-09-30")
mavgYrs <- 5
lb<-220*mavgYrs 

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

ratioName <- 'PB'
indexName1<-"NIFTY 50"
indexName2<-"NIFTY MIDCAP 50"

factorIndex <- 'NIFTY500 VALUE 50 TR'

nDf1<-sqlQuery(lcon, sprintf("select TIME_STAMP, %s from INDEX_NSE_VALUATION where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", ratioName, indexName1, startDate, endDate))
nXts1<-xts(nDf1[,2], as.Date(nDf1[,1]))

nDf2<-sqlQuery(lcon, sprintf("select TIME_STAMP, %s from INDEX_NSE_VALUATION where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", ratioName, indexName2, startDate, endDate))
nXts2<-xts(nDf2[,2], as.Date(nDf2[,1]))

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", factorIndex, startDate, endDate))
pXts <- merge.xts(xts(pDf[,2], pDf[,1]))

allXts<-merge(nXts1, nXts2)
names(allXts)<-c(indexName1, indexName2)
allXts[,1] <- na.locf(allXts[,1])
allXts[,2] <- na.locf(allXts[,2])

mavg <- merge(rollapply(allXts, lb, mean), rollapply(allXts, lb, sd))
allXts <- na.omit(merge(allXts, mavg[,1], mavg[,2], mavg[,1]+mavg[,3], mavg[,1]-mavg[,3], mavg[,2]+mavg[,4], mavg[,2]-mavg[,4], dailyReturn(pXts)))
names(allXts) <- c(indexName1, indexName2, 'i1', 'i2', 'i1a', 'i1b', 'i2a', 'i2b', factorIndex)

rets <- merge(ifelse(allXts[,indexName1] > allXts[,'i1b'] & allXts[,indexName1] < allXts[,'i1a'], allXts[,factorIndex], 0), 
				ifelse(allXts[,indexName2] > allXts[,'i2b'] & allXts[,indexName2] < allXts[,'i2a'], allXts[,factorIndex], 0), 
				allXts[,factorIndex])

rets <- rets[-1,]
names(rets) <- c(paste0(indexName1, ' valuation'), paste0(indexName2, ' valuation'), 'Buy & Hold')

Common.PlotCumReturns(rets, paste(ratioName, 'Timing', factorIndex), "gross/daily", sprintf("%s/%s.%s.png", reportPath, ratioName, factorIndex), NULL)
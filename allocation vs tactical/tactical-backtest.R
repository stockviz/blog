library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('lubridate')
library('reshape2')

library('ggthemes')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")

pdf(NULL)
reportPath <- "D:/StockViz/public/blog/allocation vs tactical"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

smaLb <- 200 #days

indexName <- "NIFTY 50 TR"
startDate <- as.Date("2003-12-31")
endDate <- as.Date("2020-05-31")

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate, endDate))
eqXts <- xts(pDf[,2], pDf[,1])

eqXts <- eqXts[-1,]
names(eqXts) <- c("EQUITY")

pDf <- sqlQuery(lcon, sprintf("select time_stamp, tri from index_ccil_tenor where index_name='0_5' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
bndXts <- xts(pDf[,2], pDf[,1])

bndXts <- bndXts[-1,]
names(bndXts) <- c("BOND")

######################################

eqBndXts <- merge(eqXts, bndXts)
eqBndXts[,2] <- na.locf(eqBndXts[,2])
eqBndXts <- na.omit(eqBndXts)

allXts <- merge(eqXts, SMA(eqXts, smaLb), weeklyReturn(eqXts), weeklyReturn(eqBndXts[,2]))
allXts <- na.omit(allXts)

allXts <- merge(allXts, stats::lag(allXts[,3], -1), stats::lag(allXts[,4], -1))

btest <- ifelse(allXts[,1] > allXts[,2], allXts[,5], allXts[,6])

toPlot <- merge(btest, allXts[,5], allXts[,6])
names(toPlot) <- c('TACTICAL', 'EQUITY', 'BOND')

Common.PlotCumReturns(toPlot$EQUITY, indexName, "", sprintf("%s/%s.%d.cumulative-return-actual.png", reportPath, indexName, smaLb), NULL)
Common.PlotCumReturns(toPlot$BOND, "Short-term Bonds", "", sprintf("%s/Short-term Bonds.%d.cumulative-return-actual.png", reportPath, smaLb), NULL)
Common.PlotCumReturns(toPlot, sprintf("Tactical %d-SMA: %s and Short-term Bonds", smaLb, indexName), "weekly rebalance", sprintf("%s/tactical.%d-SMA.%s-bonds.cumulative-return-actual.png", reportPath, smaLb, indexName), NULL)

write.csv(data.frame(toPlot), sprintf("%s/tactical.%d-SMA.%s-bonds.cumulative-return-actual.csv", reportPath, smaLb, indexName), row.names=F)


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

indexDf <- sqlQuery(lcon, sprintf("select time_stamp, val from INDEX_CCIL_ALL where index_id='%s' and index_name = '%s'", 'BOND', 'BROAD TRI'))
broadXts <- xts(indexDf[,-1], indexDf[,1])

indexDf <- sqlQuery(lcon, sprintf("select time_stamp, val from INDEX_CCIL_ALL where index_id='%s' and index_name = '%s'", 'BOND', 'LIQUID TRI'))
liqXts <- xts(indexDf[,-1], indexDf[,1])

broadSmas <- do.call(merge.xts, lapply(smaLbs, function(X) SMA(broadXts, X)))
liqSmas <- do.call(merge.xts, lapply(smaLbs, function(X) SMA(liqXts, X)))

broadAll <- merge(broadXts, dailyReturn(broadXts))
broadAll <- merge(broadAll, stats::lag(broadAll[,2], -1))

liqAll <- merge(liqXts, dailyReturn(liqXts))
liqAll <- merge(liqAll, stats::lag(liqAll[,2], -1))

broadRets <- do.call(merge.xts, lapply(1:length(smaLbs), function(X) ifelse(broadAll[,1] > broadSmas[,X], broadAll[,3], 0)))
liqRets <- do.call(merge.xts, lapply(1:length(smaLbs), function(X) ifelse(liqAll[,1] > liqSmas[,X], liqAll[,3], 0)))

broadRets <- merge(broadRets, broadAll[,3])
liqRets <- merge(liqRets, liqAll[,3])

names(broadRets) <- c(sapply(smaLbs, function(X) paste0('SMA_', X)), 'BH')
names(liqRets) <- c(sapply(smaLbs, function(X) paste0('SMA_', X)), 'BH')

broadRets <- na.omit(broadRets)
liqRets <- na.omit(liqRets)

Common.PlotCumReturns(broadRets, "CCIL BOND INDEX (BROAD)", "TRI", sprintf("%s/CCIL.BOND.INDEX.BROAD.png", reportPath))
Common.PlotCumReturns(broadRets, "CCIL BOND INDEX (LIQUID)", "TRI", sprintf("%s/CCIL.BOND.INDEX.LIQUID.png", reportPath))
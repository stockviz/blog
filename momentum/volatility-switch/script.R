library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('tidyverse')
library('lubridate')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexName <- "NIFTY200 MOMENTUM 30 TR"

windowDays <- 3*220
volDays <- 20
numTiles <- 4

startDate <- as.Date("2010-01-01")
endDate <- as.Date("2023-05-31")

trainTs <- "/2019"
testTs <- "2020-05-01/"

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate, endDate))
indexXts <- xts(indexPx[,2], indexPx[,1])

allVol <- na.omit(volatility(indexXts, volDays, calc='close'))
allVolTile <- rollapply(allVol, windowDays, function(X) last(ntile(coredata(X), n=numTiles)))

rollTile <- allVolTile[trainTs]

retXts <- merge(rollTile, dailyReturn(indexXts[trainTs]))
retXts <- merge(retXts, stats::lag(retXts[,2], -1))

retXts <- na.omit(retXts[, c(1,3)])
names(retXts) <- c('TILE', 'LAG_RET')

tileRets <- NULL
for(i in 1:numTiles){
	tileRets <- merge.xts(tileRets, ifelse(retXts$TILE == i, retXts$LAG_RET, 0))
}

tileRets <- merge(tileRets, retXts$LAG_RET)

Return.cumulative(tileRets)
maxDrawdown(tileRets)
SharpeRatio.annualized(tileRets)

###

tileCutoff <- 2
trainRet <- ifelse(retXts$TILE <= tileCutoff, retXts$LAG_RET, 0)
names(trainRet) <- c('TILE_RET')
trainRet <- merge(trainRet, retXts$LAG_RET)

Return.cumulative(trainRet)
maxDrawdown(trainRet)
SharpeRatio.annualized(trainRet)

############################

rollTile <- allVolTile[testTs]

retXts <- merge(rollTile, dailyReturn(indexXts[testTs]))
retXts <- merge(retXts, stats::lag(retXts[,2], -1))

retXts <- na.omit(retXts[, c(1,3)])
names(retXts) <- c('TILE', 'LAG_RET')

testRet <- ifelse(retXts$TILE <= tileCutoff, retXts$LAG_RET, 0)
names(testRet) <- c('TILE_RET')
testRet <- merge(testRet, retXts$LAG_RET)

Return.cumulative(testRet)
maxDrawdown(testRet)
SharpeRatio.annualized(testRet)

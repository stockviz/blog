source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/stockviz/r/theme.returns.common.r")

reportPath<-"."

library('RODBC')

library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('tidyverse')
library('reshape2')
library('ggthemes')
library('MetBrewer')

options(stringsAsFactors = FALSE)
options("scipen"=100)
pdf(NULL)

stt<-0.1/100
brkg<-0.05/100
drag <- 0.2/100

con <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", dbserver, dbname, dbuser, dbpassword), case = "nochange", believeNRows = TRUE)

userId <- '30B18A87-D803-4476-8469-858454C2C49A'
model <- c('2DF79FD7-64F4-47F2-9F53-6B1ACE8012A3', 'Quality to Price')


themeReturns <- Common.GetEquityThemeReturns(model[1], userId, top=NA, stt, brkg)
themeReturns <- na.omit(themeReturns)

startTest <- as.Date("2016-01-01")
endTest <- as.Date("2019-12-31")
startValid <- as.Date("2020-01-01")
endValid <- as.Date("2022-11-25")
startInspect <- as.Date("2020-05-01")

smaLbs <- c(10, 20, 50, 100, 200)
smaNames <- sapply(smaLbs, function(X) paste0('SMA', X))

nXts <- themeReturns[, c('MODEL_PX', 'MODEL_RET_BRK')] #1, 2
nXts <- merge(nXts, stats::lag(nXts[,2], -1))

nXts <- merge(nXts, do.call(merge, lapply(smaLbs, function(X) SMA(nXts[,1], X))))

names(nXts) <- c('INDEX', 'RET', 'RET_LAG1', smaNames)

smaTradeSeq <- do.call(merge, lapply(smaLbs, function(X) ifelse(nXts$INDEX > nXts[, paste0('SMA', X)], 1, 0)))
names(smaTradeSeq) <- smaNames

smaTrades <- do.call(merge, lapply(smaLbs, function(X) smaTradeSeq[, paste0('SMA', X)] - stats::lag(smaTradeSeq[, paste0('SMA', X)],1)))
names(smaTrades) <- smaNames

smaRets <- do.call(merge, lapply(smaLbs, function(X) ifelse(nXts$INDEX > nXts[, paste0('SMA', X)], nXts$RET_LAG1, 0)))
names(smaRets) <- smaNames

smaRetsDragged <- do.call(merge, lapply(smaLbs, function(X) ifelse(smaTrades[, paste0('SMA', X)] != 0, smaRets[, paste0('SMA', X)] - drag, smaRets[, paste0('SMA', X)])))
retsAll <- merge(nXts$RET_LAG1, smaRetsDragged)

names(retsAll) <- c('BH', smaNames)

testRets <- na.omit(retsAll[paste0(startTest, '/', endTest)])

Common.PlotCumReturns(testRets, sprintf("%s", model[2]), "(EOD rebalance)", sprintf("%s/%s.test.cumulative.png", reportPath, model[1]))

cumRets <- sort(apply(testRets[, -1], 2, Return.cumulative), decreasing=T)
maxDDs <- sort(apply(testRets[, -1], 2, maxDrawdown))

print(paste(cumRets[1], '/', maxDDs[1]))

maxRetSMA <- names(cumRets[1])
minDDSMA <- names(maxDDs[1])

print(paste(maxRetSMA, '/', minDDSMA))

##### validate

valRets <- retsAll[paste0(startValid, '/', endValid), c('BH', maxRetSMA, minDDSMA)]
names(valRets) <- c(model[2], maxRetSMA, minDDSMA)

Common.PlotCumReturns(valRets, sprintf("%s", model[2]), "(EOD rebalance)", sprintf("%s/%s.validate.cumulative.png", reportPath, model[1]))

##### inspect

insRets <- retsAll[paste0(startInspect, '/'), c('BH', maxRetSMA, minDDSMA)]

names(insRets) <- c(model[2], maxRetSMA, minDDSMA)

Common.PlotCumReturns(insRets, sprintf("%s", model[2]), "(EOD rebalance)", sprintf("%s/%s.inspect.cumulative.png", reportPath, model[1]))


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
#model <- c('D6663194-A965-4262-84F6-BB553B0B8998', '1A6C40B8-BDF1-43E5-829C-E3265BDB7F1A', 'Momentum')
#model <- c('6CAECA7A-A2C9-4B9D-BD0B-397FCE9597D2', 'AFD0DFFF-2EA7-4E4D-BA50-D9CC0E4B5052', 'Velocity')
model <- c('A251E57D-B2B2-43CA-A457-E25C2CAD08A9', '22849AAA-554B-4859-B06E-760B1902104B', 'Acceleration')


themeReturns <- Common.GetEquityThemeReturns(model[1], userId, top=NA, stt, brkg)
themeReturns <- na.omit(themeReturns)

momothemeReturns <- Common.GetEquityThemeReturns(model[2], userId, top=NA, stt, brkg)
momothemeReturns <- na.omit(momothemeReturns)

startTest <- as.Date("2016-01-01")
endTest <- as.Date("2019-12-31")
startValid <- as.Date("2020-01-01")
endValid <- as.Date("2022-11-25")
startInspect <- as.Date("2020-05-01")

smaLbs <- c(10, 20, 50)
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

Common.PlotCumReturns(testRets, sprintf("%s", model[3]), "(EOD rebalance)", sprintf("%s/%s.test.cumulative.png", reportPath, model[1]))

cumRets <- sort(apply(testRets[, -1], 2, Return.cumulative), decreasing=T)
maxDDs <- sort(apply(testRets[, -1], 2, maxDrawdown))

print(paste(cumRets[1], '/', maxDDs[1]))

maxRetSMA <- names(cumRets[1])
minDDSMA <- names(maxDDs[1])

print(paste(maxRetSMA, '/', minDDSMA))

##### validate

valRets <- retsAll[paste0(startValid, '/', endValid), c('BH', maxRetSMA, minDDSMA)]
valRets <- merge(valRets, momothemeReturns[paste0(startValid, '/', endValid), 'MODEL_RET_BRK'])
names(valRets) <- c(model[3], maxRetSMA, minDDSMA, paste(model[3], '(momo)'))

Common.PlotCumReturns(valRets, sprintf("%s", model[3]), "(EOD rebalance)", sprintf("%s/%s.validate.cumulative.png", reportPath, model[1]))

##### inspect

insRets <- retsAll[paste0(startInspect, '/'), c('BH', maxRetSMA, minDDSMA)]
insRets <- merge(insRets, momothemeReturns[paste0(startInspect, '/'), 'MODEL_RET_BRK'])

names(insRets) <- c(model[3], maxRetSMA, minDDSMA, paste(model[3], '(momo)'))

Common.PlotCumReturns(insRets, sprintf("%s", model[3]), "(EOD rebalance)", sprintf("%s/%s.inspect.cumulative.png", reportPath, model[1]))


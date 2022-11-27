source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")

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

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDateTest <- as.Date("2010-01-01")
endDateTest <- as.Date("2015-12-31")

startDateValidate <- as.Date("2016-01-01")
endDateValidate <- as.Date("2022-10-31")

smaLbs <- c(10, 20, 50, 100, 200)
smaNames <- sapply(smaLbs, function(X) paste0('SMA', X))

indexName <- "NIFTY MIDCAP150 MOMENTUM 50 TR"
#indexName <- "NIFTY200 MOMENTUM 30 TR"

##### back-test

nEod <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDateTest, endDateTest))

nXts <- xts(nEod[,1], nEod[,2])

nXts <- merge(nXts, dailyReturn(nXts)) #1, 2
nXts <- merge(nXts, stats::lag(nXts[,2], -1))

nXts <- merge(nXts, do.call(merge, lapply(smaLbs, function(X) SMA(nXts[,1], X))))

names(nXts) <- c('INDEX', 'RET', 'RET_LAG1', smaNames)

smaRets <- merge(nXts$RET_LAG1, do.call(merge, lapply(smaLbs, function(X) ifelse(nXts$INDEX > nXts[, paste0('SMA', X)], nXts$RET_LAG1, 0))))
names(smaRets) <- c('BH', smaNames)

toPlotRets <- na.omit(smaRets)

Common.PlotCumReturns(toPlotRets, sprintf("%s", indexName), "(EOD rebalance)", sprintf("%s/%s.test.cumulative.png", reportPath, indexName))

cumRets <- sort(apply(toPlotRets[, -1], 2, Return.cumulative), decreasing=T)
maxDDs <- sort(apply(toPlotRets[, -1], 2, maxDrawdown))

print(paste(cumRets[1], '/', maxDDs[1]))

maxRetSMA <- as.numeric(gsub("SMA", "", names(cumRets[1])))
minDDSMA <- as.numeric(gsub("SMA", "", names(maxDDs[1])))

print(paste(maxRetSMA, '/', minDDSMA))

##### validate

nEod <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDateValidate - 360, endDateValidate))

nXts <- xts(nEod[,1], nEod[,2])

nXts <- merge(nXts, dailyReturn(nXts)) #1, 2
nXts <- merge(nXts, stats::lag(nXts[,2], -1))
names(nXts) <- c('INDEX', 'RET', 'RET_LAG1')

if (maxRetSMA != minDDSMA){
	valRets <- merge(nXts$RET_LAG1, ifelse(nXts$INDEX > SMA(nXts$INDEX, maxRetSMA), nXts$RET_LAG1, 0), ifelse(nXts$INDEX > SMA(nXts$INDEX, minDDSMA), nXts$RET_LAG1, 0))
	names(valRets) <- c('BH', paste0('SMA', maxRetSMA), paste0('SMA', minDDSMA))
} else {
	valRets <- merge(nXts$RET_LAG1, ifelse(nXts$INDEX > SMA(nXts$INDEX, maxRetSMA), nXts$RET_LAG1, 0))
	names(valRets) <- c('BH', paste0('SMA', maxRetSMA))
}

toPlotRets <- na.omit(valRets[paste0(startDateValidate, '/'), ])

Common.PlotCumReturns(toPlotRets, sprintf("%s", indexName), "(EOD rebalance)", sprintf("%s/%s.validate.cumulative.png", reportPath, indexName))

years <- unique(year(index(toPlotRets)))

annRets <- do.call(rbind, lapply(years, function(X) xts(matrix(Return.cumulative(toPlotRets[toString(X),]), nrow=1), as.Date(paste0(X, '-12-31')))))
names(annRets) <- names(valRets)

toPlotAnnRets <- data.frame(annRets * 100)
toPlotAnnRets$Y <- year(index(annRets))
toPlotAnnRets$Y <- factor(toPlotAnnRets$Y, levels=toPlotAnnRets$Y)

toPlotAnnRets <- melt(toPlotAnnRets, id='Y')

ggplot(toPlotAnnRets, aes(x=Y, y=value, fill=variable)) +
	theme_economist() +
	scale_fill_manual(values=met.brewer("Wissing", ncol(annRets), type="discrete")) +
	geom_bar(stat="identity", position=position_dodge()) +
	labs(x = 'year', y = 'annual returns (%)', fill = "", color="", size="", title=sprintf("%s", indexName), subtitle=sprintf("[%s:%s]", startDateValidate, endDateValidate)) +
	annotate("text", x=1, y=min(annRets), label = "@StockViz", hjust=0, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)

ggsave(sprintf("%s/%s.validate.annual.png", reportPath, indexName), width=16, height=8, units="in")	
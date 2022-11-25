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

startDate <- as.Date("2005-04-01")
endDate <- as.Date("2013-12-31")

indexName <- "NIFTY MIDCAP150 MOMENTUM 50 TR"
#indexName <- "NIFTY200 MOMENTUM 30 TR"

nEod <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate, endDate))

nXts <- xts(nEod[,1], nEod[,2])

nXts <- merge(nXts, dailyReturn(nXts)) #1, 2

nXts <- merge(nXts, 
				rollapply(nXts[,2], 50, sd), #3 
				rollapply(nXts[,2], 100, sd), #4
				rollapply(nXts[,2], 200, sd), #5
				rollapply(nXts[,2], 5, Return.cumulative), #6
				rollapply(nXts[,2], 20, Return.cumulative) #7
				)

nXts <- merge(nXts, stats::lag(nXts[, 6], -5), stats::lag(nXts[, 7], -20))

names(nXts) <- c("INDEX", "RET", "SD50", "SD100", "SD200", "RET5", "RET20", "RET5_LAG5", "RET20_LAG20")

aNames <- c("SD50", "SD100", "SD200")
bNames <- c("RET5_LAG5", "RET20_LAG20")

doScatter <- function(aName, bName){
	toPlot <- data.frame(nXts[, c(aName, bName)])
	
	ggplot(toPlot, aes_string(x=aName, y=bName)) +
	theme_economist() +
	geom_point() +
	labs(fill="", color="", size="", title=sprintf("%s Forward Returns vs. Historical Volatility", indexName), subtitle=sprintf("[%s:%s]", startDate, endDate)) +
	annotate("text", x=min(toPlot[,1]), y=min(toPlot[,2]), label = "@StockViz", hjust=0, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
	ggsave(sprintf("%s/%s.%s.%s.png", reportPath, indexName, aName, bName), width=16, height=8, units="in")
}

plotOverlapping <- function(){
	for(i in 1:length(aNames)){
		for(j in 1:length(bNames)){
			doScatter(aNames[i], bNames[j])
		}
	}
}

plotOverlapping()

################################################################

# monthly rebalance based on static look-back SD and static threshold

backtest <- function(sdLb){
	# back test to calc max return & min drawdown SDs
	
	nXts <- xts(nEod[,1], nEod[,2])
	nXts <- merge(nXts, dailyReturn(nXts), monthlyReturn(nXts)) #1, 2, 3
	nXts <- merge(nXts, rollapply(nXts[,2], sdLb, sd)) #4
	nXts <- na.omit(nXts)
	nXts <- merge(nXts, stats::lag(nXts[, 3], -1)) #5

	names(nXts) <- c("INDEX", "RET", "RETM", "SD200", "RETM_LAG1")

	sdThreshs <- seq(0.01, 0.05, by=0.005)

	threshRets <- do.call(merge, lapply(sdThreshs, function(X) ifelse(nXts$SD200 > X, 0, nXts$RETM_LAG1)))

	names(threshRets) <- sapply(sdThreshs, function(X) paste0("RETM", X))

	toPlot <- na.omit(merge(nXts$RETM_LAG1, threshRets))

	Common.PlotCumReturns(toPlot, sprintf("%s/%d-day std. dev.", indexName, sdLb), "(EOM rebalance)", sprintf("%s/%s.%d.cumulative.png", reportPath, indexName, sdLb))

	cumRets <- sort(apply(toPlot, 2, Return.cumulative), decreasing=T)
	print(cumRets)

	maxDDs <- sort(apply(toPlot, 2, maxDrawdown))

	maxRetSd <- as.numeric(gsub("RETM", "", names(cumRets[1])))
	minDDSd <- as.numeric(gsub("RETM", "", names(maxDDs[1])))

	# forward test

	startDate <- as.Date("2014-01-01")
	endDate <- as.Date("2022-10-31")

	nEod <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate-360, endDate))

	nXts <- xts(nEod[,1], nEod[,2])
	nXts <- merge(nXts, dailyReturn(nXts), monthlyReturn(nXts)) #1, 2, 3
	nXts <- merge(nXts, rollapply(nXts[,2], sdLb, sd)) #4
	nXts <- na.omit(nXts)
	nXts <- merge(nXts, stats::lag(nXts[, 3], -1)) #5

	names(nXts) <- c("INDEX", "RET", "RETM", "SD200", "RETM_LAG1")

	nXts$MAX_RET <- ifelse(nXts$SD200 > maxRetSd, 0, nXts$RETM_LAG1)
	nXts$MIN_DD <- ifelse(nXts$SD200 > minDDSd, 0, nXts$RETM_LAG1)

	toPlot <- na.omit(nXts[paste0(startDate, "/"), c("RETM_LAG1", "MAX_RET", "MIN_DD")])

	Common.PlotCumReturns(toPlot, sprintf("%s/%.3f %.3f %d-day std. dev.", indexName, maxRetSd, minDDSd, sdLb), "(EOM rebalance)", sprintf("%s/%s.max.%d.cumulative.png", reportPath, indexName, sdLb))

	toPlot <- toPlot["2020-05-01/",]
	Common.PlotCumReturns(toPlot, sprintf("%s/%.3f %.3f %d-day std. dev.", indexName, maxRetSd, minDDSd, sdLb), "(EOM rebalance)", sprintf("%s/%s.max.%d.cumulative.2020.png", reportPath, indexName, sdLb))
}

sdLbs <- c(10, 20, 50, 100, 200) #days

lapply(sdLbs, function(X) backtest(X))

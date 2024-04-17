library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tseries')
library('FactorAnalytics')

library('reshape2')
library('lubridate')
library('tidyverse')
library('ggthemes')
library('viridis')

options("scipen"=100)
options(stringsAsFactors = FALSE)

pdf(NULL)
reportPath <- "."
source("d:/stockviz/r/config.r")
source("d:/stockviz/r/plot.common.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUS <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUS", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

nseIndices <- c('NIFTY 50 TR', 'NIFTY MIDCAP SELECT TR', 'NIFTY BANK TR')
yahooTicker <- "^GSPC"

lbDays <- 220

startDates <- sqlQuery(lcon, sprintf("select index_name, min(time_stamp) from bhav_index where index_name in ('%s') group by index_name", paste(nseIndices, collapse="','")))
startDate <- max(startDates[,2])

yahooXts <- get.hist.quote(instrument=yahooTicker, start=startDate, 
                       end=Sys.Date(), 
                       quote="AdjClose", 
                       compression="d", retclass="zoo")


usdInr <- sqlQuery(lconUS, sprintf("select time_stamp, val from FRED_OBSERVATION where SERIES_ID=-2147478748 and time_stamp >= '%s'", startDate))
inrXts <- xts(usdInr[,-1], usdInr[,1])

aXts <- merge(yahooXts, inrXts)
aXts[,2] <- na.locf(aXts[,2])
aXts <- na.omit(aXts)
aXts <- merge(aXts, aXts[,1] * aXts[,2])
adRet <- dailyReturn(xts(aXts[, 3]))
awRet <- weeklyReturn(xts(aXts[, 3]))
amRet <- monthlyReturn(xts(aXts[, 3]))

bXts <- NULL
bwXts <- NULL
bmXts <- NULL
for(iName in nseIndices){
	pxDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = '%s' and time_stamp >= '%s'", iName, startDate))
	pXts <- xts(pxDf[,-1], pxDf[,1])
	bXts <- merge.xts(bXts, dailyReturn(pXts))
	bwXts <- merge.xts(bwXts, weeklyReturn(pXts))
	bmXts <- merge.xts(bmXts, monthlyReturn(pXts))
}

allXts <- merge(adRet, bXts)
allXts <- allXts[-1,]
allXts <- na.locf(allXts)

names(allXts) <- c('Y', nseIndices)

factMuls <- rollapply(allXts, lbDays, function(X) {
	sqp <- style.fit(X[,1], X[,-1], method='constrained', leverage = FALSE)
	xts(t(sqp$weights), last(index(X)))
}, by.column = FALSE)

dfactOp <- xts(rowSums(do.call(merge.xts, lapply(1:ncol(factMuls), function(i) factMuls[,i] * stats::lag(allXts[,i + 1], -1)))), index(factMuls))

toPlot <- merge(adRet, dfactOp)
names(toPlot) <- c(yahooTicker, "replication")

#Common.PlotCumReturns(toPlot, sprintf("%s Replication", yahooTicker), "daily; gross", NULL)
Common.PlotCumReturns(toPlot, sprintf("%s Replication", yahooTicker), "daily; gross", sprintf("%s/%s.daily.png", reportPath, str_replace_all(yahooTicker, "[^[:alnum:]]", "")))

wXts <- merge(do.call(merge.xts, lapply(1:ncol(factMuls), function(i) SMA(factMuls[,i],3))), stats::lag(bwXts, -1))
wXts <- na.omit(wXts)
wfactOp <- xts(rowSums(do.call(merge.xts, lapply(1:ncol(factMuls), function(i) wXts[,i] * wXts[,i + ncol(factMuls)]))), index(wXts))

toPlot <- merge(stats::lag(awRet, -1), wfactOp)
names(toPlot) <- c(yahooTicker, "replication")

#Common.PlotCumReturns(toPlot, sprintf("%s Replication", yahooTicker), "weekly; gross", NULL)
Common.PlotCumReturns(toPlot, sprintf("%s Replication", yahooTicker), "weekly; gross", sprintf("%s/%s.weekly.png", reportPath, str_replace_all(yahooTicker, "[^[:alnum:]]", "")))

mXts <- merge(do.call(merge.xts, lapply(1:ncol(factMuls), function(i) SMA(factMuls[,i],3))), stats::lag(bmXts, -1))
mXts <- na.omit(mXts)
mfactOp <- xts(rowSums(do.call(merge.xts, lapply(1:ncol(factMuls), function(i) mXts[,i] * mXts[,i + ncol(factMuls)]))), index(mXts))

toPlot <- merge(stats::lag(amRet, -1), mfactOp)
names(toPlot) <- c(yahooTicker, "replication")

#Common.PlotCumReturns(toPlot, sprintf("%s Replication", yahooTicker), "monthly; gross", NULL)
Common.PlotCumReturns(toPlot, sprintf("%s Replication", yahooTicker), "monthly; gross", sprintf("%s/%s.monthly.png", reportPath, str_replace_all(yahooTicker, "[^[:alnum:]]", "")))

mLoadings <- mXts[, 1:ncol(factMuls)]
names(mLoadings) <- nseIndices

toPlot <- data.frame(mLoadings)
toPlot$T <- index(mLoadings)
toPlot <- melt(toPlot, id='T')

ggplot(toPlot, aes(x=T, y=value, fill=variable)) + 
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_bar(stat="identity") +
	scale_fill_viridis_d() +
	scale_x_date(breaks = "6 months") +
	labs(x = "", y="multiplier", fill="", color="", title=sprintf("%s Replication", yahooTicker), subtitle=sprintf("%s:%s", startDate, last(index(yahooXts))), caption = '@StockViz')
	
ggsave(sprintf("%s/%s.monthly-loadings.png", reportPath, str_replace_all(yahooTicker, "[^[:alnum:]]", "")), width=16, height=8, units="in")	

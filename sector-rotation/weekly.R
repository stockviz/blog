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
source("D:/StockViz/public/blog/common/misc.R")
source("D:/StockViz/public/blog/common/plot.common.R")

#reportPath <- "c:/stockviz/report_eod_heat"
reportPath <- "d:/stockviz/report"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)


startDateCutoff <- as.Date("2012-03-01")

sectorIndexNames <- c("NIFTY BANK TR", "NIFTY AUTO TR", "NIFTY COMMODITIES TR", "NIFTY CONSUMER DURABLES TR", 
					"NIFTY ENERGY TR", "NIFTY FMCG TR", "NIFTY INDIA CONSUMPTION TR", "NIFTY INFRASTRUCTURE TR", "NIFTY IT TR",
					"NIFTY MEDIA TR", "NIFTY METAL TR", "NIFTY PHARMA TR", "NIFTY PRIVATE BANK TR", "NIFTY PSU BANK TR", "NIFTY REALTY TR", "NIFTY SERVICES SECTOR TR")
					
mktCapIndexNames <- "NIFTY 100 TR"

minMax <- sqlQuery(lcon, sprintf("select index_name, min(time_stamp) s, max(time_stamp) m from bhav_index where index_name in ('%s') group by index_name", paste(sectorIndexNames, collapse="','")))

if(!all(minMax$m == minMax$m[1])){
	error("end dates don't match")
	q()
}

minMax <- minMax[minMax$s <= startDateCutoff,]
startDate <- max(minMax$s)
endDate <- minMax$m[1]

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s'", mktCapIndexNames, startDate))
pXts <- xts(pDf$px_close, pDf$time_stamp)
bench <- weeklyReturn(pXts)

pXts <- NULL
for(iName in minMax$index_name){
	pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s'", iName, startDate))
	pXts <- merge.xts(pXts, xts(pDf$px_close, pDf$time_stamp))
}

dXts <- NULL
for(j in 1:ncol(pXts)){
	dXts <- merge.xts(dXts, weeklyReturn(pXts[,j]))
}
names(dXts) <- minMax$index_name

dXts <- dXts[-1,]
dXts <- dXts[-nrow(dXts),]

cumRetXts <- NULL
for(j in 1:ncol(dXts)){
	cumRetXts <- merge.xts(cumRetXts, rollapply(dXts[,j], 5, Return.cumulative))
}
names(cumRetXts) <- minMax$index_name
numCols <- ncol(cumRetXts)

allXts <- merge(Common.NormalizeWeeklyDates(cumRetXts), Common.NormalizeWeeklyDates(stats::lag(dXts, -1)), Common.NormalizeWeeklyDates(stats::lag(bench, -1)))
allXts <- na.omit(allXts)
maxCum <- data.frame(apply(allXts[, 1:numCols], 1, function(x) which(x == max(x))))
maxXts <- xts(maxCum, index(allXts))

rets <- NULL
for(i in 1:nrow(allXts)){
	j <- numCols + as.numeric(maxXts[i])
	rets <- rbind(rets, c(toString(index(allXts[i,])), as.numeric(allXts[i, j])))
}

retXts <- merge(xts(as.numeric(rets[,2]), as.Date(rets[,1])), allXts[,ncol(allXts)])
names(retXts) <- c('SR', mktCapIndexNames)
Common.PlotCumReturns(retXts, "Sector Rotation", "4/1")
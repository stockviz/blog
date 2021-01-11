source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.r")
source("D:/StockViz/public/blog/common/misc.r")

reportPath<-"."

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('reshape2')
library('tidyverse')

pdf(NULL)
options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

lb <- 3 #months look-back

startDate <- as.Date("2004-01-01")
endDate <- as.Date("2020-12-31")

eqIndex <- "NIFTY 50 TR"
bndIndex <- "NIFTY GS 10YR"

pDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", eqIndex, startDate, endDate))
eqXts <- xts(pDf[,1], pDf[,2])

pDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", bndIndex, startDate, endDate))
bndXts <- xts(pDf[,1], pDf[,2])

pDf <- sqlQuery(lcon, sprintf("select tri, time_stamp from INDEX_CCIL_TENOR where index_name = '0_5' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
rfXts <- xts(pDf[,1], pDf[,2])

monthlies <- merge(Common.NormalizeMonthlyDates(monthlyReturn(eqXts)), Common.NormalizeMonthlyDates(monthlyReturn(bndXts)), Common.NormalizeMonthlyDates(monthlyReturn(rfXts))) #1,2,3
names(monthlies) <- c(eqIndex, bndIndex, 'RF')

monthlies <- merge(monthlies, rollapply(monthlies[,1], lb, Return.cumulative), rollapply(monthlies[,2], lb, Return.cumulative)) #4,5
monthlies <- merge(monthlies, stats::lag(monthlies[,1], -1), stats::lag(monthlies[,2], -1), stats::lag(monthlies[,3], -1)) #6, 7, 8

# a) E past is positive and B past is positive: Buy equity
# b) E past is negative and B past is negative: Sell equity
# c) E past is negative and B past is positive: Buy bonds
# d) E past is positive and B past is negative: Sell bonds

monthlies <- merge(monthlies, ifelse(monthlies[,4] > 0 & monthlies[,5] > 0, monthlies[,6],
							  ifelse(monthlies[,4] < 0 & monthlies[,5] > 0, monthlies[,7], monthlies[,8])),

							  ifelse(monthlies[,4] > 0 & monthlies[,5] > 0, monthlies[,6],
							  ifelse(monthlies[,4] < 0 & monthlies[,5] < 0, -monthlies[,6],
							  ifelse(monthlies[,4] < 0 & monthlies[,5] > 0, monthlies[,7], 
							  ifelse(monthlies[,4] > 0 & monthlies[,5] < 0, -monthlies[,7], monthlies[,8]))))) #9, 10
	
toPlot <- na.omit(monthlies[, c(9, 10, 6, 7, 8)])
names(toPlot) <- c('CATSM-LO', 'CATSM-LS', eqIndex, bndIndex, 'RF')

Common.PlotCumReturns(toPlot, 'Cross-Asset Time-Series Momentum', sprintf("%d-month lookback", lb), sprintf("%s/cumulative.%d-lb.png", reportPath, lb))

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('ggrepel')
library('patchwork')
library('viridis')

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/msci.R")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDt <- as.Date("2005-04-01")
endDt <- as.Date("2023-04-30")

msciIndices <- c("INDIA", "INDIA QUALITY", "INDIA MOMENTUM")
nseIndices <- c("NIFTY 200 TR", "NIFTY200 QUALITY 30 TR", "NIFTY200 MOMENTUM 30 TR") 
fredUsdInr <- "DEXINUS"

#### us indices
msciXts <- NULL
for(mi in msciIndices){
	indexId <- sqlQuery(lconUs2, sprintf("select index_code from MSCI_META where index_name='%s'", mi))[[1]]
	msciXts <- merge.xts(msciXts, Common.DownloadMsci(indexId, msciType='G', startDt, endDt))
}
names(msciXts) <- msciIndices
msciXts <- na.trim(msciXts, sides='left')
msciXts <- na.trim(msciXts, sides='right')
msciXts <- na.locf(msciXts)

fredId <- sqlQuery(lconUs, sprintf("select id from FRED_SERIES where series_id='%s'", fredUsdInr))[[1]]
fredDf <- sqlQuery(lconUs, sprintf("select val, time_stamp from FRED_OBSERVATION where series_id=%d and time_stamp >= '%s'", fredId, startDt))
fredUsdXts <- xts(fredDf$val, fredDf$time_stamp)

msciXts <- na.omit(merge(msciXts, fredUsdXts))

msciInr <- apply(msciXts[, 1:(ncol(msciXts)-1)], 2, function(X) coredata(msciXts[, ncol(msciXts)] * X))
msciInrXts <- xts(msciInr, index(msciXts))

#### indian indices

iXts <- NULL
for(iName in nseIndices){
	iDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", iName, startDt, endDt))
	iXts <- merge.xts(iXts, xts(iDf[,2], iDf[,1]))
}

names(iXts) <- nseIndices
miXts <- merge(iXts, msciInrXts)
miXts <- na.omit(merge(na.locf(miXts[, 1:ncol(iXts)]), msciInrXts))

miRets <- na.omit(lag(diff(stats::lag(miXts, -1))/miXts, 1))

for(i in 1:length(msciIndices)){
	toPlot <- miRets[, c(i, i+length(msciIndices))]
	Common.PlotCumReturns(toPlot, "Index Performance", "NSE vs. MSCI", sprintf("%s/%s.png", reportPath, paste0(names(toPlot), collapse="-")))
}

msciRets <- na.omit(lag(diff(stats::lag(msciXts, -1))/msciXts, 1))
toPlot <- msciRets[, 1:length(msciIndices)]
Common.PlotCumReturns(toPlot, "MSCI Index Performance", "(USD)", sprintf("%s/%s.png", reportPath, paste0(names(toPlot), collapse="-")))


toPlot <- miRets[, 1:length(msciIndices)]
Common.PlotCumReturns(toPlot, "NSE Index Performance", "(INR)", sprintf("%s/%s.png", reportPath, paste0(names(toPlot), collapse="-")))








source("D:/stockviz/r/config.r")
reportPath <- "D:/StockViz/public/blog/fft"
library('RODBC')
library('tidyverse')
library('reshape2')
library('ggthemes')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('gsignal')

source("D:/StockViz/public/blog/common/misc.R")

options(stringsAsFactors = FALSE)
options("scipen"=100)
pdf(NULL)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

niftyDf <- sqlQuery(lcon, "select time_stamp, px_close from bhav_index where index_name = 'NIFTY 50' and time_stamp >= '2000-01-01'")
niftyXts <- xts(niftyDf[,2], niftyDf[,1])

dretXts <- dailyReturn(niftyXts)
wretXts <- weeklyReturn(niftyXts)
mretXts <- monthlyReturn(niftyXts)

dretXts <- dretXts[-1]
wretXts <- wretXts[-1]
mretXts <- mretXts[-1]


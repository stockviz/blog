library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
uscon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "stockvizus2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs<-odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)

TZ <- "Asia/Calcutta"
startDate<-'2019-03-01'
endDate<-'2026-06-10'

schemeCode <- 122639

refIndex<-dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse where ticker = 'MID150BEES' and date_stamp >= $1 and date_stamp <= $2", params=list(startDate, endDate))
refXts<-xts(refIndex[,2], as.Date(refIndex[,1], tz=TZ))

refEtf<-sqlQuery(uscon, sprintf("select TIME_STAMP, C FROM BHAV_EQ_TD WHERE SYMBOL='SPY' and TIME_STAMP >='%s' and TIME_STAMP <='%s'", startDate, endDate))
etfXts<-xts(refEtf[,2], as.Date(refEtf[,1], tz=TZ))

usdinr<-sqlQuery(lconUs, sprintf("select VAL, TIME_STAMP from FRED_OBSERVATION where SERIES_ID=-2147478748 and TIME_STAMP >='%s' and TIME_STAMP <='%s'", startDate, endDate))
usdXts<-xts(usdinr$VAL, as.Date(usdinr$TIME_STAMP, tz=TZ))

mfNav<-sqlQuery(lcon, sprintf("select AS_OF, NAV from MF_NAV_HISTORY where scheme_code=%d and AS_OF >= '%s' and AS_OF <='%s'", schemeCode, startDate, endDate))
mfXts<-xts(mfNav[,2], as.Date(mfNav[,1], tz=TZ))

allXts<-merge(refXts, mfXts, etfXts, usdXts)

allXts<-na.locf(allXts)

allRets<-merge(monthlyReturn(allXts[,1]), monthlyReturn(allXts[,2]), monthlyReturn(allXts[,3]*allXts[,4]))
names(allRets)<-c('MID150BEES', 'MF', 'SPY_INR')

allRets$ETF_MIX<-allRets$MID150BEES*0.65 + allRets$SPY_INR*0.35

toPlot <- allRets[, c('MF', 'ETF_MIX', 'MID150BEES', 'SPY_INR')]
sharpe <- SharpeRatio.annualized(toPlot)

Common.PlotCumReturns(toPlot, 
  "Parag Parikh Flexi Cap Fund (D) vs. 65/35 MID150BEES/SPY", 
  sprintf("SR: %s", paste(round(sharpe,2), collapse="/")),
  sprintf("%s/PPFAS.vs.ETFs.cumulative.png", reportPath))





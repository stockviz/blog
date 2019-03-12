library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')

source("D:/stockviz/r/config.r")
reportPath <- "."
source("D:/StockViz/public/blog/common/plots.R")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
uscon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "stockvizus2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs<-odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

TZ <- "Asia/Calcutta"
startDate<-'2013-06-01'
endDate<-'2019-02-28'

schemeCodes<-c(122640, 122639)
schemeTypes<-c('Reg', 'Dir')

plotReturns<-function(schemeCode, schemeType){
	refIndex<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE FROM PX_HISTORY WHERE SYMBOL='M100' and TIME_STAMP >='%s' and TIME_STAMP <='%s'", startDate, endDate))
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
	names(allRets)<-c('M100', 'MF', 'SPY_INR')

	allRets$ETF_MIX<-allRets$M100*0.65 + allRets$SPY_INR*0.35

	Common.PlotCumReturns(allRets[, c('MF', 'ETF_MIX', 'M100', 'SPY_INR')], sprintf("PPFAS Long Term Value Fund (%s) vs. 65/35 M100/SPY", schemeType), sprintf("%s/PPFAS.%s.vs.ETFs.cumulative.png", reportPath, schemeType))
	print(round(100*apply(allRets[, c('MF', 'ETF_MIX', 'M100', 'SPY_INR')], 2, Return.annualized), 2))
}

plotReturns(schemeCodes[1], schemeTypes[1])
plotReturns(schemeCodes[2], schemeTypes[2])
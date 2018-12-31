library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')


options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date('2018-10-01')
endDate<-as.Date(sqlQuery(lconUs2, "select max(time_stamp) from BHAV_EQ_IEX where symbol='mtum'")[[1]])

momentumDf<-sqlQuery(lcon, sprintf("select symbol, fund from etf_meta where fund like '%%momentum%%' and inception_date <= '%s'", startDate))
momentumDf$AUM_ST<-NA
momentumDf$AUM_ED<-NA

pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

for(i in 1:nrow(momentumDf)){
	symbol<-toString(momentumDf$symbol[i])
	
	res <- dbSendQuery(pgCon, "SELECT aum FROM etf_meta WHERE symbol=$1 AND time_stamp >= $2 AND time_stamp <= $3 order by time_stamp", list(symbol, startDate, endDate))
	inrAv <- dbFetch(res)
	dbClearResult(res)

	momentumDf$AUM_ST[i]<-as.numeric(first(inrAv$aum))
	momentumDf$AUM_ED[i]<-as.numeric(last(inrAv$aum))
}

dbDisconnect(pgCon)

momentumDf$PX_ST<-NA
momentumDf$PX_ED<-NA

for(i in 1:nrow(momentumDf)){
	symbol<-toString(momentumDf$symbol[i])
	stPx<-as.numeric(sqlQuery(lconUs2, sprintf("select C from BHAV_EQ_IEX where symbol='%s' and time_stamp='%s'", symbol, startDate))$C[1])
	edPx<-as.numeric(sqlQuery(lconUs2, sprintf("select C from BHAV_EQ_IEX where symbol='%s' and time_stamp='%s'", symbol, endDate))$C[1])
	
	momentumDf$PX_ST[i]<-stPx
	momentumDf$PX_ED[i]<-edPx
}

momentumDf<-na.omit(momentumDf)

momentumDf$AUM_CHG<-momentumDf$AUM_ED-momentumDf$AUM_ST

momentumDf$PX_CHG_PCT<-momentumDf$PX_ED/momentumDf$PX_ST-1

momentumDf$AUM_CHG_PX<-momentumDf$PX_CHG_PCT*momentumDf$AUM_ST
momentumDf$OUTFLOW<-momentumDf$AUM_CHG-momentumDf$AUM_CHG_PX

momentumDf$AUM_CHG_PCT<-momentumDf$AUM_ED/momentumDf$AUM_ST-1
momentumDf$OUTFLOW_PCT<-momentumDf$OUTFLOW/momentumDf$AUM_ST

write.csv(momentumDf, sprintf("%s/momentum-aum.csv", reportPath))
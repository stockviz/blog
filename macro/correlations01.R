library('RODBC')
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
library('dplyr')
library('RPostgres')

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_181')
library('Deducer')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
#reportPath <- "D:/StockViz/report"

source("d:/stockviz/r/config.r")

startDate<-as.Date("2004-01-01") #start of the small cap index
endDate<-as.Date("2018-09-30")

args = commandArgs(TRUE)
asset1Name <- args[1]
if(!is.na(args[2])){
	startDate<-as.Date(args[2])
}
if(!is.na(args[3])){
	endDate<-as.Date(args[3])
}

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs<-odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

#asset1Name<-"NIFTY MIDCAP 100"
asset1Px<-sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", asset1Name, startDate, endDate))
asset1Xts<-xts(asset1Px[,2], as.Date(asset1Px[,1], tz=TZ))

usdinr<-sqlQuery(lconUs, sprintf("select VAL, TIME_STAMP from FRED_OBSERVATION where SERIES_ID=-2147478748 and TIME_STAMP >='%s' and TIME_STAMP <= '%s'", startDate, endDate))
usdinrXts<-xts(usdinr$VAL, as.Date(usdinr$TIME_STAMP, tz=TZ))

pgCon <- dbConnect(RPostgres::Postgres(), host='windows', user=ldbuser2, password=ldbpassword2, dbname='StockVizBeka', sslmode='allow')
res <- dbSendQuery(pgCon, "SELECT time_stamp, px_close FROM av_fx_usd_daily_ts WHERE curr_code='INR' AND time_stamp >= $1 AND time_stamp <= $2", list(startDate, endDate))
inrAv <- dbFetch(res)
dbClearResult(res)
dbDisconnect(pgCon)

if(nrow(inrAv) > 0){
	inrAvXts<-merge(usdinrXts, xts(inrAv[,2], as.Date(inrAv[,1])))
	inrAvXts[is.na(inrAvXts[,1]),1]<-inrAvXts[is.na(inrAvXts[,1]),2]
} else {
	inrAvXts<-usdinrXts
}

oil<-sqlQuery(lconUs, sprintf("select VAL, TIME_STAMP from FRED_OBSERVATION where SERIES_ID=-2147252260 and TIME_STAMP >='%s' and TIME_STAMP <= '%s'", startDate, endDate))
oilXts<-xts(oil$VAL, as.Date(oil$TIME_STAMP, tz=TZ))

allXts<-merge(asset1Xts, inrAvXts[,1], oilXts)
names(allXts)<-c('INDEX', 'INR', 'OIL')
allXts$INR<-na.locf(allXts$INR)
allXts$OIL<-na.locf(allXts$OIL)
allXts<-na.omit(allXts)

retXts<-merge(weeklyReturn(allXts$INDEX), weeklyReturn(allXts$INR), weeklyReturn(allXts$OIL))
names(retXts)<-c('INDEX', 'INR', 'OIL')

pdf(NULL)
#retDf<-data.frame(retXts)
#p1<-ggcorplot(cor.matrix(retDf), retDf, main=sprintf('Coincident %s [%s:%s]', asset1Name, startDate, endDate))
#tt2<-arrangeGrob(p1, ncol=1, bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
#ggsave(sprintf('%s/%s.INR.OIL.coincident.%s.png', reportPath, asset1Name, startDate), tt2, width=9, height=8, units='in')

############################

retDf<-data.frame(retXts)
retDf$INDEX<-stats::lag(retDf$INDEX, -1)
retDf<-na.omit(retDf)

p1<-ggcorplot(cor.matrix(retDf), retDf, main=sprintf('Lag -1 %s [%s:%s]', asset1Name, startDate, endDate))
tt2<-arrangeGrob(p1, ncol=1, bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
ggsave(sprintf('%s/%s.INR.OIL.lag-1.%s.%s.png', reportPath, asset1Name, startDate, endDate), tt2, width=9, height=8, units='in')

q()
png(sprintf('%s/%s.INR.lag-1.lm.%s.%s.png', reportPath, asset1Name, startDate, endDate), width=10, height=10, units='in', res=300)
layout(matrix(1:4, ncol = 2))
par(family='Segoe UI')
plot(lm("INDEX~INR", retDf), main=sprintf('Lag -1 %s vs. INR [%s:%s]', asset1Name, startDate, endDate))
layout(1)
dev.off()


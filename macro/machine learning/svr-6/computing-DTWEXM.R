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
#library('ggrepel')
#library('dplyr')
library('RPostgres')
library('Metrics')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("d:/stockviz/r/config.r")

lconUs <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
startDate<-as.Date("2015-01-01")
endDate<-as.Date("2018-10-31")
index1<-'DTWEXM'

usdDf1<-sqlQuery(lconUs, sprintf("select val, time_stamp from FRED_OBSERVATION 
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and series_id=(select id from FRED_SERIES where series_id='%s')", startDate, endDate, index1))
usdXts1<-xts(usdDf1$val, as.Date(usdDf1$time_stamp))
usd1Weekly<-to.weekly(usdXts1, indexAt='lastof')[,4]
usd1WeeklyRets<-usd1Weekly/stats::lag(usd1Weekly, 1)-1

#https://www.federalreserve.gov/releases/h10/weights/default.htm
tradeWeightsDf<-data.frame(CUR=c('EUR', 'CAD', 'JPY', 'GBP', 'CHF', 'AUD', 'SEK'), WEIGHTS=c(17.199, 11.89, 6.524, 3.595, 2.182, 1.136, 0.686))
tradeWeightsDf$SCALED<-tradeWeightsDf$WEIGHTS/sum(tradeWeightsDf$WEIGHTS)

curXts<-xts()
pgCon <- dbConnect(RPostgres::Postgres(), host='windows', user=ldbuser2, password=ldbpassword2, dbname='StockVizBeka', sslmode='allow')

for(i in 1:nrow(tradeWeightsDf)){
	curCode<-toString(tradeWeightsDf$CUR[i])
	res <- dbSendQuery(pgCon, "SELECT time_stamp, px_close FROM av_fx_usd_daily_ts WHERE curr_code=$1 AND time_stamp >= $2 AND time_stamp <= $3", list(curCode, startDate, endDate))
	inrAv <- dbFetch(res)
	dbClearResult(res)
	
	curXts<-merge(curXts, xts(inrAv[,2], as.Date(inrAv[,1])))
}

dbDisconnect(pgCon)

names(curXts)<-tradeWeightsDf$CUR

curWeekly<- lapply(curXts, function(X) to.weekly(X, indexAt='lastof')[,4])
curWeeklyRets<-lapply(curWeekly, function(X) X/stats::lag(X, 1)-1)
curWeeklyRetXts<-na.omit(do.call('merge', curWeeklyRets))
names(curWeeklyRetXts)<-tradeWeightsDf$CUR

scaledWeeklyReturns<-apply(curWeeklyRetXts, 1, function(X) sum(X*tradeWeightsDf$SCALED))
swrXts<-xts(scaledWeeklyReturns, as.Date(names(scaledWeeklyReturns)))

mwXts<-merge(usd1WeeklyRets, swrXts)
mwXts[,1]<-na.locf(mwXts[,1])
actPretXts<-na.omit(mwXts)
names(actPretXts)<-c('ACTUAL', 'COMPUTED')

meanSquaredError<-mse(actPretXts[,1], actPretXts[,2])

plotDf<-data.frame(actPretXts)
plotDf$T<-as.Date(index(actPretXts))
plotStart<-min(index(actPretXts))
plotEnd<-max(index(actPretXts))
plotDf$COLOR<-ifelse((plotDf$ACTUAL > 0 & plotDf$COMPUTED > 0) | (plotDf$ACTUAL < 0 & plotDf$COMPUTED < 0), 'grey', 'black')
	
pdf(NULL)	
ggplot(plotDf, aes(x=ACTUAL, y=COMPUTED)) + 
	theme_economist() +
	geom_point(color=plotDf$COLOR) +
	labs(x = "actual", y="computed", fill="", color="", title=sprintf("%s", index1), subtitle=sprintf("Actual vs. Computed [%s:%s] mse = %f", plotStart, plotEnd, meanSquaredError)) +
	annotate("text", x=max(plotDf$ACTUAL), y=min(plotDf$COMPUTED), label = "@StockViz", hjust=1, vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.6)
ggsave(sprintf("%s/%s.actual.vs.computed.png", reportPath, index1), width=12, height=6, units="in")

plotDf<-data.frame(actPretXts[,1], ape(actPretXts[,1], actPretXts[,2]))
names(plotDf)<-c('ACTUAL', 'APE')

ggplot(plotDf, aes(x=ACTUAL,y=APE)) + 
	theme_economist() +
	geom_point() +
	labs(x = "actual", y="ape", fill="", color="", title=sprintf("%s", index1), subtitle=sprintf("Absolute Percent Error [%s:%s]", plotStart, plotEnd)) +
	annotate("text", x=max(plotDf$ACTUAL), y=min(plotDf$APE), label = "@StockViz", hjust=1, vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.6)
ggsave(sprintf("%s/%s.absolute.pct.error.png", reportPath, index1), width=12, height=6, units="in")

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('schoRsch')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')
library('ggrepel')
#library('dplyr')
library('RPostgres')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
#reportPath <- "D:/StockViz/report"

source("d:/stockviz/r/config.r")

plotCumReturns<-function(toPlot, chartTitle, fileName){
	pdf(NULL)
	png(fileName, width=1400, height=800, bg="white")
	layout(matrix(c(1, 2)), heights = c(2, 1.3), widths = 1)
	par(mar = c(0, 4, 4, 2), family='Segoe UI', bty="n")
	plot_object <-chart.CumReturns(toPlot, cex.legend=1, main=NA, xaxis = FALSE, legend.loc = "topleft", begin = c("first", "axis"), geometric = TRUE) 
	print(plot_object)
	mtext("Cumulative Return", side=2, line=1)
	title(main=chartTitle, family='Segoe UI') 
	mtext(paste(sprintf("%.2f%%", 100*apply(toPlot, 2, Return.cumulative)), collapse=" / "), cex=0.8)
	par(mar = c(5, 4, 0, 2))
	plot_object <-chart.Drawdown(toPlot, main = NA, ylab = "Drawdown", event.labels = NULL, ylog = FALSE, geometric = TRUE, bty="n")
	print(plot_object)
	mtext("Drawdown", side=2, line=1)
	mtext("@StockViz", side=4, col='grey')
	dev.off()
}

btest<-function(aXts, indexName, plotDateRange = NULL){
	retList<-do.call('merge', lapply(binIds, function(X) ifelse(aXts$TILE ==X, aXts$R_LAG_1, 0)))
	names(retList)<-sapply(binIds, function(X) sprintf("L%d", X))
	retList<-merge(retList, aXts$R_LAG_1)
	
	toPlot<-retList
	if(!is.null(plotDateRange)){
		toPlot<-retList[plotDateRange,]
	}
	
	plotStart<-as.Date(index(first(toPlot)))
	plotEnd<-as.Date(index(last(toPlot)))
	plotCumReturns(toPlot, sprintf("%s Long-only Returns [%s:%s]", indexName, plotStart, plotEnd), sprintf("%s/%s.vix.cumulative.%s.%s.png", reportPath, indexName, plotStart, plotEnd))
}

numBins<-5
binName<-'quintiles'
binIds<-1:5

rcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", dbserver, dbname, dbuser, dbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("1990-01-01")
endDate<-as.Date("2018-10-31")
lookback<-1000 #days

#implied volatility

df<-sqlQuery(lconUs2, sprintf("select time_stamp, ac from BHAV_YAHOO where symbol='^VIX' and time_stamp >= '%s' and time_stamp <= '%s' and o > 0", startDate, endDate))
spyVixXts<-xts(df[,-1], as.Date(df[,1]))

pgCon <- dbConnect(RPostgres::Postgres(), host='windows', user=ldbuser2, password=ldbpassword2, dbname='StockVizBeka', sslmode='allow')
res <- dbSendQuery(pgCon, "SELECT time_stamp, val->>'Close' as close FROM quandl_ts_json WHERE id=8470456 AND time_stamp >= $1 AND time_stamp <= $2", list(startDate, endDate))
df <- dbFetch(res)
dbClearResult(res)
dbDisconnect(pgCon)
nikkeiVixXts<-xts(df[,-1], as.Date(df[,1]))

df<-sqlQuery(rcon, sprintf("select time_stamp, px_close from VIX_HISTORY where time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
n50VixXts<-xts(df[,-1], as.Date(df[,1]))

#indices

df<-sqlQuery(lconUs2, sprintf("select time_stamp, C from BHAV_YAHOO where symbol='^GSPC' and time_stamp >= '%s' and time_stamp <= '%s' and c > 0", startDate, endDate))
spyXts<-xts(df[,-1], as.Date(df[,1]))
spyDret<-dailyReturn(spyXts)

df<-sqlQuery(lconUs2, sprintf("select time_stamp, C from BHAV_YAHOO where symbol='^N225' and time_stamp >= '%s' and time_stamp <= '%s' and c > 0", startDate, endDate))
nikkeiXts<-xts(df[,-1], as.Date(df[,1]))
nkDret<-dailyReturn(nikkeiXts)

df<-sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='nifty 50' and time_stamp >= '%s' and time_stamp <= '%s' and px_close > 0", startDate, endDate))
niftyXts<-xts(df[,-1], as.Date(df[,1]))
n50Dret<-dailyReturn(niftyXts)

#rolling tiles

spyVixXts<-merge(spyVixXts, rollapply(spyVixXts, lookback, function(X) last(ntiles(data.frame(X), 1, bins=numBins))))
nikkeiVixXts<-merge(nikkeiVixXts, rollapply(nikkeiVixXts, lookback, function(X) last(ntiles(data.frame(X), 1, bins=numBins))))
n50VixXts<-merge(n50VixXts, rollapply(n50VixXts, lookback, function(X) last(ntiles(data.frame(X), 1, bins=numBins))))

spy1<-na.omit(merge(spyVixXts, stats::lag(spyDret,-1)))
nik1<-na.omit(merge(nikkeiVixXts, stats::lag(nkDret,-1)))
n501<-na.omit(merge(n50VixXts, stats::lag(n50Dret,-1)))

cNames<-c('VIX', 'TILE', 'R_LAG_1')
names(spy1)<-cNames
names(nik1)<-cNames
names(n501)<-cNames

plotRange<-"2010/"
btest(spy1, "SP500")
btest(spy1, "SP500", plotRange)
btest(nik1, "NK225", plotRange)
btest(n501, "NIFTY50", plotRange)
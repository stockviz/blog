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

args = commandArgs(TRUE)
numBins<-as.numeric(args[1])
binName<-sprintf("%d-tiles", numBins)
if(numBins == 5){
	binName<-'quintiles'
} else if(numBins == 10){
	binName<-'deciles'
}

plotBox<-function(pXts, indexName){
	plotStart<-as.Date(index(first(pXts)))
	plotEnd<-as.Date(index(last(pXts)))
	
	df<-data.frame(pXts)
	df<-df[order(df$Q),]
	df<-melt(df, id='Q')
	df$Q<-factor(df$Q, levels=unique(df$Q)) 
	
	pdf(NULL)
	ggplot(df, aes(x=Q, y=value, fill=variable)) +
		theme_economist() +
		geom_boxplot(outlier.shape = 1) +
		labs(x = "", y="", fill="", color="", title=sprintf("Distribution of %s Index Returns vs. VIX %s", indexName, binName), subtitle=sprintf("Subsequent n-day returns [%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=1, y=max(pXts[,2:ncol(pXts)]), label = "@StockViz", hjust=0, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.6)
	
	ggsave(sprintf("%s/%s.vix.%d.tile.boxplot.%s.%s.png",reportPath, indexName, numBins, plotStart, plotEnd), width=12, height=6, units="in")	
}

rcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", dbserver, dbname, dbuser, dbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("1990-01-01")
endDate<-as.Date("2018-10-31")
rPeriods<-c(5, 10, 15, 20) #days forward returns
indexNames<-c('SP500', 'NK225', 'N50')
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

#rolling tiles

spyVixXts<-merge(spyVixXts, rollapply(spyVixXts, lookback, function(X) last(ntiles(data.frame(X), 1, bins=numBins))))
nikkeiVixXts<-merge(nikkeiVixXts, rollapply(nikkeiVixXts, lookback, function(X) last(ntiles(data.frame(X), 1, bins=numBins))))
n50VixXts<-merge(n50VixXts, rollapply(n50VixXts, lookback, function(X) last(ntiles(data.frame(X), 1, bins=numBins))))

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

#analyze
aNames<-c('Q', 'D5', 'D10', 'D15', 'D20')

spyRets<-na.omit(merge(spyVixXts[,2], 100*do.call('merge', lapply(rPeriods, function(X) stats::lag(rollapply(spyDret, X, Return.cumulative), -X)))))
nkRets<-na.omit(merge(nikkeiVixXts[,2], 100*do.call('merge', lapply(rPeriods, function(X) stats::lag(rollapply(nkDret, X, Return.cumulative), -X)))))
n50Rets<-na.omit(merge(n50VixXts[,2], 100*do.call('merge', lapply(rPeriods, function(X) stats::lag(rollapply(n50Dret, X, Return.cumulative), -X)))))

names(spyRets)<-aNames
names(nkRets)<-aNames
names(n50Rets)<-aNames

plotBox(spyRets, 'SP500')
plotBox(spyRets["2010/",], 'SP500')
plotBox(nkRets, 'NK225')
plotBox(n50Rets, 'NIFTY50')

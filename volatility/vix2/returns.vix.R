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

numBins<-10
binName<-'deciles'
binIds<-1:10
rPeriodsVix<-c(1,2,5) #days returns
rPeriodsIndex<-c(5, 10, 15, 20) #days forward returns

plotBox<-function(vixTiles, indexRets, vday, indexName){
	pXts<-na.omit(merge(vixTiles, indexRets))
	qName<-toString(names(vixTiles)[1])
	
	plotStart<-as.Date(index(first(pXts)))
	plotEnd<-as.Date(index(last(pXts)))
	
	dfr<-data.frame(pXts)
	dfr<-dfr[order(dfr[,qName]),]
	meltedDfr<-melt(dfr, id=qName)
	meltedDfr[,qName]<-factor(meltedDfr[,qName], levels=unique(meltedDfr[,qName])) 
	
	print(head(meltedDfr, 20))
	
	pdf(NULL)
	ggplot(meltedDfr, aes_string(x=qName)) +
		theme_economist() +
		geom_boxplot(aes(y=value, fill=variable), outlier.shape = 1) +
		labs(x = "", y="", fill="", color="", title=sprintf("Distribution of %s Index Returns vs. %d-day VIX Return %s", indexName, vday, binName), subtitle=sprintf("Subsequent n-day returns [%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=1, y=max(pXts[,2:ncol(pXts)]), label = "@StockViz", hjust=0, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.6)
	
	ggsave(sprintf("%s/%s.vix.%d.tile.boxplot.%d.%s.%s.%s.png",reportPath, indexName, numBins, vday, qName, plotStart, plotEnd), width=12, height=6, units="in")	
}

rcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", dbserver, dbname, dbuser, dbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("1990-01-01")
endDate<-as.Date("2018-10-31")
lookback<-1000 #days

#DOWNLOAD VIX

dfr<-sqlQuery(lconUs2, sprintf("select time_stamp, ac from BHAV_YAHOO where symbol='^VIX' and time_stamp >= '%s' and time_stamp <= '%s' and o > 0", startDate, endDate))
spyVixXts<-xts(dfr[,-1], as.Date(dfr[,1]))
spyVixDretXts<-dailyReturn(spyVixXts)

pgCon <- dbConnect(RPostgres::Postgres(), host='windows', user=ldbuser2, password=ldbpassword2, dbname='StockVizBeka', sslmode='allow')
res <- dbSendQuery(pgCon, "SELECT time_stamp, val->>'Close' as close FROM quandl_ts_json WHERE id=8470456 AND time_stamp >= $1 AND time_stamp <= $2", list(startDate, endDate))
dfr <- dbFetch(res)
dbClearResult(res)
dbDisconnect(pgCon)
nikVixXts<-xts(as.numeric(dfr[,-1]), as.Date(dfr[,1]))
nikVixDretXts<-dailyReturn(nikVixXts)

dfr<-sqlQuery(rcon, sprintf("select time_stamp, px_close from VIX_HISTORY where time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
n50VixXts<-xts(dfr[,-1], as.Date(dfr[,1]))
n50VixDretXts<-dailyReturn(n50VixXts)

print("finished downloading vix")

spyVixRets<-na.omit(do.call('merge', lapply(rPeriodsVix, function(X) rollapply(spyVixDretXts, X, Return.cumulative))))
nikVixRets<-na.omit(do.call('merge', lapply(rPeriodsVix, function(X) rollapply(nikVixDretXts, X, Return.cumulative))))
n50VixRets<-na.omit(do.call('merge', lapply(rPeriodsVix, function(X) rollapply(n50VixDretXts, X, Return.cumulative))))

#rolling tiles
spyTiles<-rollapply(spyVixRets, lookback, function(X) last(ntiles(data.frame(X), 1, bins=numBins)))
nikTiles<-rollapply(nikVixRets, lookback, function(X) last(ntiles(data.frame(X), 1, bins=numBins)))
n50Tiles<-rollapply(n50VixRets, lookback, function(X) last(ntiles(data.frame(X), 1, bins=numBins)))

tileNames<-sapply(rPeriodsVix, function(X) sprintf("Q%d", X))
names(spyTiles)<-tileNames
names(nikTiles)<-tileNames
names(n50Tiles)<-tileNames

#DOWNLOAD indices

dfr<-sqlQuery(lconUs2, sprintf("select time_stamp, C from BHAV_YAHOO where symbol='^GSPC' and time_stamp >= '%s' and time_stamp <= '%s' and c > 0", startDate, endDate))
spyXts<-xts(dfr[,-1], as.Date(dfr[,1]))
spyDretXts<-dailyReturn(spyXts)

dfr<-sqlQuery(lconUs2, sprintf("select time_stamp, C from BHAV_YAHOO where symbol='^N225' and time_stamp >= '%s' and time_stamp <= '%s' and c > 0", startDate, endDate))
nikkeiXts<-xts(dfr[,-1], as.Date(dfr[,1]))
nikDretXts<-dailyReturn(nikkeiXts)

dfr<-sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='nifty 50' and time_stamp >= '%s' and time_stamp <= '%s' and px_close > 0", startDate, endDate))
niftyXts<-xts(dfr[,-1], as.Date(dfr[,1]))
n50DretXts<-dailyReturn(niftyXts)

print("finished downloading indices")

######## calculate returns

spyRets<-na.omit(100*do.call('merge', lapply(rPeriodsIndex, function(X) stats::lag(rollapply(spyDretXts, X, Return.cumulative), -X))))
nikRets<-na.omit(100*do.call('merge', lapply(rPeriodsIndex, function(X) stats::lag(rollapply(nikDretXts, X, Return.cumulative), -X))))
n50Rets<-na.omit(100*do.call('merge', lapply(rPeriodsIndex, function(X) stats::lag(rollapply(n50DretXts, X, Return.cumulative), -X))))

retNames<-sapply(rPeriodsIndex, function(X) sprintf("D%d", X))
names(spyRets)<-retNames
names(nikRets)<-retNames
names(n50Rets)<-retNames

for(i in 1:length(rPeriodsVix)){
	plotBox(spyTiles[,i], spyRets, rPeriodsVix[i], "SP500")
}

for(i in 1:length(rPeriodsVix)){
	plotBox(nikTiles[,i], nikRets, rPeriodsVix[i], "NK255")
}

for(i in 1:length(rPeriodsVix)){
	plotBox(n50Tiles[,i], n50Rets, rPeriodsVix[i], "NIFTY50")
}

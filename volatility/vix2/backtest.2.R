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

plotSeries<-function(pXts, indexName){
	plotStart<-as.Date(index(first(pXts)))
	plotEnd<-as.Date(index(last(pXts)))
	xAxisTicks<-seq(from=plotStart, to=plotEnd, length.out=10)
	
	dfr<-data.frame(pXts)
	dfr$T<-as.Date(index(pXts[,1]))
	dfr$TILE_STR<-as.character(dfr$TILE)
	
	pdf(NULL)
	ggplot(dfr, aes(x=T, y=INDEX)) +
		theme_economist() +
		geom_line(aes(color=TILE_STR, group=1)) +
		scale_x_date(breaks = xAxisTicks) +
		scale_y_log10() +
		labs(x = "", y="log()", fill="", color="", title=sprintf("%s", indexName), subtitle=sprintf("rolling 1000-day VIX quintiles [%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=plotEnd, y=min(pXts$INDEX, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.6)
	
	ggsave(sprintf("%s/%s.vix.overlay.%s.%s.png",reportPath, indexName, plotStart, plotEnd), width=12, height=6, units="in")	
}


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

btest<-function(aXts){
	aXts$L5_5<-0
	aXts$L5_10<-0
	aXts$L5_20<-0
	for(i in 1:(nrow(aXts)-21)){
		if(aXts[i,1]==5){
			aXts$L5_5[i+seq(1, 5)]<-aXts[i+seq(1, 5), 2]
			aXts$L5_10[i+seq(1, 10)]<-aXts[i+seq(1, 10), 2]
			aXts$L5_20[i+seq(1, 20)]<-aXts[i+seq(1, 20), 2]
		}
	}
	
	return(aXts)
}

numBins<-5
binName<-'quintiles'
binIds<-1:5
rPeriods<-c(1,2,5) #days forward returns

rcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", dbserver, dbname, dbuser, dbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("1990-01-01")
endDate<-as.Date("2018-10-31")
lookback<-1000 #days
fallThreshold<- -0.01

#DOWNLOAD VIX

df<-sqlQuery(lconUs2, sprintf("select time_stamp, ac from BHAV_YAHOO where symbol='^VIX' and time_stamp >= '%s' and time_stamp <= '%s' and o > 0", startDate, endDate))
spyVixXts<-xts(df[,-1], as.Date(df[,1]))
spyVixDretXts<-dailyReturn(spyVixXts)

pgCon <- dbConnect(RPostgres::Postgres(), host='windows', user=ldbuser2, password=ldbpassword2, dbname='StockVizBeka', sslmode='allow')
res <- dbSendQuery(pgCon, "SELECT time_stamp, val->>'Close' as close FROM quandl_ts_json WHERE id=8470456 AND time_stamp >= $1 AND time_stamp <= $2", list(startDate, endDate))
df <- dbFetch(res)
dbClearResult(res)
dbDisconnect(pgCon)
nikkeiVixXts<-xts(as.numeric(df[,-1]), as.Date(df[,1]))
nikkeiVixDretXts<-dailyReturn(nikkeiVixXts)

df<-sqlQuery(rcon, sprintf("select time_stamp, px_close from VIX_HISTORY where time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
n50VixXts<-xts(df[,-1], as.Date(df[,1]))
n50VixDretXts<-dailyReturn(n50VixXts)

print("finished downloading vix")

#rolling tiles
spyTiles<-rollapply(spyVixXts, lookback, function(X) last(ntiles(data.frame(X), 1, bins=numBins)))
nikTiles<-rollapply(nikkeiVixXts, lookback, function(X) last(ntiles(data.frame(X), 1, bins=numBins)))
n50Tiles<-rollapply(n50VixXts, lookback, function(X) last(ntiles(data.frame(X), 1, bins=numBins)))

#DOWNLOAD indices

df<-sqlQuery(lconUs2, sprintf("select time_stamp, C from BHAV_YAHOO where symbol='^GSPC' and time_stamp >= '%s' and time_stamp <= '%s' and c > 0", startDate, endDate))
spyXts<-xts(df[,-1], as.Date(df[,1]))
spyDretXts<-dailyReturn(spyXts)

df<-sqlQuery(lconUs2, sprintf("select time_stamp, C from BHAV_YAHOO where symbol='^N225' and time_stamp >= '%s' and time_stamp <= '%s' and c > 0", startDate, endDate))
nikkeiXts<-xts(df[,-1], as.Date(df[,1]))
nikDretXts<-dailyReturn(nikkeiXts)

df<-sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='nifty 50' and time_stamp >= '%s' and time_stamp <= '%s' and px_close > 0", startDate, endDate))
niftyXts<-xts(df[,-1], as.Date(df[,1]))
n50DretXts<-dailyReturn(niftyXts)

print("finished downloading indices")

######## calculate returns

spyAnalXts<-na.omit(merge(spyTiles, spyDretXts))
names(spyAnalXts)<-c('TILE', 'RET')
spyAx<-btest(spyAnalXts)

plotCumReturns(spyAx[, c('L5_5','L5_10','L5_20')], "SP500 L5", sprintf("%s/SPY500.L5.cumulative.png", reportPath))
plotCumReturns(spyAx["2010/",c('L5_5','L5_10','L5_20')], "SP500 L5", sprintf("%s/SPY500.L5.cumulative.2010-.png", reportPath))

nikAnalXts<-na.omit(merge(nikTiles, nikDretXts))
names(nikAnalXts)<-c('TILE', 'RET')
nikAx<-btest(nikAnalXts)
plotCumReturns(nikAx[, c('L5_5','L5_10','L5_20')], "N225 L5", sprintf("%s/N225.L5.cumulative.png", reportPath))

n50AnalXts<-na.omit(merge(n50Tiles, n50DretXts))
names(n50AnalXts)<-c('TILE', 'RET')
n50Ax<-btest(n50AnalXts)
plotCumReturns(n50Ax[, c('L5_5','L5_10','L5_20')], "NIFTY 50 L5", sprintf("%s/N50.L5.cumulative.png", reportPath))

######## plot tiles over index
cNames<-c('INDEX', 'TILE')
spyViXts<-merge(spyXts, spyTiles)
names(spyViXts)<-cNames

nikViXts<-merge(nikkeiXts, nikTiles)
names(nikViXts)<-cNames

n50ViXts<-merge(niftyXts, n50Tiles)
names(n50ViXts)<-cNames

plotRange<-"2010/"
plotSeries(na.omit(spyViXts), "SP500")
plotSeries(na.omit(spyViXts[plotRange,]), "SP500")
plotSeries(na.omit(nikViXts), "NK225")
plotSeries(na.omit(n50ViXts), "NIFTY50")
######## 

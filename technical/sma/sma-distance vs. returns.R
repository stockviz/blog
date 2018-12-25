library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('schoRsch')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("1970-01-01")
endDate<-as.Date("2018-11-30")
index1<-'^GSPC'

usdDf1<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO 
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and symbol='%s'", startDate, endDate, index1))

usdXts1<-xts(usdDf1$ac, as.Date(usdDf1$time_stamp))

usdXts1<-merge(usdXts1, SMA(usdXts1[,1], 50))
usdXts1<-merge(usdXts1, SMA(usdXts1[,1], 100))
usdXts1<-merge(usdXts1, SMA(usdXts1[,1], 200))

pXts<-na.omit(usdXts1)
names(pXts)<-c('INDEX', 'S50', 'S100', 'S200')

pXts$DAILY_RET<-dailyReturn(pXts[,1])
pXts$RET_20<-100*stats::lag(rollapply(pXts$DAILY_RET, 20, Return.cumulative), -20)
pXts$RET_50<-100*stats::lag(rollapply(pXts$DAILY_RET, 50, Return.cumulative), -50)
pXts$RET_100<-100*stats::lag(rollapply(pXts$DAILY_RET, 100, Return.cumulative), -100)

pXts$D50<-100*(pXts$S50/pXts$INDEX-1)
pXts$D100<-100*(pXts$S100/pXts$INDEX-1)
pXts$D200<-100*(pXts$S200/pXts$INDEX-1)

pXts<-na.omit(pXts)
pSubset<-pXts['/2005',]

pSubset$BIN_50<-ntiles(data.frame(pSubset$D50), dv=1, bins=5)
pSubset$BIN_100<-ntiles(data.frame(pSubset$D100), dv=1, bins=5)
pSubset$BIN_200<-ntiles(data.frame(pSubset$D200), dv=1, bins=5)

plotBox<-function(pSubset, indexName, smaName){
	plotStart<-as.Date(index(first(pSubset)))
	plotEnd<-as.Date(index(last(pSubset)))
	
	binName<-toString(names(pSubset)[1])

	dfr<-data.frame(pSubset)
	dfr<-dfr[order(dfr[,binName]),]
	meltedDfr<-melt(dfr, id=binName)
	meltedDfr[,binName]<-factor(meltedDfr[,binName], levels=unique(meltedDfr[,binName])) 

	pdf(NULL)
	ggplot(meltedDfr, aes_string(x=binName)) +
		theme_economist() +
		geom_boxplot(aes(y=value, fill=variable), outlier.shape = 1) +
		labs(x = "", y="", fill="", color="", title=sprintf("Distribution of %s Index Returns vs. %s-day SMA distance quintiles", indexName, smaName), subtitle=sprintf("Subsequent n-day returns [%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=1, y=max(pSubset[,-1]), label = "@StockViz", hjust=0, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.6)

	ggsave(sprintf("%s/%s.boxplot.%s.%s.%s.png",reportPath, indexName, smaName, plotStart, plotEnd), width=12, height=6, units="in")	
}

plotBox(pSubset[, c('BIN_50', 'RET_20', 'RET_50', 'RET_100')], "SP500", "50")
plotBox(pSubset[, c('BIN_100', 'RET_20', 'RET_50', 'RET_100')], "SP500", "100")
plotBox(pSubset[, c('BIN_200', 'RET_20', 'RET_50', 'RET_100')], "SP500", "200")

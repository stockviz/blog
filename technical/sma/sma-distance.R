library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

plotTs<-function(usdXts1, indexName){
	usdXts1<-merge(usdXts1, SMA(usdXts1[,1], 50))
	usdXts1<-merge(usdXts1, SMA(usdXts1[,1], 100))
	usdXts1<-merge(usdXts1, SMA(usdXts1[,1], 200))

	pXts<-na.omit(usdXts1)
	names(pXts)<-c('INDEX', 'S50', 'S100', 'S200')

	pXts$D50<-100*(pXts$S50/pXts$INDEX-1)
	pXts$D100<-100*(pXts$S100/pXts$INDEX-1)
	pXts$D200<-100*(pXts$S200/pXts$INDEX-1)

	btDf<-data.frame(pXts[,c('D50', 'D100', 'D200')])
	btDf$T<-as.Date(index(pXts))

	meltedDf<-melt(btDf, id='T')

	plotStart<-first(index(pXts))
	plotEnd<-last(index(pXts))
	xAxisTicks<-seq(from=plotStart, to=plotEnd, length.out=10)

	pdf(NULL)
	ggplot(meltedDf, aes(x=T, y=value, color=variable)) +
		theme_economist() +
		geom_line() +
		scale_x_date(breaks=xAxisTicks) +
		labs(y='distance (%)', x='', fill='', color='', title=sprintf("%s SMA Distance", indexName), subtitle=sprintf("%s:%s", plotStart, plotEnd)) +
		annotate("text", x=plotEnd, y=0, label = "@StockViz", hjust=0.8, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.9)
	ggsave(sprintf("%s/%s.sma.distance.png", reportPath, indexName), width=12, height=6, units="in")
}
#############################

startDate<-as.Date("1970-01-01")
endDate<-as.Date("2018-11-30")
index1<-'^GSPC'

usdDf1<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO 
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and symbol='%s'", startDate, endDate, index1))

usdXts1<-xts(usdDf1$ac, as.Date(usdDf1$time_stamp))							
plotTs(usdXts1, "SP500")

startDate<-as.Date("1995-01-01")
endDate<-as.Date("2018-11-30")
index1<-'NIFTY 50'

usdDf1<-sqlQuery(lcon, sprintf("select px_close, time_stamp from BHAV_INDEX
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and index_name='%s'", startDate, endDate, index1))
							
usdXts1<-xts(usdDf1$px_close, as.Date(usdDf1$time_stamp))
plotTs(usdXts1, "NIFTY50")
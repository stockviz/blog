library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')

source("d:/stockviz/r/config.r")

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."

indices<-c('NIFTY 50', 'NIFTY MIDCAP 50') 

startDate<-as.Date("2009-01-02")
endDate<-as.Date("2019-04-15")
lb<-220*3 #3 years

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

plotRatio<-function(ratioName){
	allXts<-xts()
	for(indexName in indices){
		nDf1<-sqlQuery(lcon, sprintf("select TIME_STAMP, %s from INDEX_NSE_VALUATION where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", ratioName, indexName, startDate, endDate))
		nXts1<-xts(nDf1[,2], as.Date(nDf1[,1]))
		allXts<-merge(allXts, nXts1)
	}

	names(allXts)<-indices
	index(allXts)<-as.Date(index(allXts))

	############
	
	firstDate<-first(index(allXts))
	lastDate<-last(index(allXts))
	xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)

	ctr2Df<-data.frame(allXts)
	ctr2Df$T<-as.Date(index(allXts))

	ctr2Melt<-melt(ctr2Df, id='T')

	pdf(NULL)
	ggplot(ctr2Melt, aes(x=T, y=value, color=variable)) +
		theme_economist() +
		geom_line() +
		scale_x_date(breaks = xAxisTicks) +
		labs(x='', y=ratioName, color='', title=sprintf("%s Ratio", ratioName), subtitle=sprintf("[%s:%s]", firstDate, lastDate)) +
		annotate("text", x=lastDate, y=min(allXts, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
			
	ggsave(sprintf("%s/%s.%s.png", reportPath, paste(indices, collapse="-"), ratioName), width=16, height=8, units="in")
}	

plotRatio("PE")
plotRatio("PB")
library('RODBC')
library('quantmod')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')

source("d:/stockviz/r/config.r")

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."
indexName<-"NIFTY BANK"
startDate<-as.Date("2011-01-01")
endDate<-as.Date("2019-02-28")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

nDf<-sqlQuery(lcon, sprintf("select * from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate, endDate))
nXts<-xts(nDf[, c('PX_OPEN', 'PX_HIGH', 'PX_LOW', 'PX_CLOSE')], as.Date(nDf$TIME_STAMP))
nXts$PREV_CLOSE<-stats::lag(nXts$PX_CLOSE, 1)

plotOuts<-function(threshold, lb){
	c2oCtr<-ifelse(abs(nXts$PX_OPEN/nXts$PREV_CLOSE-1) >= threshold, 1, 0)
	c2cCtr<-ifelse(abs(dailyReturn(nXts$PX_CLOSE)) >= threshold, 1, 0)
	o2cCtr<-ifelse(abs(nXts$PX_CLOSE/nXts$PX_OPEN-1) >= threshold, 1, 0)
	h2lCtr<-ifelse(abs(nXts$PX_HIGH/nXts$PX_LOW-1) >= threshold, 1, 0)

	c2oCtr2<-rollapply(c2oCtr, lb, sum)
	c2cCtr2<-rollapply(c2cCtr, lb, sum)
	o2cCtr2<-rollapply(o2cCtr, lb, sum)
	h2lCtr2<-rollapply(h2lCtr, lb, sum)

	ctr2<-na.omit(merge(c2oCtr2, c2cCtr2, o2cCtr2, h2lCtr2))
	names(ctr2)<-c('c2o', 'c2c', 'o2c', 'h2l')

	firstDate<-first(index(ctr2))
	lastDate<-last(index(ctr2))
	xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)
		
	ctr2Df<-data.frame(ctr2)
	ctr2Df$T<-as.Date(index(ctr2))

	ctr2Melt<-melt(ctr2Df, id='T')

	pdf(NULL)
	ggplot(ctr2Melt, aes(x=T, y=value, color=variable)) +
		theme_economist() +
		geom_line() +
		scale_x_date(breaks = xAxisTicks) +
		labs(x='', y='count', color='', title=sprintf("%s", indexName), subtitle=sprintf("%d-day rolling number of abs(returns) >= %.2f%% [%s:%s]", lb, 100*threshold, firstDate, lastDate)) +
		annotate("text", x=lastDate, y=0, label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
			
	ggsave(sprintf("%s/%s.DAILY.%.2f.%d.png", reportPath, indexName, 100*threshold, lb), width=16, height=8, units="in")	
}

plotOuts(0.01, 100)
plotOuts(0.02, 100)
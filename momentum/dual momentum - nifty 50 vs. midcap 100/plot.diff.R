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
indexName1<-"NIFTY 50 TR"
indexName2<-"NIFTY MIDCAP 100 TR"
startDate<-as.Date("2003-12-31")
endDate<-as.Date("2019-02-28")
lb<-200

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

nDf1<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName1, startDate, endDate))
nXts1<-xts(nDf1$PX_CLOSE, as.Date(nDf1$TIME_STAMP))
nRet1<-dailyReturn(nXts1)

nDf2<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName2, startDate, endDate))
nXts2<-xts(nDf2$PX_CLOSE, as.Date(nDf2$TIME_STAMP))
nRet2<-dailyReturn(nXts2)

bDf<-sqlQuery(lcon, sprintf("select TIME_STAMP, TRI from INDEX_CCIL_TENOR where index_name='0_5' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
bXts<-xts(bDf$TRI, as.Date(bDf$TIME_STAMP))
bRet<-dailyReturn(bXts)

rollingReturns<-100*na.omit(merge(rollapply(nRet1, lb, Return.cumulative), rollapply(nRet2, lb, Return.cumulative), rollapply(bRet, lb, Return.cumulative)))
rollingDiff<-merge(rollingReturns[,1]-rollingReturns[,3], rollingReturns[,2]-rollingReturns[,3], rollingReturns[,2]-rollingReturns[,1])
names(rollingDiff)<-c(indexName1, indexName2, "RELATIVE")

firstDate<-first(index(rollingDiff))
lastDate<-last(index(rollingDiff))
xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)

ctr2Df<-data.frame(rollingDiff)
ctr2Df$T<-as.Date(index(rollingDiff))

ctr2Melt<-melt(ctr2Df, id='T')

pdf(NULL)
ggplot(ctr2Melt, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	geom_line() +
	scale_x_date(breaks = xAxisTicks) +
	labs(x='', y='(%)', color='', title=sprintf("%s/%s/Short-term Bonds", indexName1, indexName2), subtitle=sprintf("%d-day rolling difference in returns [%s:%s]", lb, firstDate, lastDate)) +
	annotate("text", x=lastDate, y=min(rollingDiff), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
ggsave(sprintf("%s/%s.%s.%d.png", reportPath, indexName1, indexName2, lb), width=16, height=8, units="in")	
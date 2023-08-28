library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."
indexName1<-"NIFTY 50 TR"
indexName2<-"NIFTY LOW VOLATILITY 50 TR"
startDate<-as.Date("2004-01-01")
endDate<-as.Date("2019-04-30")
lb<-200 #weeks

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 13 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

pxDf1<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where INDEX_NAME='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName1, startDate, endDate))
pxDf2<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where INDEX_NAME='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName2, startDate, endDate))

pXts1<-xts(pxDf1[,1], as.Date(pxDf1[,2]))
pXts2<-xts(pxDf2[,1], as.Date(pxDf2[,2]))

dXts<-na.omit(merge(weeklyReturn(pXts1), weeklyReturn(pXts2)))
dXts<-dXts[-1,]
dXts<-dXts[-nrow(dXts),]
names(dXts)<-c(indexName1, indexName2)

sdXts<-na.omit(merge(rollapply(dXts[,1], lb, function(X) ES(X, p=.95, method="modified")), rollapply(dXts[,2], lb, function(X) ES(X, p=.95, method="modified"))))
names(sdXts)<-c(indexName1, indexName2)

######### plot ES

sdDf<-data.frame(sdXts)
sdDf$T<-index(sdXts)
sdMelt<-melt(sdDf, id='T')

firstDate<-first(index(sdXts))
lastDate<-last(index(sdXts))
xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)

pdf(NULL)
ggplot(sdMelt, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	geom_line() +
	scale_x_date(breaks = xAxisTicks) +
	labs(x='', y='ES', color='', title=sprintf("%s/%s Expected Shortfall 95", indexName1, indexName2), subtitle=sprintf("%d week returns [%s:%s]", lb, first(index(dXts)), last(index(dXts)))) +
	annotate("text", x=lastDate, y=min(sdXts, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
ggsave(sprintf("%s/%s.%s.ES.png", reportPath, indexName1, indexName2), width=16, height=8, units="in")

######### plot ratio

ratio<-sdXts[,2]/sdXts[,1]
ratioDf<-data.frame(ratio)
names(ratioDf)<-c('Ratio')
ratioDf$T<-index(ratio)

ggplot(ratioDf, aes(x=T, y=Ratio)) +
	theme_economist() +
	geom_line() +
	scale_x_date(breaks = xAxisTicks) +
	labs(x='', y='ES ratio', color='', title=sprintf("%s/%s Ratio of ES 95", indexName2, indexName1), subtitle=sprintf("%d week returns [%s:%s]", lb, first(index(dXts)), last(index(dXts)))) +
	annotate("text", x=lastDate, y=min(ratio, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
ggsave(sprintf("%s/%s.%s.ES-ratio.png", reportPath, indexName1, indexName2), width=16, height=8, units="in")


#################################################################
































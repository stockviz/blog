library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('dplyr')

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."
indexName1<-"NIFTY 50 TR"
indexName2<-"NIFTY LOW VOLATILITY 50 TR"
startDate<-as.Date("2004-01-01")
endDate<-as.Date("2019-04-30")

cumLb<-seq(2, 12*5)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 13 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

pxDf1<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where INDEX_NAME='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName1, startDate, endDate))
pxDf2<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where INDEX_NAME='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName2, startDate, endDate))

pXts1<-xts(pxDf1[,1], as.Date(pxDf1[,2]))
pXts2<-xts(pxDf2[,1], as.Date(pxDf2[,2]))

dXts<-merge(monthlyReturn(pXts1), monthlyReturn(pXts2))
dXts<-dXts[-1,]
dXts<-dXts[-nrow(dXts),]
names(dXts)<-c(indexName1, indexName2)

diffXts<-dXts[,2]-dXts[,1]
sdDf<-data.frame(100*diffXts)
names(sdDf)<-c('DIFF')
sdDf$T<-index(diffXts)

firstDate<-first(index(diffXts))
lastDate<-last(index(diffXts))
xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)

pdf(NULL)
ggplot(sdDf, aes(x=T, y=DIFF)) +
	theme_economist() +
	geom_line() +
	scale_x_date(breaks = xAxisTicks) +
	labs(x='', y='%', color='', title=sprintf("%s - %s Return Differential", indexName2, indexName1), subtitle=sprintf("monthly returns [%s:%s]", first(index(dXts)), last(index(dXts)))) +
	annotate("text", x=lastDate, y=min(sdDf$DIFF, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
ggsave(sprintf("%s/%s.%s.monthly.return-diff.png", reportPath, indexName1, indexName2), width=16, height=8, units="in")

########################

cumDiffXts<-NULL
for(lb in cumLb){
	cdiff1<-rollapply(dXts[,1], lb, Return.cumulative)
	cdiff2<-rollapply(dXts[,2], lb, Return.cumulative)
	cumDiffXts<-merge.xts(cumDiffXts, cdiff2-cdiff1)
}

numNegatives<-apply(data.frame(cumDiffXts), 2, function(X) sum(ifelse(X < 0, 1, 0), na.rm=T))
lbVsNeg <- data.frame(LB=cumLb, NN=numNegatives)

ggplot(lbVsNeg, aes(x=LB, y=NN)) +
	theme_economist() +
	geom_point() +
	labs(x='Holding Period', y='#', color='', title=sprintf("%s - %s Number of Negative Differentials vs. Holding Period", indexName2, indexName1), subtitle=sprintf("monthly returns [%s:%s]", first(index(dXts)), last(index(dXts)))) +
	annotate("text", x=max(lbVsNeg$LB, na.rm=T), y=min(lbVsNeg$NN, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
ggsave(sprintf("%s/%s.%s.negative.holding-period.png", reportPath, indexName1, indexName2), width=16, height=8, units="in")
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
indexNames2<-c("NIFTY LOW VOLATILITY 50 TR", "NIFTY ALPHA 50 TR", "NIFTY200 QUALITY 30 TR", "NIFTY500 VALUE 50 TR")
endDate<-as.Date("2019-04-30")

cumLb<-seq(2, 12*10)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 13 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

pxDf1<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where INDEX_NAME='%s'", indexName1))
pXts1<-xts(pxDf1[,1], as.Date(pxDf1[,2]))
dXts<-monthlyReturn(pXts1)

for(indexName2 in indexNames2){
	pxDf2<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where INDEX_NAME='%s'", indexName2))
	pXts2<-xts(pxDf2[,1], as.Date(pxDf2[,2]))

	dXts<-merge(dXts, monthlyReturn(pXts2))
}
dXts<-dXts[-1,]
dXts<-dXts[-nrow(dXts),]
names(dXts)<-c(indexName1, indexNames2)
dXts<-na.omit(dXts)

Common.PlotCumReturns(dXts, "Factor Index Returns", "NSE Indices", sprintf("%s/factor-index.cumulative.png", reportPath))

lbVsNeg <- data.frame(LB=cumLb)
for(i in 1:length(indexNames2)){
	cumDiffXts<-NULL
	for(lb in cumLb){
		cdiff1<-rollapply(dXts[,1], lb, Return.cumulative)
		cdiff2<-rollapply(dXts[,i+1], lb, Return.cumulative)
		cumDiffXts<-merge.xts(cumDiffXts, cdiff2-cdiff1)
	}
	numNegatives<-apply(data.frame(cumDiffXts), 2, function(X) sum(ifelse(X < 0, 1, 0), na.rm=T))
	lbVsNeg <- cbind(lbVsNeg, data.frame(NN=numNegatives))
}

names(lbVsNeg)<-c('LB', indexNames2)
lbVsNegMelt<-melt(lbVsNeg, id='LB')

ggplot(lbVsNegMelt, aes(x=LB, y=value, color=variable)) +
	theme_economist() +
	geom_point() +
	labs(x='Holding Period', y='#', color='', title=sprintf("Number of Negative Differentials with %s vs. Holding Period", indexName1), subtitle=sprintf("monthly returns [%s:%s]", first(index(dXts)), last(index(dXts)))) +
	annotate("text", x=max(lbVsNeg$LB, na.rm=T), y=min(lbVsNeg[, indexNames2], na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
ggsave(sprintf("%s/negative.holding-period.vs.%s.png", reportPath, indexName1), width=16, height=8, units="in")
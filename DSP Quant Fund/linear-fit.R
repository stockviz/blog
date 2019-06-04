library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')

library('extrafont')
library('ggplot2')
library('ggthemes')
library('reshape2')


source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."
rollingLb <- 36 #months
indexNames2<-c("NIFTY 200 TR", "NIFTY200 QUALITY 30 TR", "NIFTY500 VALUE 50 TR")
indexName1<-'Quant Fund'

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

backtestDf<-read.csv("backtest.csv")
backtestXts<-xts(backtestDf[,2], as.Date(backtestDf[,1]))

startDate<-first(index(backtestXts))
endDate<-last(index(backtestXts))

dXts<-Common.NormalizeMonthlyDates(monthlyReturn(backtestXts))
for(iName in indexNames2){
	pxDf1<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where INDEX_NAME='%s'", iName))
	pXts1<-xts(pxDf1[,1], as.Date(pxDf1[,2]))
	
	dXts<-merge.xts(dXts, Common.NormalizeMonthlyDates(monthlyReturn(pXts1)))
}

names(dXts)<-c(indexName1, indexNames2)
dXts<-na.omit(dXts)

dXts<-dXts[-1,]
dXts<-dXts[-nrow(dXts),]

dDf<-data.frame(dXts)
cnames<-colnames(dDf)

lmFormula <- formula(sprintf("`%s` ~ `%s`", cnames[1], paste(cnames[-1], collapse="`+`")))

fitMat<-rollapply(dDf, rollingLb, function(X) {
	lfit <- lm(lmFormula, data.frame(X))
	c(toString(rownames(last(X))), as.vector(lfit[[1]])) #1 => intercept
}, by.column=FALSE)

fitDf<-data.frame(fitMat)
fitDf[,1]<-as.Date(fitDf[,1])
fitDf[,2]<-as.numeric(fitDf[,2])
fitDf[,3]<-as.numeric(fitDf[,3])
fitDf[,4]<-as.numeric(fitDf[,4])
fitDf[,5]<-as.numeric(fitDf[,5])

names(fitDf)<-c('T', 'a', indexNames2)

firstDate<-first(index(dXts))
lastDate<-last(index(dXts))
xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)

pdf(NULL)
ggplot(melt(fitDf, id='T'), aes(x=T, y=value, color=variable)) +
	theme_economist() +
	geom_line() +
	scale_x_date(breaks = xAxisTicks) +
	labs(x='', y='coefficients', color='', title=sprintf("Linear Fit: %s ~ %s", 'Quant Fund', paste(cnames[-1], collapse=" + ")), subtitle=sprintf("%d rolling monthly returns [%s:%s]", rollingLb, firstDate, lastDate)) +
	annotate("text", x=lastDate, y=0, label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s/linear-fit.%d.png", reportPath, rollingLb), width=16, height=8, units="in")	
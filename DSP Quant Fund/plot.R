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
indexName<- 'NIFTY 100 TR' #"NIFTY200 QUALITY 30 TR" #
indexName2<-'NIFTY 100'
pbCutoff<-5

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

backtestDf<-read.csv("backtest.csv")
backtestXts<-xts(backtestDf[,2], as.Date(backtestDf[,1]))

startDate<-first(index(backtestXts))
endDate<-last(index(backtestXts))

indexPx<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate, endDate))
indexPxXts<-xts(indexPx[,1], as.Date(indexPx[,2]))

monthlyRets<-merge(Common.NormalizeMonthlyDates(monthlyReturn(indexPxXts)), Common.NormalizeMonthlyDates(monthlyReturn(backtestXts)))
names(monthlyRets)<-c(indexName, 'Quant Fund')

Common.PlotCumReturns(monthlyRets, 'Back-test Returns', "", sprintf("%s/cumulative.inception.%s.png", reportPath, indexName))

############################################

q()

excessReturns<-100*na.omit(rollapply(monthlyRets[,2], rollingLb, Return.cumulative) - rollapply(monthlyRets[,1], rollingLb, Return.cumulative))

excessRetDf<-data.frame(excessReturns)
excessRetDf$T<-index(excessReturns)

firstDate<-first(index(excessReturns))
lastDate<-last(index(excessReturns))
xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)

pdf(NULL)
ggplot(excessRetDf, aes(x=T, y=Quant.Fund)) +
	theme_economist() +
	geom_line() + 
	scale_x_date(breaks = xAxisTicks) +
	labs(x='', y='%', color='', title=sprintf("%s - %s Return Differential", 'Quant Fund', indexName), subtitle=sprintf("%d rolling monthly returns [%s:%s]", rollingLb, firstDate, lastDate)) +
	annotate("text", x=lastDate, y=min(excessReturns, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s/%s.%d.rolling-monthly.return-diff.png", reportPath, indexName, rollingLb), width=16, height=8, units="in")

############################################

indexVal<-sqlQuery(lcon, sprintf("select TIME_STAMP, PB from INDEX_NSE_VALUATION where index_name='NIFTY 50'"))
indexValXts<-xts(indexVal[,-1], as.Date(indexVal[,1]))

indexVal2<-sqlQuery(lcon, sprintf("select TIME_STAMP, PB from INDEX_NSE_VALUATION where index_name='NIFTY 100'"))
indexValXts2<-xts(indexVal2[,-1], as.Date(indexVal2[,1]))

indexPbXts<-merge(indexValXts, indexValXts2)
indexPb<-data.frame(indexPbXts)
names(indexPb)<-c('NIFTY 50', 'NIFTY 100')
indexPb$T<-index(indexPbXts)
indexPbMelt<-melt(indexPb, id='T')

ggplot(indexPbMelt, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	geom_line() + 
	scale_x_date(date_labels="%Y", date_breaks="1 year") +
	labs(x='', y='PB', color='', title="Index PB Ratios", subtitle=sprintf("[%s:%s]", first(index(indexPbXts)), last(index(indexPbXts)))) +
	annotate("text", x=last(index(indexPbXts)), y=min(indexPbXts, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)

ggsave(sprintf("%s/index.PB.png", reportPath), width=16, height=8, units="in")
	
allXts<-merge(lag(monthlyRets, -1), indexValXts)
allXts[,3]<-na.locf(allXts[,3])
allXts<-na.omit(allXts)

btXts<-ifelse(allXts[,3] < pbCutoff, allXts[,1], 0)
singlePb<-merge(btXts, allXts[,1])
names(singlePb)<-c(sprintf("PB%d", pbCutoff), 'Quant Fund')
Common.PlotCumReturns(singlePb, 'Back-test Returns', "", sprintf("%s/cumulative.pb-cutoff.%d.png", reportPath, pbCutoff))

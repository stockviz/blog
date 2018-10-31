library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')
library('ggrepel')
library('dplyr')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
#reportPath <- "D:/StockViz/report"

source("d:/stockviz/r/config.r")

args = commandArgs(TRUE)
asset1Name <- args[1]

endDate<-as.Date("2018-09-30")
numIters<-10000
set.seed(1)

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUS2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

if(asset1Name == "NIFTY 50"){
	asset1Px<-sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = '%s' and time_stamp <= '%s'", asset1Name, endDate))
	asset1Xts<-xts(asset1Px[,2], as.Date(asset1Px[,1], tz=TZ))
} else if(asset1Name == "SP500"){
	asset1Px<-sqlQuery(lconUS2, sprintf("select time_stamp, ac from bhav_yahoo where SYMBOL = '^GSPC' and time_stamp <= '%s'", endDate))
	asset1Xts<-xts(asset1Px[,2], as.Date(asset1Px[,1], tz=TZ))
} else {
	print("INVALID TICKER")
	q()
}

print(head(asset1Xts))
print(tail(asset1Xts))

retXs<-monthlyReturn(asset1Xts) #we will use monthly returns instead, since the rest of the analysis uses monthly time-series
retXs<-retXs[-1,]

################## 12-1 momentum 

excessReturns<-xts()
for(i in 1:numIters){
	modRet<-retXs + rnorm(nrow(retXs), 0, 0.30/100) #Add a small amount of white noise (mean 0%; standard deviation 0.025%) to daily market returns

	#calculate 12-1 signal
	modRet<-merge(modRet, rollapply(modRet, 12, Return.cumulative)) 
	names(modRet)<-c('RET', 'RET12')
	modRet$SIG<-modRet$RET12-modRet$RET #Calculate a long/flat trend equity strategy using 12-1 month momentum signals

	#offset monthly returns
	modRet$RET_1<-stats::lag(modRet$RET, -1)

	modRet<-na.omit(modRet)

	#long if 12-1 signal is positive, 0 otherwise
	modRet$L<-ifelse(modRet$SIG > 0, modRet$RET_1, 0)

	#Calculate the rolling 12-month return of the strategy minus the alternate market history return.
	excessReturns<-merge(excessReturns, na.omit(rollapply(modRet$L, 12, Return.cumulative) - rollapply(modRet$RET_1, 12, Return.cumulative)))
}

excessReturns$AVG<-rowMeans(excessReturns) #first calculate the average 12-month relative return of all 1,000 strategies

#calculate the maximum and minimum relative 12-month relative performance and subtract the average.
excessReturns$SPREAD_MAX<-rollapply(excessReturns, 1, max, by.column=F) - excessReturns$AVG 
excessReturns$SPREAD_MIN<-rollapply(excessReturns, 1, min, by.column=F) - excessReturns$AVG 

toPlotDf<-data.frame(excessReturns[, c('SPREAD_MAX', 'SPREAD_MIN')])
toPlotDf<-toPlotDf*100
toPlotDf$T<-as.Date(row.names(toPlotDf))

plotStart<-min(toPlotDf$T)
plotEnd<-max(toPlotDf$T)

pdf(NULL)
ggplot(melt(toPlotDf, id='T'), aes(x=T, y=value, color=variable)) +
	theme_economist() +
	geom_line() +
	#guides(color=F) +
	labs(x = "", y="luck", fill="", color="", title="Performance of Best & Worst Relative 12-month Return Difference vs. Avg Relative 12-month Return Difference ", subtitle=sprintf("%s [%s:%s]", asset1Name, plotStart, plotEnd)) +
	annotate("text", x=plotEnd, y=100*min(excessReturns[, c('SPREAD_MAX', 'SPREAD_MIN')]), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
ggsave(sprintf("%s/%s.12-1.mom.spread.png", reportPath, asset1Name), width=12, height=6, units="in")
	
#########################################################################################

excessReturns<-xts()
for(i in 1:numIters){
	modDret<-dailyReturn(asset1Xts) + rnorm(nrow(asset1Xts), 0, 0.025/100) #Add a small amount of white noise (mean 0%; standard deviation 0.025%) to daily market returns
	modDPx<-as.numeric(asset1Xts[1])*cumprod(1+modDret) #calculate a synthetic daily price series
	modMret<-to.monthly(monthlyReturn(modDPx), indexAt='lastof')[,4]
	
	################## price - 10-month moving average
	modMSig<-to.monthly(ifelse(modDPx[,1] - SMA(modDPx[,1], 10*20) > 0, 1, 0), indexAt='lastof')[,4]
	
	################## 12-1 month momentum
	modMSig$SIG_2<-ifelse(rollapply(modMret, 12, Return.cumulative)-modMret > 0, 1, 0)
	
	################## 13-34week EMA crossover
	modMSig$SIG_3<-to.monthly(ifelse(EMA(modDPx, 13*5) - EMA(modDPx, 34*5) > 0, 1, 0), indexAt='lastof')[,4]
	
	modMSig<-merge(modMSig, stats::lag(modMret, -1))
	modMSig<-na.omit(modMSig)
	
	modMSig$L<-ifelse(modMSig[,1] == 1 & modMSig[,2] == 1 & modMSig[,3] == 1, modMSig[,4], 0)
	excessReturns<-merge(excessReturns, na.omit(rollapply(modMSig$L, 12, Return.cumulative) - rollapply(modMSig[,4], 12, Return.cumulative)))
}

excessReturns$AVG<-rowMeans(excessReturns) #first calculate the average 12-month relative return of all 1,000 strategies

#calculate the maximum and minimum relative 12-month relative performance and subtract the average.
excessReturns$SPREAD_MAX<-rollapply(excessReturns, 1, max, by.column=F) - excessReturns$AVG 
excessReturns$SPREAD_MIN<-rollapply(excessReturns, 1, min, by.column=F) - excessReturns$AVG 

toPlotDf<-data.frame(excessReturns[, c('SPREAD_MAX', 'SPREAD_MIN')])
toPlotDf<-toPlotDf*100
toPlotDf$T<-as.Date(row.names(toPlotDf))

plotStart<-min(toPlotDf$T)
plotEnd<-max(toPlotDf$T)

pdf(NULL)
ggplot(melt(toPlotDf, id='T'), aes(x=T, y=value, color=variable)) +
	theme_economist() +
	geom_line() +
	#guides(color=F) +
	labs(x = "", y="luck", fill="", color="", title="Performance of Best & Worst Relative 12-month Return Difference vs. Avg Relative 12-month Return Difference ", subtitle=sprintf("%s [%s:%s]", asset1Name, plotStart, plotEnd)) +
	annotate("text", x=plotEnd, y=100*min(excessReturns[, c('SPREAD_MAX', 'SPREAD_MIN')]), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
ggsave(sprintf("%s/%s.aggregate.spread.png", reportPath, asset1Name), width=12, height=6, units="in")
	
#########################################################################################

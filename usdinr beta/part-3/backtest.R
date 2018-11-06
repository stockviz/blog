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
#library('dplyr')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
#reportPath <- "D:/StockViz/report"

source("d:/stockviz/r/config.r")

plotCumReturns<-function(toPlot, chartTitle, fileName){
	pdf(NULL)
	png(fileName, width=1400, height=800, bg="white")
	layout(matrix(c(1, 2)), heights = c(2, 1.3), widths = 1)
	par(mar = c(0, 4, 4, 2), family='Segoe UI', bty="n")
	plot_object <-chart.CumReturns(toPlot, cex.legend=1, main=NA, xaxis = FALSE, legend.loc = "topleft", begin = c("first", "axis"), geometric = TRUE) 
	print(plot_object)
	mtext("Cumulative Return", side=2, line=1)
	title(main=chartTitle, family='Segoe UI') 
	mtext(paste(sprintf("%.2f%%", 100*apply(toPlot, 2, Return.cumulative)), collapse=" / "), cex=0.8)
	par(mar = c(5, 4, 0, 2))
	plot_object <-chart.Drawdown(toPlot, main = NA, ylab = "Drawdown", event.labels = NULL, ylog = FALSE, geometric = TRUE, bty="n")
	print(plot_object)
	mtext("Drawdown", side=2, line=1)
	mtext("@StockViz", side=4, col='grey')
	dev.off()
}

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("2004-01-01")
endDate<-as.Date("2018-10-25")
dateRange<-"2005/"
lb<-50

indices<-c('DEXINUS', 'DTWEXB', 'DTWEXM', 'DTWEXO')

refXts<-xts()
for(ind in indices){
	usdDf<-sqlQuery(lcon, sprintf("select val, time_stamp from FRED_OBSERVATION 
								where time_stamp >= '%s' and time_stamp <= '%s' 
								and series_id=(select id from FRED_SERIES where series_id='%s')", startDate, endDate, ind))
	usdXts<-xts(usdDf$val, as.Date(usdDf$time_stamp))
	refXts<-merge(refXts, usdXts)
}								
names(refXts)<-indices

allXts<-na.locf(refXts)
drXts<-ROC(allXts)

spreadReturns<-xts()
for(i in 2:length(indices)){
	betaFormula<-sprintf("%s~%s+0", indices[1], indices[i])
	lmRet<- rollapply(drXts, lb, function(X) lm(betaFormula, X)$coefficients[[1]], by.column = F)
	
	betaXts<-drXts[,1] - lmRet[,1]*drXts[,i] #1
	names(betaXts)<-c('spread')
	
	betaXts<-na.omit(betaXts)
	
	betaXts<-merge(betaXts, rollapply(betaXts[,1], 100, mean), rollapply(betaXts[,1], 100, sd)) #2, 3
	betaXts<-merge(betaXts, betaXts[,2]+betaXts[,3], betaXts[,2]-betaXts[,3]) #4, 5
	
	betaXts<-merge(betaXts, stats::lag(betaXts$spread, -1))
	betaXts<-na.omit(betaXts)
	
	names(betaXts)<-c('spread', 'avg', 'sd', 'avgpsd', 'avgmsd', 'spread_lag_1')
	
	#if the spread has diverged beyond 1-sigma, bet on mean-reversion
	converge1<-ifelse(betaXts$spread > betaXts$avgpsd, -betaXts$spread_lag_1, ifelse(betaXts$spread_lag_1 < betaXts$avgmsd, betaXts$spread_lag_1, 0)) 
	
	#if the spread has diverged beyond 1-sigma, bet on it getting bigger
	converge2<-ifelse(betaXts$spread > betaXts$avgpsd, betaXts$spread_lag_1, ifelse(betaXts$spread_lag_1 < betaXts$avgmsd, -betaXts$spread_lag_1, 0)) 
	
	#if the spread is between the average and 1-sigma, bet on it blowing out
	diverge1<-ifelse(betaXts$spread > betaXts$avg & betaXts$spread < betaXts$avgpsd, betaXts$spread_lag_1, ifelse(betaXts$spread < betaXts$avg & betaXts$spread > betaXts$avgmsd, betaXts$spread_lag_1, 0))
	
	spreadReturns<-merge(spreadReturns, converge1, converge2, diverge1)
}

srNames<-unlist(lapply(indices[2:length(indices)], function(X) c(sprintf("%s.C1",X), sprintf("%s.C2",X), sprintf("%s.D1", X))))
names(spreadReturns)<-srNames

toPlot<-spreadReturns[dateRange,]
cumRets<-apply(toPlot, 2, Return.cumulative)
toPlot<-toPlot[,names(cumRets[cumRets > mean(cumRets[cumRets > 0])])]
plotCumReturns(toPlot, "USDINR Spread trading back-test (daily)", sprintf("%s/daily.spread.back-test.png", reportPath))

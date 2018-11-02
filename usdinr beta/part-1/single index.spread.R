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

plotDf<-function(toPlotDf, mainTitle, subTitle, fileName){
	plotStart<-min(toPlotDf$T)
	plotEnd<-max(toPlotDf$T)
	xAxisTicks<-seq(plotStart, plotEnd, length.out=10)

	meltedDf<-melt(toPlotDf, id='T')

	pdf(NULL)
	ggplot(meltedDf, aes(x=T, y=value, color=variable)) +
		theme_economist() +
		geom_line() +
		scale_x_date(breaks = xAxisTicks) +
		labs(x = "", y="", fill="", color="", title=mainTitle, subtitle=subTitle) +
		annotate("text", x=plotEnd, y=min(meltedDf$value), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(fileName, width=12, height=6, units="in")	
}

plotBeta<-function(drXts, lb){
	betaXts<-xts()
	for(i in 2:length(indices)){
		betaFormula<-sprintf("%s~%s+0", indices[1], indices[i])
		lmRet<- rollapply(drXts, lb, function(X) lm(betaFormula, X)$coefficients[[1]], by.column = F)
		betaXts<-merge(betaXts, lmRet)
	}

	betaXts<-na.omit(betaXts)
	betaDf<-data.frame(betaXts)
	names(betaDf)<-indices[-1]
	betaDf$T<-as.Date(index(betaXts[,1]))
	
	plotStart<-min(betaDf$T)
	plotEnd<-max(betaDf$T)
	
	plotDf(betaDf, 
		sprintf("Beta between %s and %s", indices[1], paste(indices[-1], collapse=",")), 
		sprintf("%d-day lookback [%s:%s]", lb, plotStart, plotEnd), 
		sprintf("%s/%s.%s.index.spread.%d.%s.%s.png", reportPath, indices[1], paste(indices[-1], collapse="-"), lb, plotStart, plotEnd))
}

plotTimeSeries<-function(tsXts){
	tsDf<-data.frame(tsXts)
	names(tsDf)<-indices
	tsDf$T<-as.Date(index(tsXts[,1]))

	plotStart<-min(tsDf$T)
	plotEnd<-max(tsDf$T)
	
	plotDf(tsDf, 
		sprintf("%s", paste(indices, collapse=",")), 
		sprintf("[%s:%s]", plotStart, plotEnd), 
		sprintf("%s/%s.index.%s.%s.png", reportPath, paste(indices, collapse="-"), plotStart, plotEnd))
}

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("1995-01-04")
endDate<-as.Date("2018-10-25")

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
dateRange<-"2005/"

#plotTimeSeries(allXts)
plotTimeSeries(allXts[dateRange,])
	
drXts<-ROC(allXts)
#plotBeta(drXts, 20)
#plotBeta(drXts, 50)
#plotBeta(drXts, 100)
#plotBeta(drXts, 200)

plotBeta(drXts[dateRange,], 20)
plotBeta(drXts[dateRange,], 50)
#plotBeta(drXts[dateRange,], 100)
#plotBeta(drXts[dateRange,], 200)

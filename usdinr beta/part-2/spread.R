library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library("fUnitRoots")
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

plotDf<-function(toPlotDf, mainTitle, subTitle, fileName){
	plotStart<-min(toPlotDf$T)
	plotEnd<-max(toPlotDf$T)
	xAxisTicks<-seq(plotStart, plotEnd, length.out=10)

	pdf(NULL)
	ggplot(toPlotDf, aes(x=T, y=spread)) +
		theme_economist() +
		geom_line(aes(color=pCol, group=1)) +
		geom_ribbon(aes(ymin = avgmsd, ymax=avgpsd), fill='grey70', alpha=0.5) +
		scale_x_date(breaks = xAxisTicks) +
		labs(x = "", y="", fill="", color="", title=mainTitle, subtitle=subTitle) +
		annotate("text", x=plotEnd, y=min(toPlotDf$spread), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(fileName, width=12, height=6, units="in")	
}

plotSpread<-function(drXts, lb){
	for(i in 2:length(indices)){
		betaFormula<-sprintf("%s~%s+0", indices[1], indices[i])
		lmRet<- rollapply(drXts, lb, function(X) { 
			linearFit<-lm(betaFormula, X)
			coeff<-linearFit$coefficients[[1]]
			adf<-adfTest(data.frame(linearFit$residuals)[,1], type="nc")
			p<-adf@test$p.value
			
			c(coeff, p)
		}, by.column = F)
		
		betaXts<-merge(drXts[,1] - lmRet[,1]*drXts[,i], lmRet[,2]) #1,2
		betaXts<-na.omit(betaXts)
		
		betaXts<-merge(betaXts, rollapply(betaXts[,1], 100, mean), rollapply(betaXts[,1], 100, sd)) #3, 4
		betaXts<-merge(betaXts, betaXts[,3]+betaXts[,4], betaXts[,3]-betaXts[,4]) #5, 6
		
		betaDf<-data.frame(betaXts)
		names(betaDf)<-c('spread', 'p', 'avg', 'sd', 'avgpsd', 'avgmsd')
		betaDf$T<-as.Date(index(betaXts[,1]))
		betaDf$pCol<-ifelse(betaDf$p > 0.01, 'p > 0.01', 'p <= 0.01')
	
		plotStart<-min(betaDf$T)
		plotEnd<-max(betaDf$T)
	
		plotDf(betaDf, 
			sprintf("Spread between %s and %s", indices[1], indices[i]), 
			sprintf("%d-day lookback [%s:%s]", lb, plotStart, plotEnd), 
			sprintf("%s/%s.%s.index.spread.%d.%s.%s.png", reportPath, indices[1], indices[i], lb, plotStart, plotEnd))
	}
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

drXts<-ROC(allXts)

plotSpread(drXts[dateRange,], 20)
plotSpread(drXts[dateRange,], 50)

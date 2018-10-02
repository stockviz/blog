library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('gld') #Generalised Lambda Distribution
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')

options(stringsAsFactors = FALSE)
reportPath <- "."
source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

simReturns<-function(seriesRet, numSims, reportFix){
	#fit a gld
	seriesFit<-fit.fkml(as.numeric(seriesRet[,1]))

	#extract lambdas
	series.lambda1<-as.numeric(seriesFit$lambda[1])
	series.lambda2<-as.numeric(seriesFit$lambda[2])
	series.lambda3<-as.numeric(seriesFit$lambda[3])
	series.lambda4<-as.numeric(seriesFit$lambda[4])

	#sample
	simLength<-length(seriesRet[,1])
	rndSamp<-rgl(simLength*numSims,  lambda1 = series.lambda1, lambda2 = series.lambda2, lambda3 = series.lambda3, lambda4 = series.lambda4)

	cumRets<-c() #accumulates lumpsum returns (cumulative)
	profits<-c() #accumulates dca/sip returns
	for(i in 1:numSims){
		i1<-simLength*(i-1)+1
		i2<-simLength*i
		cumRets<-c(cumRets, Return.cumulative(rndSamp[(i1+1):i2]))
		pxSeries<-cumprod(1+rndSamp[i1:i2])
		profits<-c(profits, sum(1/pxSeries)*last(pxSeries)/(i2-i1+1)-1)
	}

	series.avgR<-Return.cumulative(seriesRet[,2])
	series.cR<-as.numeric(Return.cumulative(seriesRet[,1])) #observed lumpsum returns from day 0
	
	#density plots of lumpsum and dca/sip returns
	denA<-density(cumRets)
	denB<-density(profits)
	
	distFunLump<-ecdf(cumRets)
	distFunDca<-ecdf(profits)
	
	png(sprintf("%s\\cumulative.simulation.avg.%s.%s.%s.png", reportPath, reportFix, first(index(seriesRet[,1])), last(index(seriesRet[,1]))), bg='white', width=1200, height=700)
	par(family='Segoe UI')
	plot(range(denA$x[denA$x<4*series.cR], denB$x[denB$x<4*series.cR]), range(denA$y, denB$y), type = "n", ylab="density", xlab="returns", main=sprintf("cumulative %s distribution [%s,%s] @StockViz", reportFix, first(index(seriesRet[,1])), last(index(seriesRet[,1]))))
	lines(denA, col='blue', lwd=2)
	lines(denB, col='darkgreen')
	abline(v=series.cR, col='red')
	abline(v=series.avgR, col='orange')
	abline(v=0, col='grey')
	text(x=series.cR, y=0, labels=sprintf("%.2f", series.cR), col='red')
	text(x=series.avgR, y=0, labels=sprintf("%.2f", series.avgR), col='orange')

	text(x=0, y=0, pos=3, offset=5, labels=sprintf("%.2f", 100.0*distFunLump(0)), col='blue')
	text(x=0, y=0, pos=3, offset=6, labels=sprintf("%.2f", 100.0*distFunDca(0)), col='darkgreen')
	
	text(x=series.cR, y=0, pos=3, offset=5, labels=sprintf("%.2f", 100.0*(1-distFunLump(series.cR))), col='blue')
	text(x=series.cR, y=0, pos=3, offset=6, labels=sprintf("%.2f", 100.0*(1-distFunDca(series.cR))), col='darkgreen')
	
	text(x=series.avgR, y=0, pos=3, offset=5, labels=sprintf("%.2f", 100.0*(1-distFunLump(series.avgR))), col='blue')
	text(x=series.avgR, y=0, pos=3, offset=6, labels=sprintf("%.2f", 100.0*(1-distFunDca(series.avgR))), col='darkgreen')
	
	legend('topright', legend=c('lumpsum', 'dca/sip'), col=c('blue', 'darkgreen'), lwd=1)
	dev.off()
}

analyzeIndex<-function(indexName){
	dataDf<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE from BHAV_INDEX where index_name='%s'", indexName))
	dataXts<-xts(dataDf$PX_CLOSE, as.Date(dataDf$TIME_STAMP))
	weeklyRet<-weeklyReturn(dataXts)

	#remove the first and last values
	weeklyRet<-weeklyRet[-1,]
	weeklyRet<-weeklyRet[-nrow(weeklyRet),]

	avgReturns<-mean(weeklyRet)
	weeklyRet<-merge(weeklyRet, xts(rep(avgReturns, length(weeklyRet)), index(weeklyRet)))
	names(weeklyRet)<-c("INDEX", "CONSTANT")

	png(sprintf("%s\\cumulative.raw.%s.png", reportPath, indexName), bg = "white", width = 1200, height = 600)
	par(family='Segoe UI')
	charts.PerformanceSummary(weeklyRet, main=NA, cex.legend=1.5)
	title(sprintf("%s vs. constant @StockViz", indexName))
	mtext(paste(sprintf("%.2f%%", 100*apply(weeklyRet, 2, Return.cumulative)), collapse=" / "), family='Segoe UI')
	dev.off()

	simReturns(weeklyRet, 10000, indexName)
}

analyzeIndex('NIFTY 50')
analyzeIndex('NIFTY MIDCAP 100')
analyzeIndex('NIFTY SMLCAP 100')
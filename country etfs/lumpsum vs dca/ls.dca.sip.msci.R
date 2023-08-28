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
source("D:/StockViz/public/blog/common/msci.R")
source("D:/StockViz/public/blog/common/misc.R")

msciIndices <- c(935600, 939200, 984000)
msciIndexNames <- c('INDIA', 'JAPAN', 'USA')

startDate <- as.Date("1993-01-01")
endDate <- as.Date("2019-08-31")

simReturns<-function(seriesRet, numSims, reportFix, realDcaCum, realLsCum){
	#fit a gld
	seriesFit<-fit.fkml(as.numeric(seriesRet[,1]))

	#extract lambdas
	series.lambda1<-as.numeric(seriesFit$lambda[1])
	series.lambda2<-as.numeric(seriesFit$lambda[2])
	series.lambda3<-as.numeric(seriesFit$lambda[3])
	series.lambda4<-as.numeric(seriesFit$lambda[4])

	#sample
	simLength<-length(seriesRet)
	rndSamp<-rgl(simLength*numSims, lambda1 = series.lambda1, lambda2 = series.lambda2, lambda3 = series.lambda3, lambda4 = series.lambda4)

	print(paste("sample length: ", simLength, length(rndSamp)))
	
	cumRets<-c() #accumulates lumpsum returns (cumulative)
	profits<-c() #accumulates dca/sip returns
	
	cumNeg <- 0 #number of -ve returns (cumulative)
	dcaNeg <- 0 #number of -ve returns (dca)
	
	cumGtReal <- 0 #number of returns > real-world lumpsum return
	dcaGtReal <- 0
	for(i in 1:numSims){
		i1<-simLength*(i-1)+1
		i2<-simLength*i
		cret <- Return.cumulative(rndSamp[(i1+1):i2])
		cumRets<-c(cumRets, cret)
		
		pxSeries<-cumprod(1+rndSamp[i1:i2])
		dcaRet <- sum(1/pxSeries)*last(pxSeries)/(i2-i1+1)-1
		profits<-c(profits, dcaRet)
		
		if(cret < 0){
			cumNeg <- cumNeg + 1
		}
		if(dcaRet < 0){
			dcaNeg <- dcaNeg + 1
		}
		
		if(cret > realLsCum){
			cumGtReal <- cumGtReal + 1
		}
		if(dcaRet > realLsCum){
			dcaGtReal <- dcaGtReal + 1
		}
	}

	#density plots of lumpsum and dca/sip returns
	denA<-density(cumRets)
	denB<-density(profits)
	
	pctCumNeg<-cumNeg/numSims
	pctDcaNeg<-dcaNeg/numSims
	
	pctCumGtReal<-cumGtReal/numSims
	pctDcaGtReal<-dcaGtReal/numSims
	
	png(sprintf("%s\\cumulative.simulation.%s.%s.%s.png", reportPath, reportFix, first(index(seriesRet)), last(index(seriesRet))), bg='white', width=1200, height=700)
	par(family='Segoe UI')
	plot(range(denA$x[denA$x<5*realLsCum], denB$x[denB$x<5*realLsCum]), 
			range(denA$y, denB$y), 
			type = "n", ylab="density", xlab="returns", 
			main=sprintf("cumulative %s distribution [%s,%s] @StockViz", reportFix, first(index(seriesRet)), last(index(seriesRet))))
			
	lines(denA, col='blue', lwd=2)
	lines(denB, col='darkgreen')
	abline(v=realLsCum, col='red')
	abline(v=realDcaCum, col='orange')
	
	abline(v=0, col='grey')
	text(x=realLsCum, y=0, labels=sprintf("%.2f", realLsCum), col='red')
	text(x=realDcaCum, y=0, labels=sprintf("%.2f", realDcaCum), col='orange')

	text(x=0, y=0, pos=3, offset=5, labels=sprintf("%.2f", 100.0*pctCumNeg), col='blue')
	text(x=0, y=0, pos=3, offset=6, labels=sprintf("%.2f", 100.0*pctDcaNeg), col='darkgreen')
	
	text(x=realLsCum, y=0, pos=3, offset=5, labels=sprintf("%.2f", 100.0*pctCumGtReal), col='blue')
	text(x=realLsCum, y=0, pos=3, offset=6, labels=sprintf("%.2f", 100.0*pctDcaGtReal), col='darkgreen')
	
	legend('topright', legend=c('lumpsum', 'dca/sip'), col=c('blue', 'darkgreen'), lwd=1)
	dev.off()
}

analyzeIndex<-function(indexName, indexCode){
	dataTs<-Common.DownloadMsci(indexCode, 'G', startDate, endDate)
	dataTs<-Common.NormalizeMonthlyDates(dataTs)
	rets<-monthlyReturn(dataTs)
	
	#remove the first and last values
	rets<-rets[-1,]
	rets<-rets[-nrow(rets),]
	
	dcaR<-as.numeric(sum(1/dataTs)*last(dataTs)/nrow(dataTs)-1)
	cR<-as.numeric(Return.cumulative(rets))

	simReturns(rets, 500000, indexName, dcaR, cR)
}

#i<-1
#analyzeIndex(msciIndexNames[i], msciIndices[i])
#q()
for(i in 1:length(msciIndices)){
	print(msciIndexNames[i])
	analyzeIndex(msciIndexNames[i], msciIndices[i])
}
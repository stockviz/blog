library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('gld') #Generalised Lambda Distribution
library('extrafont')

options(stringsAsFactors = FALSE)
reportPath <- "D:\\StockViz\\public\\blog\\lumpsum vs sip (dca)"
source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

simReturns<-function(seriesRet, numSims, reportFix){
	#remove the first and last values
	seriesRet<-seriesRet[-1,]
	seriesRet<-seriesRet[-nrow(seriesRet),]
	
	#fit a gld
	seriesFit<-fit.fkml(as.numeric(seriesRet))

	#extract lambdas
	series.lambda1<-as.numeric(seriesFit$lambda[1])
	series.lambda2<-as.numeric(seriesFit$lambda[2])
	series.lambda3<-as.numeric(seriesFit$lambda[3])
	series.lambda4<-as.numeric(seriesFit$lambda[4])

	#sample
	simLength<-length(seriesRet)
	rndSamp<-rgl(simLength*numSims,  lambda1 = series.lambda1, lambda2 = series.lambda2, lambda3 = series.lambda3, lambda4 = series.lambda4)

	#begin diagnostics
	#make sure the gld fits the observations better than a normal distribution
	series.sigma<-sd(seriesRet)
	series.mean<-mean(seriesRet)

	png(sprintf("%s\\density.distribution.%s.%s.%s.png", reportPath, reportFix, first(index(seriesRet)), last(index(seriesRet))), bg='white', width=1000, height=700)
	par(family='Segoe UI')
	plot(density(as.numeric(seriesRet)), type='l', col='blue', lwd=2, main=sprintf("%s distribution [%s,%s] @StockViz", reportFix, first(index(seriesRet)), last(index(seriesRet))))
	lines(density(rnorm(simLength, series.mean, series.sigma)), col='green')
	lines(density(rnorm(simLength, series.mean, series.sigma)), col='green4')									
	lines(density(rndSamp[1:simLength]), col='red')
	lines(density(rndSamp[(simLength+1):(simLength*2)]), col='maroon')
	legend('topright', legend=c('data', 'normal dist', 'normal dist', 'gld dist', 'gld dist'), col=c('blue', 'green', 'green4', 'red', 'maroon'), lwd=1)
	dev.off()

	#end diagnostics
	
	cumRets<-c() #accumulates lumpsum returns (cumulative)
	profits<-c() #accumulates dca/sip returns
	for(i in 1:numSims){
		i1<-simLength*(i-1)+1
		i2<-simLength*i
		cumRets<-c(cumRets, Return.cumulative(rndSamp[(i1+1):i2]))
		pxSeries<-cumprod(1+rndSamp[i1:i2])
		profits<-c(profits, sum(1/pxSeries)*last(pxSeries)/(i2-i1+1)-1)
	}

	series.cR<-as.numeric(Return.cumulative(seriesRet)) #observed lumpsum returns
	
	#density plots of lumpsum and dca/sip returns
	denA<-density(cumRets)
	denB<-density(profits)
	png(sprintf("%s\\cumulative.simulation.%s.%s.%s.png", reportPath, reportFix, first(index(seriesRet)), last(index(seriesRet))), bg='white', width=1200, height=700)
	par(family='Segoe UI')
	plot(range(denA$x[denA$x<4*series.cR], denB$x[denB$x<4*series.cR]), range(denA$y, denB$y), type = "n", ylab="density", xlab="returns", main=sprintf("cumulative %s distribution [%s,%s] @StockViz", reportFix, first(index(seriesRet)), last(index(seriesRet))))
	lines(denA, col='blue', lwd=2)
	lines(denB, col='green')
	abline(v=series.cR, col='red')
	text(x=series.cR, y=0, labels=sprintf("%.2f", series.cR), col='red')
	legend('topright', legend=c('lumpsum', 'dca/sip'), col=c('blue', 'green'), lwd=1)
	dev.off()
}

#MIDCAP
dataDf<-sqlQuery(lcon, "select TIME_STAMP, PX_CLOSE from BHAV_INDEX where index_name='nifty midcap 100'")
dataXts<-xts(dataDf$PX_CLOSE, as.Date(dataDf$TIME_STAMP))
weeklyRet<-weeklyReturn(dataXts)

simReturns(weeklyRet, 10000, "MIDCAP weekly.returns")
simReturns(weeklyRet["2010/"], 10000, "MIDCAP weekly.returns")

#NIFTY
dataDf<-sqlQuery(lcon, "select TIME_STAMP, PX_CLOSE from BHAV_INDEX where index_name='nifty 50'")
dataXts<-xts(dataDf$PX_CLOSE, as.Date(dataDf$TIME_STAMP))
weeklyRet<-weeklyReturn(dataXts)

simReturns(weeklyRet, 10000, "NIFTY weekly.returns")
simReturns(weeklyRet["2010/"], 10000, "NIFTY weekly.returns")

#GOLDPMGBD228NLBM Gold Fixing Price 3:00 P.M. (London time) in London Bullion Market, based in U.S. Dollars
dataDf<-sqlQuery(lconUs, "select TIME_STAMP, VAL from FRED_OBSERVATION where SERIES_ID=-2147252004")
dataXts<-xts(dataDf$VAL, as.Date(dataDf$TIME_STAMP))
weeklyRet<-weeklyReturn(dataXts)
simReturns(weeklyRet, 10000, "GOLD weekly.returns")
simReturns(weeklyRet["2010/"], 10000, "GOLD weekly.returns")

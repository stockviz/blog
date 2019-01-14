library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('gld') #Generalised Lambda Distribution

library('lubridate')
library('reshape2')

library('ggplot2')
library('extrafont')
library('ggthemes')
library('grid')
library('gridExtra')
library('gtable')

options("scipen"=100)
options(stringsAsFactors = FALSE)
source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

reportPath <- "."

startDate<-as.Date("1991-01-01")
endDate<-as.Date("2018-12-31")

numIterations<-10000
investmentDuration<-30*12 #months

getRndAssetReturns<-function(singleAssetReturn){
	retRnd<-base::sample(singleAssetReturn, min(nrow(singleAssetReturn), investmentDuration))
	while(nrow(retRnd) < investmentDuration){
		retRnd<-rbind(retRnd, base::sample(singleAssetReturn, min(nrow(singleAssetReturn), investmentDuration - nrow(retRnd))))
	}
	return(retRnd)
}

getFKMLAssetReturns<-function(singleAssetReturn){
	sarVec<-coredata(singleAssetReturn)
	if(length(unique(sarVec))==1){
		return(rep(unique(sarVec)[1],investmentDuration))
	}
	
	seriesFit<-fit.fkml(coredata(sarVec))

	#extract lambdas
	series.lambda1<-as.numeric(seriesFit$lambda[1])
	series.lambda2<-as.numeric(seriesFit$lambda[2])
	series.lambda3<-as.numeric(seriesFit$lambda[3])
	series.lambda4<-as.numeric(seriesFit$lambda[4])
	
	rndSamp<-rgl(investmentDuration,  lambda1 = series.lambda1, lambda2 = series.lambda2, lambda3 = series.lambda3, lambda4 = series.lambda4)
	return(rndSamp)
}

getNormalAssetReturns<-function(singleAssetReturn){
	distMean<-mean(singleAssetReturn)
	distSigma<-sd(singleAssetReturn)
	
	rndSamp<-rnorm(investmentDuration, distMean, distSigma)
	return(rndSamp)
}

projectReturns<-function(singleAssetReturn, getAssetReturnsFn){
	projections<-c()
	dateSeq<-seq(from=Sys.Date(), by='month', length.out = investmentDuration)
	for(i in 1:numIterations){
		projAssetRets<-getAssetReturnsFn(singleAssetReturn)	
		parXts<-xts(projAssetRets, dateSeq)
		projections<-c(projections, 100*as.numeric(Return.annualized(parXts)))
	}
	
	projDf<-data.frame(projections)
	names(projDf)<-c('AR')
	return(projDf)
}

plotProjection<-function(projDf, mainTitle, subTitle, fileName){
	medianAr<-median(projDf[,1])
	pdf(NULL)
	ggplot(projDf, aes(AR)) +
		theme_economist() +
		geom_histogram(aes(y=..density..), binwidth=0.25, colour="black", fill="lightblue") +
		geom_density(alpha=.2, fill="#FF6666") +
		geom_vline(aes(xintercept=medianAr), colour="blue", size=1, linetype='dashed', alpha=0.5) +
		geom_label(aes(x=medianAr, label=sprintf("%.2f%%", medianAr), y=0), colour="blue") +
		labs(y='density', x='projected annualized returns', color='', title=mainTitle, subtitle=subTitle) 
	
	ggsave(fileName, width=12, height=6, units="in")
}

runSim<-function(mret, titleFix, fileFix){
	projRets<-projectReturns(mret, getRndAssetReturns)
	minAr<-min(projRets[,1])
	maxAr<-max(projRets[,1])
	plotProjection(projRets, sprintf("%s (Randomized)", titleFix), 
		sprintf("%d year expected returns: [%.2f%%, %.2f%%] ~ sample: [%s:%s]", round(investmentDuration/12,0), minAr, maxAr, startDate, endDate), 
		sprintf("%s/%s.RND.%s.%s.%d.png", reportPath, fileFix, startDate, endDate, round(investmentDuration/12,0)))

	projRets<-projectReturns(mret, getFKMLAssetReturns)
	minAr<-min(projRets[,1])
	maxAr<-max(projRets[,1])
	plotProjection(projRets, sprintf("%s (GLD)", titleFix), 
		sprintf("%d year expected returns: [%.2f%%, %.2f%%] ~ sample: [%s:%s]", round(investmentDuration/12,0), minAr, maxAr, startDate, endDate), 
		sprintf("%s/%s.GLD.%s.%s.%d.png", reportPath, fileFix, startDate, endDate, round(investmentDuration/12,0)))

	projRets<-projectReturns(mret, getNormalAssetReturns)
	minAr<-min(projRets[,1])
	maxAr<-max(projRets[,1])
	plotProjection(projRets, sprintf("%s (Normal)", titleFix), 
		sprintf("%d year expected returns: [%.2f%%, %.2f%%] ~ sample: [%s:%s]", round(investmentDuration/12,0), minAr, maxAr, startDate, endDate), 
		sprintf("%s/%s.NRM.%s.%s.%d.png", reportPath, fileFix, startDate, endDate, round(investmentDuration/12,0)))
}

df1<-sqlQuery(lcon, sprintf("select px_close, time_stamp from BHAV_INDEX
					where time_stamp >= '%s' and time_stamp <= '%s' 
					and index_name='%s'", startDate, endDate, 'NIFTY 50'))
					
xts2<-xts(df1$px_close, as.Date(df1$time_stamp))
mret<-monthlyReturn(xts2)
mret<-mret[-1]
mret<-mret[-nrow(mret)]

runSim(mret, "NIFTY 50", "NIFTY50")

q()

df1<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO 
					where time_stamp >= '%s' and time_stamp <= '%s' 
					and symbol='%s'", startDate, endDate, '^GSPC'))

xts2<-xts(df1$ac, as.Date(df1$time_stamp))
mret<-monthlyReturn(xts2)
mret<-mret[-1]
mret<-mret[-nrow(mret)]

runSim(mret, "S&P 500", "SP500")

df1<-sqlQuery(lcon, sprintf("select px_close, time_stamp from BHAV_INDEX
					where time_stamp >= '%s' and time_stamp <= '%s' 
					and index_name='%s'", startDate, endDate, 'NIFTY50 USD'))
					
xts2<-xts(df1$px_close, as.Date(df1$time_stamp))
mret<-monthlyReturn(xts2)
mret<-mret[-1]
mret<-mret[-nrow(mret)]

runSim(mret, "NIFTY 50 DOLLAR", "NIFTY50DLR")

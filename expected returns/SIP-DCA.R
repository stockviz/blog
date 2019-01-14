library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('gld') #Generalised Lambda Distribution
library('tvm')

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
investmentDuration<-10*12 #months
monthlySip<-1000 #in dollars

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

projectReturns<-function(singleAssetReturn, getAssetReturnsFn){
	projections<-c()
	dateSeq<-seq(from=Sys.Date(), by='month', length.out = investmentDuration)
	sipFlow<-rep(monthlySip, investmentDuration)
	
	for(i in 1:numIterations){
		assetReturns<-getAssetReturnsFn(singleAssetReturn)	
		
		assetPrices<-data.frame(E=rep(100.0, investmentDuration)) #keep track of asset prices
		investorAsset<-data.frame(E=rep(0.0, investmentDuration)) #keep track of investor asset quantity
		investorVal<-data.frame(E=rep(0.0, investmentDuration)) #keep track of investor asset value
		
		investorVal[1,]<-sipFlow[1]
		investorAsset[1,]<-sipFlow[1]/assetPrices[1,]
		
		for(m in 2:investmentDuration){
			assetPrices[m,]<-assetPrices[m-1,]*(1+assetReturns[m]) 
			
			investorAsset[m,] <- investorAsset[m-1,] + sipFlow[m]/assetPrices[m,]
			investorVal[m,] <- investorAsset[m-1,]*assetPrices[m,] + sipFlow[m]
		}
		
		IRR<-xirr(c(-sipFlow, sum(last(investorVal))), d=seq.Date(from=Sys.Date(), by='month', length.out=(1+length(sipFlow))), interval=c(0, 1))
		
		projections<-c(projections, 100*IRR)
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
		labs(y='density', x='projected IRR', color='', title=mainTitle, subtitle=subTitle) 
	
	ggsave(fileName, width=12, height=6, units="in")
}

runSim<-function(mret, titleFix, fileFix){
	projRets<-projectReturns(mret, getFKMLAssetReturns)
	minAr<-min(projRets[,1])
	maxAr<-max(projRets[,1])
	plotProjection(projRets, sprintf("%s (GLD)", titleFix), 
		sprintf("%d year expected SIP/DCA returns: [%.2f%%, %.2f%%] ~ sample: [%s:%s]", round(investmentDuration/12,0), minAr, maxAr, startDate, endDate), 
		sprintf("%s/%s.GLD.SIP-DCA.%s.%s.%d.png", reportPath, fileFix, startDate, endDate, round(investmentDuration/12,0)))
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

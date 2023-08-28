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

msciIndices<-read.csv("../COUNTRY_MSCI_BEFORE_1993.csv")

startDate <- as.Date("1993-01-01")
endDate <- as.Date("2019-08-31")

returnStatistics <- data.frame(INDEX_NAME="", REAL_LS=0.0, REAL_DCA=0.0, PROB_LS_LT_0=0.0, PROB_DCA_LT_0=0.0, PROB_LS_GT_REAL=0.0, PROB_DCA_GT_REAL=0.0, ST_DATE="", ED_DATE="")
simReturns<-function(seriesRet, numSims, reportFix, realDcaCum, realLsCum){
	stDate <- first(index(seriesRet))
	edDate <- last(index(seriesRet))
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
	
	actualSims <- 0
	dtSeq <- seq(from=stDate, to=edDate, by='months')
	for(i in 1:numSims){
		tryCatch({
			i1<-simLength*(i-1)+1
			i2<-simLength*i
			
			rndSampSubSet <- rndSamp[(i1+1):i2]
			rndXts <- xts(rndSampSubSet, dtSeq[1:length(rndSampSubSet)])
			cret <- Return.annualized(rndXts)
			cumRets<-c(cumRets, cret)
			
			pxSeries<-cumprod(1+rndSamp[i1:i2])
			numMonths <- length(pxSeries)
			dcaRet <- (sum(1/pxSeries)*last(pxSeries)/numMonths)^(1/(numMonths/12))-1
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
			
			actualSims <- actualSims + 1
		}, error = function(e){
			#print(paste(i, e))
			#print(paste(cret, dcaRet))
		})
	}

	#density plots of lumpsum and dca/sip returns
	denLD<-melt(data.frame(L = cumRets*100, D = profits*100))
	
	pctCumNeg<-100.0*cumNeg/actualSims
	pctDcaNeg<-100.0*dcaNeg/actualSims
	
	pctCumGtReal<-100.0*cumGtReal/actualSims
	pctDcaGtReal<-100.0*dcaGtReal/actualSims
	
	returnStatistics <<- rbind(returnStatistics, c(reportFix, 100*realLsCum, 100*realDcaCum, pctCumNeg, pctDcaNeg, pctCumGtReal, pctDcaGtReal, toString(stDate), toString(edDate)))
	
	ggplot(denLD, aes(x=value, color=variable)) +
		theme_economist() +
		geom_density() +
		annotate("text", x = realLsCum*100, y = 0, label = sprintf('real lumpsum %.2f%%', realLsCum*100), col='red', cex = 4, hjust = 0.5, vjust=-10) +
		annotate("text", x = realLsCum*100, y = 0, label = sprintf("%.2f/%.2f", pctCumGtReal, pctDcaGtReal), col='maroon', cex = 4, hjust = 0.5, vjust=-2) +
		geom_vline(xintercept = realLsCum*100, color = 'red') +
		
		annotate("text", x = realDcaCum*100, y = 0, label = sprintf('real dca %.2f%%', realDcaCum*100.0), col='orange', cex = 4, hjust = 0.5, vjust=-5) +
		geom_vline(xintercept = realDcaCum*100, color = 'orange') +
		
		annotate("text", x = 0, y = 0, label = sprintf("%.2f/%.2f", pctCumNeg, pctDcaNeg), col='darkgreen', cex = 4, hjust = 0.5, vjust=0) +
		geom_vline(xintercept = 0, color = 'grey') +
		
		labs(x = "returns", y="density", fill="", color="", 
			title=sprintf("Annualized Returns MSCI: %s", reportFix), 
			subtitle=sprintf("gld model fit on monthly returns between %s:%s", stDate, edDate))
		
	ggsave(sprintf("%s\\annualized.%s.%s.%s.png", reportPath, reportFix, stDate, edDate), width=16, height=8, units="in")
}

analyzeIndex<-function(indexName, indexCode){
	dataTs<-Common.DownloadMsci(indexCode, 'G', startDate, endDate)
	dataTs<-Common.NormalizeMonthlyDates(dataTs)
	rets<-monthlyReturn(dataTs)
	
	#remove the first and last values
	rets<-rets[-1,]
	rets<-rets[-nrow(rets),]
	
	numMonths <- nrow(dataTs)
	dcaR <- (sum(1/dataTs)*last(dataTs)/numMonths)^(1/(numMonths/12))-1
	cR<-as.numeric(Return.annualized(rets))

	simReturns(rets, 20000, indexName, dcaR, cR)
}

#i<-1
#analyzeIndex(msciIndexNames[i], msciIndices[i])
#q()
for(i in 1:nrow(msciIndices)){
	print(toString(msciIndices[i,1]))
	analyzeIndex(msciIndices[i,1], msciIndices[i,2])
}
returnStatistics <- returnStatistics[-1,]
write.csv(returnStatistics, "stats.csv", row.names=F)

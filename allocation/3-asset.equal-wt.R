library('RODBC')
library('extrafont')
library('quantmod')
library('PerformanceAnalytics')

library('ggplot2')
library('lubridate')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')
library('dplyr')

source("d:/stockviz/r/config.r")

options(stringsAsFactors = FALSE)
options("scipen"=100)
TZ <- "Asia/Calcutta"

#reportPath <- "."
reportPath <- "D:/StockViz/public/blog/allocation"
mytheme <- ttheme_default(
		core = list(fg_params=list(fontfamily='Segoe UI', hjust=1, x=1)),
		colhead = list(fg_params=list(fontfamily='Segoe UI')),
		rowhead = list(fg_params=list(fontfamily='Segoe UI')))

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs<-odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("2010-01-01")
endDate<-as.Date("2018-09-30")

initialInvestment<-1000000
asset1Name<-"NIFTY MIDCAP 100"
asset1Px<-sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", asset1Name, startDate, endDate))
asset1Xts<-xts(asset1Px[,2], as.Date(asset1Px[,1], tz=TZ))

asset2Name<-"0_5"
asset2Px<-sqlQuery(lcon, sprintf("select time_stamp, tri from INDEX_CCIL_TENOR where index_name = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", asset2Name, startDate, endDate))
asset2Xts<-xts(asset2Px[,2], as.Date(asset2Px[,1], tz=TZ))

qqqPx<-sqlQuery(lcon, sprintf("select time_stamp, px_close_adj from BHAV_EQ_AV where symbol='qqq' and vol > 0 and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
usdinr<-sqlQuery(lconUs, sprintf("select VAL, TIME_STAMP from FRED_OBSERVATION where SERIES_ID=-2147478748 and TIME_STAMP >='%s' and TIME_STAMP <= '%s'", startDate, endDate))

qqqXts<-merge(xts(qqqPx[,2], as.Date(qqqPx[,1], tz=TZ)), xts(usdinr$VAL, as.Date(usdinr$TIME_STAMP, tz=TZ)))
qqqXts[,2]<-na.locf(qqqXts[,2])
qqqXts<-na.omit(qqqXts)
asset3Xts<-qqqXts[,1]*qqqXts[,2]

runScenario<-function(assetWeights, rebalThreshold, stt=0, tax=0){
	allXts<-merge(asset1Xts, asset2Xts, asset3Xts)
	names(allXts)<-c('PX_A1', 'PX_A2', 'PX_A3')

	allXts[,2]<-na.locf(allXts[,2])
	allXts<-na.omit(allXts)
	
	assetCols<-1:3
	assetVals<-4:6
	holder<-matrix(0L, nrow=length(allXts[,1]), ncol=6)
	
	holder[1,assetCols]<-assetWeights*initialInvestment/allXts[1,]
	holder[1,assetVals]<-assetWeights*initialInvestment

	for(i in 2:length(allXts[,1])) {
		currentVal <- as.numeric(allXts[i,] * holder[i-1, assetCols])
		totalVal <- sum(currentVal)
		currentWt<- currentVal/totalVal
		
		if(all(currentWt < (1+rebalThreshold) * assetWeights)){
			#no rebal required
			holder[i,assetCols]<-holder[i-1,assetCols]
			holder[i,assetVals]<-currentVal
			next
		} 
		
		#print(i)

		targetVal <- assetWeights * totalVal
		excessAmt <- (currentVal - targetVal)
		excessAsset <- as.numeric(excessAmt/allXts[i,])
		
		sttOnExcess <- sum(stt*abs(excessAmt))
		taxOnProfits <- sum(tax*excessAmt[excessAmt > 0])
		totalDrag <- sttOnExcess+taxOnProfits
		
		holder[i,assetCols] <- holder[i-1,assetCols] - excessAsset - as.numeric(totalDrag/3/allXts[i,])
		holder[i,assetVals] <- currentVal - excessAmt - totalDrag/3
	}

	allXts$TOTAL <- rowSums(holder[,assetVals])

	retXts<-merge(dailyReturn(allXts[,1]), dailyReturn(allXts[,2]), dailyReturn(allXts[,3]), dailyReturn(allXts$TOTAL))
	names(retXts)<-c('A1', 'A2', 'A3', 'ALLOC')

	return(retXts)
}

plotResults<-function(toPlot, chartTitle, suffix){
	png(sprintf("%s/cumulative.3-asset.eq-wt.%s.png", reportPath, suffix), width=1400, height=800, bg="white")
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

runAnalysis<-function(){
	#thresholdPct<-c(0.05)
	thresholdPct<-c(c(0.05, 0.1), seq(0.2, 1, 0.2))
	aAllocPct<-rep(1/3, 3)

	allResultDf<-data.frame(matrix(ncol=3,nrow=0))

	for(aT in thresholdPct){
		a1<-runScenario(aAllocPct, aT)
		a2<-runScenario(aAllocPct, aT, 0.1/100, 10/100)
		
		allResultDf<-rbind(allResultDf, c(aT, 0, Return.cumulative(a1$ALLOC)))
		allResultDf<-rbind(allResultDf, c(aT, 1, Return.cumulative(a2$ALLOC)))
		
		plotResults(a1, sprintf("MIDCAP/0-5yr/QQQ Equal Weight (%.0f)", 100*aT), sprintf("%.0f", 100*aT))
		plotResults(a2, sprintf("MIDCAP/0-5yr/QQQ Equal Weight (%.0f)", 100*aT), sprintf("drag.%.0f", 100*aT))
	}
	
	names(allResultDf)<-c('THRESH', 'DRAG', 'CUMULATIVE')
	write.csv(allResultDf, sprintf('%s/cumulative.3-asset.eq-wt.csv', reportPath), row.names = F)
}

createTable1<-function(){
	pdf(NULL)
	retDf<-read.csv(sprintf('%s/cumulative.3-asset.eq-wt.csv', reportPath))
	
	toPrint<- retDf %>%
		group_by(THRESH) %>%
		summarize(NO_TAX = mean(CUMULATIVE[DRAG==0]), TAX = mean(CUMULATIVE[DRAG==1])) %>%
		mutate(DIFF=TAX/NO_TAX-1) %>%
		arrange(desc(DIFF)) %>%
		print()
	
	toPrintDf<-data.frame(toPrint)
	toPrintDf<-round(100*toPrintDf, 2)
	
	tt2<-arrangeGrob(tableGrob(toPrintDf, rows=NULL, theme=mytheme), ncol=1, 
				top = textGrob(sprintf("3-asset EQL"),gp=gpar(fontsize=12, fontfamily='Segoe UI')), 
				bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
	ggsave(sprintf('%s/table.cumulative.3-asset.EQL.png', reportPath), tt2, width=3, height=3, units='in')
}


runAnalysis()
createTable1()

library('RODBC')
library('ggplot2')
library('extrafont')
library('quantmod')
library('PerformanceAnalytics')
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

reportPath <- "."
mytheme <- ttheme_default(
		core = list(fg_params=list(fontfamily='Segoe UI', hjust=1, x=1)),
		colhead = list(fg_params=list(fontfamily='Segoe UI')),
		rowhead = list(fg_params=list(fontfamily='Segoe UI')))

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("2004-01-01")
endDate<-as.Date("2018-09-30")

initialInvestment<-1000000
asset1Name<-"NIFTY MIDCAP 100"
asset1Px<-sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", asset1Name, startDate, endDate))

asset2Name<-"0_5"
asset2Px<-sqlQuery(lcon, sprintf("select time_stamp, tri from INDEX_CCIL_TENOR where index_name = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", asset2Name, startDate, endDate))

allXts<-merge(xts(asset1Px[,2], as.Date(asset1Px[,1])), xts(asset2Px[,2], as.Date(asset2Px[,1])))
names(allXts)<-c('PX_A1', 'PX_A2')
allXts[,2]<-na.locf(allXts[,2])
allXts<-na.omit(allXts)

runScenario<-function(assetWeights, rebalThreshold, stt=0, tax=0){
	allSubsetXts<-allXts
	assetCols<-1:2
	assetVals<-3:4
	holder<-matrix(0L, nrow=length(allSubsetXts[,1]), ncol=4)
	
	holder[1,assetCols]<-assetWeights*initialInvestment/allSubsetXts[1,]
	holder[1,assetVals]<-assetWeights*initialInvestment


	for(i in 2:length(allXts[,1])) {
		currentVal <- as.numeric(allSubsetXts[i,] * holder[i-1, assetCols])
		totalVal <- sum(currentVal)
		currentWt<- currentVal/totalVal
		if(all(currentWt < (1+rebalThreshold) * assetWeights)){
			#no rebal required
			holder[i,assetCols]<-holder[i-1,assetCols]
			holder[i,assetVals]<-currentVal
			next
		}
		
		targetVal <- assetWeights * totalVal
		excessAmt <- (currentVal - targetVal)
		
		sttOnExcess <- sum(stt*abs(excessAmt))
		taxOnProfits <- sum(tax*excessAmt[excessAmt > 0])
		totalDrag <- sttOnExcess+taxOnProfits
		
		excessAsset <- as.numeric(excessAmt/allSubsetXts[i,])
		
		holder[i,assetCols] <- holder[i-1,assetCols] - excessAsset - as.numeric(totalDrag/2/allSubsetXts[i,])
		holder[i,assetVals] <- currentVal - excessAmt - totalDrag/2
	}

	allSubsetXts$TOTAL <- rowSums(holder[,assetVals])

	retXts<-merge(dailyReturn(allSubsetXts[,1]), dailyReturn(allSubsetXts[,2]), dailyReturn(allSubsetXts$TOTAL))
	names(retXts)<-c('A1', 'A2', 'ALLOC')
	#browser()
	return(retXts)
}


plotResults<-function(toPlot, chartTitle, suffix){
	png(sprintf("%s/cumulative.2-asset.%s.png", reportPath, suffix), width=1400, height=800, bg="white")
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
	thresholdPct<-c(c(0.05, 0.1), seq(0.2, 1, 0.2))
	aAllocPct<-seq(0.4, 0.9, 0.1)

	allResultDf<-data.frame(matrix(ncol=4,nrow=0))

	for(aT in thresholdPct){
		resultXts<-NULL
		resultDragXts<-NULL
		for(aa in aAllocPct){
			a1<-runScenario(c(aa, 1-aa), aT)
			if(is.null(resultXts)){
				resultXts<-a1
			} else {
				resultXts<-merge(resultXts, a1$ALLOC)
			}
			
			allResultDf<-rbind(allResultDf, c(aT, aa, 0, Return.cumulative(a1$ALLOC)))
			
			a1<-runScenario(c(aa, 1-aa), aT, 0.1/100, 10/100)
			if(is.null(resultDragXts)){
				resultDragXts<-a1
			} else {
				resultDragXts<-merge(resultDragXts, a1$ALLOC)
			}
			
			allResultDf<-rbind(allResultDf, c(aT, aa, 1, Return.cumulative(a1$ALLOC)))
		}
		names(resultXts)<-c("A", "B", sapply(aAllocPct, function(x) sprintf("A_%.0f", 100*x)))
		names(resultDragXts)<-c("A", "B", sapply(aAllocPct, function(x) sprintf("A_%.0f", 100*x)))
		
		plotResults(resultXts, sprintf("MIDCAP/0-5yr (%.0f)", 100*aT), sprintf("%.0f", 100*aT))
		plotResults(resultDragXts, sprintf("MIDCAP/0-5yr (%.0f)", 100*aT), sprintf("drag.%.0f", 100*aT))
	}

	names(allResultDf)<-c('THRESH', 'A_ALLOC', 'DRAG', 'CUMULATIVE')
	write.csv(allResultDf, sprintf('%s/cumulative.2-asset.csv', reportPath), row.names = F)
}

createTable1<-function(){
	pdf(NULL)
	retDf<-read.csv(sprintf('%s/cumulative.2-asset.csv', reportPath))
	
	toPrint<- retDf %>%
		group_by(THRESH) %>%
		summarize(NO_TAX_MEAN = mean(CUMULATIVE[DRAG==0]), TAX_MEAN = mean(CUMULATIVE[DRAG==1])) %>%
		mutate(DIFF=TAX_MEAN/NO_TAX_MEAN - 1) %>%
		arrange(desc(THRESH)) 
		
	toPrintDf<-data.frame(toPrint)
	toPrintDf<-round(100*toPrintDf, 2)
	
	tt2<-arrangeGrob(tableGrob(toPrintDf, rows=NULL, theme=mytheme), ncol=1, 
				top = textGrob(sprintf("Avg. Returns by threshold and tax difference"),gp=gpar(fontsize=12, fontfamily='Segoe UI')), 
				bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
	ggsave(sprintf('%s/table.cumulative.2-asset.png', reportPath), tt2, width=5, height=3, units='in')
}

createTable2<-function(aa){
	pdf(NULL)
	retDf<-read.csv(sprintf('%s/cumulative.2-asset.csv', reportPath))
	toPrintDf<-retDf[retDf$A_ALLOC == aa, c('THRESH', 'DRAG', 'CUMULATIVE')]
	toPrintDf<-toPrintDf[order(toPrintDf$DRAG, toPrintDf$THRESH),]
	toPrintDf$TAX<-ifelse(toPrintDf$DRAG == 0, "NO", "YES")
	toPrintDf<-toPrintDf[, c('THRESH', 'TAX', 'CUMULATIVE')]
	toPrintDf$CUMULATIVE<-round(toPrintDf$CUMULATIVE*100.0, 2)
	toPrintDf$THRESH<-round(toPrintDf$THRESH*100.0, 0)
	
	tt2<-arrangeGrob(tableGrob(toPrintDf, rows=NULL, theme=mytheme), ncol=1, 
				top = textGrob(sprintf("%.0f%% A1 %.0f%% A2", 100*aa, 100*(1-aa)),gp=gpar(fontsize=12, fontfamily='Segoe UI')), 
				bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
	ggsave(sprintf('%s/table.cumulative.2-asset.%.0f.png', reportPath, 100*aa), tt2, width=3, height=6, units='in')
}

runAnalysis()
#createTable2(0.6)
#createTable1()
#createTable1B()

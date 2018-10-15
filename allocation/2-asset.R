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
		
		png(sprintf("%s\\cumulative.2-asset.%.2f.png", reportPath, 100*aT), bg = "white", width = 1200, height = 600)
		par(family='Segoe UI')
		charts.PerformanceSummary(resultXts, main=NA)
		title(sprintf("Two Asset Portfolio (%.0f) @StockViz", 100*aT, 100*aa))
		mtext(paste(sprintf("%.2f%%", 100*apply(resultXts, 2, Return.cumulative)), collapse=" / "), family='Segoe UI')
		dev.off()
		
		png(sprintf("%s\\cumulative.2-asset.drag.%.2f.png", reportPath, 100*aT), bg = "white", width = 1200, height = 600)
		par(family='Segoe UI')
		charts.PerformanceSummary(resultDragXts, main=NA)
		title(sprintf("Two Asset Portfolio w/tax drag (%.0f) @StockViz", 100*aT, 100*aa))
		mtext(paste(sprintf("%.2f%%", 100*apply(resultDragXts, 2, Return.cumulative)), collapse=" / "), family='Segoe UI')
		dev.off()
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

#runAnalysis()
createTable2(0.6)
#createTable1()
#createTable1B()

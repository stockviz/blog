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

runScenario<-function(assetWeights, rebalThreshold, stt=0, tax=0){
	allXts<-merge(xts(asset1Px[,2], as.Date(asset1Px[,1])), xts(asset2Px[,2], as.Date(asset2Px[,1])))
	names(allXts)<-c('PX_A1', 'PX_A2')

	allXts[,2]<-na.locf(allXts[,2])
	allXts<-na.omit(allXts)

	allXts$A1<-0.0
	allXts$A2<-0.0

	allXts$A1_VAL<-0.0
	allXts$A2_VAL<-0.0

	allXts$A1[1] <- assetWeights[1]*initialInvestment/allXts[1,1]
	allXts$A2[1] <- assetWeights[2]*initialInvestment/allXts[1,2]

	allXts$A1_VAL[1] <- assetWeights[1]*initialInvestment
	allXts$A2_VAL[1] <- assetWeights[2]*initialInvestment

	allDf<-data.frame(allXts)

	for(i in 2:length(allDf[,1])) {
		currentVal1 <- allDf[i,1] * allDf$A1[i-1]
		currentVal2 <- allDf[i,2] * allDf$A2[i-1]
		totalVal <- currentVal1 + currentVal2
		
		currentWt1<- currentVal1/totalVal
		currentWt2<- currentVal2/totalVal
		
		if(currentWt1 >= (1+rebalThreshold) * assetWeights[1]) {
			#rebal
			#print(sprintf("1: %d", i))
			
			targetVal1 <- assetWeights[1] * totalVal
			excessAmt <- (currentVal1 - targetVal1)*(1-stt)*(1-tax)
			
			excessAsset1 <- excessAmt/allDf[i,1]
			excessAsset2 <- excessAmt/allDf[i,2]
			
			allDf$A1[i] <- allDf$A1[i-1] - excessAsset1
			allDf$A2[i] <- allDf$A2[i-1] + excessAsset2
			allDf$A1_VAL[i] <- currentVal1 - excessAmt
			allDf$A2_VAL[i] <- currentVal2 + excessAmt
		} else if (currentWt2 >= (1+rebalThreshold) * assetWeights[2]) {
			#rebal
			#print(sprintf("2: %d", i))
			
			targetVal2 <- assetWeights[2] * totalVal
			excessAmt <- (currentVal2 - targetVal2)*(1-stt)*(1-tax)
			
			excessAsset1 <- excessAmt/allDf[i,1]
			excessAsset2 <- excessAmt/allDf[i,2]
			
			allDf$A1[i] <- allDf$A1[i-1] + excessAsset1
			allDf$A2[i] <- allDf$A2[i-1] - excessAsset2
			allDf$A1_VAL[i] <- currentVal1 + excessAmt
			allDf$A2_VAL[i] <- currentVal2 - excessAmt
		} else {
			allDf$A1[i]<-allDf$A1[i-1]
			allDf$A2[i]<-allDf$A2[i-1]
			allDf$A1_VAL[i]<-currentVal1
			allDf$A2_VAL[i]<-currentVal2
		}
	}

	allDf$TOTAL <- allDf$A1_VAL + allDf$A2_VAL
	resXts<-xts(allDf[, c('PX_A1', 'PX_A2', 'TOTAL')], as.Date(row.names(allDf)))
	retXts<-merge(dailyReturn(resXts[,1]), dailyReturn(resXts[,2]), dailyReturn(resXts$TOTAL))
	names(retXts)<-c('PX_A1', 'PX_A2', 'ALLOC')

	return(retXts)
}

runAnalysis<-function(){
	thresholdPct<-seq(0.05, 0.2, 0.05)
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
		mutate(DIFF=TAX_MEAN/NO_TAX_MEAN) 
		
	toPrintDf<-data.frame(toPrint)
	toPrintDf<-round(100*toPrintDf, 2)
	
	tt2<-arrangeGrob(tableGrob(toPrintDf, rows=NULL, theme=mytheme), ncol=1, 
				top = textGrob(sprintf("Avg. Returns by threshold and tax difference"),gp=gpar(fontsize=12, fontfamily='Segoe UI')), 
				bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
	ggsave(sprintf('%s/table.cumulative.2-asset.avg.tax-diff.png', reportPath), tt2, width=5, height=3, units='in')
}

createTable1B<-function(){
	pdf(NULL)
	retDf<-read.csv(sprintf('%s/cumulative.2-asset.csv', reportPath))
	
	options(tibble.print_max = Inf)
	toPrint<- retDf %>%
		group_by(A_ALLOC, THRESH) %>%
		summarize(NO_TAX = mean(CUMULATIVE[DRAG==0]), TAX = mean(CUMULATIVE[DRAG==1])) %>%
		mutate(DIFF=TAX/NO_TAX-1) %>%
		arrange(desc(DIFF)) %>%
		print()
	
	options(tibble.print_max = 10)
	
	toPrintDf<-data.frame(toPrint)
	toPrintDf<-round(100*toPrintDf, 2)
	
	tt2<-arrangeGrob(tableGrob(toPrintDf, rows=NULL, theme=mytheme), ncol=1, 
				top = textGrob(sprintf("Returns sorted by tax difference"),gp=gpar(fontsize=12, fontfamily='Segoe UI')), 
				bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
	ggsave(sprintf('%s/table.cumulative.2-asset.avg.tax-diff.B.png', reportPath), tt2, width=5, height=8, units='in')
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
				top = textGrob(sprintf("Returns by threshold and tax %.0f%%", 100*aa),gp=gpar(fontsize=12, fontfamily='Segoe UI')), 
				bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
	ggsave(sprintf('%s/table.cumulative.2-asset.png', reportPath, 100*aa), tt2, width=3, height=4, units='in')
}

#runAnalysis()
#createTable2(0.6)
#createTable1()
createTable1B()

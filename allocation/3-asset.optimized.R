library('RODBC')
library('extrafont')
library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')

library('ggplot2')
library('lubridate')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')
library('dplyr')

args <- commandArgs(TRUE)
objectiveToggle <- toString(args[1])
if(objectiveToggle != "ETL" && objectiveToggle != "VAR"){
	print("invalid objective")
	q()
}
print(objectiveToggle)
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

startDate<-as.Date("2004-01-01")
endDate<-as.Date("2018-09-30")

efLookback<-100 #in weeks
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

allXts<-merge(asset1Xts, asset2Xts, asset3Xts)
names(allXts)<-c('PX_A1', 'PX_A2', 'PX_A3')

allXts[,2]<-na.locf(allXts[,2])
allXts<-na.omit(allXts)

returnsXts<-merge(weeklyReturn(allXts[,1]), weeklyReturn(allXts[,2]), weeklyReturn(allXts[,3]))
returnsXts<-returnsXts[-1,]
names(returnsXts)<-names(allXts)

startDate<-index(returnsXts[efLookback,1])
allSubsetXts<-allXts[sprintf("%s/", startDate),]

pspec <- portfolio.spec(assets=names(allXts))
pspec <- add.constraint(pspec, "full_investment")
pspec <- add.constraint(pspec, "long_only")
pspec <- add.constraint(pspec, "box", min=0.1, max=0.5)

if(objectiveToggle == "ETL") {
	pspec <- add.objective(portfolio=pspec, type="risk", name="ETL", arguments=list(p=0.95))
} else if(objectiveToggle == "VAR") {
	pspec <- add.objective(portfolio=pspec, type="risk", name="var")
} else {
	print("invalid objective")
	q()
}

pspec <- add.objective(portfolio=pspec, type="return", name="mean")

getOptimWeights<-function(asof){
	print(asof)
	opOut<-optimize.portfolio(tail(returnsXts[sprintf("/%s", asof),], efLookback), portfolio = pspec)
	###chart.EfficientFrontier(opOut, match.col="ETL" or "var", n.portfolios=25, type="l")
	return(as.numeric(extractWeights(opOut)))
}

assetWeightDf<-data.frame(THRESH=0.0, DRAG=0, DATE="", A1=0.0, A2=0.0, A3=0.0)

runScenario<-function(rebalThreshold, stt=0, tax=0){
	assetWeights<-getOptimWeights(startDate)
	assetWeightDf<<-rbind(assetWeightDf, c(rebalThreshold, stt+tax, toString(startDate), assetWeights[1],assetWeights[2],assetWeights[3]))
	
	assetCols<-1:3
	assetVals<-4:6
	holder<-matrix(0L, nrow=length(allSubsetXts[,1]), ncol=6)
	
	holder[1,assetCols]<-assetWeights*initialInvestment/allSubsetXts[1,]
	holder[1,assetVals]<-assetWeights*initialInvestment

	for(i in 2:length(allSubsetXts[,1])) {
		currentVal <- as.numeric(allSubsetXts[i,] * holder[i-1, assetCols])
		totalVal <- sum(currentVal)
		currentWt<- currentVal/totalVal
		
		if(all(currentWt < (1+rebalThreshold) * assetWeights)){
			#no rebal required
			holder[i,assetCols]<-holder[i-1,assetCols]
			holder[i,assetVals]<-currentVal
			next
		} 
		
		#print(i)
		asof<-index(allSubsetXts[i,1])
		assetWeights<-getOptimWeights(asof)
		assetWeightDf<<-rbind(assetWeightDf, c(rebalThreshold, stt+tax, toString(asof), assetWeights[1],assetWeights[2],assetWeights[3]))
		
		targetVal <- assetWeights * totalVal
		excessAmt <- (currentVal - targetVal)
		excessAsset <- as.numeric(excessAmt/allSubsetXts[i,])
		
		sttOnExcess <- sum(stt*abs(excessAmt))
		taxOnProfits <- sum(tax*excessAmt[excessAmt > 0])
		totalDrag <- sttOnExcess+taxOnProfits
		
		holder[i,assetCols] <- holder[i-1,assetCols] - excessAsset - as.numeric(totalDrag/3/allSubsetXts[i,])
		holder[i,assetVals] <- currentVal - excessAmt - totalDrag/3
	}

	allSubsetXts$TOTAL <- rowSums(holder[,assetVals])

	retXts<-merge(dailyReturn(allSubsetXts[,1]), dailyReturn(allSubsetXts[,2]), dailyReturn(allSubsetXts[,3]), dailyReturn(allSubsetXts$TOTAL))
	names(retXts)<-c('A1', 'A2', 'A3', 'ALLOC')

	return(retXts)
}

plotResults<-function(toPlot, chartTitle, suffix){
	png(sprintf("%s/cumulative.3-asset.optim.%s.%s.png", reportPath, objectiveToggle, suffix), width=1400, height=800, bg="white")
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
	thresholdPct<-seq(0.05, 0.2, 0.05)

	allResultDf<-data.frame(matrix(ncol=3,nrow=0))

	for(aT in thresholdPct){
		a1<-runScenario(aT)
		a2<-runScenario(aT, 0.1/100, 10/100)
		
		allResultDf<-rbind(allResultDf, c(aT, 0, Return.cumulative(a1$ALLOC)))
		allResultDf<-rbind(allResultDf, c(aT, 1, Return.cumulative(a2$ALLOC)))
		
		plotResults(a1, sprintf("MIDCAP/0-5yr/QQQ %s (%.0f)", objectiveToggle, 100*aT), sprintf("%.0f", 100*aT))
		plotResults(a2, sprintf("MIDCAP/0-5yr/QQQ %s (%.0f)", objectiveToggle, 100*aT), sprintf("drag.%.0f", 100*aT))
	}
	
	names(allResultDf)<-c('THRESH', 'DRAG', 'CUMULATIVE')
	write.csv(allResultDf, sprintf('%s/cumulative.3-asset.optim.%s.csv', reportPath, objectiveToggle), row.names = F)
	
	assetWeightDf<<-assetWeightDf[-1,]
	assetWeightDf$DRAG<-ifelse(assetWeightDf$DRAG > 0, 1, 0)
	write.csv(assetWeightDf, sprintf('%s/cumulative.3-asset.optim.%s.weights.csv', reportPath, objectiveToggle), row.names = F)
}

createTable1<-function(){
	pdf(NULL)
	retDf<-read.csv(sprintf('%s/cumulative.3-asset.optim.%s.csv', reportPath, objectiveToggle))
	
	toPrint<- retDf %>%
		group_by(THRESH) %>%
		summarize(NO_TAX = mean(CUMULATIVE[DRAG==0]), TAX = mean(CUMULATIVE[DRAG==1])) %>%
		mutate(DIFF=TAX/NO_TAX-1) %>%
		arrange(desc(THRESH)) %>%
		print()
	
	toPrintDf<-data.frame(toPrint)
	toPrintDf<-round(100*toPrintDf, 2)
	
	tt2<-arrangeGrob(tableGrob(toPrintDf, rows=NULL, theme=mytheme), ncol=1, 
				top = textGrob(sprintf("3-asset %s", objectiveToggle),gp=gpar(fontsize=12, fontfamily='Segoe UI')), 
				bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
	ggsave(sprintf('%s/table.cumulative.3-asset.%s.png', reportPath, objectiveToggle), tt2, width=3, height=2, units='in')
}

createTable2<-function(aT, tax){
	pdf(NULL)
	retDf<-read.csv(sprintf('%s/cumulative.3-asset.optim.%s.weights.csv', reportPath, objectiveToggle))
	
	toPrint<- retDf %>%
		filter(THRESH==aT & DRAG == tax) %>%
		select(DATE, A1, A2, A3) %>%
		mutate(A1=round(100*A1,2),A2=round(100*A2,2),A3=round(100*A3,2)) %>%
		print()
		
	toPrintDf<-data.frame(toPrint)
	toPrintDf$DATE<-as.Date(toPrintDf$DATE)
	
	firstDate<-first(toPrintDf$DATE)
	lastDate<-last(toPrintDf$DATE)
	xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=5)
	
	ggplot(data = melt(toPrintDf, id='DATE'), aes(x = DATE, y=value, color=variable)) +
	  geom_point() +
	  theme_economist() +
	  scale_x_date(breaks = xAxisTicks) +
	  labs(x='', y='asset weights', color='', title=sprintf("Three asset %s weights", objectiveToggle), subtitle=sprintf("[%s:%s]", firstDate, lastDate))+
	  annotate("text", x=lastDate, y=min(toPrintDf[,c(2,3,4)]), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	  
	ggsave(sprintf("%s/3-asset.optim.%s.weights.png", reportPath, objectiveToggle), dpi=600, width=8, height=4, units="in")	
}
	
#runAnalysis()
createTable1()
#createTable2(0.2, 0)

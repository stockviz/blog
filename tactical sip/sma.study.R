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
		
lookbacks<-c(50, 100, 200, 500)
smaLower<-c(3, 5, 10)
indexNames<-c("NIFTY 50", "NIFTY BANK", "NIFTY MIDCAP 100", "NIFTY SMLCAP 100")
dailyRiskFreeRate<-0.05/240

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

retDf<-data.frame(matrix(ncol=3+length(lookbacks), nrow=0))

analyzeScenarios<-function(indexPxts, smaNames, smaFlagNames){
	#invest Rs. 1 every day. If in dd, then buy EQ. Else accumulate cash

	indexAccumulator<-matrix(nrow=length(indexPxts[,1]), ncol=length(lookbacks))
	for(j in 1:length(smaFlagNames)){
		cash<-0.0
		for(i in 1:length(indexPxts[,1])){
			flagName<-smaFlagNames[j]
			smaName<-smaNames[j]
			if(is.na(indexPxts[i, flagName]) || indexPxts[i, flagName] != 0 || indexPxts[i, 'SMA_L'] < indexPxts[i, smaName]){
				#accumulate cash
				cash <- cash*(1+dailyRiskFreeRate) + 1
			} else {
				#liquidate cash and buy index
				indexAccumulator[i, j]<-cash/indexPxts[i,1]
				cash <- 0.0
			}
		}
		if(cash > 0){
			indexAccumulator[i, j]<-cash/indexPxts[i,1]
		}
	}

	scenarioReturns<-colSums(indexAccumulator, na.rm=T)/sum(1/indexPxts[,1])-1

	return(scenarioReturns)
}

runScenarios<-function(indexName, smaL){
	print(indexName)
	indexPx<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where index_name='%s'", indexName))
	indexPxts2<-xts(indexPx[,1], as.Date(indexPx[,2]))

	indexPxts2<-merge(indexPxts2, SMA(indexPxts2[,1], smaL)) #use a smaller sma crossover to generate the signal
	
	for(lb in lookbacks){
		indexPxts2<-merge(indexPxts2, SMA(indexPxts2[,1], lb))
	}

	firstCols<-c("INDEX", 'SMA_L')
	smaNames<-sapply(lookbacks, function(x) sprintf("SMA_%d", x))
	names(indexPxts2)<-c(firstCols, smaNames)

	for(i in smaNames){
		indexPxts2<-merge(indexPxts2, ifelse(indexPxts2$SMA_L > indexPxts2[, i], 1, -1))
	}
	smaCountNames<-sapply(lookbacks, function(x) sprintf("SMA_C_%d", x))
	names(indexPxts2)<-c(firstCols, smaNames, smaCountNames)

	indexPxts2<-na.omit(indexPxts2)
	
	for(i in smaCountNames){
		indexPxts2<-merge(indexPxts2, indexPxts2[,i] + lag(indexPxts2[,i], 1)) #crossover == 0
	}
	smaFlagNames<-sapply(lookbacks, function(x) sprintf("SMA_FLAG_%d", x))
	names(indexPxts2)<-c(firstCols, smaNames, smaCountNames, smaFlagNames)
	
	startYr<-year(first(index(indexPxts2)))
	endYr<-year(first(index(indexPxts2)))
	
	for(kk in startYr:(endYr-4)){
		yearRange<-sprintf("%d/%d", kk, kk+4)
		print(yearRange)
		retDf<<-rbind(retDf, c(indexName, yearRange, smaL, round(100.0*analyzeScenarios(indexPxts2[yearRange,], smaNames, smaFlagNames), 2)))
	}

}

runAll<-function(){
	for(ii in 1:length(indexNames)){
		for(jj in 1:length(smaLower)){
			runScenarios(indexNames[ii], smaLower[jj])
		}
	}
	names(retDf)<-c("INDEX", "YEARS", "SMA", sapply(lookbacks, function(x) sprintf("SMA_%d", x)))
	write.csv(retDf, sprintf('%s/table.sma.asset-diff.csv', reportPath), row.names = F)
}

analyzeData<-function(){
	retDf<-read.csv(sprintf('%s/table.sma.asset-diff.csv', reportPath))

	summaryDt<-retDf %>%
		group_by(INDEX, SMA) %>%
		summarize_at(vars(matches("SMA_*")), funs(min, max, mean))
		
	toPrintDf<-data.frame(summaryDt)
	colNames<-names(toPrintDf)
	toPrintDf[,colNames[grepl("SMA_", colNames)]]<-round(toPrintDf[,colNames[grepl("SMA_", colNames)]], 2)

	tt2<-arrangeGrob(tableGrob(toPrintDf, rows=NULL, theme=mytheme), ncol=1, 
				top = textGrob("Asset Difference (SMA)",gp=gpar(fontsize=12, fontfamily='Segoe UI')), 
				bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
	ggsave(sprintf('%s/table.asset-diff.SMA.png', reportPath), tt2, width=21, height=5, units='in')
}

illustrate<-function(indexName, smaL, smaU){
	indexPx<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where index_name='%s'", indexName))
	indexPxts2<-xts(indexPx[,1], as.Date(indexPx[,2]))
	
	indexPxts2<-merge(indexPxts2, SMA(indexPxts2, smaL)) #use a smaller sma crossover to generate the signal
	indexPxts2<-merge(indexPxts2, SMA(indexPxts2[,1], smaU))
	names(indexPxts2)<-c("INDEX", "SMA_L", "SMA_U")
	
	indexPxts2<-merge(indexPxts2, ifelse(indexPxts2$SMA_L > indexPxts2$SMA_U, 1, -1))	
	names(indexPxts2)[length(names(indexPxts2))] <- "SMA_C"
	
	indexPxts2<-na.omit(indexPxts2)
	indexPxts2<-merge(indexPxts2, indexPxts2$SMA_C + lag(indexPxts2$SMA_C, 1)) #crossover == 0
	names(indexPxts2)[length(names(indexPxts2))] <- "SMA_FLAG"
	
	firstDate<-first(index(indexPxts2))
	lastDate<-last(index(indexPxts2))

	toPlot<-data.frame(indexPxts2[,1], index(indexPxts2[,1]), ifelse(indexPxts2$SMA_L > indexPxts2$SMA_U, 'above', 'below'), ifelse(indexPxts2$SMA_FLAG == 0 & indexPxts2$SMA_L >= indexPxts2$SMA_U, indexPxts2[,1], NA))
	names(toPlot)<-c('INDEX', 'DAY', 'SMA', 'BUY')
	xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)
	
	ggplot(data=toPlot, aes(x=DAY, y=INDEX)) +
		theme_economist() +
		geom_line(aes(color=SMA, group=1)) + 
		geom_point(aes(x=DAY, y=BUY), color='darkgreen', size=1) + 
		scale_x_date(breaks = xAxisTicks) +
		scale_y_log10() +
		labs(x='', y='log()', color='', title=sprintf("%s", indexName), subtitle=sprintf("SMA: %dx%d [%s:%s]", smaL, smaU, firstDate, lastDate)) +
		annotate("text", x=lastDate, y=100, label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/%s.SMA.%dx%d.png", reportPath, indexName, smaL, smaU), dpi=600, width=12, height=6, units="in")	
}

#runAll()
#analyzeData()
#illustrate("NIFTY 50", 3, 200)
illustrate("NIFTY SMLCAP 100", 3, 100)
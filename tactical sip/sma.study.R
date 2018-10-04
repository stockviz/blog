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

analyzeScenarios<-function(indexName, smaL){
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

	indexPxts<-na.omit(indexPxts2)
	
	for(i in smaCountNames){
		indexPxts<-merge(indexPxts, indexPxts[,i] + lag(indexPxts[,i], 1)) #crossover == 0
	}
	smaFlagNames<-sapply(lookbacks, function(x) sprintf("SMA_FLAG_%d", x))
	names(indexPxts)<-c(firstCols, smaNames, smaCountNames, smaFlagNames)

	for(i in 1:length(lookbacks)){
		toPlot<-data.frame(indexPxts[,1], index(indexPxts[,1]), ifelse(indexPxts$SMA_L > indexPxts[, smaNames[i]], 'above', 'below'), ifelse(indexPxts[,smaFlagNames[i]] == 0 & indexPxts[, 2] >= indexPxts[, smaNames[i]], indexPxts[,1], NA))
		names(toPlot)<-c('INDEX', 'DAY', 'SMA', 'BUY')
		xAxisTicks<-seq(from=first(index(indexPxts)), to=last(index(indexPxts)), length.out=10)
		ggplot(data=toPlot, aes(x=DAY, y=INDEX)) +
			theme_economist() +
			geom_line(aes(color=SMA, group=1)) + 
			geom_point(aes(x=DAY, y=BUY), color='darkgreen', size=1) + 
			scale_x_date(breaks = xAxisTicks) +
			labs(x='', y='', color='') + 
			ggtitle(sprintf("%s %dx%d [%s:%s] @StockViz", indexName, smaL, lookbacks[i], first(index(indexPxts[,1])), last(index(indexPxts[,1]))))
			
		ggsave(sprintf("%s/%s.sma.%dx%d.png", reportPath, indexName, smaL, lookbacks[i]), dpi=600, width=12, height=6, units="in")
	}
	
	indexAccumulator<-matrix(nrow=length(indexPxts[,1]), ncol=length(lookbacks))
	for(j in 1:length(smaFlagNames)){
		cash<-0.0
		for(i in 1:length(indexPxts[,1])){
			flagName<-smaFlagNames[j]
			smaName<-smaNames[j]
			if(is.na(indexPxts[i, flagName]) || indexPxts[i, flagName] != 0 || indexPxts[i, 2] < indexPxts[i, smaName]){
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

	
	return(colSums(indexAccumulator, na.rm=T)/sum(1/indexPxts[,1])-1)
}

retDf<-data.frame(matrix(ncol=2+length(lookbacks), nrow=0))

for(ii in 1:length(indexNames)){
	for(jj in 1:length(smaLower)){
		retDf<-rbind(retDf, c(indexNames[ii], smaLower[jj], round(100.0*analyzeScenarios(indexNames[ii], smaLower[jj]), 2)))
	}
}
names(retDf)<-c("INDEX", "SMA", sapply(lookbacks, function(x) sprintf("SMA_%d", x)))

tt2<-arrangeGrob(tableGrob(retDf, rows=NULL, theme=mytheme), ncol=1, 
			top = textGrob("Asset Difference",gp=gpar(fontsize=12, fontfamily='Segoe UI')), 
			bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
ggsave(sprintf('%s/table.asset-diff.png', reportPath), tt2, width=6, height=5, units='in')

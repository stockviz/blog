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

#reportPath <- "D:/StockViz/public/blog/tactical sip"
reportPath <- "."
source("d:/stockviz/r/config.r")

options(stringsAsFactors = FALSE)
options("scipen"=100)

mytheme <- ttheme_default(
		core = list(fg_params=list(fontfamily='Segoe UI', hjust=1, x=1)),
		colhead = list(fg_params=list(fontfamily='Segoe UI')),
		rowhead = list(fg_params=list(fontfamily='Segoe UI')))

dailyRiskFreeRate<-0.05/240
indexNames<-c("NIFTY 50", "NIFTY BANK", "NIFTY MIDCAP 100", "NIFTY SMLCAP 100")
ddLbs <- c(50, 100, 200, 500) #period over which drawdown has to be calculated
#ddLbs <- c(50, 100)
ddThresholds <- c(-5, -10, -15, -20, -25) #invest in equity only if it is in a drawdown higher than this threshold

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
retDf<-data.frame(matrix(ncol=3+length(ddThresholds), nrow=0))

analyzeScenarios<-function(indexPxts, ddColNames, ddThresh){
	#print(ddThresh)
	#invest Rs. 1 every day. If in dd, then buy EQ. Else accumulate cash

	indexAccumulator<-matrix(nrow=length(indexPxts[,1]), ncol=length(ddLbs))
	for(j in 1:length(ddColNames)){
		cash<-0.0
		for(i in 1:length(indexPxts[,1])){
			flagName<-ddColNames[j]
			if(indexPxts[i, flagName] > ddThresh){
				#accumulate cash
				cash <- cash*(1+dailyRiskFreeRate) + 1
			} else {
				#liquidate cash and buy index
				indexAccumulator[i, j]<-cash/indexPxts[i,1] + 1/indexPxts[i,1]
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

runScenarios<-function(indexName){
	print(indexName)
	index1<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE FROM BHAV_INDEX WHERE INDEX_NAME='%s'", indexName))
	nXts<-xts(index1[,-1], as.Date(index1[,1]))
	
	indexPxts2<-nXts
	for(ddLb in ddLbs){
		maxRolling<-rollapply(nXts, ddLb, max)
		indexPxts2<-merge(indexPxts2, maxRolling)
	}
	maxColNames<-sapply(ddLbs, function(x) sprintf("MAX_%d", x))
	names(indexPxts2)<-c("INDEX", maxColNames)

	indexPxts2<-na.omit(indexPxts2)
	
	for(i in maxColNames){
		indexPxts2<-merge(indexPxts2, 100*(indexPxts2[,1]/indexPxts2[,i]-1))
	}
	
	ddColNames<-sapply(ddLbs, function(x) sprintf("DD_%d", x))
	names(indexPxts2)<-c("INDEX", maxColNames, ddColNames)
	
	startYr<-year(first(index(indexPxts2)))
	endYr<-year(last(index(indexPxts2)))
	
	for(jj in 1:length(ddThresholds)){
		for(kk in startYr:(endYr-4)){
			yearRange<-sprintf("%d/%d", kk, kk+4)
			print(yearRange)
			retDf<<-rbind(retDf, c(indexName, yearRange, ddThresholds[jj], round(100.0*analyzeScenarios(indexPxts2[yearRange,], ddColNames, ddThresholds[jj]), 2)))
		}
	}
}

runAll<-function(){
	for(ii in 1:length(indexNames)){
		runScenarios(indexNames[ii])
	}

	names(retDf)<-c("INDEX", "YEARS", "THRESHOLD", sapply(ddLbs, function(x) sprintf("DD_%d", x)))
	write.csv(retDf, sprintf('%s/table.dd.asset-diff.csv', reportPath), row.names = F)
}

analyzeData<-function(){
	retDf<-read.csv(sprintf('%s/table.dd.asset-diff.csv', reportPath))

	summaryDt<-retDf %>%
		group_by(INDEX, THRESHOLD) %>%
		summarize_at(vars(matches("DD_*")), funs(min, max, mean))
		
	toPrintDf<-data.frame(summaryDt)
	colNames<-names(toPrintDf)
	toPrintDf[,colNames[grepl("DD_", colNames)]]<-round(toPrintDf[,colNames[grepl("DD_", colNames)]], 2)

	tt2<-arrangeGrob(tableGrob(toPrintDf, rows=NULL, theme=mytheme), ncol=1, 
				top = textGrob("Asset Difference (Drawdowns)",gp=gpar(fontsize=12, fontfamily='Segoe UI')), 
				bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
	ggsave(sprintf('%s/table.asset-diff.DD.png', reportPath), tt2, width=18, height=7, units='in')
}

illustrate<-function(indexName, ddLb, ddThresh){
	index1<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE FROM BHAV_INDEX WHERE INDEX_NAME='%s'", indexName))
	nXts<-xts(index1[,-1], as.Date(index1[,1]))
	
	maxRolling<-rollapply(nXts, ddLb, max)
	indexPxts<-merge(nXts, maxRolling)
	indexPxts<-na.omit(indexPxts)
	indexPxts$DIFF<-100*(indexPxts[,1]/indexPxts[,2]-1)
	
	firstDate<-first(index(indexPxts))
	lastDate<-last(index(indexPxts))

	toPlot<-data.frame(indexPxts[,1], index(indexPxts[,1]), ifelse(indexPxts$DIFF < ddThresh, 'in dd', 'out'))
	names(toPlot)<-c('INDEX', 'DAY', 'BUY')
	xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)

	ggplot(data=toPlot, aes(x=DAY, y=INDEX)) +
		theme_economist() +
		geom_line(aes(color=BUY, group=1)) + 
		scale_x_date(breaks = xAxisTicks) +
		scale_y_log10() +
		labs(x='', y='log()', color='', title=sprintf("%s", indexName), subtitle=sprintf("lookback: %d; threshold: %d [%s:%s]", ddLb, ddThresh, firstDate, lastDate)) +
		annotate("text", x=lastDate, y=100, label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/%s.DD.%d.%d.png", reportPath, indexName, ddLb, ddThresh), dpi=600, width=12, height=6, units="in")	
}

runAll()
analyzeData()
illustrate("NIFTY 50", 100, -10)
#for(lb in ddLbs){
#	for(thresh in ddThresholds){
#		illustrate("NIFTY MIDCAP 100", lb, thresh)
#	}
#}

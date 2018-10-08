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
		
indexNames<-c("NIFTY 50", "NIFTY BANK", "NIFTY MIDCAP 100", "NIFTY SMLCAP 100")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

retDf<-data.frame(matrix(ncol=4, nrow=0))

analyzeScenarioDaily<-function(indexPxts){
	#invest Rs. 1 every day. 
	return(sum(indexPxts$UNITS<-1/indexPxts[,1]))
}

analyzeScenarioMonthly<-function(indexPxts, numDayCountDf){
	#invest @Rs. 1 every day at the end of the month
	return(sum(numDayCountDf$NUM_DAY/indexPxts[sprintf("%4d-%02d-%02d", numDayCountDf$YEAR, numDayCountDf$MONTH, numDayCountDf$LAST_DAY),1]))
}


runScenarios<-function(indexName){
	print(indexName)
	indexPx<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where index_name='%s'", indexName))
	indexPxts2<-xts(indexPx[,1], as.Date(indexPx[,2]))
	
	names(indexPxts2)<-c("INDEX")

	startYr<-year(first(index(indexPxts2[,1])))
	endYr<-year(last(index(indexPxts2[,1])))
	
	indexPxts2$MONTH<-month(index(indexPxts2))
	indexPxts2$YEAR<-year(index(indexPxts2))
	indexPxts2$DAY<-day(index(indexPxts2))
	
	numDayCount<- data.frame(indexPxts2[, -1]) %>% group_by(YEAR, MONTH) %>% summarize(LAST_DAY=max(DAY), NUM_DAY=n())
	
	for(kk in startYr:(endYr-4)){
		yearRange<-sprintf("%d/%d", kk, kk+4)
		print(yearRange)
		
		retDf<<-rbind(retDf, c(indexName, yearRange, "DAILY", round(100.0*analyzeScenarioDaily(indexPxts2[yearRange,]), 2)))
		
		numDayCountDf<-data.frame(numDayCount %>% filter(YEAR >= kk, YEAR <= kk+4))
		retDf<<-rbind(retDf, c(indexName, yearRange, "MONTHLY", round(100.0*analyzeScenarioMonthly(indexPxts2[yearRange,], numDayCountDf), 2)))
	}
}

runAll<-function(){
	for(ii in 1:length(indexNames)){
		runScenarios(indexNames[ii])
	}
	names(retDf)<-c("INDEX", "YEARS", "TYPE", "TOTAL_UNITS")
	write.csv(retDf, sprintf('%s/table.daily-monthly-sip.asset-diff.csv', reportPath), row.names = F)
}

analyzeData<-function(){
	retDf<-read.csv(sprintf('%s/table.daily-monthly-sip.asset-diff.csv', reportPath))
	
	toPrintDf<-data.frame(retDf %>% group_by(INDEX, YEARS) %>% summarize(DIFF=mean(100*(TOTAL_UNITS[TYPE=='MONTHLY']/TOTAL_UNITS[TYPE=='DAILY']-1))))
	toPrintDf$DIFF<-round(toPrintDf$DIFF, 2)
	
	tt2<-arrangeGrob(tableGrob(toPrintDf, rows=NULL, theme=mytheme), ncol=1, 
				top = textGrob("Asset Difference (daily vs. monthly)",gp=gpar(fontsize=12, fontfamily='Segoe UI')), 
				bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
	ggsave(sprintf('%s/table.asset-diff.daily-monthly-sip.png', reportPath), tt2, width=4, height=20, units='in')
}


#runAll()
analyzeData()

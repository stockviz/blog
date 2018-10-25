library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
dataPath <- "."

reportPath <- "D:/StockViz/public/blog/entries and exits/binary"

source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

runSim<-function(iXts, exitThreshold, entryThreshold){
	iXts$IS_LONG<-0
	iXts$IS_LONG[1]<-1
	
	entryIndex<-1
	exitIndex<-1

	for(i in 2:nrow(iXts)){
		if(iXts$IS_LONG[i-1] == 1){
			profit<-Return.cumulative(iXts$RET[(entryIndex+1):i])
			if(profit < exitThreshold){
				iXts$IS_LONG[i] <- 1
			} else {
				iXts$IS_LONG[i] <- 0
				exitIndex<-i
			}
		} else {
			if(maxDrawdown(iXts$RET[(exitIndex+1):i]) > entryThreshold){
				iXts$IS_LONG[i] <- 1
				entryIndex <- i
			}
		}
	}

	return(iXts$RET_LAG_1*iXts$IS_LONG)
}

analyzeIndex<-function(indexName){
	pxSeries<-sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s'", indexName))

	allXts<-xts(pxSeries[,1], as.Date(pxSeries[,2]))
	allXts<-merge(allXts, dailyReturn(allXts[,1]))
	allXts<-merge(allXts, lag(allXts[,2],-1))

	allXts<-allXts[-1,]
	allXts<-allXts[-nrow(allXts),]
	names(allXts)<-c("INDEX", "RET", "RET_LAG_1")

	resultXts <- allXts$RET_LAG_1
	rNames<-c('BH')

	thresholds<-seq(from=0.05, to=0.8, by=0.05)

	for(i in thresholds){
		for(j in thresholds){
			rXts<-runSim(allXts, as.numeric(i), as.numeric(j))
			resultXts<-merge(resultXts, rXts)
			sName<-sprintf("S-%.2f-%.2f", i, j)
			rNames<-c(rNames, sName)
			names(resultXts)<-rNames
		}
	}

	years<-sort(unique(year(index(resultXts))))
	years<-years[-1]
	years<-years[-length(years)]

	resultDf<-data.frame(YRS="", BH=0.0, RET_SEN="", RET_MAX=0.0, IR_SEN="", RET_IR=0.0)
	for(yr in 10:length(years)){
		dateRange<-sprintf("%d/%d", years[yr-9], years[yr])
		bhRet<-Return.cumulative(resultXts[dateRange,1])
		
		bestScenarioRetName<-rNames[2]
		bestScenarioRet<-Return.cumulative(resultXts[dateRange,2])

		for(i in 3:length(resultXts[1,])){
			sRet<-Return.cumulative(resultXts[dateRange,i])

			if(bestScenarioRet < sRet){
				bestScenarioRet<-sRet
				bestScenarioRetName<- rNames[i]
			}
		}
		
		bestScenarioIrName<-rNames[2]
		bestScenarioIr<-InformationRatio(resultXts[dateRange,2], resultXts[dateRange,1])

		for(i in 3:length(resultXts[1,])){
			sRet<-InformationRatio(resultXts[dateRange,i], resultXts[dateRange,1])
			if(is.na(sRet)) next

			if(bestScenarioIr < sRet){
				bestScenarioIr<-sRet
				bestScenarioIrName<- rNames[i]
			}
		}
		
		bestScenarioIrRet<-Return.cumulative(resultXts[dateRange, bestScenarioIrName])
		
		resultDf<-rbind(resultDf, c(dateRange, bhRet, bestScenarioRetName, bestScenarioRet, bestScenarioIrName, bestScenarioIrRet))
	}

	resultDf2<-resultDf[-1,]
	resultDf2$BH<-round(as.numeric(resultDf2$BH), 2)
	resultDf2$RET_MAX<-round(as.numeric(resultDf2$RET_MAX), 2)
	resultDf2$RET_IR<-round(as.numeric(resultDf2$RET_IR), 2)

	write.csv(resultDf2, file=sprintf('%s/simple.entry.exit.%s.csv', reportPath, indexName), row.names =F)
}

analyzeIndex2<-function(indexName, exitThreshold, entryThreshold){
	pxSeries<-sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s'", indexName))

	allXts<-xts(pxSeries[,1], as.Date(pxSeries[,2]))
	allXts<-merge(allXts, dailyReturn(allXts[,1]))
	allXts<-merge(allXts, lag(allXts[,2],-1))

	allXts<-allXts[-1,]
	allXts<-allXts[-nrow(allXts),]
	names(allXts)<-c("INDEX", "RET", "RET_LAG_1")

	resultXts <- allXts$RET_LAG_1
	rXts<-runSim(allXts, exitThreshold, entryThreshold)
	resultXts<-merge(resultXts, rXts)
	scenName<-sprintf("S-%.2f-%.2f", exitThreshold, entryThreshold)
	names(resultXts)<-c('BH', scenName)
	plotCumReturns(resultXts, sprintf("%s [%.2f%%:%.2f%%]", indexName, 100*exitThreshold, 100*entryThreshold), sprintf('%s/simple.entry.exit.cumulative.%s.%s.png', reportPath, indexName, scenName))
	
	annualizedDf<-data.frame(Y=0, BH=0.0, SCEN=0.0)
	years<-sort(unique(year(index(resultXts))))
	for(y in years){
		bhAnn<-Return.cumulative(resultXts[sprintf("%d", y),1])
		scAnn<-Return.cumulative(resultXts[sprintf("%d", y),2])
		
		annualizedDf<-rbind(annualizedDf, c(y, 100*bhAnn, 100*scAnn))
	}
	
	annualizedDf<-annualizedDf[-1,]
	ggplot(melt(annualizedDf, id='Y'), aes(x=Y, y=value, fill=variable)) +
		theme_economist() +
		geom_bar(stat="identity", position=position_dodge()) + 
		scale_x_continuous(breaks=annualizedDf$Y) +
		geom_text(aes(label=round(value, 2)), vjust=1.6, color="black", position = position_dodge(0.9), size=2.5) +
		xlab("Year") +
		labs(x = "", y="Returns (%)", fill="", title=sprintf("%s Entry/Exit", indexName), subtitle=sprintf("[%.2f%%:%.2f%%]", 100*exitThreshold, 100*entryThreshold)) 
	ggsave(sprintf('%s/simple.entry.exit.annual.%s.%s.png', reportPath, indexName, scenName), width=25, height=12, units="in", device="png")
}

mytheme <- ttheme_minimal(
		core = list(fg_params=list(fontfamily='Segoe UI')),
		colhead = list(fg_params=list(fontfamily='Segoe UI')),
		rowhead = list(fg_params=list(fontfamily='Segoe UI')))

writeCsv2Png<-function(indexName){
	pdf(NULL)
	resultDf2<-read.csv(sprintf('%s/simple.entry.exit.%s.csv', reportPath, indexName))
	
	tt1<-arrangeGrob(tableGrob(resultDf2, rows=NULL, theme=mytheme), ncol=1, 
		top=textGrob(sprintf("%s Simple Exit and Entry Strategy", indexName), gp=gpar(fontsize=14, fontface ='bold', fontfamily='Segoe UI')), 
		bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))

	ggplot2::ggsave(sprintf('%s/simple.entry.exit.%s.png', reportPath, indexName), tt1, width=6, height=nrow(resultDf2)/2, units='in')
}

plotCumReturns<-function(toPlot, chartTitle, fileName){
	pdf(NULL)
	png(fileName, width=1400, height=800, bg="white")
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

analyzeIndex('NIFTY 50')
analyzeIndex('NIFTY MIDCAP 100')
analyzeIndex('NIFTY SMLCAP 100')

writeCsv2Png('NIFTY 50')
writeCsv2Png('NIFTY MIDCAP 100')
writeCsv2Png('NIFTY SMLCAP 100')
		

analyzeIndex2('NIFTY 50', 0.35, 0.1)	
analyzeIndex2('NIFTY MIDCAP 100', 0.7, 0.1)
analyzeIndex2('NIFTY SMLCAP 100', 0.8, 0.15)
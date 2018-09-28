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
indexName<-'NIFTY MIDCAP 100'
periodDays<-c(50, 100, 200)

mytheme <- ttheme_default(
		core = list(fg_params=list(fontfamily='Segoe UI', hjust=1, x=1)),
		colhead = list(fg_params=list(fontfamily='Segoe UI')),
		rowhead = list(fg_params=list(fontfamily='Segoe UI')))

		
lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

plotDentistyCharts<-function(plotXts, bhPlotXts){
	plotXts<-plotXts*100.0
	bhPlotXts<-bhPlotXts*100.0
	
	bhColNames<-gsub("LAG_", "", names(bhPlotXts))
	lowColNames<-names(plotXts)[grepl('LOW', names(plotXts))]
	highColNames<-names(plotXts)[grepl('HIGH', names(plotXts))]
	
	lows<-plotXts[, lowColNames]
	highs<-plotXts[, highColNames]
	colors<-rainbow(length(lows[1,]))
	ctrlColors<-gray((1:length(bhPlotXts[1,]))/(2*length(bhPlotXts[1,])))

	areas<-data.frame(KEY="", LTZ=0.0, GTZ=0.0)
	bhDensities<-c()
	for(i in 1:length(bhPlotXts[1,])){
		bhDensities<-rbind(bhDensities, density(bhPlotXts[,i], na.rm=T))
		distFun<-ecdf(as.numeric(na.omit(bhPlotXts[,i])))
		areas<-rbind(areas, c(bhColNames[i], round(100*distFun(0.0),2), round(100*(1-distFun(0.0)), 2)))
	}

	lowsDensities<-c()
	for(i in 1:length(lows[1,])){
		lowsDensities<-rbind(lowsDensities, density(lows[,i], na.rm=T))
		distFun<-ecdf(as.numeric(na.omit(lows[,i])))
		areas<-rbind(areas, c(lowColNames[i], round(100*distFun(0.0),2), round(100*(1-distFun(0.0)), 2)))
	}

	highsDensities<-c()
	for(i in 1:length(highs[1,])){
		highsDensities<-rbind(highsDensities, density(highs[,i], na.rm=T))
		distFun<-ecdf(as.numeric(na.omit(highs[,i])))
		areas<-rbind(areas, c(highColNames[i], round(100*distFun(0.0),2), round(100*(1-distFun(0.0)), 2)))
	}
	
	startYr<-year(index(first(plotXts)))
	endYr<-year(index(last(plotXts)))

	areas<-areas[-1,]
	tt2<-arrangeGrob(tableGrob(areas, rows=NULL, theme=mytheme), ncol=1, 
		top = textGrob(sprintf("Probability %d:%d", startYr, endYr),gp=gpar(fontsize=12, fontfamily='Segoe UI')), 
		bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
	ggplot2::ggsave(sprintf('%s/table.%s.%s.png', reportPath, indexName, startYr, endYr), tt2, width=3, height=7, units='in')
	
	png(sprintf("%s/%s.lows.%d.%d.png", reportPath, indexName, startYr, endYr), bg='white', width=1200, height=700)
	par(family='Segoe UI')
	plot(range(lowsDensities[,1], bhDensities[,1]), range(lowsDensities[,2], bhDensities[,2]), type = "n", xlab = "Returns", ylab = "Density", main=sprintf('%s Lows Returns Density @StockViz', indexName))
	for(i in 1:length(bhDensities[,1])){
		lines(bhDensities[i,], col = ctrlColors[i], lwd=2)
	}
	for(i in 1:length(lowsDensities[,1])){
		lines(lowsDensities[i,], col = colors[i])
	}
	mtext(text=sprintf("%d:%d", startYr, endYr), family='Segoe UI')
	legend('topleft', legend=c(names(bhPlotXts), gsub("LOW_", "", lowColNames)), col=c(ctrlColors, colors), lty=1)
	dev.off()
	
	png(sprintf("%s/%s.highs.%d.%d.png", reportPath, indexName, startYr, endYr), bg='white', width=1200, height=700)
	par(family='Segoe UI')
	plot(range(highsDensities[,1], bhDensities[,1]), range(highsDensities[,2], bhDensities[,2]), type = "n", xlab = "Returns", ylab = "Density", main=sprintf('%s Highs Returns Density @StockViz', indexName))
	for(i in 1:length(bhDensities[,1])){
		lines(bhDensities[i,], col = ctrlColors[i], lwd=2)
	}
	for(i in 1:length(highsDensities[,1])){
		lines(highsDensities[i,], col = colors[i])
	}
	mtext(text=sprintf("%d:%d", startYr, endYr), family='Segoe UI')
	legend('topleft', legend=c(names(bhPlotXts), gsub("HIGH_", "", highColNames)), col=c(ctrlColors, colors), lty=1)
	dev.off()
	
	
}

indexPx<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where index_name='%s'", indexName))
indexPxts2<-xts(indexPx[,1], as.Date(indexPx[,2]))
indexPxts2<-merge(indexPxts2, dailyReturn(indexPxts2))

tNames<-c('INDEX', 'DAILY_RETURN')
names(indexPxts2)<-tNames

for(lDay in periodDays){
	indexPxts2<-merge(indexPxts2, rollapply(indexPxts2[,2], lDay, Return.cumulative))
}
rNames<-sapply(periodDays, function(x) sprintf("RET_%d", x))
names(indexPxts2)<-c(tNames, rNames)

for(i in 1:length(periodDays)){
	lDay<-periodDays[i]
	indexPxts2<-merge(indexPxts2, lag(indexPxts2[,rNames[i]], -lDay))
}

rlNames<-sapply(periodDays, function(x) sprintf("LAG_RET_%d", x))
names(indexPxts2)<-c(tNames, rNames, rlNames)

retXts<-xts()
retNames<-c()

for(i in 1:length(periodDays)){
	for(j in 1:length(periodDays)){
		lbDay<-periodDays[i]
		lfDay<-periodDays[j]

		indexPxts<-merge(indexPxts2, rollapply(indexPxts2[,1], lbDay, min), rollapply(indexPxts2[,1], lbDay, max))
		names(indexPxts)<-c(tNames, rNames, rlNames, "LOW", "HIGH")
		indexPxts$NEW_LOW<-indexPxts$LOW-lag(indexPxts$LOW,1)
		indexPxts$NEW_HIGH<-indexPxts$HIGH-lag(indexPxts$HIGH,1)

		retXts <- merge(retXts, ifelse(indexPxts$NEW_LOW < 0, indexPxts[, sprintf("LAG_RET_%d", lfDay)], NA))
		retXts <- merge(retXts, ifelse(indexPxts$NEW_HIGH > 0, indexPxts[, sprintf("LAG_RET_%d", lfDay)], NA))

		retNames<-c(retNames, sprintf("LOW_%d_%d", lbDay, lfDay), sprintf("HIGH_%d_%d", lbDay, lfDay))
		names(retXts)<-retNames
	}
}

plotDentistyCharts(retXts, indexPxts[,rlNames])
plotDentistyCharts(retXts["2010/",], indexPxts["2010/",rlNames])


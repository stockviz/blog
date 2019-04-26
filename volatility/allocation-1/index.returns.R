library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."
indexName1<-"NIFTY 50 TR"
indexName2<-"NIFTY MIDCAP 100 TR"
startDate<-as.Date("2003-01-01")
endDate<-as.Date("2019-04-25")
lb<-200 #weeks

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 13 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

pxDf1<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where INDEX_NAME='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName1, startDate, endDate))
pxDf2<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where INDEX_NAME='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName2, startDate, endDate))

pXts1<-xts(pxDf1[,1], as.Date(pxDf1[,2]))
pXts2<-xts(pxDf2[,1], as.Date(pxDf2[,2]))

dXts<-na.omit(merge(weeklyReturn(pXts1), weeklyReturn(pXts2)))
dXts<-dXts[-1,]
dXts<-dXts[-nrow(dXts),]
names(dXts)<-c(indexName1, indexName2)

sdXts<-na.omit(merge(rollapply(dXts[,1], lb, sd), rollapply(dXts[,2], lb, sd)))
names(sdXts)<-c(indexName1, indexName2)

######### plot standard deviation

sdDf<-data.frame(sdXts)
sdDf$T<-index(sdXts)
sdMelt<-melt(sdDf, id='T')

firstDate<-first(index(sdXts))
lastDate<-last(index(sdXts))
xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)

pdf(NULL)
ggplot(sdMelt, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	geom_line() +
	scale_x_date(breaks = xAxisTicks) +
	labs(x='', y='sd', color='', title=sprintf("%s/%s Standard Deviation", indexName1, indexName2), subtitle=sprintf("%d week returns [%s:%s]", lb, first(index(dXts)), last(index(dXts)))) +
	annotate("text", x=lastDate, y=min(sdXts, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
ggsave(sprintf("%s/%s.%s.SD.png", reportPath, indexName1, indexName2), width=16, height=8, units="in")

######### plot ratio of standard deviation

ratio<-sdXts[,2]/sdXts[,1]
ratioDf<-data.frame(ratio)
names(ratioDf)<-c('Ratio')
ratioDf$T<-index(ratio)

ggplot(ratioDf, aes(x=T, y=Ratio)) +
	theme_economist() +
	geom_line() +
	scale_x_date(breaks = xAxisTicks) +
	labs(x='', y='sd ratio', color='', title=sprintf("%s/%s Ratio of Standard Deviations", indexName2, indexName1), subtitle=sprintf("%d week returns [%s:%s]", lb, first(index(dXts)), last(index(dXts)))) +
	annotate("text", x=lastDate, y=min(ratio, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
ggsave(sprintf("%s/%s.%s.SD-ratio.png", reportPath, indexName1, indexName2), width=16, height=8, units="in")


#################################################################

### analyze different allocation scenarios

bondPx<-sqlQuery(lcon, sprintf("select TRI, TIME_STAMP from INDEX_CCIL_TENOR where INDEX_NAME='0_5' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
bondRets<-weeklyReturn(xts(bondPx$TRI, as.Date(bondPx$TIME_STAMP)))
bondRets<-bondRets[-1,]
bondRets<-bondRets[-nrow(bondRets),]

eqRets1<-xts(coredata(dXts[,1]), round_date(index(dXts), unit="week"))
eqRets2<-xts(coredata(dXts[,2]), round_date(index(dXts), unit="week"))
bdRets<-xts(coredata(bondRets[,1]), round_date(index(bondRets), unit="week"))

bondAllocPct<-seq(5, 95, by=5)
allocRetXts<-NULL
for(bap in bondAllocPct){
	nrmRets<-na.omit(merge(eqRets2*(1-bap/100), bdRets*bap/100))
	allocRetXts<-merge.xts(allocRetXts, nrmRets[,1]+nrmRets[,2])
}
names(allocRetXts)<-sapply(bondAllocPct, function(X) sprintf("B%02.0f", X))

### calculate individual stand deviations
allocSd<-NULL
for(j in 1:ncol(allocRetXts)){
	allocSd<-merge.xts(allocSd, rollapply(allocRetXts[,j], lb, sd))
}

sdXts1<-na.omit(rollapply(eqRets1, lb, sd))
allocSdRatio<-NULL
for(j in 1:ncol(allocSd)){
	allocSdRatio<-merge.xts(allocSdRatio, allocSd[,j]/sdXts1)
}

allocSdRatio<-na.omit(allocSdRatio)
ratiosDf<-data.frame(allocSdRatio)
ratiosDf$T<-index(allocSdRatio)
ratioMelt<-melt(ratiosDf, id='T')

ggplot(ratioMelt, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	geom_line() +
	scale_x_date(breaks = xAxisTicks) +
	labs(x='', y='sd ratio', color='', title=sprintf("Ratio of Standard Deviations for different allocations vs. %s", indexName1), subtitle=sprintf("%d week returns [%s:%s]", lb, first(index(allocSdRatio)), last(index(allocSdRatio)))) +
	annotate("text", x=lastDate, y=min(allocSdRatio, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
ggsave(sprintf("%s/%s.%s.SD-allocation-ratios.png", reportPath, indexName1, indexName2), width=16, height=8, units="in")

allocName<-'B25'
toPlot<-na.omit(merge(eqRets1, eqRets2, allocRetXts[,allocName]))
names(toPlot)<-c(indexName1, indexName2, allocName)

#Common.PlotCumReturns(toPlot, sprintf("%s/%s/%s", indexName1, indexName2, allocName), NULL)
Common.PlotCumReturns(toPlot, sprintf("%s/%s/%s", indexName1, indexName2, allocName), sprintf("%s/%s.%s.%s.cumulative.png", reportPath, indexName1, indexName2, allocName))


































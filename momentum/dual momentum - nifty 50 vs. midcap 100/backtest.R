library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('ggrepel')
library('grid')
library('gridExtra')
library('gtable')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")
reportPath <- "."

indexName1<-"NIFTY 50"
indexName2<-"NIFTY MIDCAP 100"

startDate<-as.Date('2001-01-01')
endDate<-as.Date('2018-12-31')

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

saveDd<-function(retCumXts, ddTitle, ddFileName){
	tdn<-table.Drawdowns(retCumXts, 10)
	tdn<-tdn[tdn$Depth < -0.05,]
	tdn$Depth<-round(100*tdn$Depth, 2)

	tt1<-arrangeGrob(tableGrob(tdn, rows=NULL, theme=tableTheme), ncol=1, top=textGrob(ddTitle, gp=gpar(fontsize=12, fontface ='bold', fontfamily='Segoe UI')), bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))

	ggsave(ddFileName, tt1, width=7, height=nrow(tdn)*0.5, units='in')
}

runSingle<-function(mMMReturnXts){
	allXts<-merge(mMMReturnXts, monthlyReturnXts2)
	allXts<-na.omit(allXts)

	gemSp500Xts<- ifelse(allXts[,1] > allXts[,3], ifelse(allXts[, 1] > allXts[, 2], allXts[, 4], allXts[, 5]), allXts[, 6])
	indexClass(gemSp500Xts) <- "Date"
	
	return(gemSp500Xts)
}

###########################################

nDf1<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName1, startDate, endDate))
nXts1<-xts(nDf1$PX_CLOSE, as.Date(nDf1$TIME_STAMP))
nRet1<-monthlyReturn(nXts1)

nDf2<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName2, startDate, endDate))
nXts2<-xts(nDf2$PX_CLOSE, as.Date(nDf2$TIME_STAMP))
nRet2<-monthlyReturn(nXts2)

bDf<-sqlQuery(lcon, sprintf("select TIME_STAMP, TRI from INDEX_CCIL_TENOR where index_name='0_5' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
bXts<-xts(bDf$TRI, as.Date(bDf$TIME_STAMP))
bRet<-monthlyReturn(bXts)

###########################################

nRet1<-Common.NormalizeMonthlyDates(nRet1)
nRet2<-Common.NormalizeMonthlyDates(nRet2)
bRet<-Common.NormalizeMonthlyDates(bRet)

monthlyReturnXts<-merge(nRet1, nRet2, bRet)   #6
monthlyReturnXts<-monthlyReturnXts[-1,]

namesPrefix<-c(indexName1, indexName2, 'bond')
formationMonth<-c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#formationMonth<-c(12)

names(monthlyReturnXts)<-namesPrefix
						
monthlyReturnXts<-na.omit(monthlyReturnXts)						
mRollReturnsXts<-xts()
for(j in formationMonth){
	colNames<-names(mRollReturnsXts)
	for(i in 1:ncol(monthlyReturnXts)){
		mRollReturnsXts<-merge(mRollReturnsXts, rollapply(monthlyReturnXts[,i], j, Return.cumulative))
	}
	colNames<-c(colNames, as.character(unlist(sapply(namesPrefix, function(X) sprintf("%s%d", X, j)))))
	names(mRollReturnsXts)<-colNames
}

mRollReturnsXts<-na.omit(mRollReturnsXts)

monthlyReturnXts2<-xts()
for(i in 1:ncol(monthlyReturnXts)){
	monthlyReturnXts2<-merge(monthlyReturnXts2, stats::lag(monthlyReturnXts[,i],-1))
}
monthlyReturnXts2<-na.omit(monthlyReturnXts2)
names(monthlyReturnXts2)<-as.character(unlist(sapply(namesPrefix, function(X) sprintf("%s", X))))

mMMx<-xts()
for(j in formationMonth){
	colNames<-as.character(unlist(sapply(namesPrefix, function(X) sprintf("%s%d", X, j))))
	gemRet<-runSingle(mRollReturnsXts[, colNames])
	mMMx<-merge(mMMx, gemRet)
}

mAll<-na.omit(mMMx)
names(mAll)<-unlist(sapply(formationMonth, function(x) sprintf("m%2d", x)))
indexClass(mAll)<-"Date"

Common.PlotCumReturns(monthlyReturnXts, sprintf("%s/%s/bonds", indexName1, indexName2), sprintf("%s/%s.%s.cumulative.png", reportPath, indexName1, indexName2))

Common.PlotCumReturns(mAll, sprintf("%s/%s Dual Momentum", indexName1, indexName2), sprintf("%s/%s.%s.DM.%smo.cumulative.png", reportPath, indexName1, indexName2, gsub(", ", "", toString(paste(formationMonth, collapse='-')))))

for(j in 1:ncol(mAll)){
	colName<-names(mAll)[j]
	saveDd(mAll[, colName], sprintf("%s/%s %s formation Dual Momentum Drawdowns", indexName1, indexName2, colName), sprintf("%s/%s.%s.DM.%s.dd.png", reportPath, indexName1, indexName2, colName))
}

spNames<-as.character(unlist(sapply(formationMonth, function(X) gsub(" ", ".", sprintf("%s%d", indexName1, X)))))
exUsNames<-as.character(unlist(sapply(formationMonth, function(X) gsub(" ", ".", sprintf("%s%d", indexName2, X)))))
tbilNames<-as.character(unlist(sapply(formationMonth, function(X) gsub(" ", ".", sprintf("bond%d", X)))))

allXts<-merge(mRollReturnsXts, monthlyReturnXts2)
allXts<-na.omit(allXts)

rCols<-ncol(mRollReturnsXts)

gemAvg<- xts(ifelse(rowMeans(allXts[,spNames]) > rowMeans(allXts[,tbilNames]), ifelse(rowMeans(allXts[, spNames]) > rowMeans(allXts[, exUsNames]), allXts[, rCols+1], allXts[, rCols+2]), allXts[, rCols+3]), index(allXts))
indexClass(gemAvg) <- "Date"
names(gemAvg)<-'avg'

Common.PlotCumReturns(na.omit(merge(gemAvg, mAll[, 'm12'])), sprintf("%s/%s Dual Momentum", indexName1, indexName2), sprintf("%s/%s.%s.DM.avg.cumulative.png", reportPath, indexName1, indexName2))
saveDd(gemAvg, sprintf("%s/%s Avg. Dual Momentum Drawdowns", indexName1, indexName2), sprintf("%s/%s.%s.DM.avg.dd.png", reportPath, indexName1, indexName2))

gemAny<- ifelse(rollapply(allXts, 1, function(X) any(X[,spNames] > X[,tbilNames]), by.column=F), ifelse(rollapply(allXts, 1, function(X) any(X[,spNames] > X[,exUsNames]), by.column=F), allXts[, rCols+1], allXts[, rCols+2]), allXts[, rCols+3])
indexClass(gemAny) <- "Date"
names(gemAny)<-'any'

Common.PlotCumReturns(na.omit(merge(gemAny, mAll[, 'm12'])), sprintf("%s/%s Dual Momentum", indexName1, indexName2), sprintf("%s/%s.%s.DM.any.cumulative.png", reportPath, indexName1, indexName2))
saveDd(gemAny, sprintf("%s/%s Any Dual Momentum Drawdowns", indexName1, indexName2), sprintf("%s/%s.%s.DM.any.dd.png", reportPath, indexName1, indexName2))

gemAll<- ifelse(rollapply(allXts, 1, function(X) all(X[,spNames] > X[,tbilNames]), by.column=F), ifelse(rollapply(allXts, 1, function(X) all(X[,spNames] > X[,exUsNames]), by.column=F), allXts[, rCols+1], allXts[, rCols+2]), allXts[, rCols+3])
indexClass(gemAll) <- "Date"
names(gemAll)<-'all'

Common.PlotCumReturns(na.omit(merge(gemAll, mAll[, 'm12'])), sprintf("%s/%s Dual Momentum", indexName1, indexName2), sprintf("%s/%s.%s.DM.all.cumulative.png", reportPath, indexName1, indexName2))
saveDd(gemAvg, sprintf("%s/%s All Dual Momentum Drawdowns", indexName1, indexName2), sprintf("%s/%s.%s.DM.all.dd.png", reportPath, indexName1, indexName2))
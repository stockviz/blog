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

sp500Index <- 'SP 500'
sp500IndexId <- '^GSPC'

exUsIndex <- 'WORLD ex USA'
exUsIndexId <- 991000

tbilIndex <- 'Barclays 3 month T Bill'
tbilIndexId <- 'BCC23MTB'

bondIndex <- 'ICE BofAML US Corp Master TR'
bondIndexId <- -2147171415

startDate<-as.Date('1990-12-31')
endDate<-as.Date('2018-12-31')

lconUs <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

baseIndex <- "USA MOMENTUM"
worldIndex <- "WORLD ex USA MOMENTUM"

baseIndexId<-sqlQuery(lconUs2, sprintf("select index_code from MSCI_META where index_name='%s'", baseIndex))[[1]]
worldIndexId<-sqlQuery(lconUs2, sprintf("select index_code from MSCI_META where index_name='%s'", worldIndex))[[1]]

print(paste(baseIndex, baseIndexId, worldIndex, worldIndexId))

downloadMsci<-function(indexId){
	msciDf<-sqlQuery(lconUs2, sprintf("select time_stamp, val from MSCI_DATA where time_stamp >= '%s' and time_stamp <= '%s' and id=%d", startDate, endDate, indexId))
	msciDf$time_stamp<-as.Date(msciDf$time_stamp)
	msciDf$date_diff<-c(30,diff(msciDf$time_stamp))

	monthlyMsci<-msciDf[msciDf$date_diff > 15,]
	monthlyMsciXts<-xts(monthlyMsci$val, monthlyMsci$time_stamp)

	dailyMsci<-msciDf[msciDf$date_diff < 15,]

	dailyMsciXts<-xts(dailyMsci$val, dailyMsci$time_stamp)
	x1<-to.period(dailyMsciXts, 'months')
	
	if (month(first(x1)) == month(last(monthlyMsciXts))){
		monthlyMsciXts<-monthlyMsciXts[-nrow(monthlyMsciXts)]
	}

	momXts2<-rbind(monthlyMsciXts, x1[,4])
	
	return(momXts2)
}

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

	gemSp500Xts<- ifelse(allXts[,1] > allXts[,5], ifelse(allXts[, 1] > allXts[, 2], allXts[, 9], allXts[, 10]), allXts[, 12])
	indexClass(gemSp500Xts) <- "Date"
	
	return(gemSp500Xts)
}

###########################################

sp500Df<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO where time_stamp >= '%s' and time_stamp <= '%s' and symbol='%s'", startDate, endDate, '^GSPC'))
sp500Xts<-xts(sp500Df$ac, as.Date(sp500Df$time_stamp))
sp500Xts2<-to.period(sp500Xts, 'months')[,4]

###########################################

exUsXts2<-downloadMsci(exUsIndexId)
baseXts2<-downloadMsci(baseIndexId)
worldXts2<-downloadMsci(worldIndexId)

###########################################

tbilDf<-sqlQuery(lconUs2, sprintf("select time_stamp, val from BARCLAYS_DATA where time_stamp >= '%s' and time_stamp <= '%s' and ticker='%s'", startDate, endDate, tbilIndexId))
tbilXts<-xts(tbilDf$val, as.Date(tbilDf$time_stamp))
tbilXts2<-to.period(tbilXts, 'months')[,4]

###########################################

bondDf<-sqlQuery(lconUs, sprintf("select time_stamp, val from FRED_OBSERVATION where time_stamp >= '%s' and time_stamp <= '%s' and SERIES_ID=%d", startDate, endDate, bondIndexId))
bondXts<-xts(bondDf$val, as.Date(bondDf$time_stamp))
bondXts2<-to.period(bondXts, 'months')[,4]

###########################################

sp500Xts2<-Common.NormalizeMonthlyDates(sp500Xts2)
exUsXts2<-Common.NormalizeMonthlyDates(exUsXts2)
baseXts2<-Common.NormalizeMonthlyDates(baseXts2)
worldXts2<-Common.NormalizeMonthlyDates(worldXts2)
tbilXts2<-Common.NormalizeMonthlyDates(tbilXts2)
bondXts2<-Common.NormalizeMonthlyDates(bondXts2)

monthlyReturnXts<-merge(diff(sp500Xts2)/stats::lag(sp500Xts2,1), #1 
						diff(exUsXts2)/stats::lag(exUsXts2,1), 	 #2 
						diff(baseXts2)/stats::lag(baseXts2,1),   #3 trade
						diff(worldXts2)/stats::lag(worldXts2,1), #4 trade
						diff(tbilXts2)/stats::lag(tbilXts2,1),   #5
						diff(bondXts2)/stats::lag(bondXts2,1))   #6

namesPrefix<-c('sp', 'exUs', 'base', 'world', 'tbil', 'bond')
formationMonth<-c(6, 7, 8, 9, 10, 11, 12)

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
names(monthlyReturnXts2)<-as.character(unlist(sapply(namesPrefix, function(X) sprintf("%s1", X))))

mMMx<-xts()
for(j in formationMonth){
	colNames<-as.character(unlist(sapply(namesPrefix, function(X) sprintf("%s%d", X, j))))
	gemRet<-runSingle(mRollReturnsXts[, colNames])
	mMMx<-merge(mMMx, gemRet)
}

mAll<-na.omit(mMMx)
names(mAll)<-c('m06', 'm07', 'm08', 'm09', 'm10', 'm11', 'm12')
indexClass(mAll)<-"Date"

Common.PlotCumReturns(mAll, sprintf("%s/%s GEM", baseIndex, worldIndex), sprintf("%s/%s.%s.GEM.6-12mo.cumulative.png", reportPath, baseIndex, worldIndex))

for(j in 1:ncol(mAll)){
	colName<-names(mAll)[j]
	saveDd(mAll[, colName], sprintf("%s/%s %s formation GEM Drawdowns", baseIndex, worldIndex, colName), sprintf("%s/%s.%s.GEM.%s.dd.png", reportPath, baseIndex, worldIndex, colName))
}

spNames<-as.character(unlist(sapply(formationMonth, function(X) sprintf("sp%d", X))))
exUsNames<-as.character(unlist(sapply(formationMonth, function(X) sprintf("exUs%d", X))))
tbilNames<-as.character(unlist(sapply(formationMonth, function(X) sprintf("tbil%d", X))))

allXts<-merge(mRollReturnsXts, monthlyReturnXts2)
allXts<-na.omit(allXts)

gemAvg<- xts(ifelse(rowMeans(allXts[,spNames]) > rowMeans(allXts[,tbilNames]), ifelse(rowMeans(allXts[, spNames]) > rowMeans(allXts[, exUsNames]), allXts[, 'base1'], allXts[, 'world1']), allXts[, 'bond1']), index(allXts))
indexClass(gemAvg) <- "Date"
names(gemAvg)<-'avg'

Common.PlotCumReturns(na.omit(merge(gemAvg, mAll[, 'm12'])), sprintf("%s/%s GEM", baseIndex, worldIndex), sprintf("%s/%s.%s.GEM.avg.cumulative.png", reportPath, baseIndex, worldIndex))
saveDd(gemAvg, sprintf("%s/%s Avg. GEM Drawdowns", baseIndex, worldIndex), sprintf("%s/%s.%s.GEM.avg.dd.png", reportPath, baseIndex, worldIndex))

gemAny<- ifelse(rollapply(allXts, 1, function(X) any(X[,spNames] > X[,tbilNames]), by.column=F), ifelse(rollapply(allXts, 1, function(X) any(X[,spNames] > X[,exUsNames]), by.column=F), allXts[, 'base1'], allXts[, 'world1']), allXts[, 'bond1'])
indexClass(gemAny) <- "Date"
names(gemAny)<-'any'

Common.PlotCumReturns(na.omit(merge(gemAny, mAll[, 'm12'])), sprintf("%s/%s GEM", baseIndex, worldIndex), sprintf("%s/%s.%s.GEM.any.cumulative.png", reportPath, baseIndex, worldIndex))
saveDd(gemAny, sprintf("%s/%s Any GEM Drawdowns", baseIndex, worldIndex), sprintf("%s/%s.%s.GEM.any.dd.png", reportPath, baseIndex, worldIndex))

gemAll<- ifelse(rollapply(allXts, 1, function(X) all(X[,spNames] > X[,tbilNames]), by.column=F), ifelse(rollapply(allXts, 1, function(X) all(X[,spNames] > X[,exUsNames]), by.column=F), allXts[, 'base1'], allXts[, 'world1']), allXts[, 'bond1'])
indexClass(gemAll) <- "Date"
names(gemAll)<-'all'

Common.PlotCumReturns(na.omit(merge(gemAll, mAll[, 'm12'])), sprintf("%s/%s GEM", baseIndex, worldIndex), sprintf("%s/%s.%s.GEM.all.cumulative.png", reportPath, baseIndex, worldIndex))
saveDd(gemAvg, sprintf("%s/%s All GEM Drawdowns", baseIndex, worldIndex), sprintf("%s/%s.%s.GEM.all.dd.png", reportPath, baseIndex, worldIndex))
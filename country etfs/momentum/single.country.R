library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")
reportPath <- "."
MSCI_CSV <- "../COUNTRY_MSCI_BEFORE_1993.csv"

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date('1992-12-31')
endDate<-as.Date('2019-01-31')

rollPeriod<-12 #months

downloadMsci<-function(indexId){
	msciDf<-sqlQuery(lconUs2, sprintf("select time_stamp, val from MSCI_DATA where time_stamp >= '%s' and time_stamp <= '%s' and id=%d and index_type='p'", startDate, endDate, indexId))
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

msciIndices<-read.csv(MSCI_CSV)

monthlyRetsXts<-xts()
for(i in 2:nrow(msciIndices)){
	monthlyVals<-downloadMsci(msciIndices[i, 2])
	mRets<-diff(monthlyVals)/stats::lag(monthlyVals,1)
	mRets<-Common.NormalizeMonthlyDates(mRets)
	monthlyRetsXts<-merge(monthlyRetsXts, mRets)
}

names(monthlyRetsXts)<-msciIndices[2:nrow(msciIndices),1]
monthlyRetsXts<-na.omit(monthlyRetsXts)

riskRet<-rollapply(monthlyRetsXts, rollPeriod, function(X) Return.cumulative(X)/sd(X), by.column=F)
names(riskRet)<-names(monthlyRetsXts)

riskRet<-na.omit(riskRet)

allData<-merge(riskRet, xts(as.numeric(unlist(apply(data.frame(riskRet), 1, which.max))), index(riskRet)), stats::lag(monthlyRetsXts, -1))
allData<-na.omit(allData)
allData$L<- rollapply(allData, 1, function(X) X[, as.numeric(ncol(riskRet) +1 + X[, ncol(riskRet)+1])], by.column=F)

Common.PlotCumReturns(allData[, c('L', 'USA.1', 'INDIA.1')], "Single Country Momentum", sprintf("%s/MSCI.single.country.momentum.cumulative.png", reportPath))
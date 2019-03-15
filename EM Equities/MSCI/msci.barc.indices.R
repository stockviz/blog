library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('ggrepel')
library('reshape2')

msciIds<-c(990100, 891800) #WORLD, EM
msciNames<-c('MSCI World', 'MSCI EM')
msciType<-'G'

barcIds<-c('BXIIRBDT', 'BXIIRBET') #Roubini Barclays Country Insights DM Net Index TR, Roubini Barclays Country Insights EM Net Index TR
barcNames<-c('Roubini Insights DM', 'Roubini Insights EM')

startDate<-as.Date("2005-10-25")
endDate<-as.Date("2019-02-28")

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")
source("D:/StockViz/public/blog/common/msci.R")

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

### MSCI
retMonthly<-xts()
for(i in 1:length(msciIds)){
	pXts<-Common.DownloadMsci(msciIds[i], msciType, startDate, endDate)
	pXts<-Common.NormalizeMonthlyDates(pXts)
	monthlyReturnXts<-diff(pXts)/stats::lag(pXts,1)
	monthlyReturnXts<-na.omit(monthlyReturnXts)
	
	retMonthly<-merge(retMonthly, monthlyReturnXts)
}

### Barclays

for(i in 1:length(barcIds)){
	pDf<-sqlQuery(lconUs2, sprintf("select time_stamp, val from BARCLAYS_DATA where ticker='%s' and time_stamp >= '%s' and time_stamp <= '%s'", barcIds[i], startDate, endDate))
	pXts<-xts(pDf[,2], as.Date(pDf[,1]))
	monthlyReturnXts<-monthlyReturn(pXts)
	monthlyReturnXts<-Common.NormalizeMonthlyDates(monthlyReturnXts)
	
	retMonthly<-merge(retMonthly, monthlyReturnXts)
}

names(retMonthly)<-c(msciNames, barcNames)

retMonthly<-na.omit(retMonthly)

Common.PlotCumReturns(retMonthly, "Market-cap vs. Macro-quant", sprintf("%s/MSCI.BARC.%s.cumulative.%s.%s.png", reportPath, paste(names(retMonthly), collapse="-"), startDate, endDate))
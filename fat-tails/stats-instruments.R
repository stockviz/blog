library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('lubridate')
library('reshape2')

library('ggthemes')
library('ggrepel')

library('grid')
library('gridExtra')
library('gtable')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")
source("D:/StockViz/public/blog/common/msci.R")

pdf(NULL)
reportPath <- "D:/StockViz/public/blog/fat-tails/plots"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUS <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUS2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate <- as.Date("1960-01-01")
enDate <- Sys.Date()

usMonthly <- Common.DownloadMsci(984000, "G", startDate, enDate)
worldMonthly <- Common.DownloadMsci(991000, "G", startDate, enDate)
emMonthly <- Common.DownloadMsci(891800, "G", startDate, enDate)
indiaMonthly <- Common.DownloadMsci(935600, "G", startDate, enDate)

#-2147171414 BAMLCC0A1AAATRIV	ICE BofA AAA US Corporate Index Total Return Index Value
pxDf <- sqlQuery(lconUS, "select time_stamp, val from FRED_OBSERVATION where series_id=-2147171414")
corpBondMonthly <- to.period(xts(pxDf[,2], pxDf[,1]), 'months')[,4]

#-2147252004 GOLDPMGBD228NLBM	Gold Fixing Price 3:00 P.M. (London time) in London Bullion Market, based in U.S. Dollars
pxDf <- sqlQuery(lconUS, "select time_stamp, val from FRED_OBSERVATION where series_id=-2147252004")
goldMonthly <- to.period(xts(pxDf[,2], pxDf[,1]), 'months')[,4]

getStats <- function(pxXts){
	pxXtsRet <- na.omit(pxXts/lag(pxXts,1)-1)
	
	avg <- as.numeric(mean(pxXtsRet))
	stddev <- as.numeric(sd(pxXtsRet))
	es5 <- as.numeric(ES(pxXtsRet, p=.95, method="modified"))
	es9 <- as.numeric(ES(pxXtsRet, p=.99, method="modified"))
	
	return(c(avg, stddev, es5, es9))
}

results <- data.frame(INST="", PERIOD="", AVG=0.0, SD=0.0, ES5=0.0, ES9=0.0)
results <- rbind(results, c("MSCI US EQ", sprintf("%s:%s", strftime(first(index(usMonthly)), "%Y-%m"), strftime(last(index(usMonthly)), "%Y-%m")), getStats(usMonthly)))
results <- rbind(results, c("MSCI DM ex-US EQ",sprintf("%s:%s", strftime(first(index(worldMonthly)), "%Y-%m"), strftime(last(index(worldMonthly)), "%Y-%m")), getStats(worldMonthly)))
results <- rbind(results, c("MSCI EM EQ",sprintf("%s:%s", strftime(first(index(emMonthly)), "%Y-%m"), strftime(last(index(emMonthly)), "%Y-%m")), getStats(emMonthly)))
results <- rbind(results, c("MSCI INDIA EQ",sprintf("%s:%s", strftime(first(index(indiaMonthly)), "%Y-%m"), strftime(last(index(indiaMonthly)), "%Y-%m")), getStats(indiaMonthly)))
results <- rbind(results, c("ICE AA US Corp BND",sprintf("%s:%s", strftime(first(index(corpBondMonthly)), "%Y-%m"), strftime(last(index(corpBondMonthly)), "%Y-%m")), getStats(corpBondMonthly)))
results <- rbind(results, c("GOLD", sprintf("%s:%s", strftime(first(index(goldMonthly)), "%Y-%m"), strftime(last(index(goldMonthly)), "%Y-%m")), getStats(goldMonthly)))

results <- results[-1,]

results[,3] <- round(100*as.numeric(results[,3]), 2)
results[,4] <- round(100*as.numeric(results[,4]), 2)
results[,5] <- round(100*as.numeric(results[,5]), 2)
results[,6] <- round(100*as.numeric(results[,6]), 2)

tt1<-arrangeGrob(grobs=list(tableGrob(results, rows=NULL, theme=tableTheme)), ncol=1, 
					top=textGrob("Monthly Returns", gp=gpar(fontsize=14, fontface ='bold', fontfamily='Segoe UI')), 
					bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
ggsave(sprintf("%s/stats.png", reportPath), tt1, width=6, height=nrow(results)*0.5, units='in')	

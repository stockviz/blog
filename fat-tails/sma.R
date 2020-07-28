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

pdf(NULL)
reportPath <- "D:/StockViz/public/blog/fat-tails/plots"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

smaLbs <- c(10, 20, 50, 100, 200)

indexName <- "NIFTY 50 TR"

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s'", indexName))
eqXts <- xts(pDf[,2], pDf[,1])

smaXts <- NULL
for(smaLb in smaLbs){
	smaXts <- merge.xts(smaXts, SMA(eqXts, smaLb))
}

allXts <- merge(eqXts, weeklyReturn(eqXts), smaXts)
allXts <- na.omit(allXts)
allXts[,2] <- stats::lag(allXts[,2], -1)
allXts <- na.omit(allXts)

esEq5 <- ES(allXts[,2], p=.95, method="modified")
esEq9 <- ES(allXts[,2], p=.99, method="modified")

bXts <- NULL
bTests <- data.frame(SMA = 0, ES5=0.0, ES9=0.0)
for(j in 3:ncol(allXts)){
	btest <- ifelse(allXts[,1] > allXts[,j], allXts[,2], 0)
	esTactical5 <- ES(btest, p=.95, method="modified")
	esTactical9 <- ES(btest, p=.99, method="modified")
	bTests <- rbind(bTests, c(smaLbs[j-2], as.numeric(esTactical5), as.numeric(esTactical9)))
	bXts <- merge.xts(bXts, btest)
}
bTests <- rbind(bTests, c(0, as.numeric(esEq5), as.numeric(esEq9)))
bTests <- bTests[-1,]
bTests[,2] <- round(100*bTests[,2], 2)
bTests[,3] <- round(100*bTests[,3], 2)
tt1<-arrangeGrob(grobs=list(tableGrob(bTests, rows=NULL, theme=tableTheme)), ncol=1, 
					top=textGrob(sprintf("%s Expected Shortfall", indexName), gp=gpar(fontsize=14, fontface ='bold', fontfamily='Segoe UI')), 
					bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
ggsave(sprintf("%s/%s-SMA.ES.png", reportPath, indexName), tt1, width=6, height=nrow(bTests)*0.5, units='in')	


esEq5 <- ES(allXts["2011/",2], p=.95, method="modified")
esEq9 <- ES(allXts["2011/",2], p=.99, method="modified")

bTests <- data.frame(SMA = 0, ES5=0.0, ES9=0.0)
for(j in 3:ncol(allXts)){
	btest <- ifelse(allXts["2011/",1] > allXts["2011/",j], allXts["2011/",2], 0)
	esTactical5 <- ES(btest, p=.95, method="modified")
	esTactical9 <- ES(btest, p=.99, method="modified")
	bTests <- rbind(bTests, c(smaLbs[j-2], as.numeric(esTactical5), as.numeric(esTactical9)))
}
bTests <- rbind(bTests, c(0, as.numeric(esEq5), as.numeric(esEq9)))
bTests <- bTests[-1,]
bTests[,2] <- round(100*bTests[,2], 2)
bTests[,3] <- round(100*bTests[,3], 2)
tt1<-arrangeGrob(grobs=list(tableGrob(bTests, rows=NULL, theme=tableTheme)), ncol=1, 
					top=textGrob(sprintf("%s Expected Shortfall", indexName), gp=gpar(fontsize=14, fontface ='bold', fontfamily='Segoe UI')), 
					bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))
ggsave(sprintf("%s/%s-SMA.ES.2011.png", reportPath, indexName), tt1, width=6, height=nrow(bTests)*0.5, units='in')	

tpLot <- merge(allXts[,2], bXts)
names(tpLot) <- c(indexName, sapply(smaLbs, function(X) paste0('SMA-', X)))

Common.PlotCumReturns(tpLot, sprintf("%s SMA long-only", indexName), "weekly rebalance", sprintf("%s/%s.SMA.cumulative-returns-actual.png", reportPath, indexName))
Common.PlotCumReturns(tpLot["2011/",], sprintf("%s SMA long-only", indexName), "weekly rebalance", sprintf("%s/%s.SMA.cumulative-returns-actual.2011.png", reportPath, indexName))
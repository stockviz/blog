library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')

library('tidyverse')
library('lubridate')
library('ggthemes')
library('reshape2')
library('viridis')
library('ggrepel')
library('patchwork')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")

reportPath <- "."
pdf(NULL)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexName <- "NIFTY 50"

startDate <- as.Date("2015-01-01")

corLbs <- c(5, 10, 20, 50, 100) #bus-days of look-back for correlation
smaLb <- 20 #days
tileLb <- 500 #days to calc tiles
tileInspect <- 5

pxDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close [Close] from bhav_index where index_name='%s' and time_stamp >= '%s'", indexName, startDate))
pXts <- xts(pxDf[,-1], pxDf[,1])

pXts <- merge(pXts, SMA(pXts, smaLb), dailyReturn(pXts))
names(pXts) <- c("INDEX", "INDEX_SMA", "RET")
pXts <- merge(pXts, stats::lag(pXts$RET, -1))
names(pXts) <- c("INDEX", "INDEX_SMA", "RET", "RET_1")
pXts$SMA_RET <- ifelse(pXts$INDEX > pXts$INDEX_SMA, pXts$RET_1, 0)
 
for(corLb in corLbs){
	corName <- c(paste0('T', corLb))
	
	load(file=sprintf("%s/%s.corr.%d.RData", reportPath, indexName, corLb)) #eqWtCorXts
	
	corTile <- rollapply(eqWtCorXts[,1], tileLb, function(X) xts(last(ntile(coredata(X), n=5)), last(index(X))))
	
	names(corTile) <- c(corName)
	
	allXts <- na.omit(merge(pXts, corTile))
	
	allXts$SMA_COR_LO <- ifelse(allXts$INDEX >= allXts$INDEX_SMA & allXts[, corName] < tileInspect, allXts$RET_1, 0)
	allXts$SMA_COR_LS <- ifelse(allXts$INDEX >= allXts$INDEX_SMA & allXts[, corName] < tileInspect, allXts$RET_1, ifelse(allXts$INDEX < allXts$INDEX_SMA & allXts[, corName] >= tileInspect, -allXts$RET_1, 0))
	
	Common.PlotCumReturns(allXts[, c('SMA_RET', 'SMA_COR_LO', 'SMA_COR_LS', 'RET_1')], 
							sprintf("%s %d-SMA w/ Correlation", indexName, smaLb), 
							sprintf("%d-corr' %d-tile", corLb, tileInspect), 
							sprintf("%s/%s.%d-sma.%d-cor.%d-tile.%d.png", reportPath, indexName, smaLb, corLb, tileInspect, tileLb), NULL)
							
	Common.PlotCumReturns(allXts["/2020-01-31", c('SMA_RET', 'SMA_COR_LO', 'SMA_COR_LS', 'RET_1')], 
							sprintf("%s %d-SMA w/ Correlation", indexName, smaLb), 
							sprintf("%d-corr' %d-tile", corLb, tileInspect), 
							sprintf("%s/%s.%d-sma.%d-cor.%d-tile.pre.%d.png", reportPath, indexName, smaLb, corLb, tileInspect, tileLb), NULL)
							
	Common.PlotCumReturns(allXts["2020-05-01/", c('SMA_RET', 'SMA_COR_LO', 'SMA_COR_LS', 'RET_1')], 
							sprintf("%s %d-SMA w/ Correlation", indexName, smaLb), 
							sprintf("%d-corr' %d-tile", corLb, tileInspect), 
							sprintf("%s/%s.%d-sma.%d-cor.%d-tile.post.%d.png", reportPath, indexName, smaLb, corLb, tileInspect, tileLb), NULL)
}


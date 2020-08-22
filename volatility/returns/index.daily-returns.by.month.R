library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('lubridate')
library('reshape2')

library('ggthemes')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")

pdf(NULL)
reportPath <- "D:/StockViz/public/blog/volatility/returns"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexName <- "NIFTY 50"

pxDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = '%s'", indexName))
pxXts <- xts(pxDf[,2], pxDf[,1])

mRet <- dailyReturn(pxXts)

mRet <- mRet[-1]
mRet <- mRet[-nrow(mRet)]
names(mRet) <- c('R')

plotMonths <- function(tpXts){
	toPlot <- data.frame(100*tpXts)
	toPlot$M <- month(index(tpXts))
	toPlot <- toPlot[order(toPlot$M),]
	toPlot$M <- factor(toPlot$M, levels=unique(toPlot$M))

	pltDateStart <- first(index(tpXts))
	pltDateEnd <- last(index(tpXts))
	ggplot(toPlot, aes(x=M, y=R, color=M)) +
		theme_economist() +
		geom_violin() +
		geom_boxplot(width=0.1) +
		guides(color=F) +
		labs(y='return (%)', x='month', color='', fill='', 
			title = sprintf("%s Daily Returns by Month", indexName), 
			subtitle=sprintf("%s:%s", pltDateStart, pltDateEnd))
			
	ggsave(sprintf("%s/%s.daily-return-violins-by-month.%s.%s.png", reportPath, indexName, pltDateStart, pltDateEnd), width=16, height=8, units="in")	
}

plotMonths(mRet)
plotMonths(mRet["2010/"])
plotMonths(mRet["2010/2019"])
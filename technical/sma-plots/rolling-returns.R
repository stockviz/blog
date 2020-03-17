library('RODBC')
library(tidyverse)

library(ggthemes)
library(quantmod)
library(PerformanceAnalytics)
library(reshape2)
library(ggrepel)
library(lubridate)

library('grid')
library('gridExtra')
library('gtable')
library('ggpubr')

options("scipen"=999)
options(stringsAsFactors = FALSE)
source("d:/stockviz/r/config.R")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")
options(repr.plot.width=16, repr.plot.height=8)

reportPath <- "."
lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

#parameters

indexName <- 'NIFTY 50 TR'
lookbacks <- c(20, 50, 100, 200)

args <- commandArgs(TRUE)
if(!is.na(args[1])){
	indexName <- args[1]
}

sprintf("processing: %s", indexName)

#load data

indexDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s'", indexName))
pXts <- xts(indexDf[,2], indexDf[,1])
names(pXts) <- c('INDEX')

pXts$RET <- dailyReturn(pXts$INDEX)
pXts <- pXts[-1,]

rollingReturns <- NULL

for(lb in lookbacks){
	rrets <- rollapply(pXts$RET, lb, Return.cumulative)
	rollingReturns <- merge.xts(rollingReturns, rrets)
}

names(rollingReturns) <- sapply(lookbacks, function(x) sprintf("RET_%d", x))
rollingReturns <- 100*rollingReturns

toPlot <- data.frame(rollingReturns)
toPlot$T <- index(rollingReturns)
toPlot <- melt(toPlot, id='T')

plotStart <- first(index(rollingReturns))
plotEnd <- last(index(rollingReturns))

ggplot(toPlot, aes(y=value, x=variable, color=variable)) +
	theme_economist() +
	geom_violin() + 
	geom_boxplot(width=0.1) +
	guides(color=F) +
	labs(x='', y='returns (%)', fill='', color='', 
		title = sprintf("%s Rolling Returns", indexName), 
		subtitle = sprintf("[%s:%s]", plotStart, plotEnd)) +
	annotate("text", x=length(lookbacks), y=min(toPlot$value, na.rm=T), label = "@StockViz", 
			 hjust=-0.25, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.9)

ggsave(sprintf("%s/%s.rolling-returns.distribution.png", reportPath, indexName), width=16, height=8, units='in')

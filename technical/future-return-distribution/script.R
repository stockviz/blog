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
retPeriods <- c(5, 10, 20, 50, 100, 200)

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

#calculate cumulative returns

retPeriodNames <- sapply(retPeriods, function(x) sprintf("CRET_%d", x))
oldNames <- names(pXts)

for(i in 1:length(retPeriods)){
	rp <- retPeriods[i]
	pXts <- merge(pXts, rollapply(pXts$RET, rp, Return.cumulative))
}
names(pXts) <- c(oldNames, retPeriodNames)

pXts[, retPeriodNames] <- 100*pXts[, retPeriodNames]
dateStart <- first(index(pXts))
dateEnd <- last(index(pXts))

#calculate forward cumulative returns

retPeriodLagNames <- sapply(retPeriods, function(x) sprintf("CRET_%d_LAG", x))
oldNames <- names(pXts)

for(i in 1:length(retPeriods)){
	rp <- retPeriods[i]
	rpn <- retPeriodNames[i]
	pXts <- merge(pXts, stats::lag(pXts[,rpn], -rp))
}
names(pXts) <- c(oldNames, retPeriodLagNames)

pXts <- na.omit(pXts)

#classify historical cumulative returns into deciles

bucketNames <- sapply(retPeriods, function(x) sprintf("BUCKET_%d", x))
oldNames <- names(pXts)

for(i in 1:length(retPeriods)){
	rpn <- retPeriodNames[i]
	pXts <- merge(pXts, ntile(pXts[,rpn], 10))
}
names(pXts) <- c(oldNames, bucketNames)

i<-1
j<-1
for(i in 1:length(retPeriods)){
	rp <- retPeriods[i]
	rpn <- retPeriodNames[i]
	bucketName <- bucketNames[i]

	for(j in 1:length(retPeriods)){
		fp <- retPeriods[j]
		lfRetName <- retPeriodLagNames[j]

		#keep only the extremes and classify historical cumulative returns into quintiles

		pXts2 <- pXts[which(pXts[,bucketName] == 1 | pXts[,bucketName] == 10), c(rpn, lfRetName)]
		pXts2$BUCKET <- ntile(pXts2[, rpn], 5)

		toPlot <- data.frame(pXts2[, c('BUCKET', rpn, lfRetName)])
		toPlot[, 'BUCKET'] <- factor(toPlot[, 'BUCKET'], levels=sort(unique(toPlot[, 'BUCKET'])))

		plot1 <- ggplot(toPlot, aes_string(x='BUCKET', y=rpn, color = 'BUCKET')) +
			theme_economist() +
			geom_boxplot() +
			guides(color=F) +
			labs(x='', y='(%)', fill='', title=sprintf("%s %d-day Cumulative Returns by Bucket", indexName, rp), 
				 subtitle=sprintf("[%s:%s]", dateStart, dateEnd)) 

		plot2 <- ggplot(toPlot, aes_string(x='BUCKET', y=lfRetName, color = 'BUCKET')) +
			theme_economist() +
			geom_violin() + 
			geom_boxplot(width=0.1) +
			guides(color=F) +
			labs(x='', y='(%)', fill='', title=sprintf("%s %d-day Forward Cumulative Returns by %d-day Historical Cumulative Return Bucket", indexName, fp, rp), 
				 subtitle=sprintf("[%s:%s]", dateStart, dateEnd)) 
				 
		figure <- ggarrange(plot1, plot2, ncol=1, nrow=2)	
		ggsave(sprintf("%s/%s.%d-historical.%d-forward.png", reportPath, indexName, rp, fp), width=16, height=16, units="in")		 
	}
}
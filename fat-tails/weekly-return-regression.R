library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('lubridate')
library('reshape2')
library('viridis')
library('ggpubr')
library('ggthemes')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")

reportPath <- "D:/StockViz/public/blog/fat-tails/plots"

endDate <- as.Date("2020-07-07")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

lbWeeks <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

endDate <- as.Date("2020-07-20")

#indexName <- "NIFTY 50"
simIndex <- function(indexName){
	nDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp <= '%s'", indexName, endDate))
	nXts1 <- xts(nDf[,-1], nDf[,1])
	dXts1 <- weeklyReturn(nXts1)
	dXts1 <- dXts1[-1]
	dXts1 <- dXts1[-nrow(dXts1)]

	dXts2 <- NULL
	for(lb in lbWeeks){
		dXts2 <- merge.xts(dXts2, rollapply(dXts1, lb, Return.cumulative))
	}

	dXts3 <- dXts2[,1]
	for(j in 1:length(lbWeeks)){
		dXts3 <- merge(dXts3, stats::lag(dXts2[,j], 1))
	}
	dXts3 <- na.omit(dXts3)
	
	pstart <- first(index(dXts1))
	pend <- last(index(dXts1))

	r2 <- data.frame(LB=0, R=0)
		
	names(dXts3) <- c('W0', sapply(lbWeeks, function(x) paste0("W", x)))
	toPlot <- data.frame(dXts3)
	dymin <- min(toPlot[, c('W0')], na.rm=T)
	for(j in 1:length(lbWeeks)){
		ar2 <- summary(lm(formula(sprintf("W0~W%d", j)), toPlot))$adj.r.squared
		r2 <- rbind(r2, c(j, ar2))
		dxmin <- min(toPlot[, c(paste0('W', j))], na.rm=T)
		dxmax <- max(toPlot[, c(paste0('W', j))], na.rm=T)
		ggplot(toPlot, aes_string(x=paste0("W", j), y="W0")) +
			theme_economist() +
			geom_point() +
			stat_summary(fun.data=mean_cl_normal) + 
			geom_smooth(method='lm', formula= y~x) +
			geom_vline(xintercept=0, color='darkgrey', size=1, linetype='dotdash') +
			geom_hline(yintercept=0, color='darkgrey', size=1, linetype='dotdash') +
			geom_text(x=dxmin, label=paste0("adj. R^2: ", round(ar2, 2)), y=dymin, colour='darkgrey', size=7, hjust='left') +
			labs(y='this week returns', x=sprintf('previous %d week returns', j), color='', title=sprintf("%s Weekly Returns", indexName), subtitle=sprintf("%d weekly returns %s:%s", nrow(dXts1), pstart, pend)) +
			annotate("text", x=dxmax, y=0, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
			
		ggsave(sprintf("%s/%s.regression.weekly-returns.%d.png", reportPath, indexName, j), width=16, height=8, units="in", device="png")
	}

	r2 <- r2[-1,]
	return(r2)
}

simIndex('NIFTY 50')
simIndex('NIFTY MIDCAP 150')
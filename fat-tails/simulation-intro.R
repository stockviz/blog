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

sampleLegth <- 120
numIters <- 10000
endDate <- as.Date("2020-07-20")

simIndex <- function(indexName){
	nDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp <= '%s'", indexName, endDate))
	nXts1 <- xts(nDf[,-1], nDf[,1])
	dXts1 <- monthlyReturn(nXts1)
	dXts1 <- dXts1[-1]
	dXts1 <- dXts1[-nrow(dXts1)]

	#calculate the mean
	rollRets <- na.omit(rollapply(dXts1, sampleLegth, Return.annualized))
	mrr <- 100*mean(coredata(rollRets))

	sampleIp <- coredata(dXts1)
	sampleDt <- index(dXts1[1:sampleLegth])

	#uniform sampling
	annRetsNormal <- c()
	for(i in 1:numIters){
		mrets <- sample(sampleIp, size=sampleLegth, replace=T)
		cret <- Return.annualized(xts(mrets, sampleDt))
		annRetsNormal <- c(annRetsNormal, as.numeric(cret))
	}

	#strata sampling
	
	#chop the data into histogram of bin-width=1%
	retHist <- hist(sampleIp, breaks=seq(round(min(sampleIp), 2)-0.01, round(max(sampleIp), 2)+0.01, by=0.01), plot=F)

	annRetsProb <- c()
	for(i in 1:numIters){
		#probabilities=histogram counts
		mrets <- sample(retHist$breaks[-length(retHist$breaks)], size=sampleLegth, replace=T, prob=retHist$counts)
		cret <- Return.annualized(xts(mrets, sampleDt))
		annRetsProb <- c(annRetsProb, as.numeric(cret))
	}

	#plot the densities of both samples
	pstart <- first(index(dXts1))
	pend <- last(index(dXts1))

	tpAn <- data.frame(NORM=100*annRetsNormal, STRAT=100*annRetsProb)
		
	ggplot(melt(tpAn), aes(x=value, color=variable)) +
		theme_economist() +
		geom_density() +
		geom_vline(xintercept=mrr, color='darkred', size=1) +
		geom_text(aes(x=mrr, label=paste0("mean 10-year rolling return", "\n", round(mrr, 2),"%"), y=0.01), colour='black', angle=90) +
		labs(x='', y='', color='', title=sprintf("%s 10-year Annualized Return Simulation Density", indexName), subtitle=sprintf("%d monthly returns %s:%s", nrow(dXts1), pstart, pend)) +
		annotate("text", x=0, y=0, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)
		
	ggsave(sprintf("%s/simulation.%s-density.png", reportPath, indexName), width=16, height=8, units="in", device="png")
}

simIndex('NIFTY 50')
simIndex('NIFTY MIDCAP 150')
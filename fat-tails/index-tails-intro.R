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

plotIndex <- function(indexName){
	nDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp <= '%s'", indexName, endDate))
	nXts1 <- xts(nDf[,-1], nDf[,1])
	dXts1 <- monthlyReturn(nXts1)
	dXts1 <- dXts1[-1]
	dXts1 <- dXts1[-nrow(dXts1)]
	dXts1 <- 100*dXts1
	names(dXts1) <- c('RET')
	
	dret <- data.frame(dXts1)
	stBin <- as.numeric(round(min(dret[,1])-1,0))
	edBin <- as.numeric(round(max(dret[,1])+1,0))
	hist(dret[,1], breaks=seq(from=stBin, to=edBin, by=0.5), plot=F)

	meanret <- mean(dret[,1])
	sdret <- sd(dret[,1])

	left2 <- dret[dret[,1] < meanret-2*sdret,]
	right2 <- dret[dret[,1] > meanret+2*sdret,]

	plotColors <- viridis(2)
	pstart <- first(index(dXts1))
	pend <- last(index(dXts1))
	
	ggplot() +
		theme_economist() +
		geom_histogram(data=dret, aes(x=RET), binwidth=0.5, fill=plotColors[1]) +
		geom_vline(xintercept=meanret, color='darkred', size=1) +
		geom_text(aes(x=meanret, label=paste0("mean", "\n", round(meanret, 2),"%"), y=6), colour='black') +
		geom_vline(xintercept=meanret+sdret, color='darkred', linetype='dashed', size=1) +
		geom_text(aes(x=meanret+sdret, label="\u03C3", y=6), colour='black', size=5) +
		geom_vline(xintercept=meanret-sdret, color='darkred', linetype='dashed', size=1) +
		geom_text(aes(x=meanret-sdret, label="\u03C3", y=6), colour='black', size=5) +
		geom_vline(xintercept=meanret+2*sdret, color='darkred', linetype='dotted', size=0.5) +
		geom_text(aes(x=meanret+2*sdret, label="2\u03C3", y=6), colour='black', size=5) +
		geom_vline(xintercept=meanret-2*sdret, color='darkred', linetype='dotted', size=0.5) +
		geom_text(aes(x=meanret-2*sdret, label="2\u03C3", y=6), colour='black', size=5) +
		geom_vline(xintercept=meanret+3*sdret, color='darkred', linetype='twodash', size=0.5) +
		geom_text(aes(x=meanret+3*sdret, label="3\u03C3", y=6), colour='black', size=5) +
		geom_vline(xintercept=meanret-3*sdret, color='darkred', linetype='twodash', size=0.5) +
		geom_text(aes(x=meanret-3*sdret, label="3\u03C3", y=6), colour='black', size=5) +
		geom_text_repel(aes(x=left2, y=2, label=paste0(round(left2, 2), '%'))) +
		labs(x='', y='', color='', title=sprintf("%s Histogram", indexName), subtitle=sprintf("%d monthly returns %s:%s", nrow(dret), pstart, pend)) +
		annotate("text", x=max(dret[,1]), y=0, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)
	
	ggsave(sprintf("%s/%s-histogram.png", reportPath, indexName), width=16, height=8, units="in", device="png")
}

plotIndex("NIFTY 50 TR")
plotIndex("NIFTY MIDCAP 150 TR")
plotIndex("NIFTY GS 10YR")
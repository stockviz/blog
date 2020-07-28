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
sampleLegth <- 12*10*52
numIters <- 10000

indexName <- "NIFTY MIDCAP 150 TR"

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s'", indexName))
eqXts <- xts(pDf[,2], pDf[,1])

dXts1 <- weeklyReturn(eqXts)
dXts1 <- dXts1[-1]
dXts1 <- dXts1[-nrow(dXts1)]

sampleIp <- coredata(dXts1)

#uniform sampling
esNormal <- data.frame(ES5=0.0, ES9=0.0)
for(i in 1:numIters){
	mrets <- sample(sampleIp, size=sampleLegth, replace=T)
	es5 <- ES(mrets, p=.95, method="modified")
	es9 <- ES(mrets, p=.99, method="modified")
	esNormal <- rbind(esNormal, c(as.numeric(es5), as.numeric(es9)))
}
esNormal <- esNormal[-1,]

#strata sampling

#chop the data into histogram of bin-width=0.1%
retHist <- hist(sampleIp, breaks=seq(round(min(sampleIp), 4)-0.001, round(max(sampleIp), 4)+0.001, by=0.001), plot=F)

esProb <- data.frame(ES5=0.0, ES9=0.0)
for(i in 1:numIters){
	#probabilities=histogram counts
	mrets <- sample(retHist$breaks[-length(retHist$breaks)], size=sampleLegth, replace=T, prob=retHist$counts)
	es5 <- ES(mrets, p=.95, method="modified")
	es9 <- ES(mrets, p=.99, method="modified")
	esProb <- rbind(esProb, c(as.numeric(es5), as.numeric(es9)))
}
esProb <- esProb[-1,]

esEq5 <- 100*as.numeric(ES(dXts1, p=.95, method="modified"))
esEq9 <- 100*as.numeric(ES(dXts1, p=.99, method="modified"))

pstart <- first(index(dXts1))
pend <- last(index(dXts1))

tpAn <- data.frame(NORM=100*esNormal$ES5, STRAT=100*esProb$ES5)
	
ggplot(melt(tpAn), aes(x=value, color=variable)) +
	theme_economist() +
	geom_density() +
	geom_vline(xintercept=esEq5, color='darkred', size=1) +
	geom_text(aes(x=esEq5, label=paste0("historical expected shortfall", "\n", round(esEq5, 2),"%"), y=0.01, hjust='left'), colour='black', angle=90) +
	labs(x='', y='', color='', title=sprintf("%s Expected Shortfall (p=0.95) Simulation Density", indexName), subtitle=sprintf("%d weekly returns %s:%s", nrow(dXts1), pstart, pend)) +
	annotate("text", x=0, y=0, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)
	
ggsave(sprintf("%s/ES.95.simulation.%s-density.png", reportPath, indexName), width=16, height=8, units="in", device="png")

tpAn <- data.frame(NORM=100*esNormal$ES9, STRAT=100*esProb$ES9)
	
ggplot(melt(tpAn), aes(x=value, color=variable)) +
	theme_economist() +
	geom_density() +
	geom_vline(xintercept=esEq9, color='darkred', size=1) +
	geom_text(aes(x=esEq9, label=paste0("historical expected shortfall", "\n", round(esEq9, 2),"%"), y=0.01, hjust='left'), colour='black', angle=90) +
	labs(x='', y='', color='', title=sprintf("%s Expected Shortfall (p=0.99) Simulation Density", indexName), subtitle=sprintf("%d weekly returns %s:%s", nrow(dXts1), pstart, pend)) +
	annotate("text", x=0, y=0, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)
	
ggsave(sprintf("%s/ES.99.simulation.%s-density.png", reportPath, indexName), width=16, height=8, units="in", device="png")


#######

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
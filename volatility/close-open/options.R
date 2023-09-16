source("d:/stockviz/r/config.r")
source("d:/stockviz/r/plot.common.r")

reportPath<-"."

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('patchwork')

pdf(NULL)
options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate <- as.Date("2020-09-01")
indexName <- "NIFTY 50"
indexSym <- "NIFTY"
minDte <- 15

indexDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close as [Close] from bhav_index where index_name = '%s' and time_stamp >= '%s'", indexName, startDate))
iXts <- xts(indexDf[,-1], indexDf[,1])

expiries <- sqlQuery(lcon, sprintf("select distinct expiry_dt from BHAV_EQ_FUT where symbol='%s' and time_stamp >= '%s' order by expiry_dt", indexSym, startDate))[,1]

mktDates <- index(iXts)

retDf <- data.frame(T = "", PL = 0.0, PL_PCT = 0.0)
for(i in 2:length(mktDates)){
	asof <- mktDates[i-1]
	nextDt <- mktDates[i]
	
	expiry <- min(expiries[expiries > asof + minDte])
	expiryNext <- min(expiries[expiries > expiry])
	
	if(expiryNext - expiry > 5) expiryNext <- expiry #handle random holidays

	strikeCE <- as.integer(iXts[asof]/100) * 100 + 100
	strikePE <- as.integer(iXts[asof]/100) * 100 - 100
	
	pxCE1 <- sqlQuery(lcon, sprintf("select px_close from BHAV_EQ_OPT where symbol='%s' and expiry_dt in ('%s') and option_typ = 'CE' and strike_pr = %d and time_stamp = '%s'", 
										indexSym, paste0(c(expiry, expiryNext), collapse="','"), strikeCE, asof))[[1]]
										
	pxPE1 <- sqlQuery(lcon, sprintf("select px_close from BHAV_EQ_OPT where symbol='%s' and expiry_dt in ('%s') and option_typ = 'PE' and strike_pr = %d and time_stamp = '%s'", 
										indexSym, paste0(c(expiry, expiryNext), collapse="','"), strikePE, asof))[[1]]
										

	pxCE2 <- sqlQuery(lcon, sprintf("select px_open from BHAV_EQ_OPT where symbol='%s' and expiry_dt in ('%s') and option_typ = 'CE' and strike_pr = %d and time_stamp = '%s'", 
										indexSym, paste0(c(expiry, expiryNext), collapse="','"), strikeCE, nextDt))[[1]]
										
	pxPE2 <- sqlQuery(lcon, sprintf("select px_open from BHAV_EQ_OPT where symbol='%s' and expiry_dt in ('%s') and option_typ = 'PE' and strike_pr = %d and time_stamp = '%s'", 
										indexSym, paste0(c(expiry, expiryNext), collapse="','"), strikePE, nextDt))[[1]]
										
				
	if(!is.numeric(pxCE2) ||! is.numeric(pxPE2) || !is.numeric(pxCE1) ||!is.numeric(pxPE1)) {
		print(paste(asof, nextDt))
		next
	}
	
	pl <- (pxCE2 + pxPE2) - (pxCE1 + pxPE1)
	plPct <- pl/(pxCE1 + pxPE1)
										
	retDf <- rbind(retDf, c(toString(nextDt), pl, plPct))
}

retDf <- retDf[-1,]

retXts <- xts(as.numeric(retDf$PL_PCT), as.Date(retDf$T))
names(retXts) <- c("STRANGLE")

fileName <- sprintf("%s/%s-strangle.arithmetic.png", reportPath, indexName)
toPlot <- retXts
chartTitle <- sprintf("%s Close-Open Strangles", indexName)
chartSubTitle <- "Arithmetic (using EOD)"

png(fileName, width=1400, height=800, bg="white")
layout(matrix(c(1, 2)), heights = c(2, 1.3), widths = 1)
par(mar = c(0, 4, 4, 2), family='Segoe UI', bty="n")
plot_object <- chart.CumReturns(toPlot, cex.legend=1, main=NA, ylab = "Cumulative Return (Arithmetic)", xaxis = FALSE, legend.loc = "topleft", begin = c("first", "axis"), geometric = F) 
print(plot_object)
title(main=chartTitle, family='Segoe UI') 
mtext(chartSubTitle, cex=0.8, line=0.5)
mtext(sprintf("cumulative: %s; annualized: %s", paste(sprintf("%.2f%%", 100*apply(toPlot, 2, Return.cumulative, geometric = F)), collapse=" / "), paste(sprintf("%.2f%%", 100*apply(toPlot, 2, Return.annualized, geometric = F)), collapse=" / ")), cex=1, line=-1)
par(mar = c(5, 4, 0, 2))
plot_object <- chart.Drawdown(toPlot, main = NA, ylab = "Drawdown (Arithmetic)", event.labels = NULL, ylog = FALSE, geometric = F, bty="n")
print(plot_object)
mtext("@StockViz", side=4, col='grey')
dev.off()

Common.PlotCumReturns(retXts, sprintf("%s Close-Open Strangles", indexName), "Geometric (using EOD)", sprintf("%s/%s-strangle.geometric.png", reportPath, indexName), NULL)


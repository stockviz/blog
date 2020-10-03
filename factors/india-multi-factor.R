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

reportPath <- "D:/StockViz/public/blog/factors/plots"

pdf(NULL)
startDate <- as.Date("2005-04-01")
endDate <- as.Date("2020-08-31")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indices <- c('NIFTY 50 TR', 'NIFTY100 ALPHA 30 TR', 'NIFTY100 LOW VOLATILITY 30 TR', 'NIFTY200 QUALITY 30 TR', 'NIFTY500 VALUE 50 TR')
pXts <- NULL
for(i in indices){
	pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", i, startDate, endDate))
	pXts <- merge.xts(pXts, xts(pDf[,2], pDf[,1]))
}

rets <- NULL
retsQ <- NULL
for(i in 1:length(indices)){
	rets <- merge.xts(rets, monthlyReturn(na.locf(pXts[,i])))
	retsQ <- merge.xts(retsQ, quarterlyReturn(na.locf(pXts[,i])))
}
names(rets) <- indices
names(retsQ) <- indices


plotgg <- function(rets, ggTitle, ggSubTitle){
	toPlotDf <- data.frame(100*rets)
	toPlotDf$T <- year(index(rets))
	toPlotDf$T <- factor(toPlotDf$T, levels=toPlotDf$T)
	toPlotDf <- melt(toPlotDf, id='T')

	fileName <- paste0(reportPath, '/', gsub(' |/|:', '.', paste0(ggTitle, '-', ggSubTitle)), '.png')
	#print(fileName)
	
	ggplot(toPlotDf, aes(x=T, y=value, fill=variable, group=variable)) +
		theme_economist() +
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		scale_fill_viridis(discrete = TRUE) +
		geom_bar(stat="identity", position=position_dodge()) +
		labs(y='return (%)', x='', color='', fill='', title=ggTitle, subtitle=ggSubTitle) +
		annotate("text", x=1, y=min(toPlotDf$value, na.rm=T), label = "@StockViz", hjust='left', vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.5)

	ggsave(fileName, width=16, height=8, units="in")
}

#equal weight
wts <- 1/(length(indices) -1)

##monthly
trDf <- data.frame(apply(rets[,-1], 1, function(X) sum(X*wts)))
trXts <- xts(trDf[,1], as.Date(row.names(trDf)))
names(trXts) <- c('EQ_WT')

toPlot <- merge(trXts, rets)
Common.PlotCumReturns(toPlot, "Factor Equal Weight", "monthly rebalance", sprintf("%s/india-factor-eq-wt.monthly.png", reportPath), NULL)
Common.PlotCumReturns(toPlot["2010/",], "Factor Equal Weight", "monthly rebalance", sprintf("%s/india-factor-eq-wt.monthly.2010.png", reportPath), NULL)

toPlot <- rollapply(toPlot, 12, function(X) Return.annualized(X), by=12)
toPlot <- na.omit(toPlot)
plotgg(toPlot, "Factor Equal Weight", sprintf("monthly rebalance [%s:%s]", startDate, endDate))

##quarterly
trDf <- data.frame(apply(retsQ[,-1], 1, function(X) sum(X*wts)))
trXts <- xts(trDf[,1], as.Date(row.names(trDf)))
names(trXts) <- c('EQ_WT')

toPlot <- merge(trXts, retsQ)
Common.PlotCumReturns(toPlot, "Factor Equal Weight", "quarterly rebalance", sprintf("%s/india-factor-eq-wt.quarterly.png", reportPath), NULL)
Common.PlotCumReturns(toPlot["2010/",], "Factor Equal Weight", "quarterly rebalance", sprintf("%s/india-factor-eq-wt.quarterly.2010.png", reportPath), NULL)

toPlot <- rollapply(toPlot, 4, function(X) Return.annualized(X), by=4)
toPlot <- na.omit(toPlot)
plotgg(toPlot, "Factor Equal Weight", sprintf("quaterly rebalance [%s:%s]", startDate, endDate))

#50/50 low vol/alpha
wts <- 0.5

##monthly
trDf <- data.frame(apply(rets[,c('NIFTY100 ALPHA 30 TR', 'NIFTY100 LOW VOLATILITY 30 TR')], 1, function(X) sum(X*wts)))
trXts <- xts(trDf[,1], as.Date(row.names(trDf)))
names(trXts) <- c('ALPHA/LOW-VOL')

toPlot <- merge(trXts, rets)
Common.PlotCumReturns(toPlot, "Alpha/Low-Volatility Factor Equal Weight", "monthly rebalance", sprintf("%s/india-alpha-low-vol-factor-eq-wt.monthly.png", reportPath), NULL)
Common.PlotCumReturns(toPlot["2010/",], "Alpha/Low-Volatility Factor Equal Weight", "monthly rebalance", sprintf("%s/india-alpha-low-vol-factor-eq-wt.monthly.2010.png", reportPath), NULL)

toPlot <- rollapply(toPlot, 12, function(X) Return.annualized(X), by=12)
toPlot <- na.omit(toPlot)
plotgg(toPlot, "Alpha/Low-Volatility Factor Equal Weight", sprintf("monthly rebalance [%s:%s]", startDate, endDate))

##quarterly
trDf <- data.frame(apply(retsQ[,c('NIFTY100 ALPHA 30 TR', 'NIFTY100 LOW VOLATILITY 30 TR')], 1, function(X) sum(X*wts)))
trXts <- xts(trDf[,1], as.Date(row.names(trDf)))
names(trXts) <- c('ALPHA/LOW-VOL')

toPlot <- merge(trXts, retsQ)
Common.PlotCumReturns(toPlot, "Alpha/Low-Volatility Factor Equal Weight", "quarterly rebalance", sprintf("%s/india-alpha-low-vol-factor-eq-wt.quarterly.png", reportPath), NULL)
Common.PlotCumReturns(toPlot["2010/",], "Alpha/Low-Volatility Factor Equal Weight", "quarterly rebalance", sprintf("%s/india-alpha-low-vol-factor-eq-wt.quarterly.2010.png", reportPath), NULL)

toPlot <- rollapply(toPlot, 4, function(X) Return.annualized(X), by=4)
toPlot <- na.omit(toPlot)
plotgg(toPlot, "Alpha/Low-Volatility Factor Equal Weight", sprintf("quarterly rebalance [%s:%s]", startDate, endDate))
library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('lubridate')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('viridis')
library('ggpmisc')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("d:/stockviz/r/plot.common.r")

startDate <- as.Date("2005-04-01")
endDate <- as.Date("2023-02-28")

indices <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR", "NIFTY500 MULTICAP 50:25:25 TR", "NIFTY BANK TR")
indexSmaLbs <- c(50, 20, 50, 100, 20) #https://stockviz.github.io/reports/index/rp-risk.html#simple-moving-average-stats
bmName <- "0_5"

drag <- 0.02/100

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

runScen <- function(prXts, retXts, cutoff, indexName, indexSma, fid){
	allRetXts <- merge(prXts, SMA(prXts[,1], indexSma))
	allRetXts <- merge(allRetXts, retXts$MAHA, stats::lag(retXts$EQUITY, -1), stats::lag(retXts$BOND, -1))
	allRetXts <- na.omit(allRetXts)
	
	names(allRetXts) <- c('EQUITY_PX', 'SMA', 'MAHA', 'EQ_BH', 'BND_1')

	allRetXts$LO <- ifelse(allRetXts$EQUITY_PX > allRetXts$SMA & allRetXts$MAHA < cutoff, allRetXts$EQ_BH, allRetXts$BND_1)
	allRetXts$LO_R <- ifelse(allRetXts$EQUITY_PX > allRetXts$SMA & allRetXts$MAHA < cutoff, 0, 1)
	allRetXts$LO_T <- allRetXts$LO_R - stats::lag(allRetXts$LO_R, -1)
	allRetXts$LO_EQ_BND <- allRetXts$LO - drag*abs(allRetXts$LO_T) #factor in transaction costs
	
	allRetXts$LO_SMA_EQ_ONLY <- ifelse(allRetXts$EQUITY_PX > allRetXts$SMA, allRetXts$EQ_BH, 0)
	allRetXts$LO_SMA_R <- ifelse(allRetXts$EQUITY_PX < allRetXts$SMA, 0, 1)
	allRetXts$LO_SMA_T <- allRetXts$LO_SMA_R - stats::lag(allRetXts$LO_SMA_R, -1)
	allRetXts$LO_EQ_SMA <- allRetXts$LO_SMA_EQ_ONLY - drag*abs(allRetXts$LO_SMA_T)
	
	allRetXts$LO_EQ_ONLY <- ifelse(allRetXts$EQUITY_PX > allRetXts$SMA & allRetXts$MAHA < cutoff, allRetXts$EQ_BH, 0)
	allRetXts$LO_EQ <- allRetXts$LO_EQ_ONLY - drag*abs(allRetXts$LO_T)

	toPlot <- allRetXts[, c('LO_EQ_BND', 'LO_EQ', 'LO_EQ_SMA', 'EQ_BH')] #long equity or bonds, long equity, equity buy & hold
	srA <- SharpeRatio.annualized(toPlot)
	
	Common.PlotCumReturns(toPlot, sprintf("%s SMA & Mahalanobis Regime Switching Returns", indexName), 
								sprintf("Annualized Sharpe: %s", paste(round(srA, 4), collapse="/")), 
								sprintf("%s/%s.long-only.%s.cumulative.png", reportPath, gsub(':', '_', indexName), fid))
								
	mRets <- apply.quarterly(toPlot, Return.cumulative)
	toPlot <- data.frame(mRets * 100)
	toPlot$T <- index(mRets)
	toPlot <- melt(toPlot, id='T')
	
	ggplot(toPlot, aes(x=T, y=value, fill=variable)) +
		theme_economist() +
		theme(axis.text.x = element_text(angle = 90)) +
		geom_bar(stat = "identity", position = position_dodge()) +
		geom_text_repel(aes(label = sprintf('%.2f', value)), position = position_dodge(width = 0.9)) +
		scale_fill_viridis_d() +
		scale_x_date(date_breaks="3 months", date_labels="%Y-%b") +
		labs(x = "", y="Returns (%)", fill="", color="", size="", 
				title=sprintf("%s SMA & Mahalanobis Regime Switching Returns", indexName), 
				subtitle=sprintf("Quarterly Returns [%s:%s]", first(index(allRetXts)), last(index(allRetXts)))) +
		annotate("text", x=first(index(allRetXts)), y=min(toPlot$value, na.rm=T), label = "@StockViz", hjust='left', vjust='bottom', col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/%s.long-only.%s.quarterly.png", reportPath, gsub(':', '_', indexName), fid), width=16, height=8)
}

runIndex <- function(indexName, smaLb){
	pDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate, endDate))
	pXts <- xts(pDf[,1], pDf[,2])
	names(pXts) <- c('EQUITY')

	bDf <- sqlQuery(lcon, sprintf("select tri, time_stamp from INDEX_CCIL_TENOR where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", bmName, startDate, endDate))
	bXts <- xts(bDf[,1], bDf[,2])
	names(bXts) <- c('BOND')

	dretXts <- merge(weeklyReturn(pXts), weeklyReturn(bXts))
	dretXts <- dretXts[-1,]
	dretXts[,2] <- na.locf(dretXts[,2])
	dretXts <- na.omit(dretXts)

	names(dretXts) <- c('EQUITY', 'BOND')

	#use a rolling window to avoid look-forward bias
	dretXts$MAHA <- rollapply(dretXts, 100, function(X) last(mahalanobis(data.frame(X), colMeans(data.frame(X)), cov(data.frame(X)))), by.column = F)

	## create a test set to calculate the mahalanobis distance cutoff (the threshold for regime change)
	testXts <- dretXts["/2009",]
	testXts$MAHA10 <- ntile(testXts$MAHA, 10) #there are outliers.

	testXts <- na.omit(testXts)

	mahaCutoff <- as.numeric(quantile(testXts[testXts$MAHA10 < 10,]$MAHA)[4]) #remove the 10th decile (extreme outliers) and claculate the 4th quintile

	runScen(pXts["/2009",], testXts, mahaCutoff, indexName, smaLb, "test")

	## validate the cutoff on a different set
	vXts <- dretXts["2010/",]
	vXts <- na.omit(vXts)
	vXts$MAHA10 <- NA

	runScen(pXts["2010/",], vXts, mahaCutoff, indexName, smaLb, "validate")
}

for(i in 1:length(indices)){
	iName <- indices[i]
	smaLb <- indexSmaLbs[i]
	runIndex(iName, smaLb)
}
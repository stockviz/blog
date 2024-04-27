library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')	

library('tidyverse')
library('lubridate')
library('ggthemes')
library('viridis')

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

source("D:/StockViz/public/blog/common/plot.common.R")

reportPath <- "."
drag <- 0.5/100

load(sprintf("%s/symRets.weekly.Rdata", reportPath)) #symRets

symRets$RET <- as.numeric(symRets$RET)
symRets$OVERLAP <- as.numeric(symRets$OVERLAP)

counts <- symRets %>% filter(STRATEGY != "BENCH") %>% group_by(STRATEGY) %>% summarize(total = n())

symRets %>% filter(STRATEGY != "BENCH") %>%
	ggplot(aes(x=as.integer(OVERLAP*100), fill=STRATEGY)) +
		geom_histogram(position="identity", binwidth=10) +
		theme_economist() +
		scale_fill_viridis_d() +
		labs(x = "overlap (%)", y="count", fill="", color="", size="", 
				title="Weekly Portfolio Overlap", 
				subtitle=sprintf("N = %d; [%s:%s]", min(counts$total), min(symRets$PERIOD_END), max(symRets$PERIOD_END)), 
				caption='@StockViz') 

ggsave(sprintf("%s/symRets-weekly.overlap.png", reportPath), width=12, height=6, units="in")

stratNames <- unique(symRets$STRATEGY)
symXts <- do.call(merge.xts, lapply(stratNames, function(X) xts(symRets[symRets$STRATEGY == X,]$RET, symRets[symRets$STRATEGY == X,]$PERIOD_END)))
names(symXts) <- stratNames

symXts2 <- do.call(merge.xts, lapply(stratNames, function(X) {
	rets <- xts(symRets[symRets$STRATEGY == X, c('RET', 'OVERLAP')], symRets[symRets$STRATEGY == X,]$PERIOD_END)
	rets$RET_ADJ <- rets$RET - (1 - rets$OVERLAP)*drag
	rets$RET_ADJ
	}))
	
names(symXts2) <- stratNames

statsDf <- data.frame(SharpeRatio.annualized(symXts))
statsDf <- rbind(statsDf, Return.annualized(symXts)*100)
statsDf <- data.frame(t(statsDf))
colnames(statsDf) <- c('SHARPE', 'RET')
statsDf$ID <- row.names(statsDf)
write.csv(statsDf, file=sprintf("%s/symStatsAll-weekly.gross.csv", reportPath), row.names = F)

statsDf <- data.frame(SharpeRatio.annualized(symXts2))
statsDf <- rbind(statsDf, Return.annualized(symXts2)*100)
statsDf <- data.frame(t(statsDf))
colnames(statsDf) <- c('SHARPE', 'RET')
statsDf$ID <- row.names(statsDf)
write.csv(statsDf, file=sprintf("%s/symStatsAll-weekly.net.csv", reportPath), row.names = F)


Common.PlotCumReturns(symXts[, setdiff(stratNames, c("BENCH"))], "Momentum", "weekly rebal-frequency", sprintf("%s/symRets-weekly.gross.png", reportPath), NULL)
Common.PlotCumReturns(symXts["/2019", setdiff(stratNames, c("BENCH"))], "Momentum", "weekly rebal-frequency", sprintf("%s/symRets-weekly.pre.gross.png", reportPath), NULL)
Common.PlotCumReturns(symXts["2020-05-01/", setdiff(stratNames, c("BENCH"))], "Momentum", "weekly rebal-frequency", sprintf("%s/symRets-weekly.post.gross.png", reportPath), NULL)

Common.PlotCumReturns(symXts2[, setdiff(stratNames, c("BENCH"))], "Momentum", "weekly rebal-frequency", sprintf("%s/symRets-weekly.net.png", reportPath), NULL)
Common.PlotCumReturns(symXts2["/2019", setdiff(stratNames, c("BENCH"))], "Momentum", "weekly rebal-frequency", sprintf("%s/symRets-weekly.pre.net.png", reportPath), NULL)
Common.PlotCumReturns(symXts2["2020-05-01/", setdiff(stratNames, c("BENCH"))], "Momentum", "weekly rebal-frequency", sprintf("%s/symRets-weekly.post.net.png", reportPath), NULL)

for(sName in stratNames){
	if (sName == 'BENCH') next
	toPlot <- merge(symXts[, sName], symXts2[, sName], symXts[, 'BENCH'])
	Common.PlotCumReturns(toPlot, "Momentum", "weekly rebal-frequency", sprintf("%s/symRets-%s-weekly.png", reportPath, sName), NULL)
	Common.PlotCumReturns(toPlot["/2019",], "Momentum", "weekly rebal-frequency", sprintf("%s/symRets-%s-weekly.pre.png", reportPath, sName), NULL)
	Common.PlotCumReturns(toPlot["2020-05-01/",], "Momentum", "weekly rebal-frequency", sprintf("%s/symRets-%s-weekly.post.png", reportPath, sName), NULL)
}



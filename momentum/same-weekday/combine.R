library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')	

library('tidyverse')
library('lubridate')

library('ggthemes')
library('viridis')

pdf(NULL)
options("scipen"=100)
options(stringsAsFactors = FALSE)

source("D:/StockViz/public/blog/common/plot.common.R")

reportPath <- "."
drag <- 0.5/100

load(sprintf("%s/symRets.Rdata", reportPath)) #symRets
load(sprintf("%s/symRetsWeekly.Rdata", reportPath)) #symRetsWeekly

symRetsWeekly %>% {
	ggplot(., aes(x=factor(DOW), y=RET*100, color=SKIP_MO)) +
		theme_economist() +
		scale_color_viridis_d() +
		geom_violin(position = position_dodge(0.9), linewidth = 1) +
		geom_boxplot(position = position_dodge(0.9), width=0.1) +
		labs(x = "day of the week", y="returns (%)", fill="", color="skip months", size="", 
			title="Returns by Day of the Week", 
			subtitle=sprintf("[%s:%s]", min(.$PERIOD_END), max(.$PERIOD_END)), 
			caption='@StockViz') 
}
ggsave(sprintf("%s/dow-violin.png", reportPath), width=12, height=6, units="in")

symRets$SKIP_MO <- as.numeric(symRets$SKIP_MO)
symRets$RET <- as.numeric(symRets$RET)
symRets$OVERLAP <- as.numeric(symRets$OVERLAP)

skipMos <- unique(symRets$SKIP_MO)
benchXts <- xts(symRets[symRets$STRATEGY == 'BENCH',]$RET, symRets[symRets$STRATEGY == 'BENCH',]$PERIOD_END)
names(benchXts) <- c("BENCH")

for(skmo in skipMos){
	stratNames <- unique(symRets[symRets$SKIP_MO == skmo,]$STRATEGY)
	symXts <- do.call(merge.xts, lapply(stratNames, function(X) xts(symRets[symRets$STRATEGY == X & symRets$SKIP_MO == skmo,]$RET, symRets[symRets$STRATEGY == X & symRets$SKIP_MO == skmo,]$PERIOD_END)))
	names(symXts) <- stratNames

	symXts2 <- do.call(merge.xts, lapply(stratNames, function(X) {
		rets <- xts(symRets[symRets$STRATEGY == X & symRets$SKIP_MO == skmo, c('RET', 'OVERLAP')], symRets[symRets$STRATEGY == X & symRets$SKIP_MO == skmo,]$PERIOD_END)
		rets$RET_ADJ <- rets$RET - (1 - rets$OVERLAP)*drag
		rets$RET_ADJ
		}))
		
	names(symXts2) <- stratNames

	statsDf <- data.frame(SharpeRatio.annualized(symXts))
	statsDf <- rbind(statsDf, Return.annualized(symXts)*100)
	statsDf <- data.frame(t(statsDf))
	colnames(statsDf) <- c('SHARPE', 'RET')
	statsDf$ID <- row.names(statsDf)
	write.csv(statsDf, file=sprintf("%s/symStatsAll.%d.gross.csv", reportPath, skmo), row.names = F)

	statsDf <- data.frame(SharpeRatio.annualized(symXts2))
	statsDf <- rbind(statsDf, Return.annualized(symXts2)*100)
	statsDf <- data.frame(t(statsDf))
	colnames(statsDf) <- c('SHARPE', 'RET')
	statsDf$ID <- row.names(statsDf)
	write.csv(statsDf, file=sprintf("%s/symStatsAll.%d.net.csv", reportPath, skmo), row.names = F)


	Common.PlotCumReturns(symXts[, setdiff(stratNames, c("BENCH"))], "Momentum", sprintf("skip: %d", skmo), sprintf("%s/symRets.%d.gross.png", reportPath, skmo), NULL)
	Common.PlotCumReturns(symXts["/2019", setdiff(stratNames, c("BENCH"))], "Momentum", sprintf("skip: %d", skmo), sprintf("%s/symRets.%d.pre.gross.png", reportPath, skmo), NULL)
	Common.PlotCumReturns(symXts["2020-05-01/", setdiff(stratNames, c("BENCH"))], "Momentum", sprintf("skip: %d", skmo), sprintf("%s/symRets.%d.post.gross.png", reportPath, skmo), NULL)

	Common.PlotCumReturns(symXts2[, setdiff(stratNames, c("BENCH"))], "Momentum", sprintf("skip: %d", skmo), sprintf("%s/symRets.%d.net.png", reportPath, skmo), NULL)
	Common.PlotCumReturns(symXts2["/2019", setdiff(stratNames, c("BENCH"))], "Momentum", sprintf("skip: %d", skmo), sprintf("%s/symRets.%d.pre.net.png", reportPath, skmo), NULL)
	Common.PlotCumReturns(symXts2["2020-05-01/", setdiff(stratNames, c("BENCH"))], "Momentum", sprintf("skip: %d", skmo), sprintf("%s/symRets.%d.post.net.png", reportPath, skmo), NULL)

	for(sName in stratNames){
		if (sName == 'BENCH') next
		toPlot <- merge(symXts[, sName], symXts2[, sName], benchXts)
		names(toPlot) <- c(sName, paste0(sName, "_NET"), "BENCH")
		Common.PlotCumReturns(toPlot, "Momentum", sprintf("skip: %d", skmo), sprintf("%s/symRets-%s.%d.png", reportPath, sName, skmo), NULL)
		Common.PlotCumReturns(toPlot["/2019",], "Momentum", sprintf("skip: %d", skmo), sprintf("%s/symRets-%s.%d.pre.png", reportPath, sName, skmo), NULL)
		Common.PlotCumReturns(toPlot["2020-05-01/",], "Momentum", sprintf("skip: %d", skmo), sprintf("%s/symRets-%s.%d.post.png", reportPath, sName, skmo), NULL)
	}
}


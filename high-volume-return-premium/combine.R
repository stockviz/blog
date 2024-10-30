library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')	

library('tidyverse')
library('lubridate')
library('tidyquant')

pdf(NULL)
options("scipen"=100)
options(stringsAsFactors = FALSE)

source("/mnt/data/stockviz/blog/common/plot.common.r")

reportPath <- "."

load(sprintf("%s/symRets.Rdata", reportPath)) #symRets

stratXts <- symRets %>% filter(STRATEGY != "BENCH") %>% select(RET, PERIOD_END) %>% as.xts()
benchXts <- symRets %>% filter(STRATEGY == "BENCH") %>% select(RET, PERIOD_END) %>% as.xts()
toPlot <- merge.xts(stratXts, benchXts)
	
names(toPlot) <- c("HIGH_VOLUME", "BENCH")
sr <- SharpeRatio.annualized(toPlot)

Common.PlotCumReturns(toPlot, "High Volume", sprintf("sharpe: %s", paste(round(sr,2), collapse="/")),
                      sprintf("%s/symRetsAll.png", reportPath), NULL)




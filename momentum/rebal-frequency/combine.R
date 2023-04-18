library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')	

library('tidyverse')
library('lubridate')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("D:/StockViz/public/blog/common/plot.common.R")

reportPath <- "."

load(sprintf("%s/symRets.Rdata", reportPath)) #symRets

symRetDf <- symRets %>% pivot_wider(names_from = c(STRATEGY, REBAL_FREQ), values_from = RET) %>% as.data.frame()

symXts <- na.omit(xts(symRetDf[,-1], symRetDf[,1]))

statsDf <- data.frame(SharpeRatio.annualized(symXts))
statsDf <- rbind(statsDf, Return.annualized(symXts)*100)

statsDf <- data.frame(t(statsDf))
colnames(statsDf) <- c('SHARPE', 'RET')
statsDf$ID <- row.names(statsDf)

write.csv(statsDf, file=sprintf("%s/symStatsAll.csv", reportPath), row.names = F)

configs <- statsDf %>% filter(ID != 'BENCH_0') %>% slice_max(SHARPE, n = 30) %>% slice_max(RET, n=5) %>% select(ID)

toPlot <- symXts[, c(configs$ID, 'BENCH_0')]

Common.PlotCumReturns(toPlot, "Momentum", "skip-months/rebal-frequency", sprintf("%s/symRetsAll.png", reportPath), NULL)




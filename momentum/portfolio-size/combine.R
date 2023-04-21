library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')	

library('tidyverse')
library('lubridate')
library('tidyquant')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("D:/StockViz/public/blog/common/plot.common.R")

reportPath <- "."

load(sprintf("%s/symRets.Rdata", reportPath)) #symRets

sharpe <- symRets %>% group_by(STRATEGY, REBAL_FREQ, FORMATION_PERIOD, SIZE) %>% 
			tq_performance(RET, performance_fun=SharpeRatio.annualized)
			
annPerf <- symRets %>% group_by(STRATEGY, REBAL_FREQ, FORMATION_PERIOD, SIZE) %>% 
			tq_performance(RET, performance_fun=Return.annualized)
			
statsDf <- sharpe %>% inner_join(annPerf) %>% rename(SHARPE = 5, RET = 6) %>% ungroup()
write.csv(statsDf, file=sprintf("%s/symStatsAll.csv", reportPath), row.names = F)

numScenarios <- nrow(statsDf)

bestStats <- statsDf %>% filter(SHARPE > 1) %>% slice_max(SHARPE, n=round(numScenarios/4)) %>% slice_max(RET, n=round(numScenarios/8)) 
write.csv(bestStats, file=sprintf("%s/symStatsTop.csv", reportPath), row.names = F)

#statsDf %>% filter(SHARPE > 1) %>% slice_max(SHARPE, n=round(numScenarios/4)) %>% slice_max(RET, n=round(numScenarios/8)) %>% group_by(REBAL_FREQ) %>% summarize(CTR = n()) %>% arrange(desc(CTR))
#statsDf %>% filter(SHARPE > 1) %>% slice_max(SHARPE, n=round(numScenarios/4)) %>% slice_max(RET, n=round(numScenarios/8)) %>% group_by(FORMATION_PERIOD) %>% summarize(CTR = n()) %>% arrange(desc(CTR))

toPlot <- NULL
configs <- bestStats %>% filter(STRATEGY != 'BENCH_0') %>% slice_head(n = 5) 
for(i in 1:nrow(configs)){
	tsDf <- symRets %>% filter(STRATEGY == configs$STRATEGY[i] & REBAL_FREQ == configs$REBAL_FREQ[i] & FORMATION_PERIOD == configs$FORMATION_PERIOD[i] & SIZE == configs$SIZE[i]) %>% select(RET, PERIOD_END)
	toPlot <- merge.xts(toPlot, xts(tsDf[,1], tsDf[,2]))
}

tsDf <- symRets %>% filter(STRATEGY == "BENCH") %>% select(RET, PERIOD_END)
toPlot <- merge.xts(toPlot, xts(tsDf[,1], tsDf[,2]))
	
names(toPlot) <- c(paste0(configs$STRATEGY, "_", configs$REBAL_FREQ, "_", configs$FORMATION_PERIOD, "_", configs$SIZE), "BENCH")

Common.PlotCumReturns(toPlot, "Momentum", "skip-months/rebal-frequency/formation-period/size", sprintf("%s/symRetsAll.png", reportPath), NULL)




library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')	

library('tidyverse')
library('lubridate')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("D:/StockViz/public/blog/common/plot.common.R")

reportPath <- "."

load(sprintf("%s/symRets.vol.Rdata", reportPath)) #symRets

retTb <- symRets %>% pivot_wider(names_from=STRATEGY, values_from=RET)
retXts <- xts(retTb[,-1], retTb$PERIOD_END)

toPlot <- na.trim(retXts, side='left')

Common.PlotCumReturns(toPlot, "Momentum", "lm(12, 6, 3, 1) and lm(12, 3)", sprintf("%s/symRetsAll.vol.png", reportPath), NULL)
Common.PlotCumReturns(toPlot["/2019",], "Momentum", "lm(12, 6, 3, 1) and lm(12, 3)", sprintf("%s/symRetsPre.vol.png", reportPath), NULL)
Common.PlotCumReturns(toPlot["2020-05-01/",], "Momentum", "lm(12, 6, 3, 1) and lm(12, 3)", sprintf("%s/symRetsPost.vol.png", reportPath), NULL)




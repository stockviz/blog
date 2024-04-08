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

Common.PlotCumReturns(toPlot, "Momentum", "2 year lookback", sprintf("%s/symRetsAll.vol.png", reportPath), NULL)
Common.PlotCumReturns(toPlot["/2019",], "Momentum", "2 year lookback", sprintf("%s/symRetsPre.vol.png", reportPath), NULL)
Common.PlotCumReturns(toPlot["2020-05-01/",], "Momentum", "2 year lookback", sprintf("%s/symRetsPost.vol.png", reportPath), NULL)




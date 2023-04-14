library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')	

library('tidyverse')
library('lubridate')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("D:/StockViz/public/blog/common/plot.common.R")

reportPath <- "D:\\StockViz\\public\\blog\\momentum\\skip-month"

load(sprintf("%s/symRetDf.Rdata", reportPath)) #symRetDf

symXts <- na.omit(xts(symRetDf[,-1], symRetDf[,1]))

statsDf <- data.frame(SharpeRatio.annualized(symXts))
statsDf <- rbind(statsDf, Return.annualized(symXts)*100)

statsDf <- t(statsDf)
write.csv(statsDf, file=sprintf("%s/symStatsAll.csv", reportPath))

toPlot <- symXts
fileName <- sprintf("%s/symRetsAll.png", reportPath)
chartTitle <- "Momentum"
chartSubTitle <- "skip months"

png(fileName, width=1400, height=800, bg="white")
layout(matrix(c(1, 2)), heights = c(2, 1.3), widths = 1)
par(mar = c(0, 4, 4, 2), family='Segoe UI', bty="n")
plot_object <-chart.CumReturns(toPlot, cex.legend=1, main=NA, ylab = "Cumulative Return", xaxis = FALSE, legend.loc = "topleft", begin = c("first", "axis"), geometric = TRUE) 
print(plot_object)
title(main=chartTitle, family='Segoe UI') 
mtext(chartSubTitle, cex=0.8, line=0.5)
par(mar = c(5, 4, 0, 2))
plot_object <-chart.Drawdown(toPlot, main = NA, ylab = "Drawdown", event.labels = NULL, ylog = FALSE, geometric = TRUE, bty="n")
print(plot_object)
mtext("@StockViz", side=4, col='grey')
dev.off()




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
#source("/mnt/hollandr/plot.common.R")

reportPath <- "."
benchPath <- "../../momentum/svm-simple"

regLb <- 12 #months
smoothingLb <- 12 #months

load(sprintf("%s/trend-factor.%dx%d.Rdata", reportPath, regLb, smoothingLb)) #factorRets
load(sprintf("%s/simple-svm.5.Rdata", benchPath)) #symRets

scenLo <- xts(factorRets$LONG, factorRets$ASOF)
scenLS <- xts(factorRets$LONG - factorRets$SHORT, factorRets$ASOF)

index(scenLo) <- as.Date(strftime(index(scenLo), "%Y-%m-20"))
index(scenLS) <- as.Date(strftime(index(scenLS), "%Y-%m-20"))

naiveRets <- xts(symRets[symRets$STRATEGY == 'NAIVE',]$RET, symRets[symRets$STRATEGY == 'NAIVE',]$PERIOD_END)
benchRets <- xts(symRets[symRets$STRATEGY == 'BENCH',]$RET, symRets[symRets$STRATEGY == 'BENCH',]$PERIOD_END)


toPlot <- na.omit(merge(scenLo, scenLS, naiveRets, benchRets))
names(toPlot) <- c('LO', 'LS', 'NAIVE-MOM', 'BENCH')
sr <- SharpeRatio.annualized(toPlot)
print(sr)

preCov <- toPlot["/2019",]
postCov <- toPlot["2020-05-01/",]

preSr <- SharpeRatio.annualized(preCov)
postSr <- SharpeRatio.annualized(postCov)

Common.PlotCumReturns(preCov, "Trend Factor Top 200", sprintf("%dx%d regression; Gross SR: %s", regLb, smoothingLb, paste(round(preSr, 2), collapse='/')), 
		sprintf("%s/trend-factor.cumret.%dx%d.pre.png", reportPath, regLb, smoothingLb), NULL)
		
Common.PlotCumReturns(postCov, "Trend Factor Top 200", sprintf("%dx%d regression; Gross SR: %s", regLb, smoothingLb, paste(round(postSr, 2), collapse='/')), 
		sprintf("%s/trend-factor.cumret.%dx%d.post.png", reportPath, regLb, smoothingLb), NULL)		
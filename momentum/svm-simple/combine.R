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

#source("D:/StockViz/public/blog/common/plot.common.R")
source("/mnt/hollandr/plot.common.R")

reportPath <- "."

suffix <- ".5"
load(sprintf("%s/simple-svm%s.Rdata", reportPath, suffix)) #symRets

symRets$RET <- as.numeric(symRets$RET)

symXts <- symRets %>% pivot_wider(id_cols = PERIOD_END, names_from = STRATEGY, values_from = RET) %>% as.xts()
symXts <- na.omit(symXts)

statsDf <- data.frame(SharpeRatio.annualized(symXts))
statsDf <- rbind(statsDf, Return.annualized(symXts)*100)
statsDf <- data.frame(t(statsDf))
colnames(statsDf) <- c('SHARPE', 'RET')
statsDf$ID <- row.names(statsDf)
write.csv(statsDf, file=sprintf("%s/simple-svm%s.gross.csv", reportPath, suffix), row.names = F)

preXts <- symXts["/2019", ]
sr <- SharpeRatio.annualized(preXts)
Common.PlotCumReturns(preXts, "Simple SVM Momentum", sprintf("Gross SR: %s", paste(round(sr, 2), collapse='/')), sprintf("%s/simple-svm%s.pre.gross.png", reportPath, suffix), NULL)

postXts <- symXts["2020-05-01/", ]
sr <- SharpeRatio.annualized(postXts)
Common.PlotCumReturns(postXts, "Simple SVM Momentum", sprintf("Gross SR: %s", paste(round(sr, 2), collapse='/')), sprintf("%s/simple-svm%s.post.gross.png", reportPath, suffix), NULL)



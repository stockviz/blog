library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')	

library('tidyverse')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")

reportPath <- "."

sharpeDf <- data.frame(DESC="", SHARPE=0.0)

load(file=sprintf("%s/symRetsX1.Rdata", reportPath))
symXts1 <- xts(symRets[,-1], symRets[,1])
sharpeDf <- rbind(sharpeDf, c("VOL", round(SharpeRatio.annualized(symXts1[,1]), 2)))

load(file=sprintf("%s/symRetsX2.Rdata", reportPath))
symXts2 <- xts(symRets[,-1], symRets[,1])
sharpeDf <- rbind(sharpeDf, c("VOLxVOL", round(SharpeRatio.annualized(symXts2[,1]), 2)))

symXts <- merge(symXts1[,1], symXts2)
names(symXts) <- c("VOL", "VOLxVOL", "NIFTY 200 TR")

srStr <- paste(round(SharpeRatio.annualized(symXts), 2), collapse="/")

Common.PlotCumReturns(symXts, "Volatility", sprintf("Sharpe Ratio: %s", srStr), sprintf("%s/combined-vol.png", reportPath), NULL)


load(file=sprintf("%s/symRetsX2-A.Rdata", reportPath))
symXts2A <- xts(symRets[,-1], symRets[,1])
sharpeDf <- rbind(sharpeDf, c("VOLxMOM", round(SharpeRatio.annualized(symXts2A[,1]), 2)))

load(file=sprintf("%s/symRetsX3.Rdata", reportPath))
symXts3 <- xts(symRets[,-1], symRets[,1])
sharpeDf <- rbind(sharpeDf, c("VOLxVOLxMOM", round(SharpeRatio.annualized(symXts3[,1]), 2)))

symXts <- merge(symXts1[,1], symXts2[,1], symXts2A[,1], symXts3)
names(symXts) <- c("VOL", "VOLxVOL", "VOLxMOM", "VOLxVOLxMOM", "NIFTY 200 TR")

srStr <- paste(round(SharpeRatio.annualized(symXts), 2), collapse="/")

Common.PlotCumReturns(symXts, "Volatility x Momentum", sprintf("Sharpe Ratio: %s", srStr), sprintf("%s/combined-vol-mom.png", reportPath), NULL)


load(file=sprintf("%s/symRetsY1.Rdata", reportPath))
symYts1 <- xts(symRets[,-1], symRets[,1])
sharpeDf <- rbind(sharpeDf, c("MOM", round(SharpeRatio.annualized(symYts1[,1]), 2)))

load(file=sprintf("%s/symRetsY2.Rdata", reportPath))
symYts2 <- xts(symRets[,-1], symRets[,1])
sharpeDf <- rbind(sharpeDf, c("MOMxVOL", round(SharpeRatio.annualized(symYts2[,1]), 2)))

load(file=sprintf("%s/symRetsY3.Rdata", reportPath))
symYts3 <- xts(symRets[,-1], symRets[,1])
sharpeDf <- rbind(sharpeDf, c("MOMxVOLxVOL", round(SharpeRatio.annualized(symYts3[,1]), 2)))

symYts <- merge(symYts1[,1], symYts2[,1], symYts3)
names(symYts) <- c("MOM", "MOMxVOL", "MOMxVOLxVOL", "NIFTY 200 TR")

srStr <- paste(round(SharpeRatio.annualized(symYts), 2), collapse="/")

Common.PlotCumReturns(symYts, "Momentum x Volatility", sprintf("Sharpe Ratio: %s", srStr), sprintf("%s/combined-mom-vol.png", reportPath), NULL)

sharpeDf <- sharpeDf[-1,]
sharpeDf$SHARPE <- as.numeric(sharpeDf$SHARPE)
sharpeDf <- sharpeDf[order(sharpeDf$SHARPE, decreasing=T),]

write.csv(sharpeDf, file=sprintf("%s/sharpe.csv", reportPath), row.names = F)
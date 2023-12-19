library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')	

library('tidyverse')
library('lubridate')
library('reshape2')
library('viridis')
library('ggthemes')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("D:/StockViz/public/blog/common/plot.common.R")

reportPath <- "."
pdf(NULL)

lbSmall <- 21 #50 #21
lbLarge <- 200
sigmaMultiplier <- 1
fileSuffix <- "-vol"

#load(sprintf("%s/monthly-returns.%d.%d.Rdata", reportPath, lbSmall, lbLarge)) #retDf
load(sprintf("%s/monthly-returns%s.%d.%d.%d.Rdata", reportPath, fileSuffix, lbSmall, lbLarge, sigmaMultiplier)) #retDf

symRets <- xts(retDf[,-1], retDf[,1])

sharpe <- SharpeRatio.annualized(symRets)
Common.PlotCumReturns(symRets, "MAD Trend", sprintf("%dx%d; %ds; SR: %s", lbSmall, lbLarge, sigmaMultiplier, paste(round(sharpe,2), collapse="/")), 
		sprintf("%s/mad%s-cum-ret.%d.%d.%d.ALL.png", reportPath, fileSuffix, lbSmall, lbLarge, sigmaMultiplier), NULL)

sharpe <- SharpeRatio.annualized(symRets["/2019",])
Common.PlotCumReturns(symRets["/2019",], "MAD Trend", sprintf("%dx%d; %ds; SR: %s", lbSmall, lbLarge, sigmaMultiplier, paste(round(sharpe,2), collapse="/")), 
		sprintf("%s/mad%s-cum-ret.%d.%d.%d.pre-2020.png", reportPath, fileSuffix, lbSmall, lbLarge, sigmaMultiplier), NULL)

sharpe <- SharpeRatio.annualized(symRets["2020-05-01/",])
Common.PlotCumReturns(symRets["2020-05-01/",], "MAD Trend", sprintf("%dx%d; %ds; SR: %s", lbSmall, lbLarge, sigmaMultiplier, paste(round(sharpe,2), collapse="/")), 
		sprintf("%s/mad%s-cum-ret.%d.%d.%d.post-2020.png", reportPath, fileSuffix, lbSmall, lbLarge, sigmaMultiplier), NULL)

plotBars <- function(retXts, suffix){
	annPerf <- Return.annualized(retXts)*100
	
	toPlot <- data.frame(retXts*100)
	toPlot$T <- index(retXts)
	toPlot <- melt(toPlot, id='T')

	ggplot(toPlot, aes(x=T, y=value, fill=variable)) +
		theme_economist() +
		scale_fill_viridis_d() +
		geom_bar(stat="identity", position="dodge2") +
		scale_x_date(breaks = "6 months", date_labels="%Y-%b", expand=c(0, 0)) +
		labs(y='return (%)', x='', color='', fill='', title="MAD Trend", subtitle=sprintf("Monthly Rebalance; %d/%d/%d; CAGR: %s", lbSmall, lbLarge, sigmaMultiplier, paste(round(annPerf,2), collapse="/")), caption = "@StockViz")
		
	ggsave(sprintf("%s/mad%s-monthly-ret.%d.%d.%d.%s.png", reportPath, fileSuffix, lbSmall, lbLarge, sigmaMultiplier, suffix), width=16, height=8, units="in")
}

plotBars(symRets, "ALL")
plotBars(symRets["/2019",], "pre-2020")
plotBars(symRets["2020-05-01/",], "post-2020")
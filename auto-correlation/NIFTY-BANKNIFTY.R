source("D:/stockviz/r/config.r")
reportPath <- "D:/StockViz/public/blog/auto-correlation"
library('RODBC')
library('RPostgres')
library('tidyverse')
library('reshape2')
library('ggthemes')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggpubr')
library('grid')
library('gridExtra')
library('gtable')
library('viridis')

source("D:/StockViz/public/blog/common/misc.R")

options(stringsAsFactors = FALSE)
options("scipen"=100)
pdf(NULL)


lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

niftyDf <- sqlQuery(lcon, "select time_stamp, px_close from bhav_index where index_name = 'NIFTY 50' and time_stamp >= '2015-01-01'")
niftyXts <- xts(niftyDf[,2], niftyDf[,1])

bniftyDf <- sqlQuery(lcon, "select time_stamp, px_close from bhav_index where index_name = 'NIFTY BANK' and time_stamp >= '2015-01-01'")
bniftyXts <- xts(bniftyDf[,2], bniftyDf[,1])

retXts <- merge(dailyReturn(niftyXts), dailyReturn(bniftyXts))
retXts <- retXts[-1]
retXts <- na.omit(retXts)

udXts <- merge(ifelse(retXts[,1] > 0, 1, 0), ifelse(retXts[,2] > 0, 1, 0)) #up/down

years <- unique(year(index(udXts)))

for(yr in years){
	yrStr <- toString(yr)
	
	#NIFTY
	#for real returns
	bacf <- acf(retXts[yrStr,1], plot = FALSE, lag.max=10)
	bacfdf <- with(bacf, data.frame(lag, acf))
	  
	#for up/down returns
	bacf2 <- acf(udXts[yrStr,1], plot = FALSE, lag.max=10)
	bacfdf2 <- with(bacf2, data.frame(lag, acf))

	#BANKNIFTY
	#for real returns
	bacfx <- acf(retXts[yrStr,2], plot = FALSE, lag.max=10)
	bacfdfx <- with(bacfx, data.frame(lag, acf))
	  
	#for up/down returns
	bacf2x <- acf(udXts[yrStr,2], plot = FALSE, lag.max=10)
	bacfdf2x <- with(bacf2x, data.frame(lag, acf))

	toPlot <- merge(merge(bacfdf, bacfdf2, by=c('lag')), merge(bacfdfx, bacfdf2x, by=c('lag')), by=c('lag'))
	names(toPlot) <- c('lag', 'NIFTY.R', 'NIFTY.UD', 'BNIFTY.R', 'BNIFTY.UD')

	toPlot <- toPlot[-1,] #remove lag 0

	toPlot <- melt(toPlot, id='lag')
	toPlot$lag <- factor(toPlot$lag, levels=unique(toPlot$lag))
	
	ggplot(toPlot, aes(x = lag, y = value, color=variable)) +
		theme_economist() +
		scale_color_viridis(option='D', discrete = T) +
		geom_hline(aes(yintercept = 0)) +
		geom_linerange(aes(ymin=0, ymax=value), position = position_dodge(width = 0.5), size = 2) +
		labs(x = 'lag', y='ACF', fill="", color="", title=sprintf("NIFTY and BANKNIFTY Auto-Correlation (%d)", yr)) +
		annotate("text", x=10, y=0, label = "@StockViz", hjust='right', vjust='bottom', col="white", cex=6, fontface = "bold", alpha = 0.5)
		
	ggsave(sprintf("%s/N-BN.ACF.%d.png", reportPath, yr), width=16, height=8, units="in")  	
}
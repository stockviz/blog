library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('lubridate')
library('reshape2')
library('viridis')
library('ggpubr')
library('ggthemes')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")

pdf(NULL)
reportPath <- "D:/StockViz/public/blog/risk-valuations/plots"

lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

yaleDf <- sqlQuery(lconUs2, sprintf("select time_stamp, val, long_ir from YALE_SPCOMP order by time_stamp"))
yaleXts <- xts(yaleDf[,-1], yaleDf[,1])
names(yaleXts) <- c('SP500', 'IR')

yIrMonthly <- to.period(yaleXts$IR, 'months')
ySpMonthlyRet <- monthlyReturn(yaleXts$SP500)
ySpRolling10 <- rollapply(ySpMonthlyRet, 10*12, Return.annualized)

yIrSp10 <- merge(yIrMonthly[,4], 100*stats::lag(ySpRolling10, -10*12))
names(yIrSp10) <- c('IR', 'SP500')

#yIrSp10 <- na.omit(yIrSp10)

toPlot <- data.frame(yIrSp10)
toPlot$T <- index(yIrSp10)
toPlot <- melt(toPlot, id='T')

ggplot(toPlot, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	geom_line() +
	scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand=c(0, 0)) + 
	labs(y='return (%)', x='', color='', fill='', title="US: Rolling 10-year Equity Returns vs. Starting 10-year Bond Yields", subtitle='Shiller Data') +
	annotate("text", x=min(toPlot$T), y=min(toPlot$value, na.rm=T), label = "@StockViz", hjust=0, vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.5)
	
ggsave(sprintf("%s/sp500-10yr.ir.png", reportPath), width=16, height=8, units="in")	

##### 

yIrSp10X <- yIrSp10$SP500 - yIrSp10$IR
yIrSp10X <- na.omit(yIrSp10X)
names(yIrSp10X) <- c('EXCESS')
yIrSp10X$EXCESS_SD <- rollapply(yIrSp10X$EXCESS, 100, sd)
yIrSp10X$EXCESS_AVG <- rollapply(yIrSp10X$EXCESS, 100, mean)
yIrSp10X$EXCESS_U <- yIrSp10X$EXCESS_AVG + yIrSp10X$EXCESS_SD 
yIrSp10X$EXCESS_L <- yIrSp10X$EXCESS_AVG - yIrSp10X$EXCESS_SD

toPlot <- data.frame(yIrSp10X)
toPlot$T <- index(yIrSp10X)

ggplot(toPlot, aes(x=T)) +
	theme_economist() +
	geom_ribbon(aes(ymin=EXCESS_L, ymax=EXCESS_U), fill='lightgrey') +
	geom_line(aes(y=EXCESS)) +
	geom_line(aes(y=EXCESS_AVG), color='darkgrey') +
	scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand=c(0, 0)) + 
	labs(y='return (%)', x='', color='', fill='', title="US Excess Returns: Rolling 10-year Equity Returns minus Starting 10-year Bond Yields", subtitle='Shiller Data') +
	annotate("text", x=min(toPlot$T), y=min(toPlot$EXCESS_L, na.rm=T), label = "@StockViz", hjust=0, vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.5)
	
ggsave(sprintf("%s/sp500-10yr.ir.excess.png", reportPath), width=16, height=8, units="in")	
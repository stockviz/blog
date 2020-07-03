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

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate <- as.Date("2003-12-31")

eqDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='NIFTY 50' and time_stamp >= '%s'", startDate))
eqXts <- xts(eqDf[,-1], eqDf[,1])

bndDf <- sqlQuery(lcon, sprintf("select time_stamp, ytm from INDEX_CCIL_TENOR where index_name='15_20'"))
bndXts <- xts(bndDf[,-1], bndDf[,1])

eqMonthlyRet <- monthlyReturn(eqXts)
bndMonthly <- to.period(bndXts, 'months')[,4]

eqMonthlyRet <- Common.NormalizeMonthlyDates(eqMonthlyRet[-1])
bndMonthly <- Common.NormalizeMonthlyDates(bndMonthly[-1])

eqRolling10 <- rollapply(eqMonthlyRet, 10*12, Return.annualized)

yIrSp10 <- merge(bndMonthly, 100*stats::lag(eqRolling10, -10*12))
names(yIrSp10) <- c('IR', 'NIFTY50')

#yIrSp10 <- na.omit(yIrSp10)

toPlot <- data.frame(yIrSp10)
toPlot$T <- index(yIrSp10)
toPlot <- melt(toPlot, id='T')

ggplot(toPlot, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	geom_line() +
	scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand=c(0, 0)) + 
	labs(y='return (%)', x='', color='', fill='', title="India Rolling 10-year Equity Returns vs. Starting long-term Bond Yields", subtitle='NSE/CCIL Data') +
	annotate("text", x=min(toPlot$T), y=min(toPlot$value, na.rm=T), label = "@StockViz", hjust=0, vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.5)
	
ggsave(sprintf("%s/nifty-10yr.ir.png", reportPath), width=16, height=8, units="in")	

##### 

yIrSp10X <- yIrSp10$NIFTY50 - yIrSp10$IR
yIrSp10X <- na.omit(yIrSp10X)
names(yIrSp10X) <- c('EXCESS')
yIrSp10X$EXCESS_SD <- rollapply(yIrSp10X$EXCESS, 50, sd)
yIrSp10X$EXCESS_AVG <- rollapply(yIrSp10X$EXCESS, 50, mean)
yIrSp10X$EXCESS_U <- yIrSp10X$EXCESS_AVG + yIrSp10X$EXCESS_SD 
yIrSp10X$EXCESS_L <- yIrSp10X$EXCESS_AVG - yIrSp10X$EXCESS_SD

toPlot <- data.frame(yIrSp10X)
toPlot$T <- index(yIrSp10X)

ggplot(toPlot, aes(x=T)) +
	theme_economist() +
	geom_ribbon(aes(ymin=EXCESS_L, ymax=EXCESS_U), fill='lightgrey') +
	geom_line(aes(y=EXCESS)) +
	geom_line(aes(y=EXCESS_AVG), color='darkgrey') +
	scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand=c(0, 0)) + 
	labs(y='return (%)', x='', color='', fill='', title="India Excess Returns: Rolling 10-year Equity Returns minus Starting long-term Bond Yields", subtitle='NSE/CCIL Data') +
	annotate("text", x=min(toPlot$T), y=min(toPlot$EXCESS_L, na.rm=T), label = "@StockViz", hjust=0, vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.5)
	
ggsave(sprintf("%s/nifty-10yr.ir.excess.png", reportPath), width=16, height=8, units="in")	
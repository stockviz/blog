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

reportPath <- "D:/StockViz/public/blog/risk-valuations/plots"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate <- as.Date("1998-04-30")

peDf <- sqlQuery(lcon, sprintf("select time_stamp, pe from INDEX_BSE_VALUATION where index_name='S&P BSE 100'"))
peXts <- xts(peDf[,-1], peDf[,1])

pxDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX_BSE where index_name='BSE100' and time_stamp >= '%s'", startDate))
pxXts <- xts(pxDf[,-1], pxDf[,1])

pXts <- merge(pxXts, peXts)
pXts <- na.locf(pXts)

pmXts <- merge(to.period(pXts[,1], 'months')[,4], to.period(pXts[,2], 'months')[,4], monthlyReturn(pXts[,1]))
names(pmXts) <- c('INDEX', 'PE', 'RETURN')

pmXts$E <- pmXts$INDEX/pmXts$PE
pmXts$R1 <- rollapply(pmXts$RETURN, 12, Return.annualized)
pmXts$R1 <- pmXts$R1 * 100
pmXts$R5 <- rollapply(pmXts$RETURN, 5*12, Return.annualized)
pmXts$R5 <- pmXts$R5 * 100

pmXts$E1 <- rollapply(monthlyReturn(pmXts$E), 12, Return.annualized)
pmXts$E1 <- 100*pmXts$E1 
pmXts$E5 <- rollapply(monthlyReturn(pmXts$E), 5*12, Return.annualized)
pmXts$E5 <- 100*pmXts$E5 

toPlot <- data.frame(pmXts)
toPlot$T <- index(pmXts)

p1 <- ggplot(toPlot, aes(x=T, y=E)) +
	theme_economist() +
	geom_line() +
	scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand=c(0, 0)) + 
	labs(y='earnings', x='', color='', fill='', title="BSE 100 Implied Earnings", subtitle='') 
	
toPlot <- data.frame(pmXts[, c('R1', 'R5', 'E1', 'E5')])
toPlot$YM <- strftime(index(pmXts), '%Y-%m')
toPlot$YM <- factor(toPlot$YM)
toPlot <- melt(toPlot, id='YM')

bpData <- toPlot[toPlot$variable %in% c('E1', 'E5'),]
lnData <- toPlot[toPlot$variable %in% c('R1', 'R5'),]
names(lnData) <- c('YM', 'variable', 'val')

p3 <- ggplot() +
	theme_economist() +
	geom_bar(data=bpData, aes(x=YM, y=value, fill=variable), stat="identity", position=position_dodge()) +
	geom_line(data=lnData, aes(x=YM, y=val, color=variable, group=variable), size=1) +
	scale_fill_viridis(discrete = TRUE) +
	scale_x_discrete(breaks=toPlot$YM[round(seq(1, nrow(pmXts), length.out=20))], expand=c(0, 0)) +
	labs(y='returns, growth (%)', x='', color='', fill='', title="Rolling BSE 100 Annualized Earnings Growth (E) and Returns (R)", subtitle='') 
	
figure <- ggarrange(p1, p3, ncol=1, nrow=2)	
annotate_figure(figure, 
				bottom = text_grob("@StockViz", face="bold", size=12, family="Segoe UI", color='grey'))
	
	
ggsave(sprintf("%s/bse100-earnings.png", reportPath), width=16, height=16, units="in")		


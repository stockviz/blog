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

reportPath <- "D:/StockViz/public/blog/rolling returns"

pdf(NULL)
startDate <- as.Date("2005-04-01")
endDate <- as.Date("2020-08-31")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indices <- c('NIFTY 50 TR', 'NIFTY 500 TR', 'NIFTY100 ALPHA 30 TR', 'NIFTY100 LOW VOLATILITY 30 TR', 'NIFTY200 QUALITY 30 TR')

numMonths <- 5*12
rets <- NULL
for(i in indices){
	pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", i, startDate, endDate))
	mret <- monthlyReturn(xts(pDf[,2], pDf[,1]))
	rets <- merge.xts(rets, 100*rollapply(mret, numMonths, Return.annualized))
}

rets <- na.omit(rets)
names(rets) <- indices

toPlotDf <- data.frame(rets)
toPlotDf$T <- index(rets)
toPlotDf <- melt(toPlotDf, id='T')

ggplot(toPlotDf, aes(x=T, y=value, color=variable, group=variable)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_line(size=1) +
	scale_x_date(breaks = "6 months", date_labels="%Y-%b-%d", expand=c(0, 0)) +
	labs(y='return (%)', x='', color='', fill='', title=sprintf("%d-year Rolling Returns (Annualized)", numMonths/12), subtitle=sprintf("%s:%s", startDate, endDate)) +
	annotate("text", x=min(toPlotDf$T), y=min(toPlotDf$value, na.rm=T), label = "@StockViz", hjust='left', vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/%s.%d-months.rolling.returns.png", reportPath, paste(indices, collapse='.'), numMonths), width=16, height=8, units="in")




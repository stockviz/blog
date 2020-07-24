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

reportPath <- "D:/StockViz/public/blog/allocation vs tactical"

pdf(NULL)
endDate <- as.Date("2020-07-07")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

numMonths <- 120 #10 years
startEq <- 0.95
endEq <- 0.05
eqSeq <- seq(startEq, endEq, length.out=numMonths)

indexName <- "NIFTY MIDCAP 150 TR"

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp <= '%s'", indexName, endDate))
eqXts <- xts(pDf[,2], pDf[,1])
names(eqXts) <- c("EQUITY")

pDf <- sqlQuery(lcon, sprintf("select time_stamp, tri from index_ccil_tenor where index_name='0_5' and time_stamp <= '%s'", endDate))
bndXts <- xts(pDf[,-1], pDf[,1])
names(bndXts) <- c("BOND")

eqRets <- monthlyReturn(eqXts)
bndRets <- monthlyReturn(bndXts)

eqRets <- eqRets[-1,]
bndRets <- bndRets[-1,]

dateRange <- paste0(first(index(bndRets)), "/")

allRets <- merge(eqRets[dateRange], bndRets[dateRange])
allRets[,2] <- na.locf(allRets[,2], fromLast=T)
allRets <- na.omit(allRets)
allRets <- allRets[-nrow(allRets),]

allocRets <- rollapply(allRets, numMonths, function(X) Return.annualized(xts(coredata(X[,1]) * eqSeq + coredata(X[,2]) * (1-eqSeq), index(X))), by.column = F)
eqRollRets <- rollapply(allRets[,1], numMonths, Return.annualized)
bndRollRets <- rollapply(allRets[,2], numMonths, Return.annualized)

toPlot <- 100*na.omit(merge(allocRets, eqRollRets, bndRollRets))
names(toPlot) <- c('RAMP', indexName, 'BOND')

toPlotDf <- data.frame(toPlot)
toPlotDf$T <- index(toPlot)
toPlotDf <- melt(toPlotDf, id='T')
toPlotDf$sz <- ifelse(toPlotDf$variable == 'RAMP', 2, 0.5)

ggplot(toPlotDf, aes(x=T, y=value, color=variable, group=variable)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	scale_color_viridis(discrete = TRUE) +
	geom_line(aes(size=sz)) +
	scale_size_identity() +
	scale_x_date(breaks = "6 months", date_labels="%Y-%b-%d", expand=c(0, 0)) +
	labs(y='return (%)', x='', color='', fill='', title=sprintf("10-year Returns %s/Short-term Bonds %.0f...%.0f Ramp", indexName, 100*startEq, 100*endEq)) +
	annotate("text", x=max(toPlotDf$T), y=min(toPlotDf$value, na.rm=T), label = "@StockViz", hjust='right', vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/%s.RAMP.%.0f.png", reportPath, gsub(" ", ".", indexName), 100*startEq), width=16, height=8, units="in")


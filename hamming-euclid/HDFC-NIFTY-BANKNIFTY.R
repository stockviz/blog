source("D:/stockviz/r/config.r")
reportPath <- "D:/StockViz/public/blog/hamming-euclid"
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

source("D:/StockViz/public/blog/common/misc.R")

options(stringsAsFactors = FALSE)
options("scipen"=100)
pdf(NULL)


euclidDist <- function(x1, x2){
	c1 <- cumprod(1 + x1)
	c2 <- cumprod(1 + x2)
	edist <- stats::dist(t(data.frame(a=coredata(c1), b=coredata(c2))))
	edistXts <- xts(as.numeric(edist), last(index(x1)))
	return(edistXts)
}

segLength <- 250
ticker <- "HDFC"

pcon <- dbConnect(RPostgres::Postgres(), host=ldbserver2, user= ldbuser2, password=ldbpassword2, dbname=ldbname2)
lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

tickerDf <- dbGetQuery(pcon, sprintf("select date_stamp, c from eod_adjusted_nse where ticker='%s' and date_stamp >= '2015-01-01'", ticker))
tickerXts <- xts(tickerDf[,2], tickerDf[,1])

niftyDf <- sqlQuery(lcon, "select time_stamp, px_close from bhav_index where index_name = 'NIFTY 50' and time_stamp >= '2015-01-01'")
niftyXts <- xts(niftyDf[,2], niftyDf[,1])

bniftyDf <- sqlQuery(lcon, "select time_stamp, px_close from bhav_index where index_name = 'NIFTY BANK' and time_stamp >= '2015-01-01'")
bniftyXts <- xts(bniftyDf[,2], bniftyDf[,1])

retXts <- merge(dailyReturn(tickerXts), dailyReturn(niftyXts), dailyReturn(bniftyXts))
retXts <- retXts[-1]
retXts <- na.omit(retXts)

udXts <- merge(ifelse(retXts[,1] > 0, 1, 0), ifelse(retXts[,2] > 0, 1, 0), ifelse(retXts[,3] > 0, 1, 0))

rollBeta <- merge(rollapply(retXts, segLength, function(X) euclidDist(X[,1], X[, 2]), by.column = F), rollapply(retXts, segLength, function(X) euclidDist(X[,1], X[, 3]), by.column = F))
rollDist <- merge(rollapply(udXts, segLength, function(X) sum(coredata(X[,1]) != coredata(X[,2])), by.column=F), rollapply(udXts, segLength, function(X) sum(coredata(X[,1]) != coredata(X[,3])), by.column=F))

rollBeta <- na.omit(rollBeta)
rollDist <- na.omit(rollDist)

names(rollBeta) <- c('NIFTY', 'BANK NIFTY')
toPlot <- data.frame(rollBeta)
toPlot$T <- index(rollBeta)

toPlot <- melt(toPlot, id='T')

ggplot(toPlot, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	geom_line() +
	scale_x_date(breaks='6 month') +
	labs(x='', y='distance', color='', title=sprintf("%s Euclid Distance", ticker), subtitle=sprintf("%d-day [%s:%s]", segLength, first(index(retXts)), last(index(retXts)))) +
	annotate("text", x=max(index(rollBeta)), y=min(rollBeta), label = "@StockViz", hjust='right', vjust='bottom', col="white", cex=6, fontface = "bold", alpha = 0.5)
	
ggsave(sprintf("%s/%s-euclid.png", reportPath, ticker), width=16, height=8, units="in")
	
names(rollDist) <- c('NIFTY', 'BANK NIFTY')
toPlot <- data.frame(rollDist)
toPlot$T <- index(rollDist)

toPlot <- melt(toPlot, id='T')

ggplot(toPlot, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	geom_line() +
	scale_x_date(breaks='6 month') +
	labs(x='', y='VIX', color='', title=sprintf("%s Hamming Distance", ticker), subtitle=sprintf("%d-day [%s:%s]", segLength, first(index(retXts)), last(index(retXts)))) +
	annotate("text", x=max(index(rollDist)), y=min(rollDist), label = "@StockViz", hjust='right', vjust='bottom', col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/%s-hamming.png", reportPath, ticker), width=16, height=8, units="in")
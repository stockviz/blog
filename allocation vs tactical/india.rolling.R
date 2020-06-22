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
reportPath <- "D:/StockViz/public/blog/allocation vs tactical"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

rollingWindow <- 52*10 #weeks

indexName <- "NIFTY 50 TR"
endDate <- as.Date("2020-05-31")

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp <= '%s'", indexName, endDate))
eqXts <- xts(pDf[,2], pDf[,1])
names(eqXts) <- c("EQUITY")

pDf <- sqlQuery(lcon, sprintf("select time_stamp, tri from index_ccil_tenor where index_name='0_5' and time_stamp <= '%s'", endDate))
bndXts <- xts(pDf[,-1], pDf[,1])
names(bndXts) <- c("BOND")

######################################

eqRets <- weeklyReturn(eqXts)
bndRets <- weeklyReturn(bndXts)

eqRets <- eqRets[-1,]
bndRets <- bndRets[-1,]

dateRange <- paste0(first(index(bndRets)), "/")

allRets <- merge(eqRets[dateRange], bndRets[dateRange])
allRets[,2] <- na.locf(allRets[,2], fromLast=T)
allRets <- na.omit(allRets)

dateRange <- paste0("/", first(index(bndRets))-1)
rndBndRets <- xts(rnorm(nrow(eqRets[dateRange]), mean=mean(coredata(bndRets)), sd=sd(coredata(bndRets))), index(eqRets[dateRange]))

allRets <- rbind(allRets, merge(eqRets[dateRange], rndBndRets))
names(allRets) <- c('EQ.RET', 'BND.RET')

getData <- function(smaLb){
	allXts <- merge(eqXts, SMA(eqXts, smaLb), allRets[,1], allRets[,2]) #1,2,3,4
	allXts <- na.omit(allXts)
	allXts <- merge(allXts, stats::lag(allXts[,3], -1), stats::lag(allXts[,4], -1)) #5,6
	allXts <- na.omit(allXts)

	btest <- merge(ifelse(allXts[,1] > allXts[,2], allXts[,5], allXts[,6]), ifelse(allXts[,1] > allXts[,2], allXts[,5], 0))

	rollingReturns <- rollapply(btest, rollingWindow, Return.annualized, by.column=T)
	return(rollingReturns)
}

rs50 <- getData(50)
rs200 <- getData(200)

rollingReturns <- 100*na.omit(merge(rs50, rs200))
names(rollingReturns) <- c('SMA.50 EQ+BND', 'SMA.50 EQ', 'SMA.200 EQ+BND', 'SMA.200 EQ')

toPlot <- data.frame(rollingReturns)
toPlot$T <- index(rollingReturns)
toPlot <- melt(toPlot, id='T')

p1<-ggplot(toPlot, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_line() +
	scale_size_identity() +
	scale_x_date(date_breaks = "3 months", date_labels = "%Y-%b", expand=c(0, 0)) + 
	labs(y='return (%)', x='', color='', fill='', title=sprintf("Tactical %s SMA %d-week Rolling Returns", indexName, rollingWindow))
	
ggsave(sprintf("%s/tactical %s.rolling.png", reportPath, indexName), width=16, height=8, units="in")	

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')	
library('tidyverse')
library('ggrepel')
library('ggpubr')
library('ggthemes')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("D:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")

reportPath <- "."
lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate <- as.Date("2005-04-01")
pIndex <- "NIFTY200 QUALITY 30 TR"
bIndex <- "NIFTY 50"
betaLb <- 3 #years

rfDf <- sqlQuery(lcon, sprintf("select YTM, TIME_STAMP from INDEX_CCIL_TENOR where INDEX_NAME='0_5' and time_stamp >= '%s'", startDate))
rfXts <- xts(rfDf[,1], rfDf[,2])

pDf <- sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where INDEX_NAME='%s' and time_stamp >= '%s'", pIndex, startDate))
pXts <- xts(pDf[,1], pDf[,2])

nDf <- sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where INDEX_NAME='%s' and time_stamp >= '%s'", bIndex, startDate))
nXts <- xts(nDf[,1], nDf[,2])

allData <- merge(pXts, nXts, rfXts)
allData[,3] <- na.locf(allData[,3])
allData <- na.omit(allData)

allData <- merge(allData, dailyReturn(allData[,1]), dailyReturn(allData[,2]))
allData <- allData[-1,]

betas <- rollapply(allData, 220*betaLb, function(X){
	rfRate <- as.numeric(X[1,3])/100
	rfRateDaily <- ((rfRate + 1)^(1/365)) - 1
	beta <- CAPM.beta(X[,4], X[,5], rfRateDaily)
	c(as.numeric(beta), rfRate)
}, by.column = F)

startDt <- first(index(allData))
endDt <- last(index(allData))
names(betas) <- c("BETA", "Rf")
toPlot1 <- data.frame(betas)
toPlot1$T <- index(betas)

ggplot(toPlot1, aes(x=T, y=BETA)) +
	theme_economist() +
	geom_line() +
	labs(x='', y='beta', color='', title=sprintf("%s/%s %d-year beta", pIndex, bIndex, betaLb), subtitle=sprintf("%s:%s", startDt, endDt))
ggsave(sprintf("%s/beta.%s.%s.%d.png", reportPath, pIndex, bIndex, betaLb), width=16, height=8, units="in")


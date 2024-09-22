library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('tidyverse')
library('ggthemes')
library('viridis')

options("scipen"=100)
options(stringsAsFactors = FALSE)

pdf(NULL)
reportPath <- "."
source("d:/stockviz/r/config.r")
source("d:/stockviz/r/plot.common.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate <- as.Date("2015-12-31")
preCovid <- as.Date("2019-12-31")
postCovid <- as.Date("2020-05-01")

mcxIndexName <- "MCXMETLDEX"
nseIndexName <- "NIFTY METAL"
nseMktName <- "NIFTY 50"

mPx <- sqlQuery(lcon, sprintf("select time_stamp, c from BHAV_INDEX_MCX where index_code='%s' and time_stamp >= '%s'", mcxIndexName, startDate))
nPx <- sqlQuery(lcon, sprintf("select time_stamp, px_open, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s'", nseIndexName, startDate))
nPxTr <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s TR' and time_stamp >= '%s'", nseIndexName, startDate))
mktPx <- sqlQuery(lcon, sprintf("select time_stamp, px_open, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s'", nseMktName, startDate))
mktPxTr <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s TR' and time_stamp >= '%s'", nseMktName, startDate))

mPxts <- xts(mPx[,-1], mPx[,1])
nPxts <- xts(nPx[,-1], nPx[,1])
nPxtTr <- xts(nPxTr[,-1], nPxTr[,1])
mktPxts <- xts(mktPx[,-1], mktPx[,1])
mktPxtTr <- xts(mktPxTr[,-1], mktPxTr[,1])

mRet <- dailyReturn(mPxts)
mRet <- mRet[mRet < 0.2]

############################################

mRetMonthly <- monthlyReturn(mPxts)
nRetTrMonthly <- monthlyReturn(nPxtTr)
mktRetTrMonthly <- monthlyReturn(mktPxtTr)

allRetsMonthly <- merge(mRetMonthly, stats::lag(nRetTrMonthly, -1))
allRetsMonthly <- na.omit(na.locf(allRetsMonthly))

names(allRetsMonthly) <- c('METAL', 'EQUITY')
allRetsMonthlyDf <- data.frame(allRetsMonthly)
allRetsMonthlyDf$T <- index(allRetsMonthly)

retsMonthly <- allRetsMonthlyDf %>% mutate(YM = 100*year(T) + month(T)) %>% group_by(YM) %>% summarize(D = max(T), METAL = 100*last(METAL), EQUITY = 100*last(EQUITY))

retsMonthly %>% {
	ggplot(., aes(x=METAL, y=EQUITY)) +
		theme_economist() +
		geom_point() +
		labs(title=sprintf("%s Index vs. %s TR Monthly Returns", mcxIndexName, nseIndexName), subtitle=sprintf("[%s:%s]", min(.$D), max(.$D)), caption='@StockViz') 
}
ggsave(sprintf("%s/%s-%s.monthly.png", reportPath, mcxIndexName, nseIndexName), width=12, height=6, units="in")

allRetsMonthly <- merge(mRetMonthly, stats::lag(nRetTrMonthly - mktRetTrMonthly, -1))
allRetsMonthly <- na.omit(na.locf(allRetsMonthly))

names(allRetsMonthly) <- c('METAL', 'EQUITY')
allRetsMonthlyDf <- data.frame(allRetsMonthly)
allRetsMonthlyDf$T <- index(allRetsMonthly)

retsMonthly <- allRetsMonthlyDf %>% mutate(YM = 100*year(T) + month(T)) %>% group_by(YM) %>% summarize(D = max(T), METAL = 100*last(METAL), EQUITY = 100*last(EQUITY))

retsMonthly %>% {
	ggplot(., aes(x=METAL, y=EQUITY)) +
		theme_economist() +
		geom_point() +
		labs(title=sprintf("%s Index vs. %s TR Monthly Excess Returns", mcxIndexName, nseIndexName), subtitle=sprintf("[%s:%s]", min(.$D), max(.$D)), caption='@StockViz') 
}
ggsave(sprintf("%s/%s-%s.monthly-excess.png", reportPath, mcxIndexName, nseIndexName), width=12, height=6, units="in")

##############################################

mRetWeekly <- weeklyReturn(mPxts)
nRetTrWeekly <- weeklyReturn(nPxtTr)
mktRetTrWeekly <- weeklyReturn(mktPxtTr)

allRetsWeekly <- merge(mRetWeekly, stats::lag(nRetTrWeekly, -1))
allRetsWeekly <- na.omit(na.locf(allRetsWeekly))

names(allRetsWeekly) <- c('METAL', 'EQUITY')
allRetsWeeklyDf <- data.frame(allRetsWeekly)
allRetsWeeklyDf$T <- index(allRetsWeekly)

retsWeekly <- allRetsWeeklyDf %>% mutate(YM = 100*year(T) + month(T)) %>% group_by(YM) %>% summarize(D = max(T), METAL = 100*last(METAL), EQUITY = 100*last(EQUITY))

retsWeekly %>% filter(METAL < mean(METAL) + 3*sd(METAL) & METAL > mean(METAL) - 3*sd(METAL)) %>% {
	ggplot(., aes(x=METAL, y=EQUITY)) +
		theme_economist() +
		geom_point() +
		labs(title=sprintf("%s Index vs. %s TR Weekly Returns", mcxIndexName, nseIndexName), subtitle=sprintf("[%s:%s]", min(.$D), max(.$D)), caption='@StockViz') 
}
ggsave(sprintf("%s/%s-%s.weekly.png", reportPath, mcxIndexName, nseIndexName), width=12, height=6, units="in")

allRetsWeekly <- merge(mRetWeekly, stats::lag(nRetTrWeekly - mktRetTrWeekly, -1))
allRetsWeekly <- na.omit(na.locf(allRetsWeekly))

names(allRetsWeekly) <- c('METAL', 'EQUITY')
allRetsWeeklyDf <- data.frame(allRetsWeekly)
allRetsWeeklyDf$T <- index(allRetsWeekly)

retsWeekly <- allRetsWeeklyDf %>% mutate(YM = 100*year(T) + month(T)) %>% group_by(YM) %>% summarize(D = max(T), METAL = 100*last(METAL), EQUITY = 100*last(EQUITY))

retsWeekly %>% filter(METAL < mean(METAL) + 3*sd(METAL) & METAL > mean(METAL) - 3*sd(METAL)) %>% {
	ggplot(., aes(x=METAL, y=EQUITY)) +
		theme_economist() +
		geom_point() +
		labs(title=sprintf("%s Index vs. %s TR Weekly Excess Returns", mcxIndexName, nseIndexName), subtitle=sprintf("[%s:%s]", min(.$D), max(.$D)), caption='@StockViz') 
}
ggsave(sprintf("%s/%s-%s.weekly-excess.png", reportPath, mcxIndexName, nseIndexName), width=12, height=6, units="in")

##############################################

nRet <- nPxts$px_close/nPxts$px_open - 1
nRetTr <- dailyReturn(nPxtTr)
mktRet <- mktPxts$px_close/mktPxts$px_open - 1
mktRetTr <- dailyReturn(mktPxtTr)

allRets <- merge(mRet, nRetTr, mktRetTr)
names(allRets) <- c(mcxIndexName, nseIndexName, nseMktName)

Common.PlotCumReturns(allRets, "Metal Commodity vs. Equity Returns (TR)", "", sprintf("%s/%s-%s.cumulative-returns.png", reportPath, mcxIndexName, nseIndexName), NULL)

allRets <- na.omit(merge(100*mRet, 100*stats::lag(nRet, -1)))
names(allRets) <- c('METAL', 'EQUITY')

toPlot <- data.frame(allRets)
toPlot$T <- index(allRets)

toPlot %>% filter(T <= preCovid & METAL < mean(METAL) + sd(METAL) & METAL > mean(METAL) - sd(METAL)) %>%  {
	ggplot(., aes(x=METAL, y=EQUITY)) +
		theme_economist() +
		geom_point() +
		labs(title=sprintf("%s Index vs. %s TR Daily Returns", mcxIndexName, nseIndexName), subtitle=sprintf("Pre-Covid [%s:%s]", min(.$T), max(.$T)), caption='@StockViz') 
}
ggsave(sprintf("%s/%s-%s.daily-returns.pre.png", reportPath, mcxIndexName, nseIndexName), width=12, height=6, units="in")

toPlot %>% filter(T >= postCovid & METAL < mean(METAL) + sd(METAL) & METAL > mean(METAL) - sd(METAL) & EQUITY < mean(EQUITY) + 2*sd(EQUITY) & EQUITY > mean(EQUITY) - 2*sd(EQUITY)) %>%  {
	ggplot(., aes(x=METAL, y=EQUITY)) +
		theme_economist() +
		geom_point() +
		labs(title=sprintf("%s Index vs. %s TR Daily Returns", mcxIndexName, nseIndexName), subtitle=sprintf("Post-Covid [%s:%s]", min(.$T), max(.$T)), caption='@StockViz') 
}
ggsave(sprintf("%s/%s-%s.daily-returns.post.png", reportPath, mcxIndexName, nseIndexName), width=12, height=6, units="in")

allRets <- na.omit(merge(100*mRet, 100*stats::lag(nRet - mktRet, -1)))
names(allRets) <- c('METAL', 'EQUITY')

toPlot <- data.frame(allRets)
toPlot$T <- index(allRets)

toPlot %>% filter(T <= preCovid & METAL < mean(METAL) + sd(METAL) & METAL > mean(METAL) - sd(METAL)) %>%  {
	ggplot(., aes(x=METAL, y=EQUITY)) +
		theme_economist() +
		geom_point() +
		labs(title=sprintf("%s Index vs. %s TR Daily Excess Returns", mcxIndexName, nseIndexName), subtitle=sprintf("Pre-Covid [%s:%s]", min(.$T), max(.$T)), caption='@StockViz') 
}
ggsave(sprintf("%s/%s-%s.daily-excess.pre.png", reportPath, mcxIndexName, nseIndexName), width=12, height=6, units="in")

toPlot %>% filter(T >= postCovid & METAL < mean(METAL) + sd(METAL) & METAL > mean(METAL) - sd(METAL) & EQUITY < mean(EQUITY) + 2*sd(EQUITY) & EQUITY > mean(EQUITY) - 2*sd(EQUITY)) %>% {
	ggplot(., aes(x=METAL, y=EQUITY)) +
		theme_economist() +
		geom_point() +
		labs(title=sprintf("%s Index vs. %s TR Daily Excess Returns", mcxIndexName, nseIndexName), subtitle=sprintf("Post-Covid [%s:%s]", min(.$T), max(.$T)), caption='@StockViz') 
}
ggsave(sprintf("%s/%s-%s.daily-excess.post.png", reportPath, mcxIndexName, nseIndexName), width=12, height=6, units="in")

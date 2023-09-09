source("d:/stockviz/r/config.r")

reportPath<-"."

library('RODBC')
library('RPostgres')
library(data.table)
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('patchwork')

pdf(NULL)
options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pcon <- dbConnect(RPostgres::Postgres(), host=ldbserver2, user= ldbuser2, password=ldbpassword2, dbname=ldbname2)

eodMeta <- dbGetQuery(pcon, "select ticker, min(date_stamp) minds, max(date_stamp) maxds, count(*) ctr from eod_adjusted_nse group by ticker")

endDate <- max(eodMeta$maxds)
eodMeta <- eodMeta[eodMeta$maxds == endDate,]

medianCtr <- median(eodMeta$ctr)
eodMeta <- eodMeta[eodMeta$ctr >= medianCtr,]

startDate <- max(eodMeta$minds)

dretXts <- NULL
for(sym in eodMeta$ticker){
	pDf <- dbGetQuery(pcon, "select c, date_stamp from eod_adjusted_nse where ticker = $1 and date_stamp >= $2", params=list(sym, startDate))
	dretXts <- merge.xts(dretXts, dailyReturn(xts(pDf[,1], pDf[,2])))
}

names(dretXts) <- eodMeta$ticker

dretXts <- dretXts[-1,]
dretDf <- data.frame(dretXts)
dretDf$T <- index(dretXts)

dnextXts <- stats::lag(dretXts, -1)
dnextDf <- data.frame(dnextXts)
dnextDf$T <- index(dnextXts)

dretileDf <- apply(dretXts, 1, function(X) ntile(coredata(X), n=5))
dretileDf <- as.data.frame(t(dretileDf))
colnames(dretileDf) <- eodMeta$ticker
dretileDf$T <- as.Date(rownames(dretileDf))

allDf <- dretDf %>% pivot_longer(cols=-c(T), names_to = 'SYMBOL', values_to = 'RET') %>% 
	inner_join(dretileDf %>% pivot_longer(cols=-c(T), names_to = 'SYMBOL', values_to = 'TILE'), by=c('T', 'SYMBOL')) %>%
	inner_join(dnextDf %>% pivot_longer(cols=-c(T), names_to = 'SYMBOL', values_to = 'RET_1'), by=c('T', 'SYMBOL'))

retByTile <- allDf %>% group_by(T, TILE) %>% summarize(AVG_RET = mean(RET), NEXT_AVG_RET = mean(RET_1)) %>% ungroup()

setDT(retByTile)
statsByQuintile <- na.omit(retByTile)[, as.list(summary(100*AVG_RET)), by = TILE]
nextDayStatsByQuintile <- na.omit(retByTile)[, as.list(summary(100*NEXT_AVG_RET)), by = TILE]

print(statsByQuintile)
print(nextDayStatsByQuintile)


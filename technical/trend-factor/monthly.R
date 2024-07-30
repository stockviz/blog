#ssrn-4621388 A Trend Factor
#construct a long/short trend-factor

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")

reportPath <- "."

library('RODBC')
library('RPostgres')
library('quantmod')
library('tidyverse')
library('lubridate')
library('PerformanceAnalytics')

options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host=ldbserver2, user= ldbuser2, password=ldbpassword2, dbname=ldbname2)

indexName <- "NIFTY 200 TR"
smaLbs <- c(3, 5, 10, 20, 50, 100, 200) #, 400, 600, 800, 1000) #days
regLb <- 12 #months
smoothingLb <- 12 #months

miscDates <- sqlQuery(lcon, "select distinct time_stamp from EQUITY_MISC_INFO")

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s'", indexName))
indexXts <- xts(indexPx[,2], indexPx[,1])
indexMret <- monthlyReturn(indexXts)

rebalDates <- index(indexMret)
rebalDates <- rebalDates[rebalDates >= min(miscDates$time_stamp)]

smaNames <- sapply(smaLbs, function(X) paste0('SMA', X))
regFmla <- as.formula(paste0("MRET ~ ", paste0(smaNames, collapse='+')))

retsSma <- tibble()

for(ii in 1:length(rebalDates)){
	startDate <- rebalDates[ii]

	if(is.na(startDate)) next
	
	symbols <- sqlQuery(lcon, sprintf("select top 200 symbol, ff_mkt_cap_cr from EQUITY_MISC_INFO 
											where time_stamp=(select max(time_stamp) from EQUITY_MISC_INFO where time_stamp <= '%s') 
											order by ff_mkt_cap_cr desc", 
											startDate))[,1]
		
	if(length(symbols) < 100) next
	
	expectedTs <- max(smaLbs)
	maStDt <- first(index(tail(indexXts[paste0('/', startDate)], expectedTs)))
				
	for(sym in symbols){
		pxDf1 <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
									where ticker = $1 and date_stamp >= $2 and date_stamp <= $3 order by date_stamp", params=list(sym, maStDt-50, startDate))
									
		if(nrow(pxDf1) < expectedTs) next
		pxXts1 <- xts(pxDf1[,2], pxDf1[,1])
		dRet <- monthlyReturn(pxXts1)
				
		smaXts <- do.call(merge.xts, lapply(smaLbs, function(lb) { temp <- SMA(pxXts1, lb)
														temp <- as.xts(temp, dateFormat="Date")
														temp}))
		
		normSma <- xts::last(smaXts)/drop(coredata(xts::last(pxXts1))) #normalize the moving average prices by the closing price on the last trading day of the month
		names(normSma) <- smaNames
		lastRet <- xts::last(dRet)
		retsSma <- rbind(retsSma, c(toString(startDate), sym, merge(lastRet, normSma)))
	}
}

names(retsSma) <- c('ASOF', 'SYMBOL', 'RET', smaNames)
retsSma$ASOF <- as.Date(retsSma$ASOF)
retsSma$RET <- as.numeric(retsSma$RET)
retsSma <- retsSma %>% mutate_at(vars(starts_with('SMA')), as.numeric)

portRebalDates <- sort(unique(retsSma$ASOF))

betas <- tibble()
for(ii in (regLb+2):length(portRebalDates)){
	startDate <- portRebalDates[ii - regLb - 1]
	endDate <- portRebalDates[ii]
	
	retsSubset <- retsSma %>% filter(ASOF >= startDate & ASOF <= endDate) 
	
	for(sym in unique(retsSubset$SYMBOL)){
		symSubset <- retsSubset %>% filter(SYMBOL == sym) %>% arrange(ASOF) %>% mutate(MRET = lead(RET)) %>% as.data.frame()
		if(nrow(symSubset) < regLb) next
		cfs <- t(data.frame(coefficients(lm(regFmla, data = na.omit(symSubset))))) # a cross-section regression of stock returns on observed normalized MA signals. here, we use rolling regLb months 
		betas <- rbind(betas, c(sym, toString(endDate), cfs))
	}
}

names(betas) <- c('SYMBOL', 'ASOF', 'INT', smaNames)
betas$ASOF <- as.Date(betas$ASOF)
betas <- betas %>% mutate_at(vars(starts_with('SMA')), as.numeric)

portRebalDates <- sort(unique(betas$ASOF))

estimates <- tibble()
for(ii in (smoothingLb+2):length(portRebalDates)){
	startDate <- portRebalDates[ii - smoothingLb - 1]
	endDate <- portRebalDates[ii - 1]
	nextDate <- portRebalDates[ii]
	betasSubset <- betas %>% filter(ASOF >= startDate & ASOF <= endDate) 
	
	for(sym in unique(betasSubset$SYMBOL)){
		avgBetas <- betasSubset %>% filter(SYMBOL == sym) %>% summarize_at(vars(starts_with('SMA')), mean) # average of the estimated loadings on the trend signals over the past 12 months
		normSma <- retsSma %>% filter(SYMBOL == sym & ASOF == nextDate) %>% select(starts_with('SMA')) # next month trend signals
		
		if(nrow(avgBetas) == 0 || nrow(normSma) == 0) next
		
		smaMetric <- sum(as.matrix(avgBetas) %*% diag(normSma[1,]), na.rm=TRUE) # estimate the expected return for next month		
		estimates <- rbind(estimates, c(sym, toString(nextDate), smaMetric))
	}
}
	
names(estimates) <- c('SYMBOL', 'ASOF', 'EST')
estimates$ASOF <- as.Date(estimates$ASOF)
estimates$EST <- as.numeric(estimates$EST)

portRebalDates <- sort(unique(estimates$ASOF))

factorRets <- tibble()
for(ii in 2:length(portRebalDates)){
	startDate <- portRebalDates[ii-1]
	endDate <- portRebalDates[ii]
	symRanks <- estimates %>% filter(ASOF == startDate) %>% mutate(TILE = ntile(EST, 5))
	
	longs <- symRanks %>% filter(TILE == 5 & EST > 0) %>% slice_max(EST, n=20) #go long the largest quintile estimates
	shorts <- symRanks %>% filter(TILE == 1 & EST < 0) %>% slice_min(EST, n=20) #go short the smallest quintile estimates
	
	longRet <- as.numeric(retsSma %>% filter(SYMBOL %in% longs$SYMBOL & ASOF == endDate) %>% summarize(R = mean(RET, na.rm=T))) #equal weighted portfolios
	shortRet <- as.numeric(retsSma %>% filter(SYMBOL %in% shorts$SYMBOL & ASOF == endDate) %>% summarize(R = mean(RET, na.rm=T)))
	
	factorRets <- rbind(factorRets, c(toString(endDate), longRet, shortRet))
}

names(factorRets) <- c('ASOF', 'LONG', 'SHORT')
factorRets$ASOF <- as.Date(factorRets$ASOF)
factorRets$LONG <- as.numeric(factorRets$LONG)
factorRets$SHORT <- as.numeric(factorRets$SHORT)

save(factorRets, file=sprintf("%s/trend-factor.%dx%d.Rdata", reportPath, regLb, smoothingLb))


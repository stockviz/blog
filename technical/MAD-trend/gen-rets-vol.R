# SSRN-id3111334 Moving Average Distance as a Predictor of Equity Returns

library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')

library('tidyverse')
library('lubridate')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

lbSmall <- 21 #50 #21
lbLarge <- 200
portfolioSize <- 20
sigmaMultiplier <- 1

miscDates <- sqlQuery(lcon, "select distinct time_stamp from EQUITY_MISC_INFO")
pxDates <- dbGetQuery(pgCon, "select date_stamp, count(*) as cd from eod_adjusted_nse group by date_stamp")

nDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", "NIFTY 200 TR", 
								min(miscDates$time_stamp) - lbLarge, max(miscDates$time_stamp)))
n100Xts <- xts(nDf[,2], nDf[,1])

nDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", "NIFTY MIDCAP 150 TR", 
								min(miscDates$time_stamp) - lbLarge, max(miscDates$time_stamp)))
m150Xts <- xts(nDf[,2], nDf[,1])

nDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", "NIFTY SMALLCAP 50 TR", 
								min(miscDates$time_stamp) - lbLarge, max(miscDates$time_stamp)))
s50Xts <- xts(nDf[,2], nDf[,1])

rebalDates <- miscDates %>% inner_join(pxDates, by=c('time_stamp' = 'date_stamp')) %>% filter(cd > 500) %>% 
	mutate(YM = year(time_stamp)*100 + month(time_stamp)) %>%
	group_by(YM) %>%
	summarize(ST=min(time_stamp)) %>%
	select(ST) %>%
	ungroup() %>%
	as.data.frame()

getPrices <- function(syms, numDays, endDate){
	prices <- NULL
	symSubset <- c()
	for(i in 1:length(syms)){
		sym <- syms[i]
		pxDf <- dbGetQuery(pgCon, sprintf("select date_stamp, c from eod_adjusted_nse 
									where ticker = $1 and date_stamp <= $2 order by date_stamp desc limit %d", numDays), params=list(sym, endDate))
		
		if(nrow(pxDf) < numDays) next
		
		pXts <- xts(pxDf[,2], pxDf[,1])
		
		if(last(index(pXts)) != endDate) next
		
		prices <- merge.xts(prices, pXts)
		symSubset <- c(symSubset, sym)
	}

	names(prices) <- symSubset
	return(prices)
}

getReturns <- function(syms, startDate, endDate){
	rets <- data.frame(SYMBOL = "", RET = 0.0)
	for(i in 1:length(syms)){
		sym <- syms[i]
		pxDf <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
									where ticker = $1 and date_stamp >= $2 and date_stamp <= $3", params=list(sym, startDate, endDate))
		
		if(nrow(pxDf) == 0) print(paste("prices not found for", sym))
		
		pXts <- xts(pxDf[,2], pxDf[,1])
		
		if(first(index(pXts)) != startDate || last(index(pXts)) != endDate) next
		ret <- as.numeric(last(pXts)/first(pXts) - 1)
		rets <- rbind(rets, c(sym, ret))
	}
	rets <- rets[-1,]
	rets[,2] <- as.numeric(rets[,2])
	return(rets)
}
	
retDf <- data.frame(TIME_STAMP = "", RET = 0.0, N100 = 0.0, M150 = 0.0, S50 = 0.0)	
for(ii in 2:nrow(rebalDates)){
#for(ii in 2:10){
	cat(paste(ii, '/'))
	nextDate <- rebalDates$ST[ii]
	endDate <- rebalDates$ST[ii - 1]
	symbols <- sqlQuery(lcon, sprintf("select top 500 symbol, ff_mkt_cap_cr from EQUITY_MISC_INFO where time_stamp='%s' order by ff_mkt_cap_cr desc", endDate))[,1]
	
	prices <- getPrices(symbols, lbLarge + lbLarge, endDate)
	
	if(is.null(prices) || ncol(prices) == 0){
		print(paste("no prices found for", ii, endDate))
		next
	}
	
	symSubset <- names(prices)
	
	ratioDf <- data.frame(SYMBOL = "", MAD = 0.0, SIGMA = 0.0, VOL = 0.0)
	for(j in 1:ncol(prices)){
		tryCatch({
			smaSmall <- SMA(prices[,j], lbSmall)
			smaLarge <- SMA(prices[,j], lbLarge)
			smaRatio <- as.numeric(last(smaSmall))/as.numeric(last(smaLarge))
			smaSig <- sd(tail(smaSmall, 20)/tail(smaLarge, 20))
			
			dretSd <- as.numeric(sd(dailyReturn(prices[,j]), na.rm=T))
			ratioDf <- rbind(ratioDf, c(symSubset[j], smaRatio, smaSig, dretSd))
		}, error = function(e){})
	}
	ratioDf <- ratioDf[-1,]
	
	ratioDf[,2] <- as.numeric(ratioDf[,2])
	ratioDf[,3] <- as.numeric(ratioDf[,3])
	ratioDf[,4] <- as.numeric(ratioDf[,4])
	
	volCutoff <- median(ratioDf$VOL, na.rm=T) #+ sd(ratioDf$VOL, na.rm=T)
	top20Syms <- ratioDf %>% filter((MAD > 1 + sigmaMultiplier*SIGMA) & (VOL < volCutoff)) %>% slice_max(MAD, n=20) %>% select(SYMBOL)
	top20Syms <- top20Syms[,1]
	
	portRets <- getReturns(top20Syms, endDate, nextDate)
	ret <- sum(portRets[,2]/portfolioSize)
	
	cat(paste("sz:", nrow(portRets), ": ", round(ret*100, 2), "|"))
	
	benchRet1 <- as.numeric(Return.cumulative(dailyReturn(n100Xts[paste0(endDate, "/", nextDate)])))
	benchRet2 <- as.numeric(Return.cumulative(dailyReturn(m150Xts[paste0(endDate, "/", nextDate)])))
	benchRet3 <- as.numeric(Return.cumulative(dailyReturn(s50Xts[paste0(endDate, "/", nextDate)])))
	
	retDf <- rbind(retDf, c(toString(nextDate), ret, benchRet1, benchRet2, benchRet3))
}

retDf <- retDf[-1,]
retDf[,1] <- as.Date(retDf[,1])
retDf[,2] <- as.numeric(retDf[,2])
retDf[,3] <- as.numeric(retDf[,3])
retDf[,4] <- as.numeric(retDf[,4])
retDf[,5] <- as.numeric(retDf[,5])

save(retDf, file=sprintf("%s/monthly-returns-vol.%d.%d.%d.Rdata", reportPath, lbSmall, lbLarge, sigmaMultiplier))

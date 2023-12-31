# SSRN-id3111334 Moving Average Distance as a Predictor of Equity Returns

library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')

library('tidyverse')
library('lubridate')

library('foreach')
library('doParallel')
library("doFuture")
registerDoFuture()
plan(multisession, workers = 6)

options("scipen"=100)
options(stringsAsFactors = FALSE)

#source("d:/stockviz/r/config.r")
source("/mnt/hollandr/config.r")

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

lbSmalls <- seq(3, 50, by=3)
lbLarges <- seq(50, 300, by=50)

lbSmallMax <- max(lbSmalls)
lbLargeMax <- max(lbLarges)
portfolioSize <- 20


miscDates <- sqlQuery(lcon, "select distinct time_stamp from EQUITY_MISC_INFO")
pxDates <- dbGetQuery(pgCon, "select date_stamp, count(*) as cd from eod_adjusted_nse group by date_stamp")

nDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", "NIFTY 200 TR", 
								min(miscDates$time_stamp) - lbLargeMax, max(miscDates$time_stamp)))
n100Xts <- xts(nDf[,2], nDf[,1])

nDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", "NIFTY MIDCAP 150 TR", 
								min(miscDates$time_stamp) - lbLargeMax, max(miscDates$time_stamp)))
m150Xts <- xts(nDf[,2], nDf[,1])

nDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", "NIFTY SMALLCAP 50 TR", 
								min(miscDates$time_stamp) - lbLargeMax, max(miscDates$time_stamp)))
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
	
retDf <- data.frame(TIME_STAMP = "", LB_SMALL = 0, LB_LARGE = 0, P_SIZE = 0, RET = 0.0)	
for(ii in 2:nrow(rebalDates)){
#for(ii in 2:10){
	cat(paste(ii, '/'))
	nextDate <- rebalDates$ST[ii]
	endDate <- rebalDates$ST[ii - 1]
	symbols <- sqlQuery(lcon, sprintf("select top 500 symbol, ff_mkt_cap_cr from EQUITY_MISC_INFO where time_stamp='%s' order by ff_mkt_cap_cr desc", endDate))[,1]
	
	prices <- getPrices(symbols, lbLargeMax*2, endDate)
	
	if(is.null(prices) || ncol(prices) == 0){
		print(paste("no prices found for", ii, endDate))
		next
	}
	
	symSubset <- names(prices)
	
	ratioDf <- foreach(j=1:ncol(prices), .combine=rbind) %do% {
		samplePx <- na.trim(prices[,j], sides='left')
		if(anyNA(samplePx)) return(c(NA, NA, "", NA, NA, NA))
		
		ratioDfi <- foreach(i1=1:length(lbLarges), .combine=rbind) %dopar% {
			lbLarge <- lbLarges[i1]
			smaLarge <- SMA(samplePx, lbLarge)
			dretSd <- as.numeric(sd(dailyReturn(tail(samplePx, lbLarge)), na.rm=T))
		
			ratioDfj <- foreach(i2=1:length(lbSmalls), .combine=rbind) %dopar% {
				lbSmall <- lbSmalls[i2]
				smaSmall <- SMA(samplePx, lbSmall)
				
				smaRatio <- as.numeric(last(smaSmall))/as.numeric(last(smaLarge))
				
				c(lbSmall, lbLarge, symSubset[j], smaRatio, dretSd)
			}
			
			ratioDfj
		}
		ratioDfi
	}
		
	colnames(ratioDf) <- c('LB_SMALL', 'LB_LARGE', 'SYMBOL', 'MAD', 'VOL')
	
	ratioDf <- transform(ratioDf, LB_SMALL = as.numeric(as.character(LB_SMALL)))
	ratioDf <- transform(ratioDf, LB_LARGE = as.numeric(as.character(LB_LARGE)))
	ratioDf <- transform(ratioDf, MAD = as.numeric(as.character(MAD)))
	ratioDf <- transform(ratioDf, VOL = as.numeric(as.character(VOL)))
	
	for(lbSmall in lbSmalls){
		for(lbLarge in lbLarges){
			tryCatch({
				top20Syms <- ratioDf %>% filter(LB_SMALL == lbSmall & LB_LARGE == lbLarge & !is.na(MAD)) %>% 
											filter((MAD > 1) & (VOL < median(VOL, na.rm=T))) %>% 
											slice_max(MAD, n=20) %>% 
											select(SYMBOL)
				top20Syms <- top20Syms[,1]
		
				portRets <- getReturns(top20Syms, endDate, nextDate)
				ret <- sum(portRets[,2]/portfolioSize)

				retDf <- rbind(retDf, c(toString(nextDate), lbSmall, lbLarge, nrow(portRets), ret))
			}, error = function(e) { print(e) })
		}
	}
}

retDf <- retDf[-1,]
retDf[,1] <- as.Date(retDf[,1])
retDf[,2] <- as.numeric(retDf[,2])
retDf[,3] <- as.numeric(retDf[,3])
retDf[,4] <- as.numeric(retDf[,4])
retDf[,5] <- as.numeric(retDf[,5])

save(retDf, file=sprintf("%s/multi.monthly-returns-vol.Rdata", reportPath))

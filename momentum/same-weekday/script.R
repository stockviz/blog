### https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4806275

library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('prophet')

library('tidyverse')
library('lubridate')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
#source("/mnt/hollandr/config.r")

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

indexName <- "NIFTY 200 TR"
lookback <- 365 #days
portfolioSize <- 20 #items
skipMonths <- 0:1 #months

miscDates <- sqlQuery(lcon, "select distinct time_stamp from EQUITY_MISC_INFO")

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, min(miscDates$time_stamp) - lookback, max(miscDates$time_stamp)))
indexXts <- xts(indexPx[,2], indexPx[,1])

rebalDates <- index(monthlyReturn(indexXts))
rebalDates <- rebalDates[rebalDates >= min(miscDates$time_stamp)]

getReturns <- function(syms, startDate, endDate, expectedTs, pSize){
	totalRet <- 0.0
	symCount <- 0
	selectedSymbols <- c()
	rets <- NULL
	for(i in 1:length(syms)){
		sym <- syms[i]
		pxDf <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
									where ticker = $1 and date_stamp >= $2 and date_stamp <= $3 order by date_stamp", params=list(sym, startDate, endDate))
		
		if(nrow(pxDf) < expectedTs-2) next
		
		symCount <- symCount + 1
		drt <- dailyReturn(xts(pxDf[,2], pxDf[,1]))
		totalRet <- totalRet + as.numeric(Return.cumulative(drt))
		
		selectedSymbols <- c(selectedSymbols, sym)
		rets <- merge.xts(rets, drt)
		
		if (symCount >= pSize) break
	}
	periodRet <- totalRet/symCount
	names(rets) <- selectedSymbols
	return(list(rets, periodRet))
}

symRets <- data.frame(PERIOD_END="", STRATEGY="", SKIP_MO = 0, RET=0.0, OVERLAP=0)
symRetsWeekly <- data.frame(PERIOD_END="", DOW=-1, SKIP_MO = 0, RET=0.0)
for(skipMo in skipMonths){
	for(ii in (skipMo+2):(length(rebalDates) - 1)){
		skipDate <- rebalDates[ii - skipMo - 1]
		startDate <- rebalDates[ii-1]
		endDate <- rebalDates[ii]
		prevSyms <- vector()
	
		if(is.na(skipDate) || is.na(startDate) || is.na(endDate)) next
		
		if(as.numeric(endDate - startDate) < 20){
			print(paste("skipping:", startDate, endDate))
			next
		}
		
		symbols <- sqlQuery(lcon, sprintf("select top 200 symbol, ff_mkt_cap_cr from EQUITY_MISC_INFO 
												where time_stamp=(select max(time_stamp) from EQUITY_MISC_INFO where time_stamp <= '%s') 
												order by ff_mkt_cap_cr desc", 
												startDate))[,1]
			
		if(length(symbols) < 5*portfolioSize) next
		
		expectedTs <- nrow(indexXts[paste0(skipDate-lookback, "/", skipDate)])
					
		allRets <- NULL
		retDf <- data.frame(SYMBOL="", RET=0.0, VOLATILITY=0.0)
		for(sym in symbols){
			pxDf1 <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
										where ticker = $1 and date_stamp >= $2 and date_stamp < $3 order by date_stamp", params=list(sym, skipDate-lookback, skipDate))
										
			if(nrow(pxDf1) < expectedTs*0.95) next
			pxXts1 <- xts(pxDf1[,2], pxDf1[,1])
			dRet <- dailyReturn(pxXts1)
			allRets <- merge.xts(allRets, dRet)
			pxRet <- as.numeric(Return.cumulative(dRet))
			pxVol <- as.numeric(xts::last(volatility(pxXts1, n=50)))
			retDf <- rbind(retDf, c(sym, pxRet, pxVol))
		}
		retDf <- retDf[-1,]
		names(allRets) <- retDf$SYMBOL
		
		highRet <- head(retDf[order(retDf$VOLATILITY),], 100)
		highRet <- head(highRet[order(highRet$RET, decreasing=T),], portfolioSize * 2)
		
		periodRets2 <- getReturns(highRet$SYMBOL, startDate, endDate, nrow(indexXts[paste0(startDate, "/", endDate)]), portfolioSize)
		drets <- periodRets2[[1]]
		monhtlySyms <- names(drets)
		monthlyRet <- periodRets2[[2]]
		overlap <- length(base::intersect(monhtlySyms, prevSyms))/portfolioSize
		symRets <- rbind(symRets, c(toString(endDate), "NAIVE", skipMo, monthlyRet, overlap))
		
		prevSymsDowk <- list()
		
		periodRet <- 0.0
		for(dowk in 1:5){
			cat(paste(skipMo, ii, dowk, " ... "))
			
			#shortlist the best performing stocks on the dow during the formation period
			allRetsDowk <- unlist(lapply(names(allRets), function(X) Return.cumulative(allRets[.indexwday(allRets) == dowk, X])))
			allRetsDowkDf <- data.frame(SYMBOL = names(allRets), RET = allRetsDowk)
			
			highRetDowk <- head(allRetsDowkDf[order(allRetsDowkDf$RET, decreasing=T),], portfolioSize)
			periodRetsDowk <- getReturns(highRetDowk$SYMBOL, startDate - 10, endDate, nrow(indexXts[paste0(startDate - 10, "/", endDate)]), portfolioSize)
			
			#for the shortlisted stocks, calculate dow returns for the entire month
			prevSymsDowk[[dowk]] <- names(periodRetsDowk[[1]])
			retsDowk <- periodRetsDowk[[1]][paste0(startDate, "/", endDate)]
			totalRet <- sum(unlist(lapply(names(retsDowk), function(X) Return.cumulative(retsDowk[.indexwday(retsDowk) == dowk, X]))))
			periodRet <- periodRet + totalRet/ncol(retsDowk) #arithmetic returns (not geometric)!
			
			symRetsWeekly <- rbind(symRetsWeekly, c(toString(endDate), dowk, skipMo, totalRet/ncol(retsDowk)))
		}
		
		overlap <- length(Reduce(base::intersect, prevSymsDowk))/portfolioSize
		symRets <- rbind(symRets, c(toString(endDate), "DOW", skipMo, periodRet, overlap))
	}
	save(symRets, file=sprintf("%s/symRets.Rdata", reportPath))
}

for(ii in 2:length(rebalDates)){
	startDate <- rebalDates[ii-1]
	endDate <- rebalDates[ii]
	benchRet <- as.numeric(Return.cumulative(dailyReturn(indexXts[paste0(startDate, "/", endDate)])))
	symRets <- rbind(symRets, c(toString(endDate), "BENCH", 0, benchRet, 1))
}

symRets <- symRets[-1,]
symRets$PERIOD_END <- as.Date(symRets$PERIOD_END)
symRets$RET <- as.numeric(symRets$RET)
symRets$OVERLAP <- as.numeric(symRets$OVERLAP)

save(symRets, file=sprintf("%s/symRets.Rdata", reportPath))

symRetsWeekly <- symRetsWeekly[-1,]
symRetsWeekly$PERIOD_END <- as.Date(symRetsWeekly$PERIOD_END)
symRetsWeekly$RET <- as.numeric(symRetsWeekly$RET)
symRetsWeekly$DOW <- as.numeric(symRetsWeekly$DOW)

save(symRetsWeekly, file=sprintf("%s/symRetsWeekly.Rdata", reportPath))

########################################


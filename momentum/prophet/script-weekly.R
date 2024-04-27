### regress 12, 1 month returns over next month return for each stock in the universe. rank predicted returns to build portfolio

library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('prophet')

library('tidyverse')
library('lubridate')

library('foreach')
library('doParallel')
library("doFuture")
registerDoFuture()

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
#source("/mnt/hollandr/config.r")

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

indexName <- "NIFTY 200 TR"
lookback <- 2*365 #days
portfolioSize <- 20 #items

miscDates <- sqlQuery(lcon, "select distinct time_stamp from EQUITY_MISC_INFO")

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, min(miscDates$time_stamp) - lookback, max(miscDates$time_stamp)))
indexXts <- xts(indexPx[,2], indexPx[,1])

rebalDates <- index(weeklyReturn(indexXts))
rebalDates <- rebalDates[rebalDates >= min(miscDates$time_stamp)]

getReturns <- function(syms, startDate, endDate, expectedTs, pSize){
	totalRet <- 0.0
	symCount <- 0
	selectedSymbols <- c()
	for(i in 1:length(syms)){
		sym <- syms[i]
		pxDf <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
									where ticker = $1 and date_stamp >= $2 and date_stamp <= $3 order by date_stamp", params=list(sym, startDate, endDate))
		
		if(nrow(pxDf) < expectedTs-2) next
		
		symCount <- symCount + 1
		totalRet <- totalRet + as.numeric(Return.cumulative(dailyReturn(xts(pxDf[,2], pxDf[,1]))))
		
		selectedSymbols <- c(selectedSymbols, sym)
		
		if (symCount >= pSize) break
	}
	periodRet <- totalRet/symCount
	
	return(list(selectedSymbols, periodRet))
}

prevSyms <- list()
	
#symRets <- data.frame(PERIOD_END="", STRATEGY="", RET=0.0, OVERLAP=0)
#for(rfreq in 1:4){
symRets <- foreach(rfreq in 1:4, .combine=rbind) %do% {
	prevSyms[[rfreq]] <<- vector()
	freqCtr <- 0
	analDf <- data.frame(PERIOD_END="", STRATEGY="", RET=0.0, OVERLAP=0)
	for(ii in 3:length(rebalDates)){
		cat(paste(rfreq, ii, freqCtr, " ... "))
		
		modelDate <- rebalDates[ii-2]
		startDate <- rebalDates[ii-1]
		endDate <- rebalDates[ii]
		
		if(freqCtr %% rfreq == 0){
			symbols <- sqlQuery(lcon, sprintf("select top 200 symbol, ff_mkt_cap_cr from EQUITY_MISC_INFO 
												where time_stamp=(select max(time_stamp) from EQUITY_MISC_INFO where time_stamp <= '%s') 
												order by ff_mkt_cap_cr desc", 
												startDate))[,1]
			
			if(length(symbols) < 5*portfolioSize) next
			
			expectedTsModel <- nrow(indexXts[paste0(modelDate - lookback, "/", startDate)])
			
			retDfLm1 <- data.frame(SYMBOL="", RET=0.0, VOLATILITY=0.0)
			for(sym in symbols){
				pxDf1 <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
											where ticker = $1 and date_stamp >= $2 and date_stamp < $3 order by date_stamp", params=list(sym, modelDate - lookback, startDate))
											
				if(nrow(pxDf1) < expectedTsModel*0.95) next
				
				#build a linear model...
				
				pxXts1 <- xts(pxDf1[,2], pxDf1[,1])
				mHist <- na.omit(weeklyReturn(pxXts1))
								
				mHistDf <- data.frame(ds = index(mHist), y=coredata(mHist))
				colnames(mHistDf) <- c('ds', 'y')
				
				lModel1 <- prophet(mHistDf, daily.seasonality = FALSE)
				future <- make_future_dataframe(lModel1, periods=rfreq, freq='week')
				
				#predict next returns
				nextDf <- predict(lModel1, future)
				predRet1 <- as.numeric(Return.cumulative(xts(tail(nextDf$yhat, rfreq), tail(nextDf$ds, rfreq))))
				pxVol <- as.numeric(xts::last(volatility(pxXts1, n=50)))
				
				retDfLm1 <- rbind(retDfLm1, c(sym, predRet1, pxVol))
			}
			retDfLm1 <- retDfLm1[-1,]
			
			retDfLm1$RET <- as.numeric(retDfLm1$RET)
			retDfLm1$VOLATILITY <- as.numeric(retDfLm1$VOLATILITY)
			
			highRet <- retDfLm1[retDfLm1$RET > 0 & retDfLm1$RET > mean(retDfLm1$RET),]
			highRet <- highRet[order(highRet$RET, decreasing=T),]
		}
		freqCtr <- freqCtr + 1
		
		expectedTs <- nrow(indexXts[paste0(startDate, "/", endDate)])
		
		periodRets <- getReturns(highRet$SYMBOL, startDate, endDate, expectedTs, portfolioSize)
		overlap <- length(base::intersect(periodRets[[1]], prevSyms[[rfreq]]))/portfolioSize
		prevSyms[[rfreq]] <<- periodRets[[1]]
		
		analDf <- rbind(analDf, c(toString(endDate), paste0("PROPHET_", rfreq), periodRets[[2]], overlap))
	}
	analDf
}

save(symRets, file=sprintf("%s/symRets.weekly.Rdata", reportPath))

for(ii in 2:length(rebalDates)){
	startDate <- rebalDates[ii-1]
	endDate <- rebalDates[ii]
	benchRet <- as.numeric(Return.cumulative(dailyReturn(indexXts[paste0(startDate, "/", endDate)])))
	symRets <- rbind(symRets, c(toString(endDate), "BENCH", benchRet, 1))
}

symRets <- symRets[-1,]
symRets$PERIOD_END <- as.Date(symRets$PERIOD_END)
symRets$RET <- as.numeric(symRets$RET)
symRets$OVERLAP <- as.numeric(symRets$OVERLAP)

save(symRets, file=sprintf("%s/symRets.weekly.Rdata", reportPath))

########################################


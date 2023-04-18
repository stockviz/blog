### compare returns of 12mo momentum vs. 12_Xmo momentum with different rebalance frequencies

library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')	

library('tidyverse')
library('lubridate')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

indexName <- "NIFTY 200 TR"
lookback <- 500 #days
skipMonths <- 0:5 #months

miscDates <- sqlQuery(lcon, "select distinct time_stamp from EQUITY_MISC_INFO")

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, min(miscDates$time_stamp) - lookback, max(miscDates$time_stamp)))
indexXts <- xts(indexPx[,2], indexPx[,1])

rebalDates <- miscDates %>% mutate(YM = year(time_stamp)*100 + month(time_stamp)) %>%
	group_by(YM) %>%
	summarize(ST=min(time_stamp)) %>%
	select(ST) %>%
	ungroup() %>%
	as.data.frame()

getReturns <- function(syms, startDate, endDate, expectedTs){
	totalRet <- 0.0
	symCount <- 0
	for(i in 1:length(syms)){
		sym <- syms[i]
		pxDf <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
									where ticker = $1 and date_stamp >= $2 and date_stamp <= $3 order by date_stamp", params=list(sym, startDate, endDate))
		
		if(nrow(pxDf) < expectedTs-2) next
		
		symCount <- symCount + 1
		totalRet <- totalRet + as.numeric(Return.cumulative(dailyReturn(xts(pxDf[,2], pxDf[,1]))))
		
		if (symCount >= 20) break
	}
	periodRet <- totalRet/symCount
	
	return(periodRet)
}
	
symRets <- data.frame(PERIOD_END="", STRATEGY="", REBAL_FREQ=0, RET=0.0)

for(rfreq in 1:12){
	for(skipMo in skipMonths){
		print(paste(rfreq, "/", skipMo))
		freqCtr <- 0
		for(ii in (skipMo+2):nrow(rebalDates)){
			skipDate <- rebalDates$ST[ii - skipMo - 1]
			startDate <- rebalDates$ST[ii-1]
			endDate <- rebalDates$ST[ii]
			
			if(freqCtr %% rfreq == 0){
				symbols <- sqlQuery(lcon, sprintf("select top 200 symbol, ff_mkt_cap_cr from EQUITY_MISC_INFO where time_stamp='%s' order by ff_mkt_cap_cr desc", startDate))[,1]
				
				retDf <- data.frame(SYMBOL="", RET=0.0)
				for(sym in symbols){
					pxDf1 <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
												where ticker = $1 and date_stamp >= $2 and date_stamp < $3 order by date_stamp", params=list(sym, skipDate-365, skipDate))
												
					if(nrow(pxDf1) < 200) next
					
					retDf <- rbind(retDf, c(sym, as.numeric(Return.cumulative(dailyReturn(xts(pxDf1[,2], pxDf1[,1]))))))
				}
				retDf <- retDf[-1,]
				
				highRet <- head(retDf[order(retDf$RET, decreasing=T),], 100)
			}
			freqCtr <- freqCtr + 1
			
			expectedTs <- nrow(indexXts[paste0(startDate, "/", endDate)])
			
			periodRet <- getReturns(highRet$SYMBOL, startDate, endDate, expectedTs)

			symRets <- rbind(symRets, c(toString(endDate), paste0("MOM_", skipMo), rfreq, periodRet))
		}
	}
}

for(ii in 2:nrow(rebalDates)){
	startDate <- rebalDates$ST[ii-1]
	endDate <- rebalDates$ST[ii]
	benchRet <- as.numeric(Return.cumulative(dailyReturn(indexXts[paste0(startDate, "/", endDate)])))
	symRets <- rbind(symRets, c(toString(endDate), "BENCH", 0, benchRet))
}

symRets <- symRets[-1,]
symRets$PERIOD_END <- as.Date(symRets$PERIOD_END)
symRets$REBAL_FREQ <- as.numeric(symRets$REBAL_FREQ)
symRets$RET <- as.numeric(symRets$RET)

save(symRets, file=sprintf("%s/symRets.Rdata", reportPath))

########################################


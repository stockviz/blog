### compare returns of 12mo momentum with different weejkt rebalance frequencies

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

miscDates <- sqlQuery(lcon, "select distinct time_stamp from EQUITY_MISC_INFO")

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, min(miscDates$time_stamp) - lookback, max(miscDates$time_stamp)))
indexXts <- xts(indexPx[,2], indexPx[,1])

rebalDates <- index(weeklyReturn(indexXts))
rebalDates <- rebalDates[rebalDates >= min(miscDates$time_stamp)]

getReturns <- function(syms, startDate, endDate, expectedTs){
	totalRet <- 0.0
	symCount <- 0
	pSyms <- c()
	for(i in 1:length(syms)){
		sym <- syms[i]
		pxDf <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
									where ticker = $1 and date_stamp >= $2 and date_stamp <= $3 order by date_stamp", params=list(sym, startDate, endDate))
		
		if(nrow(pxDf) == 0 || nrow(pxDf) < expectedTs-2) next
		
		symCount <- symCount + 1
		totalRet <- totalRet + as.numeric(Return.cumulative(dailyReturn(xts(pxDf[,2], pxDf[,1]))))
		
		pSyms <- c(pSyms, sym)
		if (symCount >= 20) break
	}
	periodRet <- totalRet/symCount
	
	return(list(periodRet, pSyms))
}
	
symRets <- data.frame(PERIOD_END="", STRATEGY="", RET=0.0, OVERLAP=0)
prevSyms <- list()
for(rfreq in 1:4){
	prevSyms[[rfreq]] <- vector()
	freqCtr <- 0
	for(ii in 2:length(rebalDates)){
		startDate <- rebalDates[ii-1]
		endDate <- rebalDates[ii]
		
		if(freqCtr %% rfreq == 0){
			symbols <- sqlQuery(lcon, sprintf("select top 200 symbol, ff_mkt_cap_cr from EQUITY_MISC_INFO 
												where time_stamp=(select max(time_stamp) from EQUITY_MISC_INFO where time_stamp <= '%s') 
												order by ff_mkt_cap_cr desc", 
												startDate))[,1]
			
			retDf <- data.frame(SYMBOL="", RET=0.0, VOLATILITY=0.0)
			for(sym in symbols){
				pxDf1 <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
											where ticker = $1 and date_stamp >= $2 and date_stamp < $3 order by date_stamp", params=list(sym, startDate-365, startDate))
											
				if(nrow(pxDf1) < 200) next
				pxXts1 <- xts(pxDf1[,2], pxDf1[,1])
				pxVol <- as.numeric(xts::last(volatility(pxXts1, n=50)))
				perRet <- as.numeric(Return.cumulative(dailyReturn(pxXts1)))
				retDf <- rbind(retDf, c(sym, perRet, pxVol))
			}
			retDf <- retDf[-1,]
			highRet <- head(retDf[order(retDf$VOLATILITY),], 100)
			highRet <- highRet[order(highRet$RET, decreasing=T),]
		}
		freqCtr <- freqCtr + 1
		
		expectedTs <- nrow(indexXts[paste0(startDate, "/", endDate)])
		
		periodRet <- getReturns(highRet$SYMBOL, startDate, endDate, expectedTs)
		overlap <- length(base::intersect(periodRet[[2]], prevSyms[[rfreq]]))/20
		prevSyms[[rfreq]] <- periodRet[[2]]
		symRets <- rbind(symRets, c(toString(endDate), paste0("MOM_", rfreq), periodRet[[1]], overlap))
	}
}


for(ii in 2:length(rebalDates)){
	startDate <- rebalDates[ii-1]
	endDate <- rebalDates[ii]
	benchRet <- as.numeric(Return.cumulative(dailyReturn(indexXts[paste0(startDate, "/", endDate)])))
	symRets <- rbind(symRets, c(toString(endDate), "BENCH", benchRet, 0))
}

symRets <- symRets[-1,]
symRets$PERIOD_END <- as.Date(symRets$PERIOD_END)
symRets$RET <- as.numeric(symRets$RET)

save(symRets, file=sprintf("%s/symRets-weekly.Rdata", reportPath))

########################################


library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('tidyverse')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("d:/stockviz/r/config.r")
#source("/mnt/hollandr/config.r")

pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')
lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

benchIndex <- "NIFTY 50 TR"

lookback <- 365*3 #days
portfolioSize <- 20

miscDates <- sort(sqlQuery(lcon, "select distinct time_stamp from EQUITY_MISC_INFO")[,1])

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", benchIndex, min(miscDates) - lookback*2, max(miscDates)))
indexXts <- xts(indexPx[,2], indexPx[,1])
names(indexXts) <- c(benchIndex)

indexRetsMonthly <- monthlyReturn(indexXts)
names(indexRetsMonthly) <- c(benchIndex)
index(indexRetsMonthly) <- as.Date(strftime(index(indexRetsMonthly), "%Y-%m-20"))

rebalDates <- index(indexRetsMonthly)
rebalDates <- rebalDates[rebalDates >= min(miscDates)]

getReturns <- function(syms, startDate, endDate, expectedTs, pSize){
	if(length(syms) == 0) return(list(c(), 0.0))
	
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


symRets <- data.frame(PERIOD_END="", STRATEGY="", RET=0.0)
for(ii in 3:length(rebalDates)){
	cat(paste(ii, "..."))
	
	modelDate <- rebalDates[ii-2]
	startDate <- rebalDates[ii-1]
	endDate <- rebalDates[ii]
	
	symbols <- sqlQuery(lcon, sprintf("select top 200 symbol, ff_mkt_cap_cr from EQUITY_MISC_INFO 
												where time_stamp=(select max(time_stamp) from EQUITY_MISC_INFO where time_stamp <= '%s') 
												order by ff_mkt_cap_cr desc", 
												startDate))[,1]
			
	if(length(symbols) < 5*portfolioSize) next
	
	expectedTsModel <- nrow(indexXts[paste0(modelDate - lookback, "/", startDate)])
		
	retDfLm1 <- data.frame(SYMBOL="", COR=0.0, RET=0.0)
	retDf2 <- data.frame(SYMBOL="", RET=0.0, VOLATILITY=0.0) #simple 12 month returns
	for(sym in symbols){
		pxDf1 <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
									where ticker = $1 and date_stamp >= $2 and date_stamp < $3 order by date_stamp", params=list(sym, modelDate - lookback, startDate))
									
		if(nrow(pxDf1) < expectedTsModel*0.95) next
		
		#build model...
		
		pxXts1 <- xts(pxDf1[,2], pxDf1[,1])
		pxVol <- volatility(pxXts1, n=50)
		mHist <- monthlyReturn(pxXts1)
		yHist <- rollapply(mHist, 12, Return.cumulative)
		
		histXts <- merge(mHist, stats::lag(mHist, 1))
		names(histXts) <- c('RET_M', 'RET')
		histXts <- na.omit(histXts)
		
		histCor <- cor(coredata(histXts$RET_M), coredata(histXts$RET), method = 'kendall')
		
		retDfLm1 <- rbind(retDfLm1, c(sym, as.numeric(histCor), as.numeric(tail(mHist, 1))))
		
		#naive model
		retDf2 <- rbind(retDf2, c(sym, as.numeric(xts::last(yHist)), as.numeric(xts::last(pxVol))))
	}
	retDfLm1 <- retDfLm1[-1,]
	retDfLm1$COR <- as.numeric(retDfLm1$COR)
	retDfLm1$RET <- as.numeric(retDfLm1$RET)
	
	retDf2 <- retDf2[-1,]
	retDf2$RET <- as.numeric(retDf2$RET)
	retDf2$VOLATILITY <- as.numeric(retDf2$VOLATILITY)
	
	pCor <- retDfLm1 %>% filter(COR > 0.05 & RET > 0) %>% slice_max(COR, n=30) %>% arrange(RET)
	#nCor <- retDfLm1 %>% filter(COR < -0.05 & RET < 0) %>% slice_min(COR, n=20)
	
	expectedTs <- nrow(indexXts[paste0(startDate, "/", endDate)])
	#periodRets <- getReturns(rbind(pCor, nCor)$SYMBOL, startDate, endDate, expectedTs, portfolioSize)
	periodRets <- getReturns(pCor$SYMBOL, startDate, endDate, expectedTs, portfolioSize)
	symRets <- rbind(symRets, c(toString(endDate), "COR", periodRets[[2]]))
	
	## 
	highRet <- retDf2[retDf2$RET > 0 & retDf2$RET > mean(retDf2$RET),]
	highRet <- head(highRet[order(highRet$VOLATILITY),], 100)
	highRet <- highRet[order(highRet$RET, decreasing=T),]
	
	periodRets2 <- getReturns(highRet$SYMBOL, startDate, endDate, expectedTs, portfolioSize)
	symRets <- rbind(symRets, c(toString(endDate), "NAIVE", periodRets2[[2]]))
}

for(ii in 2:length(rebalDates)){
	startDate <- rebalDates[ii-1]
	endDate <- rebalDates[ii]
	benchRet <- as.numeric(Return.cumulative(dailyReturn(indexXts[paste0(startDate, "/", endDate), 1])))
	symRets <- rbind(symRets, c(toString(endDate), "BENCH", benchRet))
}

symRets <- symRets[-1,]
symRets$PERIOD_END <- as.Date(symRets$PERIOD_END)
symRets$RET <- as.numeric(symRets$RET)

save(symRets, file=sprintf("%s/simple-cor.Rdata", reportPath))




### volatility & volatility of volatility (double sort)

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
volCalcLb <- 20 #days
volvolLb <- 500 #days


miscDates <- sqlQuery(lcon, "select distinct time_stamp from EQUITY_MISC_INFO")

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, min(miscDates$time_stamp) - volvolLb*2, max(miscDates$time_stamp)))
indexXts <- xts(indexPx[,2], indexPx[,1])

rebalDates <- miscDates %>% mutate(YM = year(time_stamp)*100 + month(time_stamp)) %>%
	group_by(YM) %>%
	summarize(ST=min(time_stamp)) %>%
	select(ST) %>%
	ungroup() %>%
	as.data.frame()

symRets <- data.frame(PERIOD_END="", PORT=0.0, BENCH=0.0)
for(ii in 2:nrow(rebalDates)){
	startDate <- rebalDates$ST[ii-1]
	endDate <- rebalDates$ST[ii]
	
	tsStartDt <- first(head(index(indexXts[paste0("/", startDate)]), volvolLb))
	expectedTs <- nrow(indexXts[paste0(tsStartDt, "/", startDate)])
	
	symbols <- sqlQuery(lcon, sprintf("select top 500 symbol, ff_mkt_cap_cr from EQUITY_MISC_INFO where time_stamp='%s' order by ff_mkt_cap_cr desc", startDate))[,1]
	
	pXts <- NULL
	for(sym in symbols){
		pxDf <- dbGetQuery(pgCon, "select date_stamp, h High, l Low, o Open, c as Close from eod_adjusted_nse 
									where ticker = $1 and date_stamp >= $2 and date_stamp < $3 order by date_stamp", params=list(sym, tsStartDt, startDate))
		
		if(nrow(pxDf) < expectedTs * 0.95) next
		
		pXts <- merge.xts(pXts, volatility(xts(pxDf[,-1], pxDf[, 1]), calc="parkinson"))
		names(pXts)[length(names(pXts))] <- sym
	}

	pXts <- na.trim(pXts, side='left')
	toKeep <- c()
	for(i in 1:ncol(pXts)){
		if(anyNA(pXts[,i])){
			next
		}
		toKeep <- c(toKeep, toString(names(pXts)[i]))
	}

	pXts <- pXts[,toKeep]
	volvolDf <- data.frame(SYMBOL="", VOL=0.0, VOLVOL=0.0)
	for(i in 1:ncol(pXts)){
		volvolDf <- rbind(volvolDf, c(toString(names(pXts)[i]), last(coredata(pXts[,i])), sd(coredata(pXts[,i]))))
	}
	volvolDf <- volvolDf[-1,]
	
	lowVol <- head(volvolDf[order(volvolDf$VOL),], 100)
	lowVol <- head(lowVol[order(lowVol$VOLVOL),], 50)
	
	expectedTs <- nrow(indexXts[paste0(startDate, "/", endDate)])
	benchRet <- as.numeric(Return.cumulative(dailyReturn(indexXts[paste0(startDate, "/", endDate)])))
	
	totalRet <- 0.0
	symCount <- 0
	for(i in 1:nrow(lowVol)){
		sym <- lowVol$SYMBOL[i]
		pxDf <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
									where ticker = $1 and date_stamp >= $2 and date_stamp <= $3 order by date_stamp", params=list(sym, startDate, endDate))
		
		if(nrow(pxDf) < expectedTs-2) next
		
		symCount <- symCount + 1
		totalRet <- totalRet + as.numeric(Return.cumulative(dailyReturn(xts(pxDf[,2], pxDf[,1]))))
		
		if (symCount >= 20) break
	}
	periodRet <- totalRet/symCount
	
	if(!is.finite(periodRet)) break
	
	symRets <- rbind(symRets, c(toString(endDate), periodRet, benchRet))
}

symRets <- symRets[-1,]
symRets$PERIOD_END <- as.Date(symRets$PERIOD_END)
symRets$PORT <- as.numeric(symRets$PORT)
symRets$BENCH <- as.numeric(symRets$BENCH)

save(symRets, file=sprintf("%s/symRetsX2.Rdata", reportPath))

########################################

symXts <- xts(symRets[,-1], symRets[,1])

Common.PlotCumReturns(symXts, "Vol x VolVol", "NIFTY 200 TR", sprintf("%s/sortX2.png", reportPath), NULL)

q()
Return.cumulative(symXts)

table.Drawdowns(symXts[,1], 10)
table.Drawdowns(symXts[,2], 10)

table.Drawdowns(symXts["/2019",1], 10)
table.Drawdowns(symXts["2021/",1], 10)




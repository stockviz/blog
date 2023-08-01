library('RODBC')
library('RPostgres')
library('quantmod')

source("d:/stockviz/r/config.r")
reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

indexName <- "NIFTY 50"
indexNameTr <- "NIFTY 50 TR"

startDate <- as.Date("2015-01-01")
corLbs <- c(5, 10, 20, 50, 100) #bus-days of look-back for correlation

indexChangeDates <- sqlQuery(lcon, sprintf("select distinct(time_stamp) ts from INDEX_CONST_HISTORY where index_name = '%s' order by ts desc", indexName))[,1]

pxDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s' and time_stamp >= '%s'", indexName, startDate))
pXts <- xts(pxDf[,1], pxDf[,2])

marketDates <- index(pXts)

for(corLb in corLbs){
	print(paste("calculating", corLb))
	eqWtCorXts <- NULL
	for(i in (corLb+1):length(marketDates)){
		asof <- marketDates[i]
		indexDate <- max(indexChangeDates[indexChangeDates <= asof])
		
		constituents <- sqlQuery(lcon, sprintf("select symbol from INDEX_CONST_HISTORY where index_name = '%s' and time_stamp = '%s'", indexName, indexDate))[,1]
		constXts <- NULL
		for(ei in 1:length(constituents)){
			symbol <- constituents[ei]
			
			fromDt <- marketDates[i - corLb]
			pxSeries <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse where ticker=$1 and date_stamp >= $2 and date_stamp <= $3 order by date_stamp", params=list(symbol, fromDt, asof))
			if(nrow(pxSeries) == 0) {
				retSeris <- sqlQuery(lcon, sprintf("select time_stamp, daily_return from RETURN_SERIES_ALL where symbol = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", symbol, fromDt, asof))
				if(nrow(retSeris) == 0) {
					print(paste("skipping: ", asof, symbol))
					next
				} else {
					pxRet <- xts(retSeris[,2], retSeris[,1])
				}
			} else {
				pXts <- xts(pxSeries[,2], pxSeries[,1])
				pxRet <- dailyReturn(pXts)
			}
			
			names(pxRet) <- c(symbol)
			constXts <- merge.xts(constXts, pxRet)
		}
		
		constXts <- constXts[-1,]
		corMat <- cor(constXts)
		medianPairwiseCor <- median(corMat[lower.tri(corMat)],na.rm=TRUE)
		
		eqWtCorXts <- rbind.xts(eqWtCorXts, xts(medianPairwiseCor, asof))
	}
	
	save(eqWtCorXts, file=sprintf("%s/%s.corr.%d.RData", reportPath, indexName, corLb))
}


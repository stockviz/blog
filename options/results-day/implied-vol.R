library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('lubridate')
library('tidyverse')
library('hms')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

bhavDate <- sqlQuery(lcon, "select max(time_stamp) from bhav_eq_fut")[[1]]

futSymbols <- sqlQuery(lcon, sprintf("select distinct symbol from bhav_eq_fut where time_stamp = '%s'", bhavDate))[,1]

analDf <- data.frame(SYMBOL = "", RESULT_DATE = "", ON_DATE = "", AFTER_DATE = "", STRANGLE_PREMIUM_ON = 0.0, STRANGLE_PREMIUM_AFTER = 0.0, AVG_IV_ON = 0.0, AVG_IV_AFTER = 0.0)
for(j in 1:length(futSymbols)){
	symbol <- futSymbols[j]
	resultDates <- sqlQuery(lcon, sprintf("select distinct BM_DATE from CORP_RESULTS_DATE WHERE SYMBOL='%s' and title like '%%result%%' ORDER BY BM_DATE DESC", symbol))[,1]
	if(length(resultDates) == 0) next

	resultsAnn <- sqlQuery(lcon, sprintf("select time_stamp from CORP_ANNOUNCE_NSE where symbol='%s' and DESCRIPTION like '%%result%%' 
								and convert(date, time_stamp) in ('%s') ORDER BY TIME_STAMP DESC", symbol, paste(resultDates, collapse="','")))[,1]
	if(length(resultsAnn) == 0) next
								
	resAnnDT <- data.frame(DATE = as.Date(resultsAnn), TIME = as_hms(resultsAnn))
	
	resAnnDates <- unique(resAnnDT$DATE)

	for(i in 1:length(resAnnDates)){
		resDt <- resAnnDates[i]
		
		resTime <- (resAnnDT %>% filter(DATE == resDt) %>% slice_min(TIME, n=1) %>% select(TIME))[[1]]
		
		if(resTime > as_hms("15:30:00")){
			resBusDt <- sqlQuery(lcon, sprintf("select max(time_stamp) from px_history where symbol='%s' and time_stamp <= '%s'", symbol, resDt))[[1]]
			afterBusDt <- sqlQuery(lcon, sprintf("select min(time_stamp) from px_history where symbol='%s' and time_stamp > '%s'", symbol, resDt))[[1]]
		} else {
			resBusDt <- sqlQuery(lcon, sprintf("select max(time_stamp) from px_history where symbol='%s' and time_stamp < '%s'", symbol, resDt))[[1]]
			afterBusDt <- sqlQuery(lcon, sprintf("select min(time_stamp) from px_history where symbol='%s' and time_stamp >= '%s'", symbol, resDt))[[1]]
		}
		
		afterBusDt <- sqlQuery(lcon, sprintf("select min(time_stamp) from px_history where symbol='%s' and time_stamp > '%s'", symbol, resDt))[[1]]
		if(is.na(resBusDt) || is.na(afterBusDt)) next
		
		px_close_at <- sqlQuery(lcon, sprintf("select px_close from px_history where symbol='%s' and time_stamp = '%s'", symbol, resBusDt))[[1]]
		
		closestExp <- sqlQuery(lcon, sprintf("select min(expiry_dt) from bhav_eq_opt where symbol='%s' and time_stamp = '%s'", symbol, resBusDt))[[1]]
		if(is.na(closestExp)) next
		
		strike_ce <- sqlQuery(lcon, sprintf("select min(strike_pr) from bhav_eq_opt where symbol='%s' and time_stamp = '%s' and strike_pr >= %f and expiry_dt = '%s'", symbol, resBusDt, px_close_at, closestExp))[[1]]
		strike_pe <- sqlQuery(lcon, sprintf("select max(strike_pr) from bhav_eq_opt where symbol='%s' and time_stamp = '%s' and strike_pr <= %f and expiry_dt = '%s'", symbol, resBusDt, px_close_at, closestExp))[[1]]
		
		premium_ce_at <- sqlQuery(lcon, sprintf("select px_close from bhav_eq_opt 
													where symbol='%s' and time_stamp = '%s' and strike_pr = %f and expiry_dt = '%s' 
													and option_typ = 'CE'", 
											symbol, resBusDt, strike_ce, closestExp))[[1]]
							
		premium_pe_at <- sqlQuery(lcon, sprintf("select px_close from bhav_eq_opt 
													where symbol='%s' and time_stamp = '%s' and strike_pr = %f and expiry_dt = '%s' 
													and option_typ = 'PE'", 
											symbol, resBusDt, strike_pe, closestExp))[[1]]
											
		iv_ce_at <- sqlQuery(lcon, sprintf("select iv from EQ_OPTION_GREEKS 
													where symbol='%s' and time_stamp = '%s' and strike = %f and expiry_date = '%s' 
													and option_type = 'CE'", 
											symbol, resBusDt, strike_pe, closestExp))[[1]]
							
		iv_pe_at <- sqlQuery(lcon, sprintf("select iv from EQ_OPTION_GREEKS 
													where symbol='%s' and time_stamp = '%s' and strike = %f and expiry_date = '%s' 
													and option_type = 'PE'", 
											symbol, resBusDt, strike_pe, closestExp))[[1]]
		
		premium_ce_after <- sqlQuery(lcon, sprintf("select px_close from bhav_eq_opt 
													where symbol='%s' and time_stamp = '%s' and strike_pr = %f and expiry_dt = '%s' 
													and option_typ = 'CE'", 
											symbol, afterBusDt, strike_ce, closestExp))[[1]]
							
		premium_pe_after <- sqlQuery(lcon, sprintf("select px_close from bhav_eq_opt 
													where symbol='%s' and time_stamp = '%s' and strike_pr = %f and expiry_dt = '%s' 
													and option_typ = 'PE'", 
											symbol, afterBusDt, strike_pe, closestExp))[[1]]
											
		iv_ce_after <- sqlQuery(lcon, sprintf("select iv from EQ_OPTION_GREEKS 
													where symbol='%s' and time_stamp = '%s' and strike = %f and expiry_date = '%s' 
													and option_type = 'CE'", 
											symbol, afterBusDt, strike_pe, closestExp))[[1]]
							
		iv_pe_after <- sqlQuery(lcon, sprintf("select iv from EQ_OPTION_GREEKS 
													where symbol='%s' and time_stamp = '%s' and strike = %f and expiry_date = '%s' 
													and option_type = 'PE'", 
											symbol, afterBusDt, strike_pe, closestExp))[[1]]
				
		iv_at <- NA
		tryCatch({		
			iv_at <- (iv_ce_at + iv_pe_at)/2
		}, error = function(e) {})
		
		iv_after <- NA
		tryCatch({		
			iv_after <- (iv_ce_after + iv_pe_after)/2
		}, error = function(e) {})
		
		tryCatch({
			analDf <- rbind(analDf, c(symbol, toString(resDt), toString(resBusDt), toString(afterBusDt), 
									as.numeric(premium_ce_at + premium_pe_at), as.numeric(premium_ce_after + premium_pe_after), 
									as.numeric(iv_at), as.numeric(iv_after)))
		}, error = function(e) {print(paste("skipping", symbol, resDt))})
	}
}

analDf <- analDf[-1,]

analDf$STRANGLE_PREMIUM_ON <- as.numeric(analDf$STRANGLE_PREMIUM_ON)
analDf$STRANGLE_PREMIUM_AFTER <- as.numeric(analDf$STRANGLE_PREMIUM_AFTER)
analDf$AVG_IV_AFTER <- as.numeric(analDf$AVG_IV_AFTER)
analDf$AVG_IV_ON <- as.numeric(analDf$AVG_IV_ON)

analDf$IV_CRUSH <- 100*(analDf$AVG_IV_AFTER/analDf$AVG_IV_ON - 1)

write.csv(analDf, file=sprintf("%s/results-IV.csv", reportPath), row.names=F)
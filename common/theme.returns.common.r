#TZ <- "Asia/Calcutta"

Common.GetEquityThemeReturns<-function(modelId, userId, top=NA, stt=0.0, brkg=0.0){
	con <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", dbserver, dbname, dbuser, dbpassword), case = "nochange", believeNRows = TRUE)

	if(is.na(top)){
		aTs<-sqlQuery(con, sprintf("select COALESCE(CASH_VALUE,0) + COALESCE(EQUITY_VALUE,0) + COALESCE(FUTURES_VALUE,0) CLOSE_PX, CAST(TIME_STAMP as DATE) DS from ADVISOR_PORTFOLIO_VALUE where MODEL_ID='%s' and USER_ID='%s' order by time_stamp desc", modelId, userId))
	} else {
		aTs<-sqlQuery(con, sprintf("select top %d COALESCE(CASH_VALUE,0) + COALESCE(EQUITY_VALUE,0) CLOSE_PX, CAST(TIME_STAMP as DATE) from ADVISOR_PORTFOLIO_VALUE where MODEL_ID='%s' and USER_ID='%s' order by time_stamp desc", top, modelId, userId))
	}
	
	aXts<-xts(aTs$CLOSE_PX, aTs$DS)
	dXts<-dailyReturn(aXts)

	startDt<-first(index(aXts))
	endDt<-last(index(aXts))

	themeTrades<-sqlQuery(con, sprintf("select CAST(TIME_STAMP as DATE) TIME_STAMP, sum(ABS(QUANTITY*PRICE)), count(*) from ADVISOR_MODEL_TRADES 
										WHERE model_id='%s' AND USER_ID='%s' 
										AND TIME_STAMP >='%s' AND TIME_STAMP <= '%s' 
										AND symbol <> 'LIQUIDBEES'
										group by CONVERT(date, TIME_STAMP)
										order by TIME_STAMP", modelId, userId, startDt, endDt))

	
        #print(tail(aXts))
	if(length(themeTrades[,1]) > 0){								
		tTXts<-xts(themeTrades[,-1], as.Date(themeTrades[,1]))
		dataXts<-merge(aXts, tTXts, dXts)
		names(dataXts)<-c("VAL", "VAL_TRADED", "NUM_TRADES", "PCT_RET")
		dataXts[is.na(dataXts$VAL_TRADED),]$VAL_TRADED<-0
		dataXts[is.na(dataXts$NUM_TRADES),]$NUM_TRADES<-0
		
		analXts<-merge(dataXts, dataXts$VAL_TRADED*(stt+brkg))
		names(analXts)[length(names(analXts))]<-"COST"
		analXts<-merge(analXts, analXts$PCT_RET - analXts$COST/analXts$VAL)
		names(analXts)[length(names(analXts))]<-"BRK"
		toRet<-merge(dXts, analXts$BRK, aXts)
	} else {
		toRet<-merge(dXts, dXts, aXts)
	}
	names(toRet)<-c('MODEL_RET', 'MODEL_RET_BRK', 'MODEL_PX')
	
	odbcClose(con)
	return(toRet)
}

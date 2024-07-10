library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('tidyverse')
library('tidymodels')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
#source("d:/stockviz/r/config.r")
source("/mnt/hollandr/config.r")

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

svm_linear_spec <- svm_poly(degree = 1) %>%
	set_mode("regression") %>%
	set_engine("kernlab", scaled = FALSE)
	
svm_linear_wf <- workflow() %>%
  add_model(svm_linear_spec %>% set_args(cost = tune())) %>%
  add_formula(RET_M ~ .)	

symRets <- data.frame(PERIOD_END="", STRATEGY="", RET=0.0)
for(ii in 3:length(rebalDates)){
	print(paste(ii, "..."))
	
	modelDate <- rebalDates[ii-2]
	startDate <- rebalDates[ii-1]
	endDate <- rebalDates[ii]
	
	symbols <- sqlQuery(lcon, sprintf("select top 200 symbol, ff_mkt_cap_cr from EQUITY_MISC_INFO 
												where time_stamp=(select max(time_stamp) from EQUITY_MISC_INFO where time_stamp <= '%s') 
												order by ff_mkt_cap_cr desc", 
												startDate))[,1]
			
	if(length(symbols) < 5*portfolioSize) next
	
	expectedTsModel <- nrow(indexXts[paste0(modelDate - lookback, "/", startDate)])
		
	retDfLm1 <- data.frame(SYMBOL="", RET=0.0)
	retDf2 <- data.frame(SYMBOL="", RET=0.0, VOLATILITY=0.0) #simple 12 month returns
	for(sym in symbols){
		cat(paste(sym, " ."))
		
		pxDf1 <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
									where ticker = $1 and date_stamp >= $2 and date_stamp < $3 order by date_stamp", params=list(sym, modelDate - lookback, startDate))
									
		if(nrow(pxDf1) < expectedTsModel*0.95) next
		
		#build model...
		
		pxXts1 <- xts(pxDf1[,2], pxDf1[,1])
		
		pxVol <- volatility(pxXts1, n=50)
		
		mHist <- monthlyReturn(pxXts1)
		yHist <- rollapply(mHist, 12, Return.cumulative)
		
		pxVol2 <- merge(mHist, pxVol)
		pxVol2[,2] <- na.locf(pxVol2[,2])
		pxVol2 <- na.omit(pxVol2)
		pxVol2 <- pxVol2[,2]
		
		index(mHist) <- as.Date(strftime(index(mHist), "%Y-%m-20"))
		index(pxVol2) <- as.Date(strftime(index(pxVol2), "%Y-%m-20"))
		
		histXts <- merge(mHist, stats::lag(mHist, 1), stats::lag(pxVol2, 1))
		names(histXts) <- c('RET_M', 'RET', 'VOL')
		histXts <- na.omit(histXts)
		
		nextXts <- merge(tail(mHist, 1), tail(pxVol2, 1))
		names(nextXts) <- c('RET', 'VOL')
		
		tryCatch({
			sim_data_fold <- vfold_cv(data.frame(histXts), strata = RET_M)

			param_grid <- grid_regular(cost(), levels = 10)

			tune_res <- tune_grid(svm_linear_wf, resamples = sim_data_fold, grid = param_grid)
			
			best_cost <- select_best(tune_res, metric = "rmse")
			svm_linear_final <- finalize_workflow(svm_linear_wf, best_cost)

			svm_linear_fit <- svm_linear_final %>%
				fit(data.frame(histXts))		
				
			#predict next returns
			nextRet <- predict(svm_linear_fit, data.frame(nextXts))[[1]]
			retDfLm1 <- rbind(retDfLm1, c(sym, nextRet))
		}, error = function (e) {print(e)})
		
		#naive model
		retDf2 <- rbind(retDf2, c(sym, as.numeric(xts::last(yHist)), as.numeric(xts::last(pxVol))))
	}
	cat("|")
	retDfLm1 <- retDfLm1[-1,]
	retDfLm1$RET <- as.numeric(retDfLm1$RET)
	
	retDf2 <- retDf2[-1,]
	retDf2$RET <- as.numeric(retDf2$RET)
	retDf2$VOLATILITY <- as.numeric(retDf2$VOLATILITY)
	
	highRet <- retDfLm1[retDfLm1$RET > 0,]
	highRet <- head(highRet[order(highRet$RET, decreasing=T),], 50)
	
	expectedTs <- nrow(indexXts[paste0(startDate, "/", endDate)])
	periodRets <- getReturns(highRet$SYMBOL, startDate, endDate, expectedTs, portfolioSize)
	symRets <- rbind(symRets, c(toString(endDate), "SVM", periodRets[[2]]))
	
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

save(symRets, file=sprintf("%s/simple-svm-tuned.Rdata", reportPath))


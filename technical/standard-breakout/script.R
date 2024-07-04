#Standard breakout strategy - Following The Trend, 
# if today’s close is higher or equal to the highest close in the past 50 days, we buy 
# tomorrow; if the close is below or equal to the lowest close for the past 50 days, we sell open tomorrow 
# and go short. A similar logic is used to get out of positions, where a long trade is sold when the close 
# reaches the lowest point in 25 days and a short trade is covered when the price makes a 25-day high

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('tidyverse')
library('lubridate')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")

reportPath <- "."
pdf(NULL)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

lb1 <- 50 #entry lookback
lb2 <- 25 #exit lookback

indexNames <- c("NIFTY 50", "NIFTY BANK", "NIFTY MIDCAP SELECT", "NIFTY NEXT 50")

for(i in 1:length(indexNames)){
	indexName <- indexNames[i]

	iDf <- sqlQuery(lcon, sprintf("select time_stamp, px_open, px_high, px_low, px_close from BHAV_INDEX where index_name='%s' and px_open > 0", indexName))
	iXts <- xts(iDf[,-1], iDf[,1])

	iXts$ret_oc <- iXts$px_close/iXts$px_open -1 
	iXts$ret_oc_lag1 <- stats::lag(iXts$ret_oc, -1) #trade is taken on the next day

	iXts$ret_cc <- dailyReturn(iXts$px_close)
	iXts$ret_cc_lag1 <- stats::lag(iXts$ret_cc, -1)

	iXts$max_1_c <- rollapply(iXts$px_close, lb1, function(X) max(X))
	iXts$min_1_c <- rollapply(iXts$px_close, lb1, function(X) min(X))

	iXts$max_2_c <- rollapply(iXts$px_close, lb2, function(X) max(X))
	iXts$min_2_c <- rollapply(iXts$px_close, lb2, function(X) min(X))

	iXts$long_sig <- ifelse(iXts$px_close >= iXts$max_1_c, 1, ifelse(iXts$px_close <= iXts$min_2_c, -1, NA))
	iXts$short_sig <- ifelse(iXts$px_close <= iXts$min_1_c, 1, ifelse(iXts$px_close >= iXts$max_2_c, -1, NA))

	long_sig <- iXts$long_sig
	long_sig[1] <- -1
	long_sig <- na.locf(long_sig)
	long_sig <- merge(long_sig, stats::lag(long_sig, 1))
	long_sig$s <- long_sig[,1] + long_sig[,2]
	iXts$long_sig_tr <- ifelse(long_sig$s == 2, 0, ifelse(long_sig$s == -2, NA, iXts$long_sig))

	short_sig <- iXts$short_sig
	short_sig[1] <- -1
	short_sig <- na.locf(short_sig)
	short_sig <- merge(short_sig, stats::lag(short_sig, 1))
	short_sig$s <- short_sig[,1] + short_sig[,2]
	iXts$short_sig_tr <- ifelse(short_sig$s == 2, 0, ifelse(short_sig$s == -2, NA, iXts$short_sig))


	iXts$long <- ifelse(iXts$long_sig_tr == 1, iXts$ret_oc_lag1, #new trade at the open tomorrow
					ifelse(iXts$long_sig_tr == 0, iXts$ret_cc_lag1, #position carried over
						NA))
						
	iXts$short <- ifelse(iXts$short_sig_tr == 1, -iXts$ret_oc_lag1, #new trade at the open tomorrow
					ifelse(iXts$short_sig_tr == 0, -iXts$ret_cc_lag1, #position carried over
						NA))
							
	iXts$long_short <- ifelse(!is.na(iXts$long), iXts$long, ifelse(!is.na(iXts$short), iXts$short, 0))

	toPlot <- merge(iXts$ret_cc_lag1, na.fill(iXts$long_short, 0))
	names(toPlot) <- c('BH', 'SBS L/S')
	sharpe <- SharpeRatio.annualized(toPlot)
	Common.PlotCumReturns(toPlot, sprintf("%s Standard breakout strategy", indexName), sprintf("%dx%d; SR: %s", lb1, lb2, paste(round(sharpe,2), collapse="/")), 
			sprintf("%s/sbb.%s.%dx%d.png", reportPath, indexName, lb1, lb2), NULL)



	###################### include SMA

	iXts$sma <- SMA(iXts$px_close, lb2)
	iXts$long_sig_sma <- ifelse(iXts$px_close >= iXts$max_1_c & iXts$px_close > iXts$sma, 1, ifelse(iXts$px_close <= iXts$min_2_c, -1, NA))
	iXts$short_sig_sma <- ifelse(iXts$px_close <= iXts$min_1_c & iXts$px_close < iXts$sma, 1, ifelse(iXts$px_close >= iXts$max_2_c, -1, NA))

	long_sig <- iXts$long_sig_sma
	long_sig[1] <- -1
	long_sig <- na.locf(long_sig)
	long_sig <- merge(long_sig, stats::lag(long_sig, 1))
	long_sig$s <- long_sig[,1] + long_sig[,2]
	iXts$long_sig_sma_tr <- ifelse(long_sig$s == 2, 0, ifelse(long_sig$s == -2, NA, iXts$long_sig))

	short_sig <- iXts$short_sig_sma
	short_sig[1] <- -1
	short_sig <- na.locf(short_sig)
	short_sig <- merge(short_sig, stats::lag(short_sig, 1))
	short_sig$s <- short_sig[,1] + short_sig[,2]
	iXts$short_sig_sma_tr <- ifelse(short_sig$s == 2, 0, ifelse(short_sig$s == -2, NA, iXts$short_sig))


	iXts$long_sma <- ifelse(iXts$long_sig_sma_tr == 1, iXts$ret_oc_lag1, #new trade at the open tomorrow
					ifelse(iXts$long_sig_sma_tr == 0, iXts$ret_cc_lag1, #position carried over
						NA))
						
	iXts$short_sma <- ifelse(iXts$short_sig_sma_tr == 1, -iXts$ret_oc_lag1, #new trade at the open tomorrow
					ifelse(iXts$short_sig_sma_tr == 0, -iXts$ret_cc_lag1, #position carried over
						NA))
							
	iXts$long_short_sma <- ifelse(!is.na(iXts$long_sma), iXts$long_sma, ifelse(!is.na(iXts$long_short_sma), iXts$long_short_sma, 0))


	toPlot <- merge(iXts$ret_cc_lag1, na.fill(iXts$long_short_sma, 0))
	names(toPlot) <- c('BH', 'SBS L/S')
	sharpe <- SharpeRatio.annualized(toPlot)
	Common.PlotCumReturns(toPlot, sprintf("%s Standard breakout strategy w/ SMA", indexName), sprintf("%dx%d; SR: %s", lb1, lb2, paste(round(sharpe,2), collapse="/")), 
			sprintf("%s/sbb-sma.%s.%dx%d.png", reportPath, indexName, lb1, lb2), NULL)
}
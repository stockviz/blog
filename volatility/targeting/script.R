library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('lubridate')
library('tidyverse')

library('DT')
library('webshot2')

Sys.setenv(CHROMOTE_CHROME = "C:/Users/shyam/AppData/Local/Google/Chrome/Application/chrome.exe")

options("scipen"=100)
options(stringsAsFactors = FALSE)

pdf(NULL)
reportPath <- "."
source("d:/stockviz/r/config.r")
source("d:/stockviz/r/plot.common.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexName <- "NIFTY SMALLCAP 50 TR"

pxDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = '%s'", indexName))
pXts <- xts(pxDf[,-1], pxDf[,1])

sdLb <- 220 #days
medianLb <- 5*220 #days
sdSmaLb <- 5 #days
numTiles <- 5

annualRf <- 0.05
dailyRf <- (1 + annualRf) ^ (1/220) - 1
weeklyRf <- (1 + annualRf) ^ (1/52) - 1

drag <- 0.5/100

targetVolFactor <- seq(0.25, 1, by=0.25)

allXts <- dailyReturn(pXts)
allXts <- merge(allXts, rollapply(allXts[,1], sdLb, sd))
allXts <- merge(allXts, rollapply(allXts[,2], medianLb, median))
allXts <- merge(allXts, rollapply(allXts[,2], medianLb, sd))
allXts <- merge(allXts, SMA(allXts[,2], sdSmaLb))

names(allXts) <- c("RET", "SD", "MED_SD", "SD_SD", "SMA_SD")
allXts$RF <- rep(dailyRf, nrow(allXts))
allXts$RET_1 <- stats::lag(allXts$RET, -1)

allXts <- na.omit(allXts)

############ daily rebal

vtXts <- NULL

for(tvf in targetVolFactor){
	dailyRebal <- rollapply(tvf*allXts$MED_SD/allXts$SMA_SD, 1, function(X) min(X, 1))
	dailyRebal <- merge(dailyRebal, 1 - dailyRebal[, 1])
	names(dailyRebal) <- c('EQ_WT', 'CASH_WT')
	dailyRebal$EQ_WT_CHG <- stats::lag(dailyRebal$EQ_WT, -1) - dailyRebal$EQ_WT
	dailyRebal$CASH_WT_CHG <- stats::lag(dailyRebal$CASH_WT, -1) - dailyRebal$CASH_WT
	dailyRebal$DRAG <- drag*(abs(dailyRebal$EQ_WT_CHG) + abs(dailyRebal$CASH_WT_CHG))
	dailyRebal$GROSS <- allXts$RET_1 * dailyRebal$EQ_WT + allXts$RF * dailyRebal$CASH_WT
	dailyRebal$NET <- dailyRebal$GROSS - dailyRebal$DRAG
	
	vtXts <- merge.xts(vtXts, dailyRebal$NET)
}
names(vtXts) <- sapply(targetVolFactor, function(X) paste0('TVF_', X))

toPlot <- na.omit(merge(vtXts, allXts$RET_1))
#Common.PlotCumReturns(toPlot, sprintf("%s Long-Only Dynamic Volatility Targeting", indexName), "daily; continuous", NULL)

statsDf <- data.frame(SharpeRatio.annualized(toPlot, dailyRf))
statsDf <- rbind(statsDf, maxDrawdown(toPlot))
statsDf <- rbind(statsDf, KellyRatio(toPlot, dailyRf))

statsDt <- datatable(statsDf, rownames=TRUE, class = 'cell-border stripe', filter='none', options = list(dom = 't', pageLength = 30, ordering=F), 
					caption=htmltools::tags$caption(style = 'text-align: center; font-weight: bold; font-family: "Segoe UI"',
					htmltools::em(sprintf("%s Daily Target Volatility", indexName)))) %>%
			formatRound(which(sapply(statsDf,is.numeric)), digits = 2)
					
saveWidget(statsDt, sprintf("%s/%s.daily.continuous.stats.html", reportPath, indexName))
webshot(sprintf("%s/%s.daily.continuous.stats.html", reportPath, indexName), sprintf("%s/%s.daily.continuous.stats.png", reportPath, indexName))

Common.PlotCumReturns(toPlot, sprintf("%s Long-Only Dynamic Volatility Targeting", indexName), "daily; continuous", sprintf("%s/%s.daily.continuous.png", reportPath, indexName), NULL)


############ weekly rebal

rfXts <- cumprod(1 + xts(rep(dailyRf, nrow(pXts)), index(pXts)))
wRet <- merge(weeklyReturn(pXts), weeklyReturn(rfXts))
wRet <- merge(wRet, stats::lag(wRet[,1], -1))
wRet <- merge(wRet, stats::lag(wRet[,2], -1))

names(wRet) <- c('WRET', 'WRF', 'WRET_1', 'WRF_1')

allXts2 <- na.omit(merge(allXts, wRet$WRET_1, wRet$WRF_1))

vtXts <- NULL

for(tvf in targetVolFactor){
	dailyRebal <- rollapply(tvf*allXts2$MED_SD/allXts2$SMA_SD, 1, function(X) min(X, 1))
	dailyRebal <- merge(dailyRebal, 1 - dailyRebal[, 1])
	names(dailyRebal) <- c('EQ_WT', 'CASH_WT')
	dailyRebal$EQ_WT_CHG <- stats::lag(dailyRebal$EQ_WT, -1) - dailyRebal$EQ_WT
	dailyRebal$CASH_WT_CHG <- stats::lag(dailyRebal$CASH_WT, -1) - dailyRebal$CASH_WT
	dailyRebal$DRAG <- drag*(abs(dailyRebal$EQ_WT_CHG) + abs(dailyRebal$CASH_WT_CHG))
	dailyRebal$GROSS <- allXts2$WRET_1 * dailyRebal$EQ_WT + allXts2$WRF_1 * dailyRebal$CASH_WT
	dailyRebal$NET <- dailyRebal$GROSS - dailyRebal$DRAG
	
	vtXts <- merge.xts(vtXts, dailyRebal$NET)
}
names(vtXts) <- sapply(targetVolFactor, function(X) paste0('TVF_', X))

toPlot <- na.omit(merge(vtXts, allXts2$WRET_1))
#Common.PlotCumReturns(toPlot, sprintf("%s Long-Only Dynamic Volatility Targeting", indexName), "daily; continuous", NULL)

statsDf <- data.frame(SharpeRatio.annualized(toPlot, weeklyRf))
statsDf <- rbind(statsDf, maxDrawdown(toPlot))
statsDf <- rbind(statsDf, KellyRatio(toPlot, weeklyRf))

statsDt <- datatable(statsDf, rownames = TRUE, class = 'cell-border stripe', filter='none', options = list(dom = 't', pageLength = 30, ordering=F), 
					caption=htmltools::tags$caption(style = 'text-align: center; font-weight: bold; font-family: "Segoe UI"',
					htmltools::em(sprintf("%s Weekly Target Volatility", indexName)))) %>%
			formatRound(which(sapply(statsDf,is.numeric)), digits = 2)
					
saveWidget(statsDt, sprintf("%s/%s.weekly.continuous.stats.html", reportPath, indexName))
webshot(sprintf("%s/%s.weekly.continuous.stats.html", reportPath, indexName), sprintf("%s/%s.weekly.continuous.stats.png", reportPath, indexName))

Common.PlotCumReturns(toPlot, sprintf("%s Long-Only Dynamic Volatility Targeting", indexName), "weekly; continuous", sprintf("%s/%s.weekly.continuous.png", reportPath, indexName), NULL)

############ weekly discrete

allXts3 <- dailyReturn(pXts)
allXts3 <- merge(allXts3, rollapply(allXts3[,1], sdLb, sd))
allXts3 <- merge(allXts3, rollapply(allXts3[,2], medianLb, function(X) last(ntile(X, numTiles))))
names(allXts3) <- c("RET", "SD", "T_SD")

allXts3 <- na.omit(merge(allXts3, wRet$WRET_1, wRet$WRF_1))

dailyRebal <- (numTiles - allXts3$T_SD)/numTiles
dailyRebal <- merge(dailyRebal, allXts3$T_SD/numTiles)
names(dailyRebal) <- c('EQ_WT', 'CASH_WT')
dailyRebal$EQ_WT_CHG <- stats::lag(dailyRebal$EQ_WT, -1) - dailyRebal$EQ_WT
dailyRebal$CASH_WT_CHG <- stats::lag(dailyRebal$CASH_WT, -1) - dailyRebal$CASH_WT
dailyRebal$DRAG <- drag*(abs(dailyRebal$EQ_WT_CHG) + abs(dailyRebal$CASH_WT_CHG))
dailyRebal$GROSS <- allXts3$WRET_1 * dailyRebal$EQ_WT + allXts3$WRF_1 * dailyRebal$CASH_WT
dailyRebal$NET <- dailyRebal$GROSS - dailyRebal$DRAG

toPlot <- na.omit(merge(dailyRebal$NET, allXts3$WRET_1))
#Common.PlotCumReturns(toPlot, sprintf("%s Long-Only Dynamic Volatility Targeting", indexName), "daily; continuous", NULL)

statsDf <- data.frame(SharpeRatio.annualized(toPlot, weeklyRf))
statsDf <- rbind(statsDf, maxDrawdown(toPlot))
statsDf <- rbind(statsDf, KellyRatio(toPlot, weeklyRf))

statsDt <- datatable(statsDf, rownames = TRUE, class = 'cell-border stripe', filter='none', options = list(dom = 't', pageLength = 30, ordering=F), 
					caption=htmltools::tags$caption(style = 'text-align: center; font-weight: bold; font-family: "Segoe UI"',
					htmltools::em(sprintf("%s Weekly Target Volatility (discrete)", indexName)))) %>%
			formatRound(which(sapply(statsDf,is.numeric)), digits = 2)
					
saveWidget(statsDt, sprintf("%s/%s.weekly.discrete.stats.html", reportPath, indexName))
webshot(sprintf("%s/%s.weekly.discrete.stats.html", reportPath, indexName), sprintf("%s/%s.weekly.discrete.stats.png", reportPath, indexName))

Common.PlotCumReturns(toPlot, sprintf("%s Long-Only Dynamic Volatility Targeting", indexName), "weekly; discrete", sprintf("%s/%s.weekly.discrete.png", reportPath, indexName), NULL)

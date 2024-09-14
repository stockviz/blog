library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('DT')
library('webshot2')

Sys.setenv(CHROMOTE_CHROME = "C:/Users/shyam/AppData/Local/Google/Chrome/Application/chrome.exe")
chromote::set_chrome_args("--disable-crash-reporter")

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")	

lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate <- as.Date("2015-06-12")

smaLbs <- c(5, 10, 20, 50, 100, 200)

symbols <- list()
symbols[[1]] <- c('PTLC', 'SPY')
symbols[[2]] <- c('PTNQ', 'QQQ')

for(i in 1: length(symbols)){
	pxDf1 <- sqlQuery(lconUs2, sprintf("select time_stamp, c from BHAV_EQ_TD where SYMBOL='%s' and time_stamp >= '%s'", symbols[[i]][1], startDate))
	pxDf2 <- sqlQuery(lconUs2, sprintf("select time_stamp, c from BHAV_EQ_TD where SYMBOL='%s' and time_stamp >= '%s'", symbols[[i]][2], startDate - 1000))
	
	eXts <- xts(pxDf1[,-1], pxDf1[,1])
	eRet <- dailyReturn(eXts)
	eRetL1 <- stats::lag(eRet, -1)
	
	benchXts <- xts(pxDf2[,-1], pxDf2[,1])
	benchRet <- dailyReturn(benchXts)
	benchRetL1 <- stats::lag(benchRet, -1)
	
	dXts <- na.omit(merge(eRet, benchRet))
	dXts <- dXts[-1,]
	
	sharpe <- SharpeRatio.annualized(dXts)
	Common.PlotCumReturns(dXts, sprintf("%s ETFs", paste(symbols[[i]], collapse="/")), sprintf("SR: %s", paste(round(sharpe,2), collapse="/")),
		sprintf("%s/%s.png", reportPath, paste(symbols[[i]], collapse="-")), NULL)
		
	smaXts <- do.call(merge.xts, lapply(smaLbs, \(x) SMA(benchXts, x)))
	
	smaRets <- do.call(merge.xts, lapply(1:ncol(smaXts), \(x) ifelse(benchXts > smaXts[,x], benchRetL1, 0)))
	
	smaRets <- na.trim(merge(smaRets, eRetL1, benchRetL1), side='left')
	names(smaRets) <- c(sapply(smaLbs, \(x) paste0('RET_', x)), symbols[[i]])
	
	sharpe <- SharpeRatio.annualized(smaRets)
	annRets <- 100*Return.annualized(smaRets)
	maxDD <- 100*maxDrawdown(smaRets)
	
	Common.PlotCumReturns(smaRets, sprintf("SMAs vs. %s ETFs", paste(symbols[[i]], collapse="/")), sprintf("SR: %s", paste(round(sharpe,2), collapse="/")),
		sprintf("%s/%s.SMA.png", reportPath, paste(symbols[[i]], collapse="-")), NULL)
	
	summDf <- data.frame(t(sharpe))
	colnames(summDf) <- c('Sharpe')
	summDf <- cbind(summDf, data.frame(t(annRets)), data.frame(t(maxDD)))
		
	statsDt <- datatable(summDf, rownames=TRUE, class = 'cell-border stripe', filter='none', options = list(dom = 't', pageLength = 30, ordering=F), 
					caption=htmltools::tags$caption(style = 'text-align: center; font-weight: bold; font-family: "Segoe UI"',
					htmltools::em(sprintf("SMAs vs. %s ETFs", paste(symbols[[i]], collapse="/"))))) %>%
			formatRound(which(sapply(summDf,is.numeric)), digits = 2)
					
	saveWidget(statsDt, sprintf("%s/%s.sma.stats.html", reportPath, paste(symbols[[i]], collapse="-")))
	webshot(sprintf("%s/%s.sma.stats.html", reportPath, paste(symbols[[i]], collapse="-")), sprintf("%s/%s.sma.stats.png", reportPath, paste(symbols[[i]], collapse="-")))
}



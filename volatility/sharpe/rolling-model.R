source("D:/stockviz/r/config.r")
source("D:/stockviz/r/theme.returns.common.r")

reportPath<-"."

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')

library('tidyverse')
library('reshape2')
library('viridis')
library('ggthemes')

options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

stt<-0.1/100
brkg<-0.05/100
srLbDays <- 220 

user_id <- "30B18A87-D803-4476-8469-858454C2C49A"

modelIds <- c("D6663194-A965-4262-84F6-BB553B0B8998", "6CAECA7A-A2C9-4B9D-BD0B-397FCE9597D2", "BB3B520D-E257-4881-8340-A4F46F44DC4C", "2DF79FD7-64F4-47F2-9F53-6B1ACE8012A3", "9EABD2F0-392A-4D89-A665-EF6576A7D471")
modelNames <- c("Momentum", "Velocity", "All Stars", "Quality to Price", "Financial Strength Value")

TZ <- "Asia/Calcutta"

for(i in 1:length(modelIds)){
	modelId <- modelIds[i]
	modelName <- modelNames[i]
	
	themeReturns <- Common.GetEquityThemeReturns(modelId, user_id, top=NA, stt, brkg)
	themeReturns <- themeReturns[is.finite(themeReturns[,1]) & is.finite(themeReturns[,2]) & is.finite(themeReturns[,3]),]

	startDt<-first(index(themeReturns))
	endDt<-last(index(themeReturns))

	niftyPx<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE NIFTY from BHAV_INDEX 
						where INDEX_NAME='NIFTY 50'
						and TIME_STAMP >= '%s'
						and TIME_STAMP <= '%s'", startDt, endDt))

	juniorPx<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE JUNIOR from BHAV_INDEX 
					where INDEX_NAME='NIFTY MIDCAP 100'
					and TIME_STAMP >= '%s'
					and TIME_STAMP <= '%s'", startDt, endDt))

	perfNIFTYXts <- xts(niftyPx$NIFTY, as.Date(niftyPx$TIME_STAMP, tz=TZ))
	perfJUNIORXts <- xts(juniorPx$JUNIOR, as.Date(juniorPx$TIME_STAMP, tz=TZ))

	allXts<-merge(themeReturns$MODEL_RET_BRK, perfNIFTYXts, perfJUNIORXts)

	allXts[which(allXts[,1]==0),1]<-NA
	allXts[,1]<-na.locf(allXts[,1], fromLast=T)

	allXts<-na.omit(allXts)

	rets<-merge(allXts[,1], dailyReturn(allXts[,2]), dailyReturn(allXts[,3]))
	rets<-rets[is.finite(rets[,1]), ]
	colnames(rets)<-c('MODEL', 'NIFTY', 'MIDCAP')

	srRoll <- rollapply(rets, srLbDays, function(X) SharpeRatio.annualized(X, Rf=0.0001))
	
	srModel <- as.numeric(SharpeRatio.annualized(rets[,1], Rf=0.0001))
	srM1 <- as.numeric(SharpeRatio.annualized(rets[,2], Rf=0.0001))
	srM2 <- as.numeric(SharpeRatio.annualized(rets[,3], Rf=0.0001))

	srRoll <- na.omit(srRoll)
	toPlot <- data.frame(srRoll)
	toPlot$T <- index(srRoll)
	
	toPlot <- melt(toPlot, id='T')
	
	vColors <- viridis_pal()(3)
	plotColors <- c("MODEL" = vColors[1], "NIFTY" = vColors[2], "MIDCAP" = vColors[3])

	ggplot(toPlot, aes(x=T, y=value, color=variable)) +
		theme_economist() +
		geom_line(linewidth=1) +
		scale_color_manual(values = plotColors) +
		geom_hline(yintercept = srModel, color = plotColors["MODEL"], linewidth=1) +
		geom_hline(yintercept = srM1, color = plotColors["NIFTY"], linewidth=1) +
		geom_hline(yintercept = srM2, color = plotColors["MIDCAP"], linewidth=1) +
		geom_text(aes(y = srModel, label=round(srModel, 2), x = startDt), color='black') +
		geom_text(aes(y = srM1, label=round(srM1, 2), x = startDt), color='black') +
		geom_text(aes(y = srM2, label=round(srM2, 2), x = startDt + 75), color='black') +
		labs(x = '', y='Annualized Sharpe', fill="", color="", title=sprintf("%s Rolling Sharpe", modelName), subtitle=sprintf("%s:%s; %d-days", startDt, endDt, srLbDays), caption = '@StockViz')
		
	ggsave(sprintf("%s/rolling-sharpe.%s.png", reportPath, modelName), width=12, height=6, units="in")			
}
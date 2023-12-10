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
library('ggrepel')

options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDt <- as.Date("2012-01-01")
srLbDays <- 220 

indices <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR", "NIFTY MICROCAP 250 TR", "NIFTY200 MOMENTUM 30 TR", "NIFTY MIDCAP150 MOMENTUM 50 TR")

iRet <- NULL
for(i in 1:length(indices)){
	iName <- indices[i]

	iDf<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE from BHAV_INDEX 
						where INDEX_NAME='%s'
						and TIME_STAMP >= '%s'", iName, startDt))

	iPx <- xts(iDf[,2], iDf[,1])
	
				
	iRet <- merge.xts(iRet, dailyReturn(iPx))
}

names(iRet) <- indices
endDt <- last(index(iRet))

srRoll <- rollapply(iRet, srLbDays, function(X) SharpeRatio.annualized(X, Rf=0.0001))

srLife <- as.numeric(SharpeRatio.annualized(iRet, Rf=0.0001))

toPlot <- data.frame(srRoll)
toPlot$T <- index(srRoll)
toPlot <- melt(toPlot, id='T')
	
vColors <- viridis_pal()(length(indices))
plotColors <- vColors
names(plotColors) <- make.names(indices)

srLifeDf <- data.frame(variable = make.names(indices), value = srLife)

ggplot(toPlot, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	geom_line(linewidth=1) +
	scale_color_manual(values = plotColors) +
	geom_hline(data = srLifeDf, aes(yintercept = value, color=variable)) +
	geom_text_repel(data = srLifeDf, aes(color=variable, label=round(value, 2), x = startDt), show.legend = F, fontface='bold') +
	labs(x = '', y='Annualized Sharpe', fill="", color="", title="Index Rolling Sharpe", subtitle=sprintf("%s:%s; %d-days", startDt, endDt, srLbDays), caption = '@StockViz')
	
ggsave(sprintf("%s/rolling-sharpe.indices.png", reportPath), width=12, height=6, units="in")			
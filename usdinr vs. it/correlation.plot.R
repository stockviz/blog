library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
#reportPath <- "D:/StockViz/report"

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

lmEquation <- function(df, x, y){
    m <- lm(as.formula(sprintf("%s ~ %s", y, x)), df)
	eq <- sprintf('italic(y) == %+.4f %+.4f %%.%% italic(x)*"," ~~italic(r)^2~"="~%.4f', as.numeric(coef(m)[1]), as.numeric(coef(m)[2]), summary(m)$r.squared)
	#print(eq)
    as.character(as.expression(eq))                 
}

startDate<-as.Date("2004-06-01")
endDate<-as.Date("2019-01-31")

niftyDf<-sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='nifty it' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
niftyXts<-xts(niftyDf[,2], as.Date(niftyDf[,1]))

usdDf<-sqlQuery(lconUs, sprintf("select time_stamp, val from FRED_OBSERVATION where time_stamp >= '%s' and time_stamp <= '%s' and series_id=(select id from FRED_SERIES where series_id='DEXINUS')", startDate, endDate))
usdXts<-xts(usdDf[,2], as.Date(usdDf[,1]))

plotCorrelation<-function(nXts, uXts, nameFix){
	nXts<-nXts[-1]
	nXts<-nXts[-nrow(nXts)]
	nXts<-Common.NormalizeWeeklyDates(nXts)

	uXts<-uXts[-1]
	uXts<-uXts[-nrow(uXts)]
	uXts<-Common.NormalizeWeeklyDates(uXts)

	unXts<-merge(nXts, uXts)
	unXts<-na.omit(unXts)

	names(unXts)<-c('IT', 'USDINR')
	unXts$IT<-stats::lag(unXts$IT, -1)
	unXts<-na.omit(unXts)

	unDf<-data.frame(unXts)

	lmEq<-lmEquation(unDf, 'IT', 'USDINR')

	unDf$DATE<-as.Date(index(unXts[,1]))

	plotStart<-first(index(unXts[,1]))
	plotEnd<-last(index(unXts[,1]))
		
	pdf(NULL)
	ggplot(unDf, aes(x=IT, y=USDINR)) +
		theme_economist() +
		geom_point() +
		geom_smooth() +
		geom_text(x = 0.9*min(unDf[,1], na.rm=T), y = 0.9*max(unDf[,2], na.rm=T), label = lmEq, parse = TRUE) +
		labs(x = 'IT', y='USDINR', fill="", color="", title="Correlation: NIFTY IT vs USDINR", subtitle=sprintf("%s returns [%s:%s]", nameFix, plotStart, plotEnd)) +
		annotate("text", x=max(unDf[,1], na.rm=T), y=min(unDf[,2], na.rm=T), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)
	ggsave(sprintf("%s/IT.USDINR.correlation.%s.%s.%s.png", reportPath, nameFix, plotStart, plotEnd), width=20, height=8, units="in")  
}

nWeekly<-weeklyReturn(niftyXts)*100
uWeekly<-weeklyReturn(usdXts)*100

plotCorrelation(nWeekly, uWeekly, "weekly")

nMonthly<-monthlyReturn(niftyXts)*100
uMonthly<-monthlyReturn(usdXts)*100

plotCorrelation(nMonthly, uMonthly, "monthly")
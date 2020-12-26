library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('reshape2')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")
reportPath <- "."

startDate1<-as.Date('1990-12-31')
startDate2<-as.Date('2003-12-31')
endDate<-as.Date('2018-12-31')

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

lmEquation <- function(df, x, y){
    m <- lm(as.formula(sprintf("%s ~ %s", y, x)), df)
	eq <- sprintf('italic(y) == %+.4f %+.4f %%.%% italic(x)*"," ~~italic(r)^2~"="~%.4f', as.numeric(coef(m)[1]), as.numeric(coef(m)[2]), summary(m)$r.squared)
	#print(eq)
    as.character(as.expression(eq))                 
}

plotCorrelation<-function(mRetDf, nameFix){
	plotStart<-first(rownames(mRetDf))
	plotEnd<-last(rownames(mRetDf))
	colNames<-colnames(mRetDf)
	lmEq<-lmEquation(mRetDf, colNames[1], colNames[2])
	pdf(NULL)
	ggplot(mRetDf, aes_string(x=colNames[1], y=colNames[2])) +
		theme_economist() +
		geom_point() +
		geom_smooth() +
		geom_text(x = 0.9*min(mRetDf[,1], na.rm=T), y = 0.9*max(mRetDf[,2], na.rm=T), label = lmEq, parse = TRUE) +
		labs(x = colNames[1], y=colNames[2], fill="", color="", title=sprintf("Correlation: %s vs %s", colNames[1], colNames[2], nameFix), subtitle=sprintf("%s returns [%s:%s]", nameFix, plotStart, plotEnd)) +
		annotate("text", x=max(mRetDf[,1], na.rm=T), y=min(mRetDf[,2], na.rm=T), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)
	ggsave(sprintf("%s/%s.%s.correlation.%s.%s.%s.png", reportPath, colNames[1], colNames[2], nameFix, plotStart, plotEnd), width=20, height=8, units="in")  
}

sp500Df<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO where time_stamp >= '%s' and time_stamp <= '%s' and symbol='%s'", startDate1, endDate, '^GSPC'))
sp500Xts<-xts(sp500Df$ac, as.Date(sp500Df$time_stamp))
sp500Monthly<-monthlyReturn(sp500Xts)

tbilDf<-sqlQuery(lconUs2, sprintf("select time_stamp, val from BARCLAYS_DATA where time_stamp >= '%s' and time_stamp <= '%s' and ticker='%s'", startDate1, endDate, 'BCC23MTB'))
tbilXts<-xts(tbilDf$val, as.Date(tbilDf$time_stamp))
tbilMonthly<-monthlyReturn(tbilXts)

x1<-na.omit(merge(Common.NormalizeMonthlyDates(sp500Monthly), Common.NormalizeMonthlyDates(tbilMonthly)))
xDf1<-data.frame(x1)
colnames(xDf1)<-c('sp500', 'tbill')
plotCorrelation(xDf1, "1mo")

x2<-na.omit(merge(rollapply(x1[,1], 12, Return.cumulative), rollapply(x1[,2], 12, Return.cumulative)))
xDf1<-data.frame(x2)
colnames(xDf1)<-c('sp500', 'tbill')
plotCorrelation(xDf1, "12mo")

#########################

sp500Df<-sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where time_stamp >= '%s' and time_stamp <= '%s' and index_name='%s'", startDate2, endDate, 'NIFTY 50'))
sp500Xts<-xts(sp500Df$px_close, as.Date(sp500Df$time_stamp))
sp500Monthly<-monthlyReturn(sp500Xts)

tbilDf<-sqlQuery(lcon, sprintf("select time_stamp, tri from INDEX_CCIL_TENOR where time_stamp >= '%s' and time_stamp <= '%s' and index_name='%s'", startDate2, endDate, '0_5'))
tbilXts<-xts(tbilDf$tri, as.Date(tbilDf$time_stamp))
tbilMonthly<-monthlyReturn(tbilXts)

x1<-na.omit(merge(Common.NormalizeMonthlyDates(sp500Monthly), Common.NormalizeMonthlyDates(tbilMonthly)))

xDf1<-data.frame(x1)
colnames(xDf1)<-c('nifty50', 'z5')
plotCorrelation(xDf1, "1mo")

x2<-na.omit(merge(rollapply(x1[,1], 12, Return.cumulative), rollapply(x1[,2], 12, Return.cumulative)))
xDf1<-data.frame(x2)
colnames(xDf1)<-c('nifty50', 'z5')
plotCorrelation(xDf1, "12mo")

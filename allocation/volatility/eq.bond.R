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

plotVolatility <- function(mRetDf, nameFix){
	plotStart<-as.Date(first(rownames(mRetDf)))
	plotEnd<-as.Date(last(rownames(mRetDf)))
	colNames<-colnames(mRetDf)
	mRetDf$T<-as.Date(rownames(mRetDf))
	
	xAxisTicks<-seq(from=plotStart, to=plotEnd, length.out=10)
	
	mRetDfMelt<-melt(mRetDf, id='T')
	
	pdf(NULL)
	ggplot(mRetDfMelt, aes(x=T, y=value, color=variable)) +
		theme_economist() +
		geom_line() +
		labs(x = '', y='', fill="", color="", title=sprintf("Volatility: %s vs %s", colNames[1], colNames[2], nameFix), subtitle=sprintf("%s hloc [%s:%s]", nameFix, plotStart, plotEnd)) +
		annotate("text", x=plotEnd, y=min(mRetDf[,colNames], na.rm=T), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)
		
	ggsave(sprintf("%s/%s.%s.volatility.%s.%s.%s.png", reportPath, colNames[1], colNames[2], nameFix, plotStart, plotEnd), width=20, height=8, units="in")  
}

sp500Df<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO where time_stamp >= '%s' and time_stamp <= '%s' and symbol='%s'", startDate1, endDate, '^GSPC'))
sp500Xts<-xts(sp500Df$ac, as.Date(sp500Df$time_stamp))
sp500Monthly<-to.period(sp500Xts, "months")
vYz1<-volatility(sp500Monthly, calc='yang.zhang')

tbilDf<-sqlQuery(lconUs2, sprintf("select time_stamp, val from BARCLAYS_DATA where time_stamp >= '%s' and time_stamp <= '%s' and ticker='%s'", startDate1, endDate, 'BCC23MTB'))
tbilXts<-xts(tbilDf$val, as.Date(tbilDf$time_stamp))
tbilMonthly<-to.period(tbilXts, "months")
vYz2<-volatility(tbilMonthly, calc='yang.zhang')

x1<-na.omit(merge(Common.NormalizeMonthlyDates(vYz1), Common.NormalizeMonthlyDates(vYz2)))
xDf1<-data.frame(x1)
colnames(xDf1)<-c('sp500', 'tbill')
plotVolatility(xDf1, "1mo")

#########################

sp500Df<-sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where time_stamp >= '%s' and time_stamp <= '%s' and index_name='%s'", startDate2, endDate, 'NIFTY 50'))
sp500Xts<-xts(sp500Df$px_close, as.Date(sp500Df$time_stamp))
sp500Monthly<-to.period(sp500Xts, "months")
vYz1<-volatility(sp500Monthly, calc='yang.zhang')

tbilDf<-sqlQuery(lcon, sprintf("select time_stamp, tri from INDEX_CCIL_TENOR where time_stamp >= '%s' and time_stamp <= '%s' and index_name='%s'", startDate2, endDate, '0_5'))
tbilXts<-xts(tbilDf$tri, as.Date(tbilDf$time_stamp))
tbilMonthly<-to.period(tbilXts, "months")
vYz2<-volatility(tbilMonthly, calc='yang.zhang')

x1<-na.omit(merge(Common.NormalizeMonthlyDates(vYz1), Common.NormalizeMonthlyDates(vYz2)))

xDf1<-data.frame(x1)
colnames(xDf1)<-c('nifty50', 'z5')
plotVolatility(xDf1, "1mo")


library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("1991-01-01")
endDate<-as.Date("2018-12-31")
lb <- 12*5 #lookback in months
rf <- 0.02/12 #risk free return

index1<-'^GSPC'
index2<-'NIFTY50 USD'

df1<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO 
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and symbol='%s'", startDate, endDate, index1))

df2<-sqlQuery(lcon, sprintf("select px_close, time_stamp from BHAV_INDEX
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and index_name='%s'", startDate, endDate, index2))

xts1<-xts(df1$ac, as.Date(df1$time_stamp))							
xts2<-xts(df2$px_close, as.Date(df2$time_stamp))

mRet1<-monthlyReturn(xts1)
mRet2<-monthlyReturn(xts2)

mRet1<-mRet1[-1]
mRet1<-mRet1[-nrow(mRet1)]

mRet2<-mRet2[-1]
mRet2<-mRet2[-nrow(mRet2)]

om1<-na.omit(rollapply(mRet1, lb, function(X) Omega(X, Rf = rf)))
om2<-na.omit(rollapply(mRet2, lb, function(X) Omega(X, Rf = rf)))

omDf1<-data.frame(YMD=as.Date(sprintf("%d-%d-%d", year(index(om1)), month(index(om1)), 25)), V = coredata(om1))
omDf2<-data.frame(YMD=as.Date(sprintf("%d-%d-%d", year(index(om2)), month(index(om2)), 25)), V = coredata(om2))

omDf<-merge(omDf1, omDf2, by='YMD')
names(omDf)<-c('YMD', index1, index2)

omMelt<-melt(omDf, id='YMD')
xAxisTicks<-seq(startDate, endDate, length.out=10)

pdf(NULL)
ggplot(omMelt, aes(x=YMD, y=value, color=variable)) +
	theme_economist() +
	geom_line() +
	scale_x_date(breaks = xAxisTicks) +
	labs(x = "", y="omega", fill="", color="", title="Omega", subtitle=sprintf("%d-month lookback [%s:%s]", lb, startDate, endDate)) +
	annotate("text", x=endDate, y=min(omMelt$value), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
ggsave(sprintf("%s/OMEGA.%s.%s.%s.%s.%d.png", reportPath, index1, index2, startDate, endDate, lb), width=12, height=6, units="in")
	
library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('mixtools')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("2001-01-01")
endDate<-as.Date("2018-11-30")
index1<-'^MID'
index3<-'^RUA'
index2<-'NIFTY MIDCAP 100'
indexUSDINR<-'DEXINUS'

usdDf1<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO 
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and symbol='%s'", startDate, endDate, index1))
usdXts1<-xts(usdDf1$ac, as.Date(usdDf1$time_stamp))

usdDf2<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO 
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and symbol='%s'", startDate, endDate, index3))
usdXts2<-xts(usdDf2$ac, as.Date(usdDf2$time_stamp))

inrDf1<-sqlQuery(lcon, sprintf("select px_close, time_stamp from BHAV_INDEX
							where time_stamp >= '%s' and time_stamp <= '%s'
							and index_name='%s'", startDate, endDate, index2))
inrXts1<-xts(inrDf1$px_close, as.Date(inrDf1$time_stamp))

usdDf<-sqlQuery(lconUs, sprintf("select val, time_stamp from FRED_OBSERVATION 
								where time_stamp >= '%s' and time_stamp <= '%s' 
								and series_id=(select id from FRED_SERIES where series_id='%s')", startDate, endDate, indexUSDINR))
usdInrXts<-xts(usdDf$val, as.Date(usdDf$time_stamp))


m0<-to.period(usdXts1, period='months', indexAt='lastof')[,4]
m1<-to.period(usdXts2, period='months', indexAt='lastof')[,4]
m2<-to.period(inrXts1, period='months', indexAt='lastof')[,4]
m3<-to.period(usdInrXts, period='months', indexAt='lastof')[,4]

m0Df<-data.frame(YM=100*year(index(m0))+month(index(m0)), V = coredata(m0))
m1Df<-data.frame(YM=100*year(index(m1))+month(index(m1)), V = coredata(m1))
m2Df<-data.frame(YM=100*year(index(m2))+month(index(m2)), V = coredata(m2))
m3Df<-data.frame(YM=100*year(index(m3))+month(index(m3)), V = coredata(m3))

m123<-merge(merge(m0Df, m1Df, by='YM'), merge(m2Df, m3Df, by='YM'), by='YM')
m123$X<-m123[,4]/m123[,5]

mXts<-xts(m123[,c(2, 3, 6)], ymd(sprintf("%s25", m123[,1])))
mRets<-na.omit(mXts/stats::lag(mXts,1)-1)

toPlot<-mRets
names(toPlot)<-c(index1, index3, index2)
Common.PlotCumReturns(toPlot, sprintf("%s, %s and %s in USD", index1, index3, index2), sprintf("%s/US.IND.MIDCAP.2001.cumualtive.png", reportPath))
Common.PlotCumReturns(toPlot['2007/',], sprintf("%s, %s and %s in USD", index1, index3, index2), sprintf("%s/US.IND.MIDCAP.2007.cumualtive.png", reportPath))
Common.PlotCumReturns(toPlot['2010/',], sprintf("%s, %s and %s in USD", index1, index3, index2), sprintf("%s/US.IND.MIDCAP.2010.cumualtive.png", reportPath))

###################

m0y<-to.period(usdXts1, period='years', indexAt='lastof')[,4]
m1y<-to.period(usdXts2, period='years', indexAt='lastof')[,4]
m2y<-to.period(inrXts1, period='years', indexAt='lastof')[,4]
m3y<-to.period(usdInrXts, period='years', indexAt='lastof')[,4]

m0yDf<-data.frame(Y=year(index(m0y)), V = coredata(m0y))
m1yDf<-data.frame(Y=year(index(m1y)), V = coredata(m1y))
m2yDf<-data.frame(Y=year(index(m2y)), V = coredata(m2y))
m3yDf<-data.frame(Y=year(index(m3y)), V = coredata(m3y))

m123y<-merge(merge(m0yDf, m1yDf, by='Y'), merge(m2yDf, m3yDf, by='Y'), by='Y')
m123y$X<-m123y[,4]/m123y[,5]

yXts<-xts(m123y[,c(2, 3, 6)], ymd(sprintf("%s-12-25", m123y[,1])))
yRets<-na.omit(yXts/stats::lag(yXts,1)-1)

names(yRets)<-c(index1, index3, index2)

toPlotDf<-100*data.frame(yRets)
toPlotDf$T<-year(index(yRets))

y1<-min(toPlotDf$T)
y2<-max(toPlotDf$T)

meltedDf<-melt(toPlotDf, id='T')
meltedDf$T<-factor(meltedDf$T, levels=sort(unique(meltedDf$T)))

pdf(NULL)	
ggplot(meltedDf, aes(x=T, y=value, fill=variable)) + 
	theme_economist() +
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text(aes(label=round(value,2)), vjust=1.6, color="black", position = position_dodge(0.9), size=2) +
	labs(x = "", y="returns(%)", fill="", color="", title=sprintf("%s, %s and %s in USD", index1, index3, index2), subtitle=sprintf("%s:%s", y1, y2)) +
	annotate("text", x=y2-y1, y=min(meltedDf$value), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
ggsave(sprintf("%s/US.IND.MIDCAP.annual.png", reportPath), width=12, height=6, units="in")

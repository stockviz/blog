# correlation between SPY, QQQ and MIDCAP($)

library('RODBC')
library('ggplot2')
library('extrafont')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')
library('dplyr')

source("d:/stockviz/r/config.r")

options(stringsAsFactors = FALSE)
options("scipen"=100)
TZ <- "Asia/Calcutta"

reportPath <- "."
mytheme <- ttheme_default(
		core = list(fg_params=list(fontfamily='Segoe UI', hjust=1, x=1)),
		colhead = list(fg_params=list(fontfamily='Segoe UI')),
		rowhead = list(fg_params=list(fontfamily='Segoe UI')))

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs<-odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
		
startDate<-as.Date("2004-01-01")
endDate<-as.Date("2018-09-30")

spyPx<-sqlQuery(lcon, sprintf("select time_stamp, px_close_adj from BHAV_EQ_AV where symbol='spy' and vol > 0 and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
qqqPx<-sqlQuery(lcon, sprintf("select time_stamp, px_close_adj from BHAV_EQ_AV where symbol='qqq' and vol > 0 and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
usdinr<-sqlQuery(lconUs, sprintf("select VAL, TIME_STAMP from FRED_OBSERVATION where SERIES_ID=-2147478748 and TIME_STAMP >='%s' and TIME_STAMP <= '%s'", startDate, endDate))
midcapPx<-sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = 'NIFTY MIDCAP 100' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
bondPx<-sqlQuery(lcon, sprintf("select time_stamp, tri from INDEX_CCIL_TENOR where index_name = '0_5' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))

spyXts<-xts(spyPx[,2], as.Date(spyPx[,1], tz=TZ))
qqqXts<-xts(qqqPx[,2], as.Date(qqqPx[,1], tz=TZ))
usdXts<-xts(usdinr$VAL, as.Date(usdinr$TIME_STAMP, tz=TZ))
midcapXts<-xts(midcapPx[,2], as.Date(midcapPx[,1], tz=TZ))
bondXts<-xts(bondPx[,2], as.Date(bondPx[,1], tz=TZ))

midcapXts<-merge(midcapXts, bondXts, usdXts)
midcapXts[,2]<-na.locf(midcapXts[,2])
midcapXts[,3]<-na.locf(midcapXts[,3])
midcapXts<-na.omit(midcapXts)
midcapXts$DLR_MIDCAP<-midcapXts[,1]/midcapXts[,3]
midcapXts$DLR_BOND<-midcapXts[,2]/midcapXts[,3]

weeklyRets<-merge(weeklyReturn(spyXts), weeklyReturn(qqqXts), weeklyReturn(midcapXts$DLR_MIDCAP), weeklyReturn(midcapXts$DLR_BOND))
weeklyRets<-weeklyRets[-1,]
weeklyRets[,3]<-na.locf(weeklyRets[,3])
weeklyRets<-na.omit(weeklyRets)

names(weeklyRets)<-c("SPY","QQQ","DLR_MIDCAP","DLR_BOND")

#cor(weeklyRets[1:200,], method="kendall")
correlations<-rollapply(weeklyRets, 200, function(X) cor(X, method="kendall"), by.column = F)

indexCorrelations<-na.omit(correlations[, c(2, 3, 4, 7, 8, 12)])
names(indexCorrelations)<-c("SPY/QQQ", "SPY/MCAP", "SPY/BND", "QQQ/MCAP", "QQQ/BND", "MCAP/BND")

iCorrDf<-data.frame(indexCorrelations)
iCorrDf$time_stamp<-as.Date(row.names(iCorrDf))

firstDate<-first(iCorrDf$time_stamp)
lastDate<-last(iCorrDf$time_stamp)
xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)

pdf(NULL)
ggplot(data = melt(iCorrDf, id='time_stamp'), aes(x = time_stamp, y = value, color=variable)) +
	theme_economist() +
	geom_line(size=1, alpha=0.8) +
	labs(y="correlations", x="", color='', title='SPY, QQQ and MIDCAP($) and BOND($) Correlations', subtitle=sprintf('200-week rolling [%s:%s]', firstDate, lastDate)) +
	scale_x_date(breaks = xAxisTicks) +
	annotate("text", x=lastDate, y=min(iCorrDf[,1:3]), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s\\corelations.png", reportPath), dpi=600, width=12, height=6, units="in", device="png")	
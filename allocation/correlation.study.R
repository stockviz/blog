# correlations between different assets priced in USD

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
library('ggrepel')
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

goldPx<-sqlQuery(lconUs, sprintf("select VAL, TIME_STAMP from FRED_OBSERVATION where SERIES_ID=-2147252004 and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
spyPx<-sqlQuery(lcon, sprintf("select time_stamp, px_close_adj from BHAV_EQ_AV where symbol='spy' and vol > 0 and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
qqqPx<-sqlQuery(lcon, sprintf("select time_stamp, px_close_adj from BHAV_EQ_AV where symbol='qqq' and vol > 0 and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
usdinr<-sqlQuery(lconUs, sprintf("select VAL, TIME_STAMP from FRED_OBSERVATION where SERIES_ID=-2147478748 and TIME_STAMP >='%s' and TIME_STAMP <= '%s'", startDate, endDate))
midcapPx<-sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = 'NIFTY MIDCAP 100' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
bondPx<-sqlQuery(lcon, sprintf("select time_stamp, tri from INDEX_CCIL_TENOR where index_name = '0_5' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))

goldXts<-xts(goldPx$VAL, as.Date(goldPx$TIME_STAMP, tz=TZ))
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

weeklyRets<-merge(weeklyReturn(goldXts), weeklyReturn(spyXts), weeklyReturn(qqqXts), weeklyReturn(midcapXts$DLR_MIDCAP), weeklyReturn(midcapXts$DLR_BOND))
weeklyRets<-weeklyRets[-1,]
weeklyRets[,4]<-na.locf(weeklyRets[,4])
weeklyRets[,5]<-na.locf(weeklyRets[,5])
weeklyRets<-na.omit(weeklyRets)

names(weeklyRets)<-c("GOLD", "SPY","QQQ","DLR_MIDCAP","DLR_BOND")

correlations<-rollapply(weeklyRets, 200, function(X) {
		m<-cor(X, method="kendall")
		df<-data.frame(PAIR=sprintf("%s/%s", rownames(m)[row(m)[upper.tri(m)]], colnames(m)[col(m)[upper.tri(m)]]), corr=m[upper.tri(m)])
		rdf<-data.frame(t(df[,2]))
		names(rdf)<-df$PAIR
		xts(rdf, last(index(X[,1])))
	}, by.column = F)

correlations<-na.omit(correlations)
iCorrDf<-data.frame(correlations)
iCorrDf$time_stamp<-as.Date(row.names(iCorrDf))

firstDate<-first(iCorrDf$time_stamp)
lastDate<-last(iCorrDf$time_stamp)
xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)

iCorrDf2<-melt(iCorrDf, id='time_stamp')
iCorrDf2$label<-ifelse(iCorrDf2$time_stamp == max(iCorrDf2$time_stamp), as.character(iCorrDf2$variable), NA)

pdf(NULL)
ggplot(data = iCorrDf2, aes(x = time_stamp, y = value, color=variable)) +
	theme_economist() +
	geom_line(size=1, alpha=0.8) +
	geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) +
	labs(y="correlations", x="", color='', title=sprintf('%s Correlations', paste(names(weeklyRets), collapse=", ")), subtitle=sprintf('200-week rolling [%s:%s]', firstDate, lastDate)) +
	scale_x_date(breaks = xAxisTicks) +
	scale_color_discrete(guide = FALSE) +
	annotate("text", x=firstDate, y=min(iCorrDf2$value), label = "@StockViz", vjust=-1.1, col="white", cex=4, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s\\corelations.%s.png", reportPath, paste(names(weeklyRets), collapse=".")), dpi=600, width=12, height=6, units="in", device="png")	
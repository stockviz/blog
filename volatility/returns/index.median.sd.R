library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')

library('ggplot2')
library('ggthemes')
library('reshape2')
library('dplyr')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("1995-01-01")
endDate<-as.Date("2019-01-31")

lookback<-c(50, 100, 200)

getStats<-function(indexDf, prefix){
	indexXts<-xts(indexDf[,-1], as.Date(indexDf[,1]))
	weeklyRets<-weeklyReturn(indexXts)*100
	weeklyRets<-weeklyRets[-1]
	weeklyRets<-weeklyRets[-nrow(weeklyRets)]

	names(weeklyRets)<-c('RET')

	medianXts<-xts()
	for(lb in lookback){
		medianXts<-merge(medianXts, rollapply(weeklyRets$RET, lb, median))
	}
	medianXts<-na.omit(medianXts)
	medianNames<-sapply(lookback, function(X) sprintf("%s_M_%d", prefix, X))
	names(medianXts)<-medianNames

	sdXts<-xts()
	for(lb in lookback){
		sdXts<-merge(sdXts, rollapply(weeklyRets$RET, lb, sd))
	}
	sdXts<-na.omit(sdXts)
	sdNames<-sapply(lookback, function(X) sprintf("%s_S_%d", prefix, X))
	names(sdXts)<-sdNames
	
	retXts<-na.omit(merge(medianXts, sdXts))
	retXts<-Common.NormalizeWeeklyDates(retXts)
	return(list(retXts, medianNames, sdNames))
}

indexDf<-sqlQuery(lconUs2, sprintf("select time_stamp, C from BHAV_YAHOO where symbol='^GSPC' and time_stamp >= '%s' and time_stamp <= '%s' and o > 0", startDate, endDate))
spStats<-getStats(indexDf, "SP500")

indexDf<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE from BHAV_INDEX where index_name='NIFTY 50' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
niftyStats<-getStats(indexDf, "NIFTY50")

####################

allStats<-merge(spStats[[1]], niftyStats[[1]])

plotStart<-as.Date(first(index(allStats)))
plotEnd<-as.Date(last(index(allStats)))
xAxisTicks<-seq(from=plotStart, to=plotEnd, length.out=10)

plotSeries<-function(statNames1, statNames2, nameFix){
	for(i in 1:length(statNames1)){
		lbs<-c(statNames1[i], statNames2[i])
		print(lbs)
		weeklyRetDf<-data.frame(allStats[, lbs])
		weeklyRetDf$DATE<-as.Date(index(allStats[,1]))

		pdf(NULL)	
		ggplot(melt(weeklyRetDf, id='DATE'), aes(x=DATE, y=value, color=variable)) +
			theme_economist() +
			geom_line()+
			scale_x_date(breaks = xAxisTicks) +
			labs(x = "", y=nameFix, fill="", color="", title="S&P 500 and NIFTY 50", subtitle=sprintf("weekly returns [%s:%s]", plotStart, plotEnd)) +
			annotate("text", x=plotEnd, y=min(allStats[, lbs], na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.6)
		ggsave(sprintf("%s/%s.weeklyReturns.%s.png", reportPath, paste(lbs, collapse="."), nameFix), width=12, height=6, units="in")	
	}
}

plotSeries(spStats[[2]], niftyStats[[2]], "median")
plotSeries(spStats[[3]], niftyStats[[3]], "std. dev.")
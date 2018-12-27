library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
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
lconUs <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

process<-function(xts1, indexName){
	dXts1<-dailyReturn(xts1)

	rDd<-rollapply(dXts1, 220, function(X) {
		td<-table.Drawdowns(X, 1)[, c('Trough', 'Depth')]
		xts(matrix(c(as.numeric(format(td[1,1], "%Y%m%d")), td[1,2]), nrow=1, ncol=2), td[1,1])
	}, by.column=FALSE)

	rddf<-data.frame(na.omit(rDd))
	rddf[,1]<-ymd(rddf[,1])
	names(rddf)<-c('T', 'D')

	mddf<-data.frame(rddf %>% group_by(T) %>% summarize(MD=min(D)))
	mdXts<-xts(mddf[,2], as.Date(mddf[,1]))

	mddf$RET<-NA
	for(i in 1:nrow(mddf)){
		ddt<-as.Date(mddf$T[i])
		mddf$RET[i]<-tryCatch(Return.cumulative(dXts1[sprintf("%s/", ddt)][1:220]), error=function(e) NA)
	}

	toPlot<-merge(xts1, mdXts)

	plotStart<-first(index(toPlot))
	plotEnd<-last(index(toPlot))
	xAxisTicks<-seq(plotStart, plotEnd, length.out=10)

	toPlotDf<-data.frame(toPlot)
	names(toPlotDf)<-c('P', 'DD')
	toPlotDf$T<-index(toPlot)
	toPlotDf$DP<-ifelse(is.na(toPlotDf$DD), NA, toPlotDf$P)

	pdf(NULL)	
	ggplot(toPlotDf, aes(x=T)) + 
		theme_economist() +
		geom_line(aes(y=P), na.rm = T) +
		geom_point(aes(y=DP), na.rm = T, color='red', size=1) +
		scale_y_log10() +
		scale_x_date(breaks = xAxisTicks) +
		labs(x = "", y="log(price)", fill="", color="", title=indexName, subtitle=sprintf("points of 220-day max-drawdown [%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=plotEnd, y=min(toPlotDf$P), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	ggsave(sprintf("%s/%s.dd.png", reportPath, indexName), width=12, height=6, units="in")

	toPlot<-100*mddf[, c('MD', 'RET')]
	ggplot(toPlot, aes(x=MD, y=RET)) +
		theme_economist() +
		geom_point() +
		geom_smooth() +
		labs(x = "220-day max drawdown", y="subsequent 220-day returns", fill="", color="", title=indexName, subtitle=sprintf("[%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=max(toPlot$MD, na.rm=T), y=min(toPlot$RET, na.rm=T), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	ggsave(sprintf("%s/%s.dd-returns.png", reportPath, indexName), width=12, height=6, units="in")
}

endDate<-as.Date("2018-11-30")
index1<-'^GSPC'
index2<-'NIFTY 50'

df1<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO 
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and symbol='%s'", "1970-01-01", endDate, index1))

xts1<-xts(df1$ac, as.Date(df1$time_stamp))

process(xts1, "SP500")

df2<-sqlQuery(lcon, sprintf("select px_close, time_stamp from BHAV_INDEX
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and index_name='%s'", "1991-01-01", endDate, index2))
							
xts2<-xts(df2$px_close, as.Date(df2$time_stamp))

process(xts2, "NIFTY50")
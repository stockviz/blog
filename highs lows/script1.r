library('RODBC')
library('ggplot2')
library('extrafont')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggthemes')
library('reshape2')

source("d:/stockviz/r/config.r")

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."
indexName<-'NIFTY MIDCAP 100'
lookbacks<-c(220, 440, 660, 880)

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexPx<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where index_name='%s'", indexName))
indexPxts2<-xts(indexPx[,1], as.Date(indexPx[,2]))

for(lb in lookbacks){
	indexPxts<-merge(indexPxts2, rollapply(indexPxts2, lb, min), rollapply(indexPxts2, lb, max))
	names(indexPxts)<-c("MIDCAP100", "LOW220D", "HIGH220D")

	xAxisTicks<-seq(from=index(first(indexPxts)), to=index(last(indexPxts)), length.out=10)
	ggplot(data=indexPxts, aes(x=Index, y=MIDCAP100))+
		theme_economist() +
		geom_line() +
		scale_x_date(breaks = xAxisTicks) +
		geom_ribbon(aes(ymin=LOW220D, ymax=HIGH220D), 
					alpha=0.1,       #transparency
					linetype=1,      #solid, dashed or other line types
					size=0,          #border line size
					fill="green") +  #fill color
		geom_line(aes(y=LOW220D), color='red') +
		geom_line(aes(y=HIGH220D), color='green') +
		ylab("Index") + 
		xlab("") +
		ggtitle(sprintf("%s %d-Highs/Lows [%s:%s] @StockViz", indexName, lb, index(first(indexPxts)), index(last(indexPxts))))

	ggsave(sprintf("%s/%s.high-low.%d.png", reportPath, indexName, lb), dpi=600, width=12, height=6, units="in")  
}
#############################################################	

lb<-220
indexPxts<-merge(indexPxts2, rollapply(indexPxts2, 220, min), rollapply(indexPxts2, 220, max))

subIndexPxts<-indexPxts["2013/",]
names(subIndexPxts)<-c("MIDCAP100", "LOW220D", "HIGH220D")

xAxisTicks<-seq(from=index(first(subIndexPxts)), to=index(last(subIndexPxts)), length.out=10)
ggplot(data=subIndexPxts, aes(x=Index, y=MIDCAP100))+
	theme_economist() +
	geom_line() +
	scale_x_date(breaks = xAxisTicks) +
	geom_ribbon(aes(ymin=LOW220D, ymax=HIGH220D), 
                alpha=0.1,       #transparency
                linetype=1,      #solid, dashed or other line types
                size=0,          #border line size
                fill="green") +  #fill color
	geom_line(aes(y=LOW220D), color='red') +
	geom_line(aes(y=HIGH220D), color='green') +
	ylab("Index") + 
	xlab("") +
	ggtitle(sprintf("%s  [%s:%s] @StockViz", indexName, index(first(subIndexPxts)), index(last(subIndexPxts))))

	ggsave(sprintf("%s/%s.high-low.%d.2013-.png", reportPath, indexName, lb), dpi=600, width=12, height=6, units="in")  
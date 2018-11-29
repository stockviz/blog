library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('lubridate')
library('reshape2')
library('ggrepel')
library('dplyr')
library('grid')
library('gridExtra')
library('gtable')

options("scipen"=100)
options(stringsAsFactors = FALSE)
reportPath <- "."
#reportPath <- "D:/StockViz/public/blog/gaps"
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

plotIntraday<-function(plotDf, mainTitle){
	plotDf$TIME_STAMP<-as.Date(plotDf$TIME_STAMP)

	plotStart<-min(plotDf$TIME_STAMP)
	plotEnd<-max(plotDf$TIME_STAMP)
	xAxisTicks<-seq(plotStart, plotEnd, length.out=10)

	plotDf<-data.frame(plotDf %>% group_by(TIME_STAMP) %>% summarize(MIN=mean(MIN), MAX=mean(MAX), MEDIAN=mean(MEDIAN), SD=mean(SD)))
	plotDf$avgmsd<-plotDf$MEDIAN - plotDf$SD
	plotDf$avgpsd<-plotDf$MEDIAN + plotDf$SD
			
	colPal<-economist_pal(fill = TRUE)(10)

	pdf(NULL)
	ggplot(plotDf, aes(x=TIME_STAMP)) +
		theme_economist() +
		geom_line(aes(y=MIN), color=colPal[1]) +
		geom_line(aes(y=MAX), color=colPal[2]) +
		geom_line(aes(y=MEDIAN), color=colPal[3]) +
		geom_ribbon(aes(ymin = avgmsd, ymax=avgpsd), fill='grey70', alpha=0.5) +
		scale_x_date(breaks = xAxisTicks) +
		labs(x = "", y="returns(%)", fill="", color="", title=mainTitle) +
		annotate("text", x=plotEnd, y=max(plotDf$MAX), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	ggsave(sprintf("%s/%s.png", reportPath, mainTitle), width=12, height=6, units="in")	
}

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

niftyOpenDf<-sqlQuery(lcon, "select * from SEGMENT_RETURN_ANAL_FUT where SYMBOL='NIFTY' and segment <= 2 order by TIME_STAMP")
niftyCloseDf<-sqlQuery(lcon, "select * from SEGMENT_RETURN_ANAL_FUT where SYMBOL='NIFTY' and segment >= 24 order by TIME_STAMP")

plotIntraday(niftyOpenDf, "NIFTY first 30 minutes")
plotIntraday(niftyCloseDf, "NIFTY last 30 minutes")
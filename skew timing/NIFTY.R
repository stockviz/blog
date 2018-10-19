library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('lubridate')
library('reshape2')
library('ggrepel')

options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
reportPath <- "."
lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

args = commandArgs(TRUE)
indexName = toString(args[1])
print(indexName)

bondStartDate<-as.Date('2004-01-01')
indexStartDate<-bondStartDate-370

endDate<-as.Date("2018-10-15")

indexDf<-sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, indexStartDate, endDate))
bilDf<-sqlQuery(lcon, sprintf("select time_stamp, tri from INDEX_CCIL_TENOR where index_name='0_5' and time_stamp >= '%s' and time_stamp <= '%s'", bondStartDate, endDate))

allXts<-merge(xts(indexDf[,2], as.Date(indexDf[,1])), xts(bilDf[,2], as.Date(bilDf[,1]))) #index,bil

allXts<-merge(allXts, dailyReturn(allXts[,1]), dailyReturn(allXts[,2]))
names(allXts)<-c(indexName, 'BIL', 'DRET_INDEX', 'DRET_BIL')

allXts<-merge(allXts, lag(allXts$DRET_INDEX, -1), lag(allXts$DRET_BIL, -1)) #lag the returns by a day to avoid look-forward
names(allXts)<-c(indexName, 'BIL', 'DRET_INDEX', 'DRET_BIL', 'DRET_INDEX_LAG1', 'DRET_BIL_LAG1')
allXts<-allXts[-1,]

allXts$SKEW_INDEX<-rollapply(allXts$DRET_INDEX, 220, function(X) skewness(as.numeric(X), na.rm=T, method='fisher'))

##### BEGIN: skew plot
skewXts<-na.omit(allXts$SKEW_INDEX)
firstDate<-as.Date(first(index(skewXts)))
lastDate<-as.Date(last(index(skewXts)))
xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)
toPlotDf<-data.frame(skewXts)
toPlotDf$DAY<-as.Date(index(skewXts))
names(toPlotDf)<-c('SKEW', 'DAY')

pdf(NULL)
ggplot(data=toPlotDf, aes(x=DAY)) +
	theme_economist() +
	geom_line(aes(y=SKEW)) + 
	scale_x_date(breaks = xAxisTicks) +
	labs(x='', y='skew', color='', title=sprintf("skew(%s)", indexName), subtitle=sprintf("rolling 220-day daily returns [%s:%s]", firstDate, lastDate)) +
	annotate("text", x=lastDate, y=min(skewXts), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s/%s.SKEW.png", reportPath, gsub(" ", "_", indexName)), dpi=600, width=12, height=6, units="in")
##### END: skew plot

#returns without switching over to bonds
allXts$DRET_INDEX_P<-ifelse(allXts$SKEW_INDEX >= 0, allXts$DRET_INDEX_LAG1, 0)
allXts$DRET_INDEX_N<-ifelse(allXts$SKEW_INDEX < 0, allXts$DRET_INDEX_LAG1, 0)

plotCumReturns<-function(toPlot, chartTitle, fileName){
	pdf(NULL)
	png(fileName, width=1400, height=800, bg="white")
	layout(matrix(c(1, 2)), heights = c(2, 1.3), widths = 1)
	par(mar = c(0, 4, 4, 2), family='Segoe UI', bty="n")
	plot_object <-chart.CumReturns(toPlot, cex.legend=1, main=NA, xaxis = FALSE, legend.loc = "topleft", begin = c("first", "axis"), geometric = TRUE) 
	print(plot_object)
	mtext("Cumulative Return", side=2, line=1)
	title(main=chartTitle, family='Segoe UI') 
	mtext(paste(sprintf("%.2f%%", 100*apply(toPlot, 2, Return.cumulative)), collapse=" / "), cex=0.8)
	par(mar = c(5, 4, 0, 2))
	plot_object <-chart.Drawdown(toPlot, main = NA, ylab = "Drawdown", event.labels = NULL, ylog = FALSE, geometric = TRUE, bty="n")
	print(plot_object)
	mtext("Drawdown", side=2, line=1)
	mtext("@StockViz", side=4, col='grey')
	dev.off()
}

toPlot<-na.omit(allXts[, c('DRET_INDEX_LAG1', 'DRET_INDEX_P','DRET_INDEX_N')])
plotCumReturns(toPlot, sprintf("%s/skew(%s)", indexName, indexName), sprintf("%s/cumulative.%s.SKEW.png", reportPath, gsub(" ", "_", indexName)))

#returns with switching over to bonds
allXts$DRET_INDEX_P<-ifelse(allXts$SKEW_INDEX < 0, allXts$DRET_INDEX_LAG1, allXts$DRET_BIL_LAG1)
allXts$DRET_INDEX_N<-ifelse(allXts$SKEW_INDEX < 0, allXts$DRET_INDEX_LAG1, allXts$DRET_BIL_LAG1)
toPlot<-na.omit(allXts[, c('DRET_INDEX_LAG1', 'DRET_INDEX_P','DRET_INDEX_N')])
plotCumReturns(toPlot, sprintf("%s~LQD/skew(%s)", indexName, indexName), sprintf("%s/cumulative.%s-LQD.SKEW.png", reportPath, gsub(" ", "_", indexName)))

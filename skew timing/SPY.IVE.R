#replication of https://www.factorresearch.com/research-improving-the-odds-of-value using ETFs (SPY and IVE)

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

iveStartDate<-as.Date(sqlQuery(lcon, "select min(time_stamp) from BHAV_EQ_AV where SYMBOL='IVE'")[[1]])
spyStartDate<-iveStartDate-370

endDate<-as.Date("2018-10-15")

iveDf<-sqlQuery(lcon, sprintf("select time_stamp, px_close_adj from bhav_eq_av where symbol='ive' and vol > 0 and time_stamp >= '%s' and time_stamp <= '%s'", iveStartDate, endDate))
spyDf<-sqlQuery(lcon, sprintf("select time_stamp, px_close_adj from bhav_eq_av where symbol='spy' and vol > 0 and time_stamp >= '%s' and time_stamp <= '%s'", spyStartDate, endDate))
bilDf<-sqlQuery(lcon, sprintf("select time_stamp, px_close_adj from bhav_eq_av where symbol='LQD' and vol > 0 and time_stamp >= '%s' and time_stamp <= '%s'", spyStartDate, endDate))

allXts<-merge(xts(iveDf[,2], as.Date(iveDf[,1])), xts(spyDf[,2], as.Date(spyDf[,1])), xts(bilDf[,2], as.Date(bilDf[,1]))) #ive,spy,bil

allXts<-merge(allXts, dailyReturn(allXts[,1]), dailyReturn(allXts[,2]), dailyReturn(allXts[,3]))
names(allXts)<-c('IVE', 'SPY', 'BIL', 'DRET_IVE', 'DRET_SPY', 'DRET_BIL')
allXts[is.na(allXts$DRET_BIL),]$DRET_BIL<-0.0 #LQD was launched in 2002. we'll assume zero returns before launch

allXts<-merge(allXts, lag(allXts$DRET_IVE, -1), lag(allXts$DRET_SPY, -1), lag(allXts$DRET_BIL, -1)) #lag the returns by a day to avoid look-forward
names(allXts)<-c('IVE', 'SPY', 'BIL', 'DRET_IVE', 'DRET_SPY', 'DRET_BIL', 'DRET_IVE_LAG1', 'DRET_SPY_LAG1', 'DRET_BIL_LAG1')
allXts<-allXts[-1,]

allXts$SKEW_SPY<-rollapply(allXts$DRET_SPY, 220, function(X) skewness(as.numeric(X), na.rm=T, method='fisher'))

##### BEGIN: skew plot
skewXts<-na.omit(allXts$SKEW_SPY)
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
	labs(x='', y='skew', color='', title="skew(SPY)", subtitle=sprintf("rolling 220-day daily returns [%s:%s]", firstDate, lastDate)) +
	annotate("text", x=lastDate, y=min(skewXts), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s/SPY.SKEW.png", reportPath), dpi=600, width=12, height=6, units="in")
##### END: skew plot

#returns without switching over to bonds
allXts$DRET_SPY_P<-ifelse(allXts$SKEW_SPY >= 0, allXts$DRET_SPY_LAG1, 0)
allXts$DRET_SPY_N<-ifelse(allXts$SKEW_SPY < 0, allXts$DRET_SPY_LAG1, 0)

allXts$DRET_IVE_P<-ifelse(allXts$SKEW_SPY >= 0, allXts$DRET_IVE_LAG1, 0)
allXts$DRET_IVE_N<-ifelse(allXts$SKEW_SPY < 0, allXts$DRET_IVE_LAG1, 0)

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

toPlot<-na.omit(allXts[, c('DRET_IVE_LAG1', 'DRET_SPY_LAG1', 'DRET_IVE_P','DRET_IVE_N', 'DRET_SPY_P', 'DRET_SPY_N')])
plotCumReturns(toPlot, "IVE/skew(SPY)", sprintf("%s/cumulative.SPY.IVE.SKEW.png", reportPath))

#returns with switching over to bonds
allXts$DRET_SPY_N<-ifelse(allXts$SKEW_SPY < 0, allXts$DRET_SPY_LAG1, allXts$DRET_BIL_LAG1)
allXts$DRET_IVE_N<-ifelse(allXts$SKEW_SPY < 0, allXts$DRET_IVE_LAG1, allXts$DRET_BIL_LAG1)
toPlot<-na.omit(allXts[, c('DRET_IVE_LAG1', 'DRET_SPY_LAG1', 'DRET_IVE_N', 'DRET_SPY_N')])
plotCumReturns(toPlot, "IVE~LQD/skew(SPY)", sprintf("%s/cumulative.SPY.IVE-LQD.SKEW.png", reportPath))

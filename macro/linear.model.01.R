library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')
library('dplyr')
library('RPostgres')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
#reportPath <- "D:/StockViz/report"

source("d:/stockviz/r/config.r")

startDate<-as.Date("2010-01-01")
trainDate<-as.Date("2015-12-31")
endDate<-as.Date("2018-09-30")
asset1Name<-"NIFTY 50"

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs<-odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

asset1Px<-sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", asset1Name, startDate, endDate))
asset1Xts<-xts(asset1Px[,2], as.Date(asset1Px[,1], tz=TZ))

usdinr<-sqlQuery(lconUs, sprintf("select VAL, TIME_STAMP from FRED_OBSERVATION where SERIES_ID=-2147478748 and TIME_STAMP >='%s' and TIME_STAMP <= '%s'", startDate, endDate))
usdinrXts<-xts(usdinr$VAL, as.Date(usdinr$TIME_STAMP, tz=TZ))

pgCon <- dbConnect(RPostgres::Postgres(), host='windows', user=ldbuser2, password=ldbpassword2, dbname='StockVizBeka', sslmode='allow')
res <- dbSendQuery(pgCon, "SELECT time_stamp, px_close FROM av_fx_usd_daily_ts WHERE curr_code='INR' AND time_stamp >= $1 AND time_stamp <= $2", list(startDate, endDate))
inrAv <- dbFetch(res)
dbClearResult(res)
dbDisconnect(pgCon)

if(nrow(inrAv) > 0){
	inrAvXts<-merge(usdinrXts, xts(inrAv[,2], as.Date(inrAv[,1])))
	inrAvXts[is.na(inrAvXts[,1]),1]<-inrAvXts[is.na(inrAvXts[,1]),2]
} else {
	inrAvXts<-usdinrXts
}

allXts<-merge(asset1Xts, inrAvXts[,1])
names(allXts)<-c('INDEX', 'INR')
allXts$INR<-na.locf(allXts$INR)
allXts<-na.omit(allXts)

retXts<-merge(weeklyReturn(allXts$INDEX), weeklyReturn(allXts$INR))
names(retXts)<-c('INDEX', 'INR')
retXts$INDEX<-stats::lag(retXts$INDEX, -1)
retXts<-na.omit(retXts)

retDf<-data.frame(retXts[sprintf("%s/%s", startDate, trainDate),])

linearModel<-lm("INDEX~INR", retDf)

pdf(NULL)

png(sprintf('%s/%s.INR.lag-1.lm.%s.%s.png', reportPath, asset1Name, startDate, trainDate), width=10, height=10, units='in', res=300)
layout(matrix(1:4, ncol = 2))
par(family='Segoe UI')
plot(linearModel, main=sprintf('Lag -1 %s vs. INR [%s:%s]', asset1Name, startDate, trainDate))
layout(1)
dev.off()

testSet<-data.frame(retXts[sprintf("%s/%s", trainDate+1, endDate),])
testSet$INDEX_PRED<-predict.lm(linearModel, testSet)

ggplot(testSet, aes(x=INDEX, y=INDEX_PRED))+
	theme_economist() +
	geom_point() +
	geom_hline(yintercept=0, linetype='dashed', color='darkgrey') +
	geom_vline(xintercept=0, linetype='dashed', color='darkgrey') +
	labs(x = "actual", y="predicted", fill="", title=sprintf("Linear Model %s/USDINR", asset1Name), subtitle=sprintf("[%s:%s:%s]", startDate, trainDate, endDate)) +
	annotate("text", x=max(testSet$INDEX), y=min(testSet$INDEX_PRED), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s/actual.vs.pred.%s.png", reportPath, asset1Name), width=10, height=6, units="in")

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

testSet$L<-ifelse(testSet$INDEX_PRED > 0, testSet$INDEX, 0)
testSet$LS<-ifelse(testSet$INDEX_PRED > 0, testSet$INDEX, -testSet$INDEX)
predXts<-xts(testSet[, c('INDEX', 'L', 'LS')], as.Date(row.names(testSet)))

plotCumReturns(predXts, sprintf("Linear Model %s/USDINR", asset1Name), sprintf("%s/linear.model.cumulative.%s.png", reportPath, asset1Name))
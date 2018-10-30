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
library('ggrepel')
library('dplyr')
library('RPostgres')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
#reportPath <- "D:/StockViz/report"

source("d:/stockviz/r/config.r")

plotDensities<-function(densityList, denNames, titleStr, subtitleStr, fileName){
	denDf<-data.frame(Y=NA)
	lapply(densityList, function(DEN) denDf<<-merge(denDf, DEN, by='Y', all=T)) 
	names(denDf)<-c("DENSITY", denNames)

	meltedDf<-melt(denDf, id='DENSITY')

	pdf(NULL)
	ggplot(meltedDf, aes(x=value, y=DENSITY, color=variable))+ 
		theme_economist() +
		geom_line() +
		geom_vline(xintercept=0, linetype='dashed', color='grey')+
		labs(x = "weekly returns", y="density", fill="", color="", title=titleStr, subtitle=subtitleStr) +
		annotate("text", x=max(meltedDf$value, na.rm=T), y=max(meltedDf$DENSITY, na.rm=T), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(fileName, width=12, height=6, units="in")
}


startDate<-as.Date("2010-01-01")
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

oil<-sqlQuery(lconUs, sprintf("select VAL, TIME_STAMP from FRED_OBSERVATION where SERIES_ID=-2147252260 and TIME_STAMP >='%s' and TIME_STAMP <= '%s'", startDate, endDate))
oilXts<-xts(oil$VAL, as.Date(oil$TIME_STAMP, tz=TZ))

#####################################################

allXts<-merge(asset1Xts, oilXts)
names(allXts)<-c('INDEX', 'OIL')
allXts$INR<-na.locf(allXts$OIL)
allXts<-na.omit(allXts)

retXts<-merge(100*weeklyReturn(allXts$INDEX), 100*weeklyReturn(allXts$OIL))
names(retXts)<-c('INDEX', 'OIL')

retXts$INDEX<-stats::lag(retXts$INDEX, -1)
retXts<-na.omit(retXts)

retDf<-data.frame(retXts[sprintf("%s/%s", startDate, endDate),])

inrSeq<-seq(from=5, to=12, by=2)
denList<-lapply(inrSeq, function(X) {
	d<-density(retDf[retDf$OIL > X,'INDEX'])
	data.frame(X=d$x, Y=d$y)
	})
denNames<-sapply(inrSeq, function(X) sprintf("OIL > %.1f%%", X))	
plotDensities(denList, denNames, sprintf("%s OIL+", asset1Name), sprintf("[%s:%s]", startDate, endDate), sprintf("%s/density.%s.OIL+.png", reportPath, asset1Name))

inrSeq<-seq(from=5, to=12, by=2)
denList<-lapply(inrSeq, function(X) {
	d<-density(retDf[retDf$OIL < -X,'INDEX'])
	data.frame(X=d$x, Y=d$y)
	})
denNames<-sapply(inrSeq, function(X) sprintf("OIL < %.1f%%", -X))	
plotDensities(denList, denNames, sprintf("%s OIL-", asset1Name), sprintf("[%s:%s]", startDate, endDate), sprintf("%s/density.%s.OIL-.png", reportPath, asset1Name))

#####################################################

allXts<-merge(asset1Xts, inrAvXts[,1])
names(allXts)<-c('INDEX', 'INR')
allXts$INR<-na.locf(allXts$INR)
allXts<-na.omit(allXts)

retXts<-merge(100*weeklyReturn(allXts$INDEX), 100*weeklyReturn(allXts$INR))
names(retXts)<-c('INDEX', 'INR')

retXts$INDEX<-stats::lag(retXts$INDEX, -1)
retXts<-na.omit(retXts)

retDf<-data.frame(retXts[sprintf("%s/%s", startDate, endDate),])

inrSeq<-seq(from=0.5, to=2, by=0.5)
denList<-lapply(inrSeq, function(X) {
	d<-density(retDf[retDf$INR > X,'INDEX'])
	data.frame(X=d$x, Y=d$y)
	})
denNames<-sapply(inrSeq, function(X) sprintf("INR > %.1f%%", X))	
plotDensities(denList, denNames, sprintf("%s USDINR+", asset1Name), sprintf("[%s:%s]", startDate, endDate), sprintf("%s/density.%s.USDINR+.png", reportPath, asset1Name))

denList<-lapply(inrSeq, function(X) {
	d<-density(retDf[retDf$INR < -X,'INDEX'])
	data.frame(X=d$x, Y=d$y)
	})
denNames<-sapply(inrSeq, function(X) sprintf("INR < %.1f%%", -X))	
plotDensities(denList, denNames, sprintf("%s USDINR-", asset1Name), sprintf("[%s:%s]", startDate, endDate), sprintf("%s/density.%s.USDINR-.png", reportPath, asset1Name))

#####################################################


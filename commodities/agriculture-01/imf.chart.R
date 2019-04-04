library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('ggrepel')

source("d:/stockviz/r/config.r")

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."

series <- c("PCPS:PFERT:IX", "PCPS:PCERE:IX", "PCPS:PRICENPQ:IX", "PCPS:PWHEAMT:IX")

lconUs <- odbcDriverConnect(sprintf("Driver={ODBC Driver 13 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

getTimeSeries<-function(idStr){
	idToks<-strsplit(idStr, ":")[[1]]
	
	parentId<-sqlQuery(lconUs, sprintf("select ID from IMF_FAMILY_META where DATA_KEY='%s' and PARENT is null", idToks[1]))[1,1]
	seriesId<-sqlQuery(lconUs, sprintf("select ID from IMF_FAMILY_META where DATA_KEY='%s' and PARENT = %d", idToks[2], parentId))[1,1]
	obsInfo<-sqlQuery(lconUs, sprintf("select * from IMF_SERIES where CODE_ID = %d and UNIT_MEASURE = '%s'", seriesId, idToks[3]))
	
	if(obsInfo$TIME_FORMAT[1] != "P1M"){
		stop("invalid time format")
	}
	
	obDf<-sqlQuery(lconUs, sprintf("select * from IMF_OBSERVATION where SERIES_ID=%d", obsInfo$ID[1]))
	obXts<-xts(obDf$VAL, as.yearmon(sprintf("%d-%d", obDf$DATE_Y, obDf$DATE_M)))
	
	return(obXts)
}

timeSeries<-NULL
for(s in series){
	ts1<-getTimeSeries(s)
	ts1<-ts1 - as.numeric(coredata(ts1[1]))
	timeSeries<-merge.xts(timeSeries, ts1)
}
names(timeSeries)<-series

tsDf<-data.frame(timeSeries)
tsDf$T<-index(timeSeries)

firstDate<-first(index(timeSeries))
lastDate<-last(index(timeSeries))
	
toPlot<-melt(tsDf, id='T')
toPlot$lineWidth<-rep(seq(length(series)-1, 0), each=nrow(tsDf))
toPlot$label<-ifelse(toPlot$T == max(toPlot$T), as.character(toPlot$variable), NA)

pdf(NULL)
ggplot(toPlot, aes(x=T, y=value, color=variable, size=lineWidth)) +
	theme_economist() +
	geom_line() +
	geom_text_repel(aes(label=label), nudge_x=1, na.rm=T, size=3) +
	guides(color=F) +
	scale_size(range=c(0.5, 1.5), guide=FALSE) + 
	labs(x='', y='', color='', title="Price Index", subtitle=sprintf("[%s:%s]", firstDate, lastDate)) +
	annotate("text", x=lastDate, y=min(timeSeries, na.rm=T), label = "@StockViz", hjust=1.1, col="white", cex=6, fontface = "bold", alpha = 0.8) +
	annotate("text", x=firstDate, y=min(timeSeries, na.rm=T), label = "Source: IMF", hjust=0.5, col="black", cex=4, alpha=0.8)
	
ggsave(sprintf("%s/IMF.%s.png", reportPath, gsub(":", "-", paste(series, collapse="."))), width=16, height=8, units="in")		
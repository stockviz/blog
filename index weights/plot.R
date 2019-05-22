library('RODBC')
library('extrafont')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('dplyr')
library('ggplot2')
library('ggthemes')
library('reshape2')
library(scales)

source("d:/stockviz/r/config.r")

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startYear<-2009
endYear<-2019
sampleMonth<-3

indices<-c('NIFTY 50', 'NIFTY NEXT 50')

for(i in 1:length(indices)){
	indexName<-indices[i]

	weightsDf<-NULL
	for(yr in seq(startYear, endYear, by=1)){
		wDf<-sqlQuery(lcon, sprintf("select TIME_STAMP, INDUSTRY, sum(CAP_WEIGHT) CW from INDEX_CONST_HISTORY 
										where time_stamp='%s' and index_name='%s' 
										group by TIME_STAMP, INDUSTRY 
										order by CW desc", sprintf("%d-%02d-31", yr, sampleMonth), indexName))
										
		weightsDf<-rbind(weightsDf, wDf)
	}

	weightsDf$CW<-as.numeric(weightsDf$CW)
	weightsDf$TIME_STAMP<-as.POSIXct(weightsDf$TIME_STAMP)
	weightsDf$Y<-year(weightsDf$TIME_STAMP)
	weightsDf$Yf<-factor(weightsDf$Y, levels=unique(weightsDf$Y))

	weightsTop<-data.frame(weightsDf %>% group_by(Y) %>% mutate(CUMW = cumsum(CW))	%>% filter(CUMW <= 90))

	ggplot(weightsTop, aes(x=Yf, y=CW, fill=INDUSTRY)) +
		theme_economist() +
		geom_bar(stat="identity") +
		labs(x='', y='%', color='', fill='', title=sprintf("%s Composition", indexName))
	ggsave(sprintf("%s/weights.%s.%d-%d.png", reportPath, indexName, startYear, endYear), width=16, height=8, units="in")
		
	wtsSlice<-weightsTop[weightsTop$Y==startYear,]
	png(sprintf("%s/weights.%s.%d.png", reportPath, indexName, startYear), bg='white', height=768, width=1024)
	pie(wtsSlice$CW, labels=sprintf("%s (%.2f%%)", wtsSlice$INDUSTRY, wtsSlice$CW), col=economist_pal()(9), main=sprintf("%s Composition (%d)", indexName, startYear), family='Segoe UI', init.angle=45)
	mtext("@StockViz", side=1)
	dev.off()

	wtsSlice<-weightsTop[weightsTop$Y==endYear,]
	png(sprintf("%s/weights.%s.%d.png", reportPath, indexName, endYear), bg='white', height=768, width=1024)
	pie(wtsSlice$CW, labels=sprintf("%s (%.2f%%)", wtsSlice$INDUSTRY, wtsSlice$CW), col=economist_pal()(9), main=sprintf("%s Composition (%d)", indexName, endYear), family='Segoe UI', init.angle=45)
	mtext("@StockViz", side=1)
	dev.off()

}
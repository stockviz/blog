library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')

library('tidyverse')
library('lubridate')
library('ggthemes')
library('reshape2')
library('viridis')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

indexName <- "NIFTY 50"
indexNameTr <- "NIFTY 50 TR"

indexChangeDates <- sqlQuery(lcon, sprintf("select distinct(time_stamp) ts from INDEX_CONST_HISTORY where index_name = '%s' order by ts desc", indexName))[,1]

statsDF <- data.frame(REBAL_DATE = "", SYMBOL = "", RET = 0.0, EXCESS_RET = 0.0)
for(i in 2:length(indexChangeDates)){
	newSet <- sqlQuery(lcon, sprintf("select symbol from INDEX_CONST_HISTORY where index_name = '%s' and time_stamp = '%s'", indexName, indexChangeDates[i-1]))[,1]
	oldSet <- sqlQuery(lcon, sprintf("select symbol from INDEX_CONST_HISTORY where index_name = '%s' and time_stamp = '%s'", indexName, indexChangeDates[i]))[,1]
	
	entries <- setdiff(newSet, oldSet)
	
	if(length(entries) == 0) next
	
	endDate <- indexChangeDates[i]
	startDate <- endDate - 30
	
	for(ei in 1:length(entries)){
		symbol <- entries[ei]
		
		pxSeries <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse where ticker=$1 and date_stamp >= $2 and date_stamp <= $3 order by date_stamp", params=list(symbol, startDate, endDate))
		if(nrow(pxSeries) == 0) next
		
		pXts <- xts(pxSeries[,2], pxSeries[,1])
		pxRet <- Return.cumulative(dailyReturn(pXts))*100
		
		ixSeries <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s' order by time_stamp", indexNameTr, startDate, endDate))
		ixXts <- xts(ixSeries[,2], ixSeries[,1])
		ixRet <- Return.cumulative(dailyReturn(ixXts))*100
		
		exRet <- pxRet - ixRet
		
		statsDF <- rbind(statsDF, c(as.character(endDate), symbol, as.numeric(pxRet), as.numeric(exRet)))
	}
}

statsDF <- statsDF[-1,]
statsDF$REBAL_DATE <- as.Date(statsDF$REBAL_DATE)
statsDF$RET <- as.numeric(statsDF$RET)
statsDF$EXCESS_RET <- as.numeric(statsDF$EXCESS_RET)

statsXts <- xts(statsDF$EXCESS_RET/100, statsDF$REBAL_DATE)

toPlot <- subset(statsDF, select=-RET)

ggplot(toPlot, aes(x=factor(REBAL_DATE), y=EXCESS_RET, fill=SYMBOL, label=SYMBOL, color=SYMBOL)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	scale_fill_viridis_d(option='C') +
	scale_color_viridis_d(option='C') +
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text_repel(force=2, color='darkgrey') +
	guides(fill='none', color='none') +
	labs(y='Excess Returns (%)', x='', title='NIFTY 50 Index Effect')

ggsave(sprintf("%s/nifty-50-index-effect.png", reportPath), width=12, height=6, units="in")

stats2 <- statsDF %>% select(REBAL_DATE, EXCESS_RET) %>% group_by(REBAL_DATE) %>% summarize(XR = sum(EXCESS_RET)) %>% as.data.frame()
statsXts <- xts(stats2$XR/100, stats2$REBAL_DATE)

cumRet <- cumprod(1+statsXts) * 100
names(cumRet) <- c('EXCESS_RET')
toPlot <- data.frame(cumRet)
toPlot$T <- index(cumRet)

ggplot(toPlot, aes(x=T, y=EXCESS_RET)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	geom_line(size=2, color='darkgreen') +
	scale_x_date(date_labels = "%Y", date_breaks="1 year") +
	labs(y='Cumulative Excess Returns', x='', title='NIFTY 50 Index Effect')

ggsave(sprintf("%s/nifty-50-index-effect-cum-ret.png", reportPath), width=12, height=6, units="in")
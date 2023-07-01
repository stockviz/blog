library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')

library('tidyverse')
library('lubridate')
library('reshape2')
library('ggthemes')
library('patchwork')
library('viridis')

library('knitr')
library('kableExtra')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

indexNames <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR", "NIFTY MICROCAP 250 TR", "NIFTY200 MOMENTUM 30 TR", "NIFTY MIDCAP150 MOMENTUM 50 TR")

indexPx <- NULL
for(iName in indexNames){
	iDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name = '%s'", iName))
	indexPx <- merge.xts(indexPx, xts(iDf[,1], iDf[,2]))
}
names(indexPx) <- indexNames

startDate <- first(index(indexPx))

fredUsdInr <- 'DEXINUS'
usdDf<-sqlQuery(lconUs, sprintf("select val, time_stamp from FRED_OBSERVATION 
								where time_stamp >= '%s' 
								and series_id=(select id from FRED_SERIES where series_id='%s')", startDate, fredUsdInr))

usdXts<-xts(usdDf$val, as.Date(usdDf$time_stamp))

usdDf <- dbGetQuery(pgCon, "select px_close, time_stamp from av_fx_usd_daily_ts where curr_code=$1 and time_stamp > $2", params=list("INR", last(index(usdXts))))

usdXts<-rbind(usdXts, xts(usdDf$px_close, as.Date(usdDf$time_stamp)))


for(j in 1:ncol(indexPx)){
	inrIndexPx <- na.omit(indexPx[,j])
	inrRollMax <- inrIndexPx[inrIndexPx == cummax(inrIndexPx)]
	inrIndexRet <- dailyReturn(inrIndexPx)
	
	tmpIndex <- merge(inrIndexPx, usdXts)
	tmpIndex[,2] <- na.locf(tmpIndex[,2])
	tmpIndex <- na.omit(tmpIndex)
	usdIndex <- tmpIndex[,1]/tmpIndex[,2]
	usdIndexRet <- dailyReturn(usdIndex)
	usdRollMax <- usdIndex[usdIndex == cummax(usdIndex)]
	
	athDates <- index(inrRollMax)
	athDatesDf <- data.frame(D1=athDates, D2=c(athDates[2:length(athDates)], NA))
	athDatesDf$DIFF <- as.numeric(athDatesDf$D2 - athDatesDf$D1)
	
	inrAthDatesTb <- athDatesDf %>% arrange(desc(DIFF)) %>% filter(DIFF > 20)
	inrAthDatesTb$MAX_DD <- NA
	
	for(k in 1:nrow(inrAthDatesTb)){
		inrAthDatesTb$MAX_DD[k] <- round(maxDrawdown(inrIndexRet[paste0(inrAthDatesTb$D1[k], '/', inrAthDatesTb$D2[k])]) * 100, 2)
	}
	

	athDates2 <- index(usdRollMax)
	athDatesDf <- data.frame(D1=athDates2, D2=c(athDates2[2:length(athDates2)], NA))
	athDatesDf$DIFF <- as.numeric(athDatesDf$D2 - athDatesDf$D1)
	
	usdAthDatesTb <- athDatesDf %>% arrange(desc(DIFF)) %>% filter(DIFF > 20)
	usdAthDatesTb$MAX_DD <- NA
	
	for(k in 1:nrow(usdAthDatesTb)){
		usdAthDatesTb$MAX_DD[k] <- round(maxDrawdown(usdIndexRet[paste0(usdAthDatesTb$D1[k], '/', usdAthDatesTb$D2[k])]) * 100, 2)
	}
	
	### plots
	
	### time between ATH
	kable(inrAthDatesTb, caption = sprintf('%s Days between All Time Highs (INR)', indexNames[j]), booktabs = T) %>% 
			kable_styling(bootstrap_options = 'striped', full_width = F) %>% 
			save_kable(file=sprintf("%s/%s-ath-drawdowns-inr.png", reportPath, indexNames[j]), bs_theme = 'flatly')
			
	kable(usdAthDatesTb, caption = sprintf('%s Days between All Time Highs (USD)', indexNames[j]), booktabs = T) %>% 
			kable_styling(bootstrap_options = 'striped', full_width = F) %>% 
			save_kable(file=sprintf("%s/%s-ath-drawdowns-usd.png", reportPath, indexNames[j]), bs_theme = 'flatly')
	
	### price ATH
	tpXts <- merge(log(inrIndexPx), log(inrRollMax))
	names(tpXts) <- c(indexNames[j], "ATH")
	toPlot <- data.frame(tpXts)
	toPlot$T <- index(tpXts)
	jName <- colnames(toPlot)[1]
	
	p1 <- ggplot(toPlot, aes(x=T, y=.data[[jName]])) +
		theme_economist() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		geom_line(color='darkgrey') +
		geom_point(aes(x=T, y=ATH), color='darkgreen') +
		scale_x_date(date_labels = "%Y", date_breaks="1 year") +
		labs(y='log(index)', x='', title=sprintf('%s All Time Highs (INR)', indexNames[j]), subtitle=sprintf("[%s:%s]", first(index(tpXts)), last(index(tpXts)))) 
		
	tpXts <- merge(log(usdIndex), log(usdRollMax))
	names(tpXts) <- c(indexNames[j], "ATH")
	toPlot <- data.frame(tpXts)
	toPlot$T <- index(tpXts)
	jName <- colnames(toPlot)[1]
		
	p2 <- ggplot(toPlot, aes(x=T, y=.data[[jName]])) +
		theme_economist() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		geom_line(color='darkgrey') +
		geom_point(aes(x=T, y=ATH), color='darkgreen') +
		scale_x_date(date_labels = "%Y", date_breaks="1 year") +
		labs(y='log(usd index)', x='', title=sprintf('%s All Time Highs (USD)', indexNames[j]), subtitle=sprintf("[%s:%s]", first(index(tpXts)), last(index(tpXts)))) +
		annotate("text", x=last(index(tpXts)), y=min(tpXts[,1]), label = "@StockViz", hjust='right', vjust='bottom', col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	p1+p2+plot_layout(ncol=1)
	
	ggsave(sprintf("%s/%s-ath-price.png", reportPath, indexNames[j]), width=12, height=12, units="in")

	
}
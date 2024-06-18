#ssrn-2440866 Market Intraday Momentum
#using returns of the first 15/30 min to predict last 30 min

source("d:/stockviz/r/config.r")
reportPath <- "."

library('RODBC')
library('RPostgres')
library('quantmod')
library('tidyverse')
library('lubridate')
library('PerformanceAnalytics')
library('ggthemes')
library('patchwork')
library('viridis')

options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pcon <- dbConnect(RPostgres::Postgres(), host=ldbserver2, user= ldbuser2, password=ldbpassword2, dbname=ldbname2)

baseDate <- as.Date("1990-01-01", tz='UTC')
startTime <- hms("09:15:00")
endTime <- hms("15:30:00")

indexName <- 'NIFTY BANK'
amStart <- "09:15:00"
amEnd <- c("09:30:00", "09:45:00")
pmStart <- "15:00:00"
pmEnd1 <- "15:29:00"
pmEnd2 <- "15:30:00"

indexDf <- dbGetQuery(pcon, sprintf("select c as cl, tick_stamp from zd_index_bars where symbol='%s' ", indexName))
indexDf2 <- indexDf
indexDf2$tick <- as.POSIXct(baseDate + seconds(indexDf2$tick_stamp))
indexDf2$tod <- hms(format(indexDf2$tick, "%H:%M:%S"))

indexDf2 <- indexDf2 %>% filter(tod >= startTime & tod <= endTime) %>% select(cl, tick) 

pXts <- xts(indexDf2$cl, indexDf2$tick)
names(pXts) <- c('IN')

pmXts1 <- pXts[paste0("T", pmStart, "/T", pmStart),]
index(pmXts1) <- as.Date(index(pmXts1))

pmXts21 <- pXts[paste0("T", pmEnd1, "/T", pmEnd1),]
pmXts22 <- pXts[paste0("T", pmEnd2, "/T", pmEnd2),]

index(pmXts21) <- as.Date(index(pmXts21))
index(pmXts22) <- as.Date(index(pmXts22))

pmXts2 <- merge(pmXts21, pmXts22)
pmClose <- ifelse(is.na(pmXts2[,2]), pmXts2[,1], pmXts2[,2])

pmXts2 <- xts(pmClose[,1], index(pmXts2))

pmRet <- pmXts2/pmXts1 -1

##### ignoring gaps

for(amI in 1:length(amEnd)){
	amXts1 <- pXts[paste0("T", amStart, "/T", amStart),]
	amXts2 <- pXts[paste0("T", amEnd[amI], "/T", amEnd[amI]),]

	index(amXts1) <- as.Date(index(amXts1))
	index(amXts2) <- as.Date(index(amXts2))

	amRet <- amXts2/amXts1 - 1
	
	grossRet <- ifelse(amRet > 0, pmRet, -pmRet)	
	gRetAnn <- as.numeric(Return.annualized(grossRet))

	stratRets <- merge(amRet, pmRet)
	names(stratRets) <- c("AM", "PM")

	p1 <- ggplot(as.data.frame(stratRets), aes(x=AM*100, y=PM*100)) +
		theme_economist() +
		geom_point() +
		scale_color_viridis_d() +
		labs(x = "Open (%)", y="Close (%)", fill="", color="", size="", 
			title=sprintf("Open (%s/%s) vs. Close Returns (%s/%s)", amStart, amEnd[amI], pmStart, pmEnd2)) +
		annotate("text", x = -Inf, y = Inf, label = paste0("Gross Returns: ", round(gRetAnn*100, 2), '%'), vjust = 1, hjust = 0) +
		xlim(-5, 5)
		
	

	##### considering gaps

	amXts1g <- merge(amXts1, stats::lag(pmXts2, 1))

	amRet <- amXts2/amXts1g[,2] - 1

	grossRet <- ifelse(amRet > 0, pmRet, -pmRet)	
	gRetAnn <- as.numeric(Return.annualized(grossRet))
	
	stratRets <- merge(amRet, pmRet)
	names(stratRets) <- c("AM", "PM")

	p2 <- ggplot(as.data.frame(stratRets), aes(x=AM*100, y=PM*100)) +
		theme_economist() +
		geom_point() +
		scale_color_viridis_d() +
		labs(x = "Open (%)", y="Close (%)", fill="", color="", size="", 
			title=sprintf("Open w/gap (%s/%s) vs. Close Returns (%s/%s)", amStart, amEnd[amI], pmStart, pmEnd2)) +
		annotate("text", x = -Inf, y = Inf, label = paste0("Gross Returns: ", round(gRetAnn*100, 2), '%'), vjust = 1, hjust = 0)+
		xlim(-5, 5)
		
	(p1 / p2) + 
		plot_layout(axes = "collect") + 
		plot_annotation(title = indexName, 
			subtitle = sprintf("[%s:%s]", first(index(stratRets)), last(index(stratRets))),
			caption='@StockViz') 
			
	ggsave(sprintf("%s/simple.open-close.%s.%s.png", reportPath, indexName, gsub(':', '', amEnd[amI])), width=12, height=12, units="in")
		
}
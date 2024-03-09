library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')

library('lubridate')
library('tidyverse')
library('ggthemes')
library('viridis')

reportPath<-"."

pdf(NULL)

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

baseDate <- as.Date("1990-01-01", tz='UTC')

masterTs <- dbGetQuery(pgCon, "select max(time_stamp) from zd_master")[[1]]
mcxTickers <- dbGetQuery(pgCon, "select distinct name from zd_master where exch = 'MCX' and time_stamp = $1", params=list(masterTs))[,1]

volDf <- dbGetQuery(pgCon, sprintf("select symbol, v, time_stamp, tick_stamp from zd_index_bars where symbol in ('%s')", paste(mcxTickers, collapse="','")))

volDf$tick <- as.POSIXct(baseDate + seconds(volDf$tick_stamp))
volDf$tod <- hms(format(volDf$tick, "%H:%M:%S"))


volumeByMinute <- volDf %>% mutate(hod = hour(tod), mod = hour(tod)*100+minute(tod)) %>% group_by(hod, mod) %>% summarize(volume = sum(v)) %>% arrange(mod)

volumeByMinute %>% {
ggplot(., aes(x=mod, y=volume, fill=factor(hod))) +
	theme_economist() +
	geom_bar(stat='identity', position=position_dodge2()) +
	scale_fill_viridis_d() +
	guides(fill='none') +
	scale_x_continuous(breaks = 100*seq(min(.$hod), max(.$hod))) +
	labs(x='time of day', y='volume', title='MCX Futures Volume', subtitle=sprintf("%s:%s", min(volDf$time_stamp), max(volDf$time_stamp)), caption="@StockViz")
}

ggsave(sprintf("%s/basic.volume-by-minute.png", reportPath), width=12, height=6, units="in")

volDf %>% mutate(hod = 100*hour(tod)) %>% group_by(symbol, hod) %>% summarize(volume = sum(v)) %>% {
	ggplot(., aes(x=factor(hod), y=volume, fill=factor(symbol, levels=unique(symbol)))) +
		theme_economist() +
		theme(legend.text=element_text(size=5)) +
		geom_bar(stat='identity', position=position_dodge2()) +
		scale_fill_viridis_d() +
		labs(x='time of day', y='volume', fill='', title='MCX Futures Volume', subtitle=sprintf("%s:%s", min(volDf$time_stamp), max(volDf$time_stamp)), caption="@StockViz")
}

ggsave(sprintf("%s/basic.volume-by-hour-ticker.png", reportPath), width=12, height=6, units="in")

volDf %>% group_by(symbol) %>% summarize(volume=sum(v)) %>% mutate(symbol = factor(symbol, levels=unique(symbol))) %>% {
	ggplot(., aes(x=symbol, y=volume, fill=symbol)) +
		theme_economist() +
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		geom_bar(stat='identity', position=position_dodge2()) +
		scale_fill_viridis_d() +
		guides(fill='none') +
		labs(x='', y='volume', fill='ticker', title='MCX Futures Volume', subtitle=sprintf("%s:%s", min(volDf$time_stamp), max(volDf$time_stamp)), caption="@StockViz")
}

ggsave(sprintf("%s/basic.agg-volume-by-ticker.png", reportPath), width=12, height=6, units="in")
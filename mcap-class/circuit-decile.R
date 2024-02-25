library('RODBC')
library('quantmod')

library('lubridate')
library('scales')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate <- as.Date("2017-05-10")

decileConsts <- sqlQuery(lcon, sprintf("select * from DECILE_CONSTITUENTS where time_stamp >= '%s'", startDate)) #based on avg. ff mkt cap on every nifty expiry
decileDates <- sort(unique(decileConsts$TIME_STAMP))

cktHits <- NULL
upperCkt <- NULL
lowerCkt <- NULL

for(i in 2:length(decileDates)){
	stDate <- decileDates[i-1]
	edDate <- decileDates[i]
	
	symSet <- decileConsts %>% filter(TIME_STAMP == stDate) %>% select(SYMBOL) %>% as.vector()
	symSet <- symSet$SYMBOL

	upCkt <- sqlQuery(lcon, sprintf("select px.TIME_STAMP, px.SYMBOL from PX_HISTORY px, EQUITY_MISC_INFO ei
									where ei.symbol = px.symbol
									and ei.time_stamp = px.time_stamp
									and px.series in ('EQ', 'BE')
									and px.time_stamp >= '%s' and px.time_stamp <= '%s'
									and px.px_close >= 0.99*ei.upper_px_band
									and px.symbol in ('%s')", stDate, edDate, paste(symSet, collapse="','")))
									
	loCkt <- sqlQuery(lcon, sprintf("select px.TIME_STAMP, px.SYMBOL from PX_HISTORY px, EQUITY_MISC_INFO ei
									where ei.symbol = px.symbol
									and ei.time_stamp = px.time_stamp
									and px.series in ('EQ', 'BE')
									and px.time_stamp >= '%s' and px.time_stamp <= '%s'
									and px.px_close <= 1.01*ei.lower_px_band
									and px.symbol in ('%s')", stDate, edDate, paste(symSet, collapse="','")))
			
	upperCkt <- bind_rows(upperCkt, tibble(upCkt))
	lowerCkt <- bind_rows(lowerCkt, tibble(loCkt))
	
	cktHits <- bind_rows(cktHits,
		upperCkt %>% select(SYMBOL) %>% distinct(SYMBOL) %>% inner_join(decileConsts %>% filter(TIME_STAMP == stDate), by='SYMBOL') %>% select(SYMBOL, DECILE) %>% mutate(TIME_STAMP = stDate, DIR = "UP"),
		lowerCkt %>% select(SYMBOL) %>% distinct(SYMBOL) %>% inner_join(decileConsts %>% filter(TIME_STAMP == stDate), by='SYMBOL') %>% select(SYMBOL, DECILE) %>% mutate(TIME_STAMP = stDate, DIR = "DN"))
}

toPlot <- cktHits %>% filter(DECILE < 10) %>% mutate(CLASS = ifelse(DECILE %in% c(0, 1, 2), 'LARGE', 
															 ifelse(DECILE %in% c(3, 4), "MID", 
															 ifelse(DECILE %in% c(5, 6), "SML", "XSML")))) %>% group_by(TIME_STAMP, CLASS, DIR) %>% summarize(CTR = n()) %>% ungroup()

ggplot(toPlot, aes(x=TIME_STAMP, y=CTR, color=factor(CLASS), shape=factor(DIR))) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	geom_point(size=3) +
	scale_x_date(breaks = "6 months", date_labels="%Y-%b") +
	scale_color_viridis_d() +
	labs(x='', y='count', color='mkt cap decile', shape='circuit', title="Stocks hitting their Circuit Limits by Market-cap Decile", subtitle=sprintf("%s:%s", min(toPlot$TIME_STAMP), max(toPlot$TIME_STAMP)), caption = "@StockViz")
	
ggsave(sprintf("%s/circuits-mkt-cap.decile.png", reportPath), width=16, height=8, units="in")			
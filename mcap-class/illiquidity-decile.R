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

decileConsts <- sqlQuery(lcon, "select * from DECILE_CONSTITUENTS") #based on avg. ff mkt cap on every nifty expiry
decileDates <- sort(unique(decileConsts$TIME_STAMP))

medIlliq <- NULL
for(i in 2:length(decileDates)){
	stDate <- decileDates[i-1]
	edDate <- decileDates[i]
	
	symSet <- decileConsts %>% filter(TIME_STAMP == stDate) %>% select(SYMBOL) %>% as.vector()
	symSet <- symSet$SYMBOL
	
	illiqDf <- sqlQuery(lcon, sprintf("select r.symbol SYMBOL, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
											where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp < '%s'
											and r.symbol = p.symbol and r.time_stamp = p.time_stamp
											and p.series in ('EQ', 'BE')
											group by r.symbol", paste(symSet, collapse="','"), stDate, edDate))
										
	illiqDecile <- illiqDf %>% inner_join(decileConsts %>% filter(TIME_STAMP == stDate), by='SYMBOL') %>% select(SYMBOL, DECILE, ILLIQ)
	ggplot(illiqDecile, aes(x = factor(DECILE), y = ILLIQ*100000, factor(DECILE))) +
		theme_economist() +
		geom_violin() +
		stat_summary(fun=median, geom="point", size=2, color="red") +
		scale_color_viridis_d() +
		scale_y_continuous(trans='log', labels = label_number(accuracy = 0.01)) +
		labs(x='', y='log illiquidity', color='', fill='', title="Median Illiquidity by Market-cap Decile", subtitle=sprintf("%s:%s", stDate, edDate), caption = "@StockViz")
		
	ggsave(sprintf("%s/illiquidity-mkt-cap.decile.%s.png", reportPath, stDate), width=16, height=8, units="in")	
	
	medIlliq <- bind_rows(medIlliq, illiqDecile %>% group_by(DECILE) %>% summarize(IL_M = median(ILLIQ*100000)) %>% mutate(TIME_STAMP = stDate)) 
}

ggplot(medIlliq, aes(x = factor(TIME_STAMP), y = IL_M, color = factor(DECILE))) +
		theme_economist() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		geom_point(size=3) +
		scale_color_viridis_d() +
		scale_y_continuous(trans='log', labels = label_number(accuracy = 0.01)) +
		labs(x='', y='log illiquidity', color='', fill='', title="Median Illiquidity by Market-cap Decile", caption = "@StockViz")
		
ggsave(sprintf("%s/illiquidity-mkt-cap.decile.median.png", reportPath), width=16, height=8, units="in")	
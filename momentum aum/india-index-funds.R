library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('lubridate')
library('tidyverse')
library('ggpmisc')
library('ggthemes')
library('viridis')


options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

largeCapMomDf <- sqlQuery(lcon, "select * from MF_SCHEMEWISE_AUM where SCHEME_NAME like '%200%mom%'")
midCapMomDf <- sqlQuery(lcon, "select * from MF_SCHEMEWISE_AUM where SCHEME_NAME like '%midcap%mom%'")

largeCapMomDf$FUND_HOUSE <- unlist(lapply(strsplit(largeCapMomDf$SCHEME_NAME, ' '), function(X) as.character(X[1])))
midCapMomDf$FUND_HOUSE <- unlist(lapply(strsplit(midCapMomDf$SCHEME_NAME, ' '), function(X) as.character(X[1])))

largeCapMomDf %>% group_by(PERIOD, FUND_HOUSE) %>% summarize(AUM = sum(AVG_AUM_WO_FOFD + AVG_AUM_FOFD)) %>% 
	ungroup() %>% arrange(PERIOD) %>% {
		ggplot(., aes(x=PERIOD, y=AUM, fill=FUND_HOUSE, group=FUND_HOUSE)) +
		geom_bar(stat="identity") +
		theme_economist() +
		scale_fill_viridis_d() +
		scale_x_date(breaks = '3 months', date_labels='%Y-%b') +
		labs(x = "", y="AUM (Cr.)", fill="", color="", title="Nifty 200 Momentum 30 Index Funds", subtitle = sprintf("%s/%s", strftime(min(.$PERIOD), '%Y-%b'), strftime(max(.$PERIOD), '%Y-%b'))) +
		annotate("text", x=min(.$PERIOD), y=max(.$AUM), label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	}
	
ggsave(sprintf("%s/india-largecap-momentum-index-funds.png", reportPath), width=12, height=6, units="in")	
	
midCapMomDf %>% group_by(PERIOD, FUND_HOUSE) %>% summarize(AUM = sum(AVG_AUM_WO_FOFD + AVG_AUM_FOFD)) %>% 
	ungroup() %>% arrange(PERIOD) %>% {
		ggplot(., aes(x=PERIOD, y=AUM, fill=FUND_HOUSE, group=FUND_HOUSE)) +
		geom_bar(stat="identity") +
		theme_economist() +
		scale_fill_viridis_d() +
		scale_x_date(breaks = '3 months', date_labels='%Y-%b') +
		labs(x = "", y="AUM (Cr.)", fill="", color="", title="Nifty Midcap150 Momentum 50 Index Funds", subtitle = sprintf("%s/%s", strftime(min(.$PERIOD), '%Y-%b'), strftime(max(.$PERIOD), '%Y-%b'))) +
		annotate("text", x=min(.$PERIOD), y=max(.$AUM), label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	}	
	
ggsave(sprintf("%s/india-midcap-momentum-index-funds.png", reportPath), width=12, height=6, units="in")	
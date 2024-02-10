library('RODBC')
library('quantmod')

library('lubridate')
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

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

iDf <- sqlQuery(lcon, "select period, sum(AVG_AUM_WO_FOFD + AVG_AUM_FOFD) from MF_SCHEMEWISE_AUM where SCHEME_NAME like '%index%' or SCHEME_NAME like '%etf%' group by PERIOD order by PERIOD")
allDf <- sqlQuery(lcon, "select period, sum(AVG_AUM_WO_FOFD + AVG_AUM_FOFD) from MF_SCHEMEWISE_AUM group by PERIOD order by PERIOD")


aumXts <- merge(xts(iDf[,2], iDf[,1]), xts(allDf[,2], allDf[,1]))
names(aumXts) <- c('INDEX', 'ALL')
aumXts$INDEX_PCT <- aumXts$INDEX/aumXts$ALL

aumDf <- data.frame(aumXts[, c('INDEX', 'ALL')])
aumDf$T <- index(aumXts)

toPlot <- melt(aumDf, id='T')

ggplot(toPlot, aes(x=T, y=value/100000, fill=variable)) +
	theme_economist() +
	geom_bar(stat='identity', position='dodge2') +
	scale_fill_viridis_d() +
	scale_x_date(breaks = "1 year", date_labels="%Y") +
	labs(x = "", y="AUM (Rs. '000 crores)", fill="", color="", size="", title="Fund Assets Under Management", subtitle=sprintf("[%s:%s]", first(index(aumXts)), last(index(aumXts))), caption='@StockViz') 

ggsave(sprintf("%s/aum-growth.png", reportPath), width=12, height=6, units="in")
	
aumPct <- data.frame(100*aumXts$INDEX_PCT)
aumPct$T <- index(aumXts)
	
ggplot(aumPct, aes(x=T, y=INDEX_PCT, fill=T)) +
	theme_economist() +
	geom_bar(stat='identity', position='dodge2') +
	scale_fill_viridis() +
	scale_x_date(breaks = "1 year", date_labels="%Y") +
	guides(fill='none') +
	labs(x = "", y="Passive AUM (%)", fill="", color="", size="", title="Passive Funds Under Management", subtitle=sprintf("[%s:%s]", first(index(aumXts)), last(index(aumXts))), caption='@StockViz') 	
	
ggsave(sprintf("%s/aum-pct.png", reportPath), width=12, height=6, units="in")	
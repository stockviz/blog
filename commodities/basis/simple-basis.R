library('RODBC')
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

comFut <- "MCXBULLDEX"
comIndex <- "MCXBULLDEX"

comFutDf <- sqlQuery(lcon, sprintf("select expiry_series, expiry, time_stamp, px_close from BHAV_COM_MCX where contract = '%s' and otype='XX'", comFut))
comIndDf <- sqlQuery(lcon, sprintf("select time_stamp, c from BHAV_INDEX_MCX where index_code = '%s'", comIndex))

comBasis <- comFutDf %>% inner_join(comIndDf, by='time_stamp') %>% mutate(dte = as.numeric(expiry - time_stamp), basis = 100*(px_close/c-1))

comBasis %>% select(expiry_series, dte, basis) %>% 
	ggplot(aes(x=factor(dte), y=basis, fill=factor(expiry_series))) +
		theme_economist() +
		geom_violin(scale='width') +
		stat_summary(fun=median, geom="point", size=2, color="red") +
		scale_fill_viridis_d() +
		guides(color='none', fill='none') +
		labs(x = "days to expiry", y="basis (%)", fill="series", title="MCX iCOMDEX Bullion Index-Futures Basis", subtitle=sprintf("[%s:%s]", min(comBasis$time_stamp), max(comBasis$time_stamp)), caption='@StockViz') 
		
ggsave(sprintf("%s/%s.basis.png", reportPath, comFut), width=18, height=6, units="in")		

indexFut <- "NIFTY"
indexName <- "NIFTY 50"
startDate <- as.Date("2020-05-01")

inFutDf <- sqlQuery(lcon, sprintf("select expiry_dt, time_stamp, px_close from bhav_eq_fut where symbol='%s' and time_stamp >= '%s'", indexFut, startDate))
indDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close as c from bhav_index where index_name='%s' and time_stamp >= '%s'", indexName, startDate))


inBasis <- inFutDf %>% inner_join(indDf, by='time_stamp') %>% mutate(dte = as.numeric(expiry_dt - time_stamp), basis = 100*(px_close/c-1)) %>% group_by(time_stamp) %>% mutate(expiry_series = rank(dte) - 1)

inBasis %>% select(expiry_series, dte, basis) %>% 
	ggplot(aes(x=factor(dte), y=basis, fill=factor(expiry_series))) +
		theme_economist() +
		geom_violin(scale='width') +
		stat_summary(fun=median, geom="point", size=2, color="red") +
		scale_fill_viridis_d() +
		guides(color='none', fill='none') +
		labs(x = "days to expiry", y="basis (%)", fill="series", title="NIFTY 50 Index-Futures Basis", subtitle=sprintf("[%s:%s]", min(inBasis$time_stamp), max(inBasis$time_stamp)), caption='@StockViz') 
		
ggsave(sprintf("%s/%s.basis.png", reportPath, indexFut), width=18, height=6, units="in")		

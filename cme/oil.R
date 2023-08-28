library('tidyverse')

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('extrafont')
library('lubridate')
library('ggthemes')
library('ggpubr')
library('reshape2')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/misc.R")
source("D:/StockViz/public/blog/common/msci.R")
source("D:/StockViz/public/blog/common/plot.common.R")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

#######################
contract <- 'CL'

cmeBhav <- sqlQuery(lcon, sprintf("select * from CME_BHAV where PRODUCT_SYMBOL = '%s' order by TIME_STAMP", contract))

timeStamps <- sort(unique(cmeBhav$TIME_STAMP))

top2 <- cmeBhav %>% mutate(C_YM = CONTRACT_YEAR*100+CONTRACT_MONTH) %>% group_by(TIME_STAMP) %>% arrange(C_YM) %>% slice(seq_len(2)) %>% 
					mutate(YM = paste0('N_', row_number())) %>% select(TIME_STAMP, YM, C_YM) %>% spread(YM, C_YM) %>% ungroup()
					
tops1 <- cmeBhav %>% mutate(C_YM = CONTRACT_YEAR*100+CONTRACT_MONTH) %>% 
			inner_join(top2) %>%
			filter(C_YM == N_1) %>%
			select(TIME_STAMP, N_1, SETTLE)
			
tops2 <- cmeBhav %>% mutate(C_YM = CONTRACT_YEAR*100+CONTRACT_MONTH) %>% 
			inner_join(top2) %>%
			filter(C_YM == N_2) %>%
			select(TIME_STAMP, N_2, SETTLE)

top2Settle <- tops1 %>% inner_join(tops2, by=c('TIME_STAMP'))


#######################

vixBhav <- sqlQuery(lconUs2, "select * from BHAV_CBOE_VIX_FUT where CONTRACT_WEEK=0 order by TIME_STAMP")

top2 <- vixBhav %>% group_by(TIME_STAMP) %>% arrange(EXPIRY) %>% slice(seq_len(8)) %>% 
					mutate(YM = paste0('N_', row_number())) %>% select(TIME_STAMP, YM, PX_SETTLE) %>% filter(PX_SETTLE > 0) %>% pivot_wider(names_from=YM, values_from = PX_SETTLE) %>% ungroup()
					

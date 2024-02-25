library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

baseDate <- as.Date("1990-01-01", tz='UTC')

startTime <- hms("09:15:00")
endTime <- hms("15:30:00")


reportPath <- "."
source("d:/stockviz/r/config.r")

pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

allDepthDf <- dbGetQuery(pgCon, "select zm.expiry, zm.strike, zm.inst_type, zds.*, date '1990-01-01' + zds.tick_stamp * interval '1 second' as ts from zd_depth_stream zds, zd_master zm 
									where zm.inst_token = zds.instrument_token and zm.time_stamp = DATE(date '1990-01-01' + zds.tick_stamp * interval '1 second')
									and zm.inst_type in ('CE', 'PE') and zm.name='MIDCPNIFTY'")

allDepthDf2 <- allDepthDf
allDepthDf2$tod <- hms(format(allDepthDf2$ts, "%H:%M:%S"))
allDepthDf2 <- allDepthDf2 %>% filter(tod >= startTime & tod <= endTime) 

quoteDf2 <- dbGetQuery(pgCon, "select zm.expiry, zm.strike, zm.inst_type, zds.*, date '1990-01-01' + zds.tick_stamp * interval '1 second' as ts from zd_quotes_stream zds, zd_master zm 
							where zm.inst_token = zds.instrument_token and zm.time_stamp = DATE(date '1990-01-01' + zds.tick_stamp * interval '1 second')
							and zm.inst_type in ('CE', 'PE') and zm.name='MIDCPNIFTY'")
							
quoteDf2$tod <- hms(format(quoteDf2$ts, "%H:%M:%S"))
quoteDf2 <- quoteDf2 %>% filter(tod >= startTime & tod <= endTime) 

startDate <- as.Date(min(allDepthDf2$ts))
startDate <- as.Date(min(quoteDf2$ts))

indexDf <- dbGetQuery(pgCon, "select c as cl, tick_stamp from zd_index_bars where symbol='MIDCPNIFTY' and time_stamp >= $1", params=list(startDate))
indexDf$tick <- as.POSIXct(baseDate + seconds(indexDf$tick_stamp))
indexDf$tod <- hms(format(indexDf$tick, "%H:%M:%S"))

indexDf2 <- indexDf %>% filter(tod >= startTime & tod <= endTime) %>% select(cl, tick, tick_stamp) 

################################################################################################################################################################################################################################
#last traded time lags

allData0 <- quoteDf2 %>% full_join(indexDf2, by='tick_stamp') %>% arrange(tick_stamp) 
allData0 <- allData0 %>% arrange(tick_stamp) %>% fill(cl, .direction = 'up') %>% drop_na(strike)
							
allSubCe <- allData0 %>% filter(inst_type == 'CE') %>% group_by(tick_stamp) %>% mutate(exp_min = min(expiry), s_diff = abs(strike-cl)/cl) %>% ungroup() %>% filter(expiry == exp_min)
allSubPe <- allData0 %>% filter(inst_type == 'PE') %>% group_by(tick_stamp) %>% mutate(exp_min = min(expiry), s_diff = abs(strike-cl)/cl) %>% ungroup() %>% filter(expiry == exp_min)

n0ce <- allSubCe %>% filter(s_diff < 0.01)
n0pe <- allSubPe %>% filter(s_diff < 0.01)

n0ce2 <- allSubCe %>% filter(s_diff >= 0.01 & s_diff < 0.02)
n0pe2 <- allSubPe %>% filter(s_diff >= 0.01 & s_diff < 0.02)

n0ce3 <- allSubCe %>% filter(s_diff >= 0.02 & s_diff < 0.03)
n0pe3 <- allSubPe %>% filter(s_diff >= 0.02 & s_diff < 0.03)

n0ce4 <- allSubCe %>% filter(s_diff >= 0.03 & s_diff < 0.04)
n0pe4 <- allSubPe %>% filter(s_diff >= 0.03 & s_diff < 0.04)
							
n1ce <- n0ce %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							
n1pe <- n0pe %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							

n1ce2 <- n0ce2 %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							
n1pe2 <- n0pe2 %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							

n1ce3 <- n0ce3 %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							
n1pe3 <- n0pe3 %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							

n1ce4 <- n0ce4 %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							
n1pe4 <- n0pe4 %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							


n1ce <- n1ce %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe <- n1pe %>% filter(tod >= startCutoff & tod <= endCutOff)

n1ce2 <- n1ce2 %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe2 <- n1pe2 %>% filter(tod >= startCutoff & tod <= endCutOff)

n1ce3 <- n1ce3 %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe3 <- n1pe3 %>% filter(tod >= startCutoff & tod <= endCutOff)

n1ce4 <- n1ce4 %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe4 <- n1pe4 %>% filter(tod >= startCutoff & tod <= endCutOff)


topBoTod <- bind_rows(n1ce %>% mutate(inst_type='CE', MONEY = 0), 
					n1ce2 %>% mutate(inst_type='CE', MONEY = 1),
					n1ce3 %>% mutate(inst_type='CE', MONEY = 2),
					n1ce4 %>% mutate(inst_type='CE', MONEY = 3)) %>% arrange(tod)

ggplot(topBoTod, aes(x=tod, y=ltd, color=factor(MONEY))) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_point() +
	scale_y_log10() +
	scale_x_time(breaks = scales::breaks_width("15 min")) +
	scale_color_viridis_d() +
	labs(x="time of day", y="lag (secs) log scale", color='moneyness', title = "MIDCPNIFTY call options ltt lag", subtitle = "closest expiry") 
	
ggsave(sprintf("%s/liquidity.MIDCPNIFTY.ce.closest-expiry.ltt.png", reportPath), width=12, height=7, units="in")	

topBoTod <- bind_rows(n1pe %>% mutate(inst_type='PE', MONEY = 0),
					n1pe2 %>% mutate(inst_type='PE', MONEY = 1),
					n1pe3 %>% mutate(inst_type='PE', MONEY = 2),
					n1pe4 %>% mutate(inst_type='PE', MONEY = 3)) %>% arrange(tod)

ggplot(topBoTod, aes(x=tod, y=ltd, color=factor(MONEY))) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_point() +
	scale_y_log10() +
	scale_x_time(breaks = scales::breaks_width("15 min")) +
	scale_color_viridis_d() +
	labs(x="time of day", y="lag (secs) log scale", color='moneyness', title = "MIDCPNIFTY put options ltt lag", subtitle = "closest expiry") 
	
ggsave(sprintf("%s/liquidity.MIDCPNIFTY.pe.closest-expiry.ltt.png", reportPath), width=12, height=7, units="in")	
					
### ltt lag by time; next expiry

allSubCe <- allData0 %>% filter(inst_type == 'CE') %>% group_by(tick_stamp) %>% mutate(exp_min = min(expiry), s_diff = abs(strike-cl)/cl) %>% filter(expiry > exp_min) %>% mutate(exp_min=min(expiry)) %>% ungroup() %>% filter(expiry == exp_min)

allSubPe <- allData0 %>% filter(inst_type == 'PE') %>% group_by(tick_stamp) %>% mutate(exp_min = min(expiry), s_diff = abs(strike-cl)/cl) %>% filter(expiry > exp_min) %>% mutate(exp_min=min(expiry)) %>% ungroup() %>% filter(expiry == exp_min)
					
n0ce <- allSubCe %>% filter(s_diff < 0.01)
n0pe <- allSubPe %>% filter(s_diff < 0.01)

n0ce2 <- allSubCe %>% filter(s_diff >= 0.01 & s_diff < 0.02)
n0pe2 <- allSubPe %>% filter(s_diff >= 0.01 & s_diff < 0.02)

n0ce3 <- allSubCe %>% filter(s_diff >= 0.02 & s_diff < 0.03)
n0pe3 <- allSubPe %>% filter(s_diff >= 0.02 & s_diff < 0.03)

n0ce4 <- allSubCe %>% filter(s_diff >= 0.03 & s_diff < 0.04)
n0pe4 <- allSubPe %>% filter(s_diff >= 0.03 & s_diff < 0.04)
							
n1ce <- n0ce %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							
n1pe <- n0pe %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							

n1ce2 <- n0ce2 %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							
n1pe2 <- n0pe2 %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							

n1ce3 <- n0ce3 %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							
n1pe3 <- n0pe3 %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							

n1ce4 <- n0ce4 %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							
n1pe4 <- n0pe4 %>% group_by(tod) %>% summarize(ltd = median(as.numeric(difftime(ts, last_trade_time, units='secs')))) %>% select(tod, ltd)  							


n1ce <- n1ce %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe <- n1pe %>% filter(tod >= startCutoff & tod <= endCutOff)

n1ce2 <- n1ce2 %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe2 <- n1pe2 %>% filter(tod >= startCutoff & tod <= endCutOff)

n1ce3 <- n1ce3 %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe3 <- n1pe3 %>% filter(tod >= startCutoff & tod <= endCutOff)

n1ce4 <- n1ce4 %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe4 <- n1pe4 %>% filter(tod >= startCutoff & tod <= endCutOff)


topBoTod <- bind_rows(n1ce %>% mutate(inst_type='CE', MONEY = 0), 
					n1ce2 %>% mutate(inst_type='CE', MONEY = 1),
					n1ce3 %>% mutate(inst_type='CE', MONEY = 2),
					n1ce4 %>% mutate(inst_type='CE', MONEY = 3)) %>% arrange(tod)

ggplot(topBoTod, aes(x=tod, y=ltd, color=factor(MONEY))) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_point() +
	scale_y_log10() +
	scale_x_time(breaks = scales::breaks_width("15 min")) +
	scale_color_viridis_d() +
	labs(x="time of day", y="lag (secs) log scale", color='moneyness', title = "MIDCPNIFTY call options ltt lag", subtitle = "next expiry") 
	
ggsave(sprintf("%s/liquidity.MIDCPNIFTY.ce.next-expiry.ltt.png", reportPath), width=12, height=7, units="in")	

topBoTod <- bind_rows(n1pe %>% mutate(inst_type='PE', MONEY = 0),
					n1pe2 %>% mutate(inst_type='PE', MONEY = 1),
					n1pe3 %>% mutate(inst_type='PE', MONEY = 2),
					n1pe4 %>% mutate(inst_type='PE', MONEY = 3)) %>% arrange(tod)

ggplot(topBoTod, aes(x=tod, y=ltd, color=factor(MONEY))) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_point() +
	scale_y_log10() +
	scale_x_time(breaks = scales::breaks_width("15 min")) +
	scale_color_viridis_d() +
	labs(x="time of day", y="lag (secs) log scale", color='moneyness', title = "MIDCPNIFTY put options ltt lag", subtitle = "next expiry") 
	
ggsave(sprintf("%s/liquidity.MIDCPNIFTY.pe.next-expiry.ltt.png", reportPath), width=12, height=7, units="in")	

################################################################################################################################################################################################################################
#b/o spreads


allData0 <- allDepthDf2 %>% filter(depth_seq == 0) %>% select(-c(instrument_token, depth_seq)) %>% full_join(indexDf2, by='tick_stamp') %>% arrange(tick_stamp)
allData0 <- allData0 %>% arrange(tick_stamp) %>% fill(cl, .direction = 'up') %>% drop_na(strike)

startCutoff <- hms("09:17:00")
endCutOff <- hms("15:28:00")

### top b/o by time; closest expiry

allSubCe <- allData0 %>% filter(inst_type == 'CE') %>% group_by(tick_stamp) %>% mutate(exp_min = min(expiry), s_diff = abs(strike-cl)/cl) %>% ungroup() %>% filter(expiry == exp_min)
allSubPe <- allData0 %>% filter(inst_type == 'PE') %>% group_by(tick_stamp) %>% mutate(exp_min = min(expiry), s_diff = abs(strike-cl)/cl) %>% ungroup() %>% filter(expiry == exp_min)

n0ce <- allSubCe %>% filter(s_diff < 0.01)
n0pe <- allSubPe %>% filter(s_diff < 0.01)

n0ce2 <- allSubCe %>% filter(s_diff >= 0.01 & s_diff < 0.02)
n0pe2 <- allSubPe %>% filter(s_diff >= 0.01 & s_diff < 0.02)

n0ce3 <- allSubCe %>% filter(s_diff >= 0.02 & s_diff < 0.03)
n0pe3 <- allSubPe %>% filter(s_diff >= 0.02 & s_diff < 0.03)

n0ce4 <- allSubCe %>% filter(s_diff >= 0.03 & s_diff < 0.04)
n0pe4 <- allSubPe %>% filter(s_diff >= 0.03 & s_diff < 0.04)

n1ce <- n0ce %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo)  
n1pe <- n0pe %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo) 

n1ce2 <- n0ce2 %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo)  
n1pe2 <- n0pe2 %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo) 

n1ce3 <- n0ce3 %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo)  
n1pe3 <- n0pe3 %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo) 

n1ce4 <- n0ce4 %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo)  
n1pe4 <- n0pe4 %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo) 

n1ce <- n1ce %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe <- n1pe %>% filter(tod >= startCutoff & tod <= endCutOff)

n1ce2 <- n1ce2 %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe2 <- n1pe2 %>% filter(tod >= startCutoff & tod <= endCutOff)

n1ce3 <- n1ce3 %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe3 <- n1pe3 %>% filter(tod >= startCutoff & tod <= endCutOff)

n1ce4 <- n1ce4 %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe4 <- n1pe4 %>% filter(tod >= startCutoff & tod <= endCutOff)


topBoTod <- bind_rows(n1ce %>% mutate(inst_type='CE', MONEY = 0), 
					n1ce2 %>% mutate(inst_type='CE', MONEY = 1),
					n1ce3 %>% mutate(inst_type='CE', MONEY = 2),
					n1ce4 %>% mutate(inst_type='CE', MONEY = 3)) %>% arrange(tod)
					
ggplot(topBoTod, aes(x=tod, y=bo, color=factor(MONEY))) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_point() +
	scale_x_time(breaks = scales::breaks_width("15 min")) +
	scale_y_log10() +
	scale_color_viridis_d() +
	labs(x="time of day", y="bid/offer spread (%) log scale", color='moneyness', title = "MIDCPNIFTY call options median b/o", subtitle = "top of the book; closest expiry") 
	
ggsave(sprintf("%s/liquidity.MIDCPNIFTY.ce.closest-expiry.bo.png", reportPath), width=12, height=7, units="in")
					

topBoTod <- bind_rows(n1pe %>% mutate(inst_type='PE', MONEY = 0),
					n1pe2 %>% mutate(inst_type='PE', MONEY = 1),
					n1pe3 %>% mutate(inst_type='PE', MONEY = 2),
					n1pe4 %>% mutate(inst_type='PE', MONEY = 3)) %>% arrange(tod)

					
ggplot(topBoTod, aes(x=tod, y=bo, color=factor(MONEY))) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_point() +
	scale_x_time(breaks = scales::breaks_width("15 min")) +
	scale_y_log10() +
	scale_color_viridis_d() +
	labs(x="time of day", y="bid/offer spread (%) log scale", color='moneyness', title = "MIDCPNIFTY put options median b/o", subtitle = "top of the book; closest expiry") 
	
ggsave(sprintf("%s/liquidity.MIDCPNIFTY.pe.closest-expiry.bo.png", reportPath), width=12, height=7, units="in")


### top b/o by time; next expiry

allSubCe <- allData0 %>% filter(inst_type == 'CE') %>% group_by(tick_stamp) %>% mutate(exp_min = min(expiry), s_diff = abs(strike-cl)/cl) %>% filter(expiry > exp_min) %>% mutate(exp_min=min(expiry)) %>% ungroup() %>% filter(expiry == exp_min)

allSubPe <- allData0 %>% filter(inst_type == 'PE') %>% group_by(tick_stamp) %>% mutate(exp_min = min(expiry), s_diff = abs(strike-cl)/cl) %>% filter(expiry > exp_min) %>% mutate(exp_min=min(expiry)) %>% ungroup() %>% filter(expiry == exp_min)

n0ce <- allSubCe %>% filter(s_diff < 0.01)
n0pe <- allSubPe %>% filter(s_diff < 0.01)

n0ce2 <- allSubCe %>% filter(s_diff >= 0.01 & s_diff < 0.02)
n0pe2 <- allSubPe %>% filter(s_diff >= 0.01 & s_diff < 0.02)

n0ce3 <- allSubCe %>% filter(s_diff >= 0.02 & s_diff < 0.03)
n0pe3 <- allSubPe %>% filter(s_diff >= 0.02 & s_diff < 0.03)

n0ce4 <- allSubCe %>% filter(s_diff >= 0.03 & s_diff < 0.04)
n0pe4 <- allSubPe %>% filter(s_diff >= 0.03 & s_diff < 0.04)


n1ce <- n0ce %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo)  
n1pe <- n0pe %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo) 

n1ce2 <- n0ce2 %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo)  
n1pe2 <- n0pe2 %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo) 

n1ce3 <- n0ce3 %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo)  
n1pe3 <- n0pe3 %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo) 

n1ce4 <- n0ce4 %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo)  
n1pe4 <- n0pe4 %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo) 


startCutoff <- hms("09:17:00")
endCutOff <- hms("15:28:00")

n1ce <- n1ce %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe <- n1pe %>% filter(tod >= startCutoff & tod <= endCutOff)

n1ce2 <- n1ce2 %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe2 <- n1pe2 %>% filter(tod >= startCutoff & tod <= endCutOff)

n1ce3 <- n1ce3 %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe3 <- n1pe3 %>% filter(tod >= startCutoff & tod <= endCutOff)

n1ce4 <- n1ce4 %>% filter(tod >= startCutoff & tod <= endCutOff)
n1pe4 <- n1pe4 %>% filter(tod >= startCutoff & tod <= endCutOff)


topBoTod <- bind_rows(n1ce %>% mutate(inst_type='CE', MONEY = 0), 
					n1ce2 %>% mutate(inst_type='CE', MONEY = 1),
					n1ce3 %>% mutate(inst_type='CE', MONEY = 2),
					n1ce4 %>% mutate(inst_type='CE', MONEY = 3)) %>% arrange(tod)
					
ggplot(topBoTod, aes(x=tod, y=bo, color=factor(MONEY))) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_point() +
	scale_x_time(breaks = scales::breaks_width("15 min")) +
	scale_y_log10() +
	scale_color_viridis_d() +
	labs(x="time of day", y="bid/offer spread (%) log scale", color='moneyness', title = "MIDCPNIFTY call options median b/o", subtitle = "top of the book; next expiry") 
	
ggsave(sprintf("%s/liquidity.MIDCPNIFTY.ce.next-expiry.bo.png", reportPath), width=12, height=7, units="in")
					

topBoTod <- bind_rows(n1pe %>% mutate(inst_type='PE', MONEY = 0),
					n1pe2 %>% mutate(inst_type='PE', MONEY = 1),
					n1pe3 %>% mutate(inst_type='PE', MONEY = 2),
					n1pe4 %>% mutate(inst_type='PE', MONEY = 3)) %>% arrange(tod)

					
ggplot(topBoTod, aes(x=tod, y=bo, color=factor(MONEY))) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_point() +
	scale_x_time(breaks = scales::breaks_width("15 min")) +
	scale_y_log10() +
	scale_color_viridis_d() +
	labs(x="time of day", y="bid/offer spread (%) log scale", color='moneyness', title = "MIDCPNIFTY put options median b/o", subtitle = "top of the book; next expiry") 
	
ggsave(sprintf("%s/liquidity.MIDCPNIFTY.pe.next-expiry.bo.png", reportPath), width=12, height=7, units="in")

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

startTime <- hms("09:15:00")
endTime <- hms("15:30:00")


reportPath <- "."
source("d:/stockviz/r/config.r")

pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

allDepthDf <- dbGetQuery(pgCon, "select zds.*, date '1990-01-01' + zds.tick_stamp * interval '1 second' as ts, zm.expiry from zd_depth_stream zds, zd_master zm 
									where zm.inst_token = zds.instrument_token and zm.time_stamp = DATE(date '1990-01-01' + zds.tick_stamp * interval '1 second')")

allDepthDf2 <- allDepthDf
allDepthDf2$tod <- hms(format(allDepthDf2$ts, "%H:%M:%S"))
allDepthDf2 <- allDepthDf2 %>% filter(tod >= startTime & tod <= endTime) 


### top b/o by time; all expiries

topBoTod <- allDepthDf2 %>% filter(depth_seq == 0 & sell_px > 0 & buy_px > 0) %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo) %>% arrange(tod)

ggplot(topBoTod, aes(x=tod, y=bo)) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_point() +
	scale_x_time(breaks = scales::breaks_width("15 min")) +
	scale_y_log10() +
	labs(x="time of day", y="bid/offer spread (%) log-scale", title = "MIDCPNIFTY futures median b/o", subtitle = "top of the book; all expiries", caption = "@StockViz") 
	
ggsave(sprintf("%s/liquidity.MIDCPNIFTY.futures.all.bo.png", reportPath), width=12, height=7, units="in")


### top b/o by time; by expiries

topBoTodExp1 <- allDepthDf2 %>% mutate(date_stamp = as.Date(ts)) %>% group_by(date_stamp) %>% filter(expiry == min(expiry)) %>% 
	filter(depth_seq == 0 & sell_px > 0 & buy_px > 0) %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo) %>% arrange(tod)
	
topBoTodExp2 <- allDepthDf2 %>% mutate(date_stamp = as.Date(ts)) %>% group_by(date_stamp) %>% filter(expiry > min(expiry) & expiry < max(expiry)) %>% 
	filter(depth_seq == 0 & sell_px > 0 & buy_px > 0) %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo) %>% arrange(tod)

topBoTodExp3 <- allDepthDf2 %>% mutate(date_stamp = as.Date(ts)) %>% group_by(date_stamp) %>% filter(expiry == max(expiry)) %>% 
	filter(depth_seq == 0 & sell_px > 0 & buy_px > 0) %>% group_by(tod) %>% summarize(bo = median(100*(sell_px/buy_px-1))) %>% select(tod, bo) %>% arrange(tod)

topBoTodExp <- topBoTodExp1 %>% full_join(topBoTodExp2, by='tod', suffix=c('.c', '.m')) %>% full_join(topBoTodExp3, by='tod')

vColors <- viridis_pal()(3)
plotColors <- c("closest" = vColors[1], "next" = vColors[2], "farthest" = vColors[3])

ggplot(topBoTodExp, aes(x=tod)) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_point(aes(y=bo.c, color="closest")) +
	geom_point(aes(y=bo.m, color="next")) +
	geom_point(aes(y=bo, color="farthest")) +
	scale_color_manual(values = plotColors) +
	scale_x_time(breaks = scales::breaks_width("15 min")) +
	scale_y_log10() +
	labs(x="time of day", y="bid/offer spread (%) log-scale", color='expiries', title = "MIDCPNIFTY futures median b/o", subtitle = "top of the book; by expiry", caption = "@StockViz") 
	
ggsave(sprintf("%s/liquidity.MIDCPNIFTY.futures.bo.png", reportPath), width=12, height=7, units="in")

library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('scales')
library('viridis')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

startTime <- hms("09:15:00")
endTime <- hms("15:30:00")

reportPath <- "."
source("d:/stockviz/r/config.r")

symbol <- "HDFCBANK"
startDate <- as.Date("2018-01-01")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

resultDates <- sqlQuery(lcon, sprintf("select convert(date, BROADCAST_STAMP) bdt, min(convert(time, BROADCAST_STAMP)) bts
											from CORP_RESULTS_KEY_NSE where SYMBOL = '%s' and BROADCAST_STAMP >= '%s' 
											group by convert(date, BROADCAST_STAMP)
											order by bdt", symbol, startDate))
											
											
ggplot(resultDates, aes(x=factor(bdt), y=hms(gsub(".0000000", "", bts)))) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_point(size=5) +
	scale_y_time(breaks=date_breaks('1 hour')) +
	geom_hline(yintercept=startTime, linetype="dashed", color='darkgreen', linewidth=1) +
	geom_hline(yintercept=endTime, linetype="dashed", color='darkred', linewidth=1) +
	labs(x='date', y='broadcast time', title = sprintf("%s Results Broadcast Time", symbol))

ggsave(sprintf("%s/%s.broadcast-time.png", reportPath, symbol), width=12, height=6, units="in")	

tradeDays <- sqlQuery(lcon, sprintf("select distinct time_stamp from px_history where symbol = '%s' and time_stamp >= '%s' order by time_stamp", symbol, startDate))[,1]

r1days <- resultDates %>% group_by(bdt) %>% mutate(rm1 = max(tradeDays[tradeDays < bdt]), rp1 = min(tradeDays[tradeDays > bdt])) 


############################################

#intraday exam

minTs <- dbGetQuery(pgCon, "select min(time_stamp) from zd_index_bars where symbol=$1", params=list(symbol))[[1]]

getIntradayXts <- function(asof){
	pxDf <- dbGetQuery(pgCon, "select *, date '1990-01-01' + tick_stamp * interval '1 second' as ts from zd_index_bars where symbol=$1 and time_stamp=$2", params=list(symbol, asof))
	if(nrow(pxDf) == 0) return (NULL)
	
	pxDf$tod <- hms(format(pxDf$ts, "%H:%M:%S"))
	pxDf <- pxDf %>% filter(tod >= startTime & tod <= endTime) 
	
	xts(pxDf$v, asof + pxDf$tod)
}

rm1All <- NULL
rp1All <- NULL
bdtAll <- NULL

r1daysIday <- r1days %>% filter(rm1 >= minTs)
for(i in 1:nrow(r1daysIday)){
	cat(paste(i, "... "))
	rm1 <- as.Date(r1daysIday$rm1[i])
	rm1Xts <- getIntradayXts(rm1)
	if(is.null(rm1Xts)) next
	rm1All <- rbind.xts(rm1All, rm1Xts)
	index(rm1Xts) <- as.POSIXct(paste("1990-01-01", strftime(index(rm1Xts), "%H:%M:S", tz="UTC")), format="%Y-%m-%d %H:%M:S", tz="UTC")
	
	rp1 <- as.Date(r1daysIday$rp1[i])
	rp1Xts <- getIntradayXts(rp1)
	if(is.null(rp1Xts)) rp1Xts <- xts(rep(NA, nrow(rm1Xts)), index(rm1Xts))
	rp1All <- rbind.xts(rp1All, rp1Xts)
	index(rp1Xts) <- as.POSIXct(paste("1990-01-01", strftime(index(rp1Xts), "%H:%M:S", tz="UTC")), format="%Y-%m-%d %H:%M:S", tz="UTC")
	
	bdt <- as.Date(r1daysIday$bdt[i])
	bdtXts <- getIntradayXts(bdt)
	if(is.null(bdtXts)) {
		bdtXts <- xts(rep(NA, nrow(rm1Xts)), index(rm1Xts))
	}
	else {
		bdtAll <- rbind.xts(bdtAll, bdtXts)
	}
	index(bdtXts) <- as.POSIXct(paste("1990-01-01", strftime(index(bdtXts), "%H:%M:S", tz="UTC")), format="%Y-%m-%d %H:%M:S", tz="UTC")
	
	trdXts <- merge(rm1Xts, bdtXts, rp1Xts)
	names(trdXts) <- c("Tm1", "T0", "Tp1")
	
	tp <- data.frame(trdXts)
	tp$T <- index(trdXts)
	tp <- melt(tp, id='T')
	
	ggplot(tp, aes(x=T, y=value, fill=variable)) +
		theme_economist() +
		theme(axis.text.x=element_text(angle=90, hjust=1)) +
		geom_bar(stat='identity', position=position_dodge2()) +
		scale_x_time(label = label_time("%H:%M"), breaks=date_breaks('15 min')) +
		scale_fill_viridis_d() +
		labs(x='', y='volume', fill='', title = sprintf("%s Futures Volume %s", symbol, bdt))
		
	ggsave(sprintf("%s/%s.futures-volume.%s.png", reportPath, symbol, bdt), width=12, height=6, units="in")
}

rm1Stat <- data.frame(rm1All) %>% rownames_to_column('T') %>% mutate(TS = as.POSIXct(strftime(T, format="%H:%M:%S"), format="%H:%M:%S")) %>% 
			pivot_longer(-c(TS, T)) %>% group_by(TS) %>% 
			summarize(H = max(value, na.rm=T), M = median(value, na.rm=TRUE))
			
rp1Stat <- data.frame(rp1All) %>% rownames_to_column('T') %>% mutate(TS = as.POSIXct(strftime(T, format="%H:%M:%S"), format="%H:%M:%S")) %>% 
			pivot_longer(-c(TS, T)) %>% group_by(TS) %>% 
			summarize(H = max(value, na.rm=T), M = median(value, na.rm=TRUE))
			
bdtStat <- data.frame(bdtAll) %>% rownames_to_column('T') %>% mutate(TS = as.POSIXct(strftime(T, format="%H:%M:%S"), format="%H:%M:%S")) %>% 
			pivot_longer(-c(TS, T)) %>% group_by(TS) %>% 
			summarize(H = max(value, na.rm=T), M = median(value, na.rm=TRUE))

tpStat <- rm1Stat %>% inner_join(rp1Stat, by='TS', suffix=c('m1', 'p1')) %>% inner_join(bdtStat, by='TS') %>% pivot_longer(-TS) %>% 
			mutate(G = ifelse(endsWith(name, 'm1'), 'm1', ifelse(endsWith(name, 'p1'), 'p1', '0'))) %>%
			arrange(TS) %>% mutate(row = row_number())

rm1Stat %>% pivot_longer(-TS) %>% 
			mutate(TS = as.POSIXct(strptime(TS, format="%Y-%m-%d %H:%M:%S"))) %>% 
			arrange(TS) %>% mutate(row = row_number()) %>%
			mutate(L = ifelse(value == max(value), as.character(index(rm1All[rm1All == max(value)])), NA)) %>%	{
			
	ggplot(., aes(x=reorder(TS, row), y=value, color=name)) +
		theme_economist() +
		theme(axis.text.x=element_text(angle=90, hjust=1)) +
		geom_point(size=1) +
		geom_label_repel(aes(label = L), show.legend = FALSE) +
		scale_color_viridis_d() +
		scale_x_discrete(labels = strftime((unique(.$TS))[seq(1, length(unique(.$TS)), by=15)], "%H:%M"), breaks = as.character((unique(.$TS))[seq(1, length(unique(.$TS)), by=15)])) +
		labs(x='', y='volume', fill='', color='', title = sprintf("%s Futures Volume t-1", symbol))
}
ggsave(sprintf("%s/%s.futures-volume.tm1.png", reportPath, symbol, bdt), width=12, height=6, units="in")

rp1Stat %>% pivot_longer(-TS) %>% 
			mutate(TS = as.POSIXct(strptime(TS, format="%Y-%m-%d %H:%M:%S"))) %>% 
			arrange(TS) %>% mutate(row = row_number()) %>%
			mutate(L = ifelse(value == max(value), as.character(index(rp1All[rp1All == max(value)])), NA)) %>%	{
			
	ggplot(., aes(x=reorder(TS, row), y=value, color=name)) +
		theme_economist() +
		theme(axis.text.x=element_text(angle=90, hjust=1)) +
		geom_point(size=1) +
		geom_label_repel(aes(label = L), show.legend = FALSE) +
		scale_color_viridis_d() +
		scale_x_discrete(labels = strftime((unique(.$TS))[seq(1, length(unique(.$TS)), by=15)], "%H:%M"), breaks = as.character((unique(.$TS))[seq(1, length(unique(.$TS)), by=15)])) +
		labs(x='', y='volume', fill='', color='', title = sprintf("%s Futures Volume t+1", symbol))
}
ggsave(sprintf("%s/%s.futures-volume.tp1.png", reportPath, symbol, bdt), width=12, height=6, units="in")

bdtStat %>% pivot_longer(-TS) %>% 
			mutate(TS = as.POSIXct(strptime(TS, format="%Y-%m-%d %H:%M:%S"))) %>% 
			arrange(TS) %>% mutate(row = row_number()) %>%
			mutate(L = ifelse(value == max(value), as.character(index(bdtAll[bdtAll == max(value)])), NA)) %>% {
			
	ggplot(., aes(x=reorder(TS, row), y=value, color=name)) +
		theme_economist() +
		theme(axis.text.x=element_text(angle=90, hjust=1)) +
		geom_point(size=1) +
		geom_label_repel(aes(label = L), show.legend = FALSE) +
		scale_color_viridis_d() +
		scale_x_discrete(labels = strftime((unique(.$TS))[seq(1, length(unique(.$TS)), by=15)], "%H:%M"), breaks = as.character((unique(.$TS))[seq(1, length(unique(.$TS)), by=15)])) +
		labs(x='', y='volume', fill='', color='', title = sprintf("%s Futures Volume earnings day", symbol))
}
ggsave(sprintf("%s/%s.futures-volume.BD.png", reportPath, symbol), width=12, height=6, units="in")
 

############################################

plotVolume <- function(rm1Vol, rp1Vol, rmVol, vol, plotTitle, plotFileName) {
	rm1VolXts <- xts(rm1Vol[,2], rm1Vol[,1])
	rp1VolXts <- xts(rp1Vol[,2], rp1Vol[,1])						
	rmVolXts <- xts(rmVol[,2], rmVol[,1])
	volXts <- xts(vol[,2], vol[,1])
	volAvgXts <- rollapply(volXts, 50, mean)

	rm1Avg <- merge(volAvgXts, rm1VolXts)
	rm1Avg[,1] <- na.locf(rm1Avg[,1])
	rm1Avg <- na.omit(rm1Avg)
	rm1Avg$prop <- rm1Avg[,2]/rm1Avg[,1]

	rp1Avg <- merge(volAvgXts, rp1VolXts)
	rp1Avg[,1] <- na.locf(rp1Avg[,1])
	rp1Avg <- na.omit(rp1Avg)
	rp1Avg$prop <- rp1Avg[,2]/rp1Avg[,1]

	rmAvg <- merge(volAvgXts, rmVolXts)
	rmAvg[,1] <- na.locf(rmAvg[,1])
	rmAvg <- na.omit(rmAvg)
	rmAvg$prop <- rmAvg[,2]/rmAvg[,1]

	propXts <- merge(rm1Avg$prop, rmAvg$prop, rp1Avg$prop)
	names(propXts) <- c("Tm1", "T0", "Tp1")

	propXts[,1] <- na.locf(propXts[,1])
	propXts[,2] <- na.locf(propXts[,2])
	if(is.na(propXts[nrow(propXts),3])) propXts[nrow(propXts),3] <- 0
	propXts <- na.omit(propXts)

	tp <- data.frame(propXts)
	tp$T <- index(propXts)
	tp <- melt(tp, id='T')
	tp <- na.omit(tp)

	ggplot(tp, aes(x=factor(T), y=value, fill=variable)) +
		theme_economist() +
		theme(axis.text.x=element_text(angle=90, hjust=1)) +
		geom_bar(stat='identity', position=position_dodge2()) +
		geom_hline(yintercept=1, linetype="dashed", color='darkgreen', linewidth=1) +
		scale_fill_viridis_d() +
		labs(x='', y='x over mean', fill='days from results', title = plotTitle)
		
	ggsave(sprintf("%s/%s.%s.png", reportPath, symbol, plotFileName), width=12, height=6, units="in")	
}


rm1VolEq <- sqlQuery(lcon, sprintf("select time_stamp, tot_trd_val from px_history where symbol = '%s' and time_stamp in ('%s') and series in('EQ', 'BE')", symbol, paste(r1days$rm1[is.finite(r1days$rm1)], collapse="','")))
rp1VolEq <- sqlQuery(lcon, sprintf("select time_stamp, tot_trd_val from px_history where symbol = '%s' and time_stamp in ('%s') and series in('EQ', 'BE')", symbol, paste(r1days$rp1[is.finite(r1days$rp1)], collapse="','")))
rmVolEq <- sqlQuery(lcon, sprintf("select time_stamp, tot_trd_val from px_history where symbol = '%s' and time_stamp in ('%s') and series in('EQ', 'BE')", symbol, paste(r1days$bdt[is.finite(r1days$bdt)], collapse="','")))

volEq <- sqlQuery(lcon, sprintf("select time_stamp, tot_trd_val from px_history where symbol = '%s' and time_stamp not in ('%s') and series in('EQ', 'BE') and time_stamp >= '%s'", 
						symbol, paste(c(r1days$rm1[is.finite(r1days$rm1)], r1days$rp1[is.finite(r1days$rp1)], r1days$bdt[is.finite(r1days$bdt)]), collapse="','"), startDate - 100))
	
plotVolume(rm1VolEq, rp1VolEq, rmVolEq, volEq, sprintf("%s Equity Value Traded around Results", symbol), "equity-volume")


rm1OiOpt <- sqlQuery(lcon, sprintf("select time_stamp, option_typ, sum(change_oi) oi from BHAV_EQ_OPT where symbol = '%s' and time_stamp in ('%s') group by time_stamp, option_typ", 
									symbol, paste(r1days$rm1[is.finite(r1days$rm1)], collapse="','")))
rp1OiOpt <- sqlQuery(lcon, sprintf("select time_stamp, option_typ, sum(change_oi) oi from BHAV_EQ_OPT where symbol = '%s' and time_stamp in ('%s') group by time_stamp, option_typ", 
									symbol, paste(r1days$rp1[is.finite(r1days$rp1)], collapse="','")))
rmOiOpt <- sqlQuery(lcon, sprintf("select time_stamp, option_typ, sum(change_oi) oi from BHAV_EQ_OPT where symbol = '%s' and time_stamp in ('%s') group by time_stamp, option_typ", 
									symbol, paste(r1days$bdt[is.finite(r1days$bdt)], collapse="','")))

volOiOpt <- sqlQuery(lcon, sprintf("select time_stamp, option_typ, sum(change_oi) from BHAV_EQ_OPT where symbol = '%s' and time_stamp not in ('%s') and time_stamp >= '%s' group by time_stamp, option_typ", 
						symbol, paste(c(r1days$rm1[is.finite(r1days$rm1)], r1days$rp1[is.finite(r1days$rp1)], r1days$bdt[is.finite(r1days$bdt)]), collapse="','"), startDate - 100))
						
rm1OiOpt <- rm1OiOpt %>% inner_join(r1days, by=c('time_stamp' = 'rm1')) %>% mutate(time_stamp=bdt) %>% select(time_stamp, option_typ, oi)
rp1OiOpt <- rp1OiOpt %>% inner_join(r1days, by=c('time_stamp' = 'rp1')) %>% mutate(time_stamp=bdt) %>% select(time_stamp, option_typ, oi)

toPlot <- rm1OiOpt %>% inner_join(rp1OiOpt, by=c('time_stamp', 'option_typ'), suffix=c('m1', 'p1')) %>% inner_join(rmOiOpt, by=c('time_stamp', 'option_typ')) %>% 
			pivot_longer(-c(time_stamp, option_typ)) %>% 
			mutate(name = gsub('oim1', '(t-1)', name)) %>%
			mutate(name = gsub('oip1', '(t+1)', name)) %>%
			mutate(name = gsub('oi', '(t)', name)) %>%
			arrange(time_stamp, name, option_typ) %>% mutate(row = row_number())
toPlot %>% 
	ggplot(aes(x=reorder(time_stamp, row), y=value, group=interaction(option_typ, name), fill=interaction(option_typ, name))) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_bar(stat='identity', position=position_dodge2()) +
	scale_fill_viridis_d() +
	labs(x='', y='change in OI', fill='', title = sprintf("%s Options Change in OI around Results", symbol))
	
ggsave(sprintf("%s/%s.options-oi-change.png", reportPath, symbol), width=12, height=6, units="in")	


rm1VolFut <- sqlQuery(lcon, sprintf("select time_stamp, sum(val_in_lakh) from BHAV_EQ_FUT where symbol = '%s' and time_stamp in ('%s') group by time_stamp", symbol, paste(r1days$rm1[is.finite(r1days$rm1)], collapse="','")))
rp1VolFut <- sqlQuery(lcon, sprintf("select time_stamp, sum(val_in_lakh) from BHAV_EQ_FUT where symbol = '%s' and time_stamp in ('%s') group by time_stamp", symbol, paste(r1days$rp1[is.finite(r1days$rp1)], collapse="','")))
rmVolFut <- sqlQuery(lcon, sprintf("select time_stamp, sum(val_in_lakh) from BHAV_EQ_FUT where symbol = '%s' and time_stamp in ('%s') group by time_stamp", symbol, paste(r1days$bdt[is.finite(r1days$bdt)], collapse="','")))

volFut <- sqlQuery(lcon, sprintf("select time_stamp, sum(val_in_lakh) from BHAV_EQ_FUT where symbol = '%s' and time_stamp not in ('%s') and time_stamp >= '%s' group by time_stamp", 
						symbol, paste(c(r1days$rm1[is.finite(r1days$rm1)], r1days$rp1[is.finite(r1days$rp1)], r1days$bdt[is.finite(r1days$bdt)]), collapse="','"), startDate - 100))
	
plotVolume(rm1VolFut, rp1VolFut, rmVolFut, volFut, sprintf("%s Futures Value Traded around Results", symbol), "futures-volume")


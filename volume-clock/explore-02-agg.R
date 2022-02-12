source("d:/stockviz/r/config.r")
source("d:/stockviz/r/plot.common.r")
source("d:/stockviz/r/theme.returns.common.r")
reportPath<-"D:/StockViz/public/blog/volume-clock"

library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('reshape2')
library('tidyverse')
library('viridis')
library('ggthemes')

pdf(NULL)
options(stringsAsFactors = FALSE)
options("scipen"=999)

pcon <- dbConnect(RPostgres::Postgres(), host=ldbserver2, user= ldbuser2, password=ldbpassword2, dbname=ldbname2)

#assume only nifty futures are stored. 
#as we add more instruments, need to look up zd_master and query by token ids

niftyDf <- dbGetQuery(pcon, "select instrument_token, last_price, last_quantity, last_trade_tick from zd_quotes_stream where last_trade_tick % 100000000 = tick_stamp % 100000000 order by last_trade_tick")

startDate <- strptime(min(niftyDf$last_trade_tick/1000000), '%Y%m%d')
endDate <- strptime(max(niftyDf$last_trade_tick/1000000), '%Y%m%d')


###########
# aggregate by clock


niftyXts <- xts(niftyDf$last_quantity, strptime(niftyDf$last_trade_tick, '%Y%m%d%H%M%S'))
niftyVol5min <- period.apply(niftyXts, endpoints(niftyXts, on='minutes', k=5), sum)

toPlot <- data.frame(niftyVol5min)
names(toPlot) <- c('Volume')
toPlot$T <- as.POSIXct(index(niftyVol5min))

meanVol5min <- mean(toPlot$Volume)
medianVol5min <- median(toPlot$Volume)

ggplot(toPlot, aes(x=T, y=Volume)) +
	theme_economist() +
	geom_point() +
	geom_hline(yintercept=meanVol5min, linetype="dashed", color='darkgreen') +
	geom_hline(yintercept=medianVol5min, linetype="dashed", color='green') +
	geom_text(aes(x=T[1], label=paste0("avg: ", round(meanVol5min, 0)), y = meanVol5min, vjust='top', hjust='left'), colour='grey') +
	geom_text(aes(x=T[1], label=paste0("median: ", round(medianVol5min, 0)), y = medianVol5min, vjust='top', hjust='left'), colour='grey') +
	labs(color='', fill='', x='', y='volume', title = "NIFTY Futures Volume (chrono clock)", subtitle=sprintf("5-min [%s:%s]", startDate, endDate)) +
	annotate("text", x=toPlot$T[1], y=-Inf, label = "@StockViz", hjust='left', vjust='bottom', col="white", cex=6, fontface = "bold", alpha = 0.4)
	  
ggsave(sprintf("%s/chrono-clock.5min.daily.png", reportPath), width=12, height=6, units="in")	  


ggplot(toPlot, aes(x=Volume)) +
	theme_economist() +
	geom_density() +
	geom_vline(xintercept=meanVol5min, linetype="dashed", color='darkgreen') +
	geom_vline(xintercept=medianVol5min, linetype="dashed", color='green') +
	geom_text(aes(x=meanVol5min, label=paste0("avg: ", round(meanVol5min, 0)), y = 0, vjust='top', hjust='left'), colour='grey', angle=90) +
	geom_text(aes(x=medianVol5min, label=paste0("median: ", round(medianVol5min, 0)), y = 0, vjust='top', hjust='left'), colour='grey', angle=90) +
	labs(color='', fill='', x='', y='', title = "NIFTY Futures Volume Density Plot", subtitle=sprintf("5-min [%s:%s]", startDate, endDate)) +
	annotate("text", x=0, y=0, label = "@StockViz", hjust='left', vjust='bottom', col="white", cex=6, fontface = "bold", alpha = 0.4)
	  
ggsave(sprintf("%s/chrono-clock.5min.density.png", reportPath), width=12, height=6, units="in")	  


###########
# aggregate by volume

groupByVol <- function(cumsum_thresh){
	vac.qty <- 0
	vac.s <- niftyDf[1, 'last_trade_tick']
	vac.e <- niftyDf[1, 'last_trade_tick']

	volDf <- data.frame(V = 0.0, s = 0, e = 0)
	for(i in 1:nrow(niftyDf)){
		vac.qty <- vac.qty + niftyDf[i, 'last_quantity']
		vac.e <- niftyDf[i, 'last_trade_tick']
		
		if(vac.qty < cumsum_thresh) next
		
		volDf <- rbind(volDf, c(vac.qty, as.numeric(vac.s), as.numeric(vac.e)))
		
		vac.qty <- 0
		vac.s <- niftyDf[i, 'last_trade_tick']
		vac.e <- niftyDf[i, 'last_trade_tick']
	}

	volDf <- volDf[-1,]
	volDf$es <- as.numeric(strptime(volDf$e, '%Y%m%d%H%M%S') - strptime(volDf$s, '%Y%m%d%H%M%S'))
	
	volDf
}

vct <- 10000
vdf <- NULL
volDf <- groupByVol(vct)
volDf$VCT <- rep(vct, nrow(volDf))
volDf$x <- seq(1:nrow(volDf))
vdf <- volDf

toPlot <- vdf[, c('es', 'x')]
toPlot$es <- ifelse(toPlot$es > 6000, NA, toPlot$es)

meanAgg <- mean(toPlot$es, na.rm=T)
medianAgg <- median(toPlot$es, na.rm=T)

ggplot(toPlot, aes(x=x, y=es)) +
	theme_economist() +
	scale_color_viridis(discrete = T) +
	geom_point() +
	geom_hline(yintercept=meanAgg, linetype="dashed", color='darkgreen') +
	geom_hline(yintercept=medianAgg, linetype="dashed", color='green') +
	geom_text(aes(x=0, label=paste0("avg: ", round(meanAgg, 0)), y = meanAgg, vjust='top', hjust='left'), colour='grey') +
	geom_text(aes(x=0, label=paste0("median: ", round(medianAgg, 0)), y = medianAgg, vjust='top', hjust='left'), colour='grey') +
	labs(color='', fill='', x='', y='seconds', title = "NIFTY Futures Volume Interval", subtitle=sprintf("volume clock: %d [%s:%s]", vct, startDate, endDate)) +
	annotate("text", x=Inf, y=-Inf, label = "@StockViz", hjust='right', vjust='bottom', col="white", cex=6, fontface = "bold", alpha = 0.4)
	  
ggsave(sprintf("%s/volume-clock.10000.png", reportPath), width=12, height=6, units="in")	  

q()
	
ivolDf <- data.frame(instrument_token = 0, h=0.0, l=0.0, V=0.0, s = 0, e = 0)
for(i in 1:nrow(volDf)){
	tdf <- niftyDf %>% filter(last_trade_tick >= volDf[i, 's'] & last_trade_tick <= volDf[i, 'e']) %>% group_by(instrument_token) %>% summarize(h=max(last_price), l=min(last_price), V = sum(last_quantity)) %>% as.data.frame()
	tdf$s <- volDf[i, 's']
	tdf$e <- volDf[i, 'e']
	
	ivolDf <- rbind(ivolDf, tdf)
}
ivolDf <- ivolDf[-1,]

ivolDf$es <- strptime(ivolDf$e, '%Y%m%d%H%M%S') - strptime(ivolDf$s, '%Y%m%d%H%M%S')

toPlot <- ivolDf
toPlot$s <- factor(toPlot$s, levels = unique(toPlot$s))
toPlot$instrument_token <- factor(toPlot$instrument_token, levels = unique(toPlot$instrument_token))

ggplot(toPlot, aes(x=s, color=instrument_token, group=instrument_token)) +
	geom_segment(aes(y=h, yend=l, xend=s))








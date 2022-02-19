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
vdf <- groupByVol(vct)
vdf <- vdf[vdf$es < 6000,]

###create candles
	
ivolDf <- data.frame(instrument_token = 0, h=0.0, l=0.0, V=0.0, s = 0, e = 0)
for(i in 1:nrow(vdf)){
	tdf <- niftyDf %>% filter(last_trade_tick >= vdf[i, 's'] & last_trade_tick <= vdf[i, 'e']) %>% group_by(instrument_token) %>% summarize(h=max(last_price), l=min(last_price), V = sum(last_quantity)) %>% as.data.frame()
	tdf$s <- vdf[i, 's']
	tdf$e <- vdf[i, 'e']
	
	ivolDf <- rbind(ivolDf, tdf)
}
ivolDf <- ivolDf[-1,]

ivolDf$es <- strptime(ivolDf$e, '%Y%m%d%H%M%S') - strptime(ivolDf$s, '%Y%m%d%H%M%S')

#all available contracts
toPlot <- ivolDf
toPlot$s <- factor(toPlot$s, levels = unique(toPlot$s))
toPlot$instrument_token <- factor(toPlot$instrument_token, levels = unique(toPlot$instrument_token))

ggplot(toPlot, aes(x=s, color=instrument_token, group=instrument_token)) +
	theme_economist() +
	theme(axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
	scale_color_viridis(discrete = T) +
	geom_segment(aes(y=h, yend=l, xend=s)) +
	labs(color='contract', fill='', y='price', x='', title = "NIFTY Futures Volume Interval", subtitle=sprintf("%s:%s", startDate, endDate)) +
	annotate("text", x=Inf, y=-Inf, label = "@StockViz", hjust='top', vjust='left', col="white", cex=6, fontface = "bold", alpha = 0.4)
	  
ggsave(sprintf("%s/volume-clock.hl.all.png", reportPath), width=12, height=6, units="in")	  

#analyze one contract

ivolDf1 <- ivolDf[ivolDf$instrument_token == 13278466,]
ivolDf1$HLpct <- ivolDf1$h/ivolDf1$l - 1

niftyDf1 <- niftyDf[niftyDf$instrument_token == 13278466,]
niftyXts <- xts(niftyDf1$last_price, strptime(niftyDf1$last_trade_tick, '%Y%m%d%H%M%S'))
nifty5Xts <- to.minutes5(niftyXts)

nifty5Xts$HLpct <- nifty5Xts[, 'niftyXts.High']/nifty5Xts[, 'niftyXts.Low']-1

nhlp <- coredata(nifty5Xts$HLpct)
vhlp <- ivolDf1$HLpct
length(nhlp) <- max(length(nhlp), length(vhlp))
length(vhlp) <- max(length(nhlp), length(vhlp))
toPlot <- melt(data.frame(CC = nhlp, VC = vhlp))

ggplot(toPlot, aes(x=value, color=variable)) +
	theme_economist() +
	scale_color_viridis(discrete = T) +
	geom_density(size=1) +
	labs(color='clock', fill='', y='density', x='', title = "NIFTY Futures high-low %", subtitle=sprintf("density plot; longest running contract [%s:%s]", startDate, endDate)) +
	annotate("text", x=Inf, y=-Inf, label = "@StockViz", hjust='top', vjust='left', col="white", cex=6, fontface = "bold", alpha = 0.4)
	
ggsave(sprintf("%s/hlpct.density.png", reportPath), width=12, height=6, units="in")	  	
	


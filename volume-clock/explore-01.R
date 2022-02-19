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

volCumsumThresholds <- c(2500, 5000, 10000)

vdf <- NULL
for(vct in volCumsumThresholds){
	volDf <- groupByVol(vct)
	volDf$VCT <- rep(vct, nrow(volDf))
	volDf$x <- seq(1:nrow(volDf))
	vdf <- rbind(vdf, volDf)
}

toPlot <- vdf[, c('VCT', 'es', 'x')]
toPlot$es <- ifelse(toPlot$es > 6000, NA, toPlot$es)
toPlot$VCT <- factor(toPlot$VCT, levels=unique(toPlot$VCT))

ggplot(toPlot, aes(x=x, y=es, color=VCT)) +
	theme_economist() +
	scale_color_viridis(discrete = T) +
	geom_point() +
	labs(color='volume', fill='', x='', y='seconds', title = "NIFTY Futures Volume Interval", subtitle=sprintf("%s:%s", startDate, endDate)) +
	annotate("text", x=Inf, y=-Inf, label = "@StockViz", hjust='top', vjust='left', col="white", cex=6, fontface = "bold", alpha = 0.4)
	  
ggsave(sprintf("%s/volume-clock.all.png", reportPath), width=12, height=6, units="in")	  
	
vct <- 2500	
ggplot(toPlot[toPlot$VCT == vct,], aes(x=x, y=es)) +
	theme_economist() +
	geom_point() +
	labs(color='volume', fill='', x='', y='seconds', title = sprintf("NIFTY Futures Volume Interval (%d)", vct), subtitle=sprintf("%s:%s", startDate, endDate)) +
	annotate("text", x=Inf, y=-Inf, label = "@StockViz", hjust='top', vjust='left', col="white", cex=6, fontface = "bold", alpha = 0.4)
	  
ggsave(sprintf("%s/volume-clock.%d.png", reportPath, vct), width=12, height=6, units="in")	  



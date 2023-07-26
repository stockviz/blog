library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')

library('tidyverse')
library('lubridate')
library('ggthemes')
library('reshape2')
library('viridis')
library('ggrepel')
library('patchwork')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")

reportPath <- "."
pdf(NULL)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexName <- "NIFTY 50"

startDate <- as.Date("2015-01-01")

corLbs <- c(5, 10, 20, 50, 100) #bus-days of look-back for correlation
volLb <- 20 #days to calc volatility
tileLb <- 500 #days to calc tiles

pxDf <- sqlQuery(lcon, sprintf("select time_stamp, px_open [Open], px_high [High], px_low [Low], px_close [Close] from bhav_index where index_name='%s' and time_stamp >= '%s'", indexName, startDate))
pXts <- xts(pxDf[,-1], pxDf[,1])

dRet <- dailyReturn(pXts$Close)
vRet <- volatility(pXts, n=volLb, calc="yang.zhang")
 
eqWtCorTile <- NULL
for(corLb in corLbs){
	load(file=sprintf("%s/%s.corr.%d.RData", reportPath, indexName, corLb)) #eqWtCorXts
	
	corTile <- rollapply(eqWtCorXts[,1], tileLb, function(X) xts(last(ntile(coredata(X), n=5)), last(index(X))))
	names(corTile) <- c(paste0('T', corLb))
	
	eqWtCorTile <- merge.xts(eqWtCorTile, corTile)
}

allXts <- na.omit(merge(dRet, vRet, eqWtCorTile))
names(allXts) <- c('RET', 'VOL', names(eqWtCorTile))

for(corLb in corLbs){
	corLbname <- paste0('T', corLb)
	
	toPlot <- data.frame(allXts[,c('RET', 'VOL', corLbname)])
	
	toPlot[,corLbname] <- factor(toPlot[,corLbname], levels = unique(toPlot[,corLbname]))

	p1 <- ggplot(toPlot, aes(y=RET, x=.data[[corLbname]], fill=.data[[corLbname]])) +
			theme_economist() +
			scale_fill_viridis_d(option='C') +
			geom_violin(position="dodge") +
			labs(x="quintiles", y="daily returns", fill="quintile", title = "Daily Returns")
		

	p2 <- ggplot(toPlot, aes(y=VOL, x=.data[[corLbname]], fill=.data[[corLbname]])) +
			theme_economist() +
			scale_fill_viridis_d(option='C') +
			geom_violin(position="dodge") +
			labs(x="quintiles", y=sprintf("volatility (%d-day)", volLb), fill="quintile", title = sprintf("%d-day Volatility", volLb))
	
	p1 + p2 + plot_layout(ncol=1) + 
		plot_annotation(
		  title = sprintf('%s; constituent pair-wise %d-day correlation', indexName, corLb),
		  subtitle = sprintf('rolling %d-day tiles', tileLb),
		  caption = '@StockViz'
		)
		
	ggsave(sprintf("%s/%s.%d-corr.ret-vol.violin.png", reportPath, indexName, corLb), width=12, height=12, units="in")
}

for(qtnum in 1:5){
	toPlot <- data.frame(allXts)
	toPlot <- melt(toPlot, id=c('RET', 'VOL'))
	toPlot <- toPlot[toPlot$value == qtnum,]
	toPlot$variable <- factor(toPlot$variable, levels = unique(toPlot$variable))

	p1 <- ggplot(toPlot, aes(y=RET, x=variable, fill=variable)) +
			theme_economist() +
			scale_fill_viridis_d(option='C') +
			geom_violin(position="dodge") +
			labs(x="corr' look-backs (days)", y="daily returns", fill="correl-lb", title = "Daily Returns")
		

	p2 <- ggplot(toPlot, aes(y=VOL, x=variable, fill=variable)) +
			theme_economist() +
			scale_fill_viridis_d(option='C') +
			geom_violin(position="dodge") +
			labs(x="corr' look-backs (days)", y=sprintf("volatility (%d-day)", volLb), fill="correl-lb", title = sprintf("%d-day Volatility", volLb))
	
	p1 + p2 + plot_layout(ncol=1) + 
		plot_annotation(
		  title = sprintf('%s; %d-quintile of constituent pair-wise correlation', indexName, qtnum),
		  subtitle = sprintf('rolling %d-day tiles', tileLb),
		  caption = '@StockViz'
		)
		
	ggsave(sprintf("%s/%s.%d-quintile.ret-vol.violin.png", reportPath, indexName, qtnum), width=12, height=12, units="in")
}

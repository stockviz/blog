library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('tidyverse')
library('lubridate')
library('reshape2')

library('ggthemes')
library('patchwork')
library('viridis')

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

usTickers <- c('QQQ', 'XLE')

getUs <- function(symbol){
	pxSeries <- sqlQuery(lconUs2, sprintf("select time_stamp, o, c from BHAV_EQ_TD where symbol='%s'", symbol))
	pXts <- xts(pxSeries[,-1], pxSeries[,1])
	dXts <- merge(diff(log(pXts$c)), log(pXts$c) - log(pXts$o), log(pXts$o) - log(stats::lag(pXts$c, 1)))
	
	names(dXts) <- c('cc', 'oc', 'co')
	return(dXts)
}

computeSdUs <- function(retList){
	sd50 <- list()
	for(i in 1:length(retList)){
		retList[[i]] <- na.omit(retList[[i]])
		retList[[i]]$days <- c(NA, diff(index(retList[[i]])))
		

		sd50[[i]] <- merge.xts(rollapply(na.omit(retList[[i]][retList[[i]]$days == 1, 'cc']), 20, sd),
					rollapply(na.omit(retList[[i]][retList[[i]]$days == 1, 'oc']), 20, sd),
					rollapply(na.omit(retList[[i]][retList[[i]]$days > 1, 'co']), 20, sd))
				
		names(sd50[[i]]) <- c('cc-daily', 'oc-daily', 'co-wknd')
	}
	
	return(sd50)
}

plotSd <- function(sdList, symNames){
	for(i in 1:length(sdList)){
		toPlot <- melt(data.frame(sdList[[i]]))
		tpMed <- toPlot %>%
				  group_by(variable) %>%
				  summarise(line = median(value, na.rm=TRUE))
		sym <- symNames[i]
		startDt <- first(index(sdList[[i]]))
		endDt <- last(index(sdList[[i]]))
		ggplot(toPlot, aes(x=value, color=variable)) + 
			theme_economist() +
			stat_density(geom = "line", position = "identity", size=1) +
			scale_color_viridis_d() +
			geom_vline(data = tpMed, mapping = aes(xintercept = line, color=variable), size=1) +
			geom_text(data = tpMed, mapping = aes(x = line, label=paste0(round(line, 5), '\n')), y = 10, angle=90, color='black') +
			labs(x='sd(20)', y='density', color='', title=sprintf("%s Returns std-dev", sym), subtitle=sprintf("%s:%s", startDt, endDt), caption = '@StockViz')
			
		ggsave(sprintf("%s/cc.vs.oc.%s.png", reportPath, sym), width=12, height=6, units="in")
	}
}

usRetList <- lapply(usTickers, getUs)
usSdList <- computeSdUs(usRetList)
plotSd(usSdList, usTickers)

#######################################################################

inIndices <- list(c('NIFTY 50', 'NIFTY 50 TR'),c('NIFTY FMCG', 'NIFTY FMCG TR'), c('NIFTY COMMODITIES', 'NIFTY COMMODITIES TR'))

getIn <- function(syms){
	pxSeries <- sqlQuery(lcon, sprintf("select time_stamp, px_open, px_close from BHAV_INDEX where INDEX_NAME='%s' and TIME_STAMP >= '2011-01-31'", syms[1]))
	pXts1 <- xts(pxSeries[,-1], pxSeries[,1])
	pxSeries <- sqlQuery(lcon, sprintf("select time_stamp, px_open, px_close from BHAV_INDEX where INDEX_NAME='%s' and TIME_STAMP >= '2011-01-31'", syms[2]))
	pXts2 <- xts(pxSeries[,-1], pxSeries[,1])
	
	dXts <- merge(diff(log(pXts2$px_close)), log(pXts1$px_close) - log(pXts1$px_open))
	
	names(dXts) <- c('cc', 'oc')
	return(dXts)

}

computeSdIn <- function(retList){
	sd50 <- list()
	for(i in 1:length(retList)){
		retList[[i]] <- na.omit(retList[[i]])
		retList[[i]]$days <- c(NA, diff(index(retList[[i]])))
		

		sd50[[i]] <- merge.xts(rollapply(na.omit(retList[[i]][retList[[i]]$days == 1, 'cc']), 20, sd),
					rollapply(na.omit(retList[[i]][retList[[i]]$days == 1, 'oc']), 20, sd))
				
		names(sd50[[i]]) <- c('cc-daily', 'oc-daily')
	}
	
	return(sd50)
}


inRetList <- lapply(inIndices, getIn)
inSdList <- computeSdIn(inRetList)
plotSd(inSdList, unlist(lapply(inIndices, first)))

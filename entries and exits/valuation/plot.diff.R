library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('viridis')

source("d:/stockviz/r/config.r")

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."
indexName1<-"NIFTY 50"
indexName2<-"NIFTY MIDCAP 50"
startDate<-as.Date("2004-01-01")
endDate<-as.Date("2020-09-30")
mavgYrs <- 5
lb<-220*mavgYrs 

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

plotRatio<-function(ratioName){
	nDf1<-sqlQuery(lcon, sprintf("select TIME_STAMP, %s from INDEX_NSE_VALUATION where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", ratioName, indexName1, startDate, endDate))
	nXts1<-xts(nDf1[,2], as.Date(nDf1[,1]))

	nDf2<-sqlQuery(lcon, sprintf("select TIME_STAMP, %s from INDEX_NSE_VALUATION where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", ratioName, indexName2, startDate, endDate))
	nXts2<-xts(nDf2[,2], as.Date(nDf2[,1]))

	allXts<-merge(nXts1, nXts2)
	names(allXts)<-c(indexName1, indexName2)
	allXts[,1] <- na.locf(allXts[,1])
	allXts[,2] <- na.locf(allXts[,2])
	relXts<-nXts2/nXts1
	
	names(relXts)<-c('RELATIVE')
	relXts$avg <-rollapply(relXts$RELATIVE, lb, mean)
	relXts$avgPsd <- relXts$avg + rollapply(relXts$RELATIVE, lb, sd)
	relXts$avgMsd<- relXts$avg - rollapply(relXts$RELATIVE, lb, sd)

	mavg <- merge(rollapply(allXts, lb, mean), rollapply(allXts, lb, sd))
	allXts <- merge(allXts, mavg[,1], mavg[,2], mavg[,1]+mavg[,3], mavg[,1]-mavg[,3], mavg[,2]+mavg[,4], mavg[,2]-mavg[,4])
	names(allXts) <- c(indexName1, indexName2, paste0(indexName1, '-', mavgYrs, 'yrs'), paste0(indexName2, '-', mavgYrs, 'yrs'), 'i1a', 'i1b', 'i2a', 'i2b')
	
	############
	
	firstDate<-first(index(allXts))
	lastDate<-last(index(allXts))
	xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)

	ctr2Df<-data.frame(allXts)
	ctr2Df$T<-as.Date(index(allXts))
	ctr2Names <- names(ctr2Df)
	
	pdf(NULL)
	ggplot(ctr2Df, aes(x=T)) +
		theme_economist() +
		scale_color_viridis() +
		scale_fill_viridis() +
		geom_line(data=ctr2Df[, c('T', ctr2Names[1])], aes_string(y=ctr2Names[1], color='1')) +
		geom_line(data=ctr2Df[, c('T', ctr2Names[2])], aes_string(y=ctr2Names[2], color='2')) +
		geom_line(data=ctr2Df[, c('T', ctr2Names[3])], aes_string(y=ctr2Names[3], color='1'), linetype = "dashed") +
		geom_line(data=ctr2Df[, c('T', ctr2Names[4])], aes_string(y=ctr2Names[4], color='2'), linetype = "dashed") +
		geom_ribbon(data=ctr2Df[, c('T', 'i1a', 'i1b')], aes_string(ymin = 'i1b', ymax='i1a', fill='1'), alpha=0.3) +
		geom_ribbon(data=ctr2Df[, c('T', 'i2a', 'i2b')], aes_string(ymin = 'i2b', ymax='i2a', fill='2'), alpha=0.3) +
		scale_x_date(breaks = xAxisTicks) +
		guides(color=F, fill=F) +
		labs(x='', y=ratioName, color='', title=sprintf("%s/%s %s Ratio", indexName1, indexName2, ratioName), subtitle=sprintf("[%s:%s]", firstDate, lastDate)) +
		annotate("text", x=lastDate, y=min(allXts, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
			
	ggsave(sprintf("%s/%s.%s.%s.png", reportPath, indexName1, indexName2, ratioName), width=16, height=8, units="in")
	
	#############
	
	ctr2Df<-data.frame(relXts)
	ctr2Df$T<-as.Date(index(relXts))

	pdf(NULL)
	ggplot(ctr2Df, aes(x=T)) +
		theme_economist() +
		geom_line(aes(y=RELATIVE)) +
		geom_line(aes(y=avg), color='grey') +
		geom_ribbon(aes(ymin = avgMsd, ymax=avgPsd), fill='grey70', alpha=0.5) +
		scale_x_date(breaks = xAxisTicks) +
		labs(x='', y="ratio", color='', title=sprintf("%s/%s Relative %s Ratio", indexName1, indexName2, ratioName), subtitle=sprintf("[%s:%s]", firstDate, lastDate)) +
		annotate("text", x=lastDate, y=min(relXts, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
			
	ggsave(sprintf("%s/%s.%s.%s.relative.png", reportPath, indexName1, indexName2, ratioName), width=16, height=8, units="in")
}	

plotRatio("PE")
plotRatio("PB")
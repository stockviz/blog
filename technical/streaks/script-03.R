source("d:/stockviz/r/config.r")
source("d:/stockviz/r/plot.common.r")
source("d:/stockviz/r/theme.returns.common.r")

reportPath<-"."

library('RODBC')
library('quantmod')
library('PortfolioAnalytics')	
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('reshape2')
library('tidyverse')
library('viridis')
library('ggthemes')


options(stringsAsFactors = FALSE)
options("scipen"=100)


lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexName <- 'NIFTY MIDCAP 150'

niftyDailyDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '2015-01-01'", indexName))
ndXts <- xts(niftyDailyDf[,2], niftyDailyDf[,1])

rets <- dailyReturn(ndXts)
rets <- rets[-1,]
names(rets) <- c(indexName)

rets$UD <- ifelse(rets[,1] > 0, 1, -1)

rle01All<-data.frame(unclass(rle(as.vector(coredata(rets$UD)))))
maxStreak <- max(rle01All[,1])

yrs <- unique(year(index(rets)))

toPlot <- NULL
for(i in 1:length(yrs)){
	yr<-yrs[i]

	rle01<-rle(as.vector(coredata(rets[paste0(yr)]$UD)))
	rle01df<-data.frame(unclass(rle01))
	upRle<-rle01df[rle01df[,1] > 1 & rle01df[,2]==1,1] #remove streak of 1
	dnRle<-rle01df[rle01df[,1] > 1 & rle01df[,2]==-1,1]
	toPlot <- rbind(toPlot, rbind(data.frame(Y=rep(yr, length(upRle)), DIR=rep("UP", length(upRle)), VAL=upRle), data.frame(Y=rep(yr, length(dnRle)), DIR=rep("DN", length(dnRle)), VAL=dnRle)))
}

pdf(NULL)
ggplot(toPlot, aes(x=VAL, fill=DIR)) +
	theme_economist() +
	scale_fill_viridis(discrete=T) +
	geom_histogram(binwidth=1, position="dodge") +
	scale_x_continuous(breaks=seq(0, maxStreak, by=1)) +
	stat_bin(aes(y=..count.., label=ifelse(..count.. < 2, '', ..count..)), binwidth=1, geom="text", position="dodge", color="darkgreen", vjust="bottom") +
	facet_grid(rows=vars(Y)) +
	labs(y='count', x='streak length (days)', fill='', color='', title=sprintf("%s Return Streaks", indexName), subtitle="@StockViz")

ggsave(sprintf("%s/%s.streak.facets.png", reportPath, indexName), width=10, height=12, units="in")	

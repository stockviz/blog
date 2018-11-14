library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
#library("fUnitRoots")
library('extrafont')
#library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
#library('grid')
#library('gridExtra')
#library('gtable')
#library('ggrepel')
library('dplyr')
#library('RPostgres')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
#reportPath <- "D:/StockViz/report"

source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

decTs<-sqlQuery(lcon, "select max(time_stamp) from DECILE_CONSTITUENTS")[[1]]
miscTs<-sqlQuery(lcon, "select max(time_stamp) from EQUITY_MISC_INFO")[[1]]

decAll<-sqlQuery(lcon, sprintf("select dc.symbol, dc.decile, mi.ff_mkt_cap_cr mkt_cap from DECILE_CONSTITUENTS dc, EQUITY_MISC_INFO mi
					where dc.symbol=mi.symbol
					and dc.time_stamp='%s'
					and mi.time_stamp='%s'", decTs, miscTs))
					
decAll<-decAll[order(decAll$decile),]

meltedDfr<-melt(subset(decAll, select=-c(symbol)), id='decile')
meltedDfr$decile<-factor(meltedDfr$decile, levels=unique(meltedDfr$decile)) 

#plot the boxplot of market-caps across deciles
pdf(NULL)
ggplot(meltedDfr, aes(x=decile, y=value, fill=decile)) +
	theme_economist() +
	geom_boxplot(outlier.shape = 1) +
	guides(fill=F) +
	scale_y_log10() +
	labs(x = "", y="log(ffmktcap in Rs. cr.)", fill="", color="", title="Distribution of free-float market cap across deciles", subtitle=sprintf("asof: %s, %s", decTs, miscTs)) +
	annotate("text", x=9, y=0, label = "@StockViz", hjust=0, vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.6)

ggsave(sprintf("%s/decile.ffmktcap.boxplot.png",reportPath), width=12, height=6, units="in")

################################################

allIndexNames<-sqlQuery(lcon, "select distinct(index_name) from INDEX_NSE_3 where TIME_STAMP > GETDATE()-45")[,1]
zeroFrame<-data.frame(decile=0:9, count=rep(0, 10))
colPal<-economist_pal(fill = TRUE)(10)

#plot decile overlap of all indices
for(i in 1:length(allIndexNames)){
	indexSymbols<-sqlQuery(lcon, sprintf("select symbol from INDEX_NSE_3 where index_name = '%s'", allIndexNames[i]))[,1]
	overlapCount<-data.frame(decAll[decAll$symbol %in% indexSymbols,] %>% group_by(decile) %>% summarize(count=n()))
	overlapCount<-rbind(overlapCount, data.frame(decile=setdiff(0:9, overlapCount$decile), count=NA))
	overlapCount<-overlapCount[order(overlapCount$decile),]
	overlapCount$decile<-factor(overlapCount$decile, levels=unique(overlapCount$decile)) 
	
	pdf(NULL)
	ggplot(overlapCount, aes(x=decile, y=count)) +
		theme_economist() +
		geom_bar(stat="identity", position=position_dodge(), fill=colPal) +
		geom_text(aes(label=count), vjust=1.6, color="black", position = position_dodge(0.9), size=4) +
		guides(fill=F) +
		labs(x = "", y="count", fill="", color="", title=sprintf("%s decile overlap", allIndexNames[i]), subtitle=sprintf("asof: %s", decTs)) +
		annotate("text", x=9, y=0, label = "@StockViz", hjust=0, vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.6)

	ggsave(sprintf("%s/decile.%s.overlap.png",reportPath, sub('%', 'pct', allIndexNames[i])), width=12, height=6, units="in")
}

################################################

boDf<-sqlQuery(lcon, "select TIME_STAMP, DECILE, BO_HISTORICAL_MEDIAN from DECILE_ANALYSIS order by TIME_STAMP, DECILE")
boDf$TIME_STAMP<-as.Date(boDf$TIME_STAMP)
boDf$DECILE<-as.character(boDf$DECILE)

plotStart<-as.Date(first(boDf[,1]))
plotEnd<-as.Date(last(boDf[,1]))
xAxisTicks<-seq(from=plotStart, to=plotEnd, length.out=10)

meltedDfr<-melt(boDf, id=c('TIME_STAMP', 'DECILE'))

pdf(NULL)
ggplot(meltedDfr, aes(x=TIME_STAMP, y=value, color=DECILE)) +
	theme_economist() +
	geom_line() +
	scale_x_date(breaks = xAxisTicks) +
	labs(x = "", y="bid/offer spread", fill="", color="", title="Bid/offer spread across deciles", subtitle=sprintf("200-day rolling median [%s:%s]", plotStart, plotEnd)) +
	annotate("text", x=plotEnd, y=min(boDf$BO_HISTORICAL_MEDIAN), label = "@StockViz", hjust=0.8, vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.6)

ggsave(sprintf("%s/decile.bid-offer.png",reportPath), width=12, height=6, units="in")

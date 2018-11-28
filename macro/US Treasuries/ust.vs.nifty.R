library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('dplyr')
library('schoRsch')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("1995-01-01")
endDate<-as.Date("2018-10-31")

ust<-"Y10"

ustDf<-sqlQuery(lcon, sprintf("select * from UST_YIELD_CURVE where time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
ustXts<-xts(ustDf[,-1], as.Date(ustDf[,1]))

niftyDf<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE from BHAV_INDEX where index_name='NIFTY 50' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
niftyXts<-xts(niftyDf[,-1], as.Date(niftyDf[,1]))

weeklyRets<-merge(weeklyReturn(ustXts[, ust]), weeklyReturn(niftyXts))
weeklyRets[,1]<-na.locf(weeklyRets[,1])
weeklyRets<-merge(weeklyRets, stats::lag(weeklyRets[,2], -1))
names(weeklyRets)<-c('UST', 'R', 'R_L1')

weeklyRets[is.infinite(weeklyRets$UST),]$UST<-NA
weeklyRets<-na.omit(weeklyRets)

weeklyRets$BIN<-ntiles(data.frame(weeklyRets), dv=1, bins=10)

binSumm<-data.frame(weeklyRets) %>% group_by(BIN) %>% summarize(MIN=min(UST, na.rm=T), AVG=median(UST, na.rm=T), MAX=max(UST, na.rm=T))
binSumm<-data.frame(binSumm)
binSumm$BIN<-factor(binSumm$BIN, levels=unique(binSumm$BIN)) 

binRange<-data.frame(weeklyRets) %>% group_by(BIN) %>% summarize(MIN=100*min(R_L1, na.rm=T), AVG=100*median(R_L1, na.rm=T), MAX=100*max(R_L1, na.rm=T))
binRange<-data.frame(binRange)
binRange$BIN<-factor(binRange$BIN, levels=unique(binRange$BIN)) 

dfr<-data.frame(weeklyRets[, c('BIN', 'R_L1')])
dfr<-dfr[order(dfr$BIN),]
dfr$BIN<-factor(dfr$BIN, levels=unique(dfr$BIN)) 
dfr$R_L1<-100*dfr$R_L1

plotStart<-min(index(weeklyRets))
plotEnd<-max(index(weeklyRets))

pdf(NULL)
ggplot(dfr, aes(x=BIN, y=R_L1, fill=BIN)) +
	theme_economist() +
	geom_boxplot(outlier.shape = 1) +
	scale_x_discrete(breaks=binSumm$BIN, labels=sprintf("[%.2f%%,%.2f%%]", 100*binSumm$MIN, 100*binSumm$MAX)) +
	geom_text(data = binRange, aes(x = BIN, y = AVG, label = sprintf("%0.2f%%", AVG)), size = 3, vjust = -0.5) +
	guides(color=F, fill=F) +
	labs(x = "", y="", fill="", color="", title=sprintf("Distribution of NIFTY 50 Returns vs. weekly US Treasury %s", ust), subtitle=sprintf("Subsequent 1-week returns [%s:%s]", plotStart, plotEnd)) +
	annotate("text", x=10, y=min(dfr$R_L1), label = "@StockViz", hjust=+1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.6)

ggsave(sprintf("%s/%s.vs.NIFTY50.png",reportPath, ust), width=12, height=6, units="in")



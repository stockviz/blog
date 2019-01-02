library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('reshape2')

library('ggplot2')
library('extrafont')
library('ggthemes')

reportPath <- "."
options("scipen"=100)
options(stringsAsFactors = FALSE)
source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

ndf<-sqlQuery(lcon, "select px_close, time_stamp from bhav_index where index_name='nifty 50' and time_stamp >= '1991-01-01' and time_stamp <= '2018-12-31'")

pdf(NULL)
plotStreak<-function(ndx, nameFix){
	ndx$RET01<-ifelse(ndx$RET > 0, 1, 0)
	rle01<-rle(as.vector(coredata(ndx$RET01)))

	rle01df<-data.frame(unclass(rle01))
	upRle<-rle01df[rle01df[,2]==1,1]
	dnRle<-rle01df[rle01df[,2]==0,1]

	toPlot<-as.data.frame(cbind(upRle, dnRle))
	plotStart<-first(index(ndx))
	plotEnd<-last(index(ndx))

	ggplot(toPlot) +
		theme_economist() +
		stat_density(aes(upRle), geom="line", position = "identity", na.rm=T, size=0.8, color='green') +
		stat_density(aes(dnRle), geom="line", position = "identity", na.rm=T, size=0.8, color='red') +
		labs(y='density', x='streak length', fill='', color='', title=sprintf("NIFTY 50 %s Return Streaks", nameFix), subtitle=sprintf("%s:%s", plotStart, plotEnd)) +
		annotate("text", x=min(toPlot, na.rm=T), y=0, label = "@StockViz", hjust=0, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.9)
		
	ggsave(sprintf("%s/nifty50.%s.streak.density.png", reportPath, nameFix), width=12, height=6, units="in")
}

ndx<-xts(ndf$px_close, as.Date(ndf$time_stamp))
dRet<-dailyReturn(ndx)
dRet<-dRet[-1]
names(dRet)<-c('RET')

plotStreak(dRet, 'Daily')

dRet<-weeklyReturn(ndx)
dRet<-dRet[-1]
names(dRet)<-c('RET')

plotStreak(dRet, 'Weekly')

dRet<-monthlyReturn(ndx)
dRet<-dRet[-1]
names(dRet)<-c('RET')

plotStreak(dRet, 'Monthly')
library('RODBC')
library('ggplot2')
library('extrafont')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')
library('dplyr')
library('grid')
library('gridExtra')
library('gtable')
library('ggrepel')

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

lookbacks<-c(10, 20, 50, 100, 200)

indexNames<-c("NIFTY 50 TR", "NIFTY NEXT 50 TR", "NIFTY MIDCAP 100 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 100 TR")

startDate<-as.Date("2005-04-01")
endDate<-as.Date("2019-04-30")

txCostPct<-0.2/100 #over transacted notional

annRetDf<-data.frame(INDEX="", LB=0, BH=0.0, GROSS=0.0, NET=0.0)

#indexName<-"NIFTY MIDCAP 100 TR"
#lb<-10

for(indexName in indexNames){
	indexPx<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate, endDate))
	indexPxXts<-xts(indexPx[,1], as.Date(indexPx[,2]))

	for(lb in lookbacks){
		indexXts<-merge(indexPxXts, SMA(indexPxXts, lb))
		names(indexXts)<-c('INDEX', 'SMA')
		indexXts$DRET<-stats::lag(dailyReturn(indexXts$INDEX), -1)

		indexXts$L <- ifelse(indexXts$INDEX >= indexXts$SMA, indexXts$DRET, 0)
		
		indexXts$L_T <- ifelse(indexXts$INDEX >= indexXts$SMA, 1, -1)
		indexXts$L_TT <- sign(indexXts$L_T) != sign(stats::lag(indexXts$L_T, -1)) #1 => a trade occurred

		indexXts<-na.omit(indexXts)
		
		#wealth price series
		indexXts$WEALTH<-cumprod(1+indexXts$L)
		indexXts$WEALTH_TX<-cumprod(1+indexXts$L-txCostPct*indexXts$L_TT)

		#wealth daily return series
		indexXts$WEALTH_RET<-dailyReturn(indexXts$WEALTH)
		indexXts$WEALTH_TX_RET<-dailyReturn(indexXts$WEALTH_TX)

		toPlot<-indexXts[, c('DRET', 'WEALTH_RET', 'WEALTH_TX_RET')]
		names(toPlot)<-c('BH', 'Gross', 'Net')
		
		## plot cumulative returns
		Common.PlotCumReturns(toPlot, sprintf("%s SMA(%d) Returns", indexName, lb), sprintf("%.2f%% total one-way transaction cost", 100*txCostPct), sprintf("%s/%s.%d.cumulative.png", reportPath, indexName, lb))
		
		## accumulate annualized returns
		annRetDf<-rbind(annRetDf, c(indexName, lb, 	round(as.numeric(100*Return.annualized(toPlot$BH)), 2), 
													round(as.numeric(100*Return.annualized(toPlot$Gross)), 2), 
													round(as.numeric(100*Return.annualized(toPlot$Net)), 2)))

		## plot annual returns
		yearlyRets<-100*merge(annualReturn(indexXts$INDEX), annualReturn(indexXts$WEALTH), annualReturn(indexXts$WEALTH_TX))
		names(yearlyRets)<-names(toPlot)
		yearlyRetDf<-data.frame(yearlyRets)
		yearlyRetDf$Y<-year(index(yearlyRets))
		yearlyRetMelt<-melt(yearlyRetDf, id='Y')
		#yearlyRetMelt$Y<-factor(yearlyRetMelt$Y, levels=unique(yearlyRetMelt$Y))
		
		ggplot(yearlyRetMelt, aes(x=Y, y=value, fill=variable)) +
			theme_economist() +
			geom_bar(stat="identity", position=position_dodge()) +
			scale_x_continuous(labels=yearlyRetDf$Y, breaks=yearlyRetDf$Y) +
			geom_text_repel(aes(label= round(value, 2)), position = position_dodge(0.9)) +
			labs(x='', y='(%)', fill='', title=sprintf("%s SMA(%d) Annual Returns", indexName, lb), subtitle=sprintf("[%s:%s]", first(index(toPlot)), last(index(toPlot)))) +
			annotate("text", x=max(yearlyRetDf$Y), y=min(yearlyRets, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
				
		ggsave(sprintf("%s/%s-%d.annual.png", reportPath, indexName, lb), width=16, height=8, units="in")
	}
}

#save annualized returns table
annRetDf<-annRetDf[-1,]
annRetDf$LB<-as.numeric(annRetDf$LB)
annRetDf$BH<-as.numeric(annRetDf$BH)
annRetDf$GROSS<-as.numeric(annRetDf$GROSS)
annRetDf$NET<-as.numeric(annRetDf$NET)

annRetDf$EXCESS<-annRetDf$NET-annRetDf$BH
annRetDf$COST<-annRetDf$GROSS-annRetDf$NET
tt1<-arrangeGrob(tableGrob(annRetDf, rows=NULL, theme=tableTheme), ncol=1, top=textGrob("SMA Strategy Transaction Cost Analysis", gp=gpar(fontsize=12, fontface ='bold', fontfamily='Segoe UI')), bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))

ggsave(sprintf("%s/SMA.Strategy.Transaction.Cost.Analysis.png", reportPath), tt1, width=5.5, height=nrow(annRetDf)*0.32, units='in')
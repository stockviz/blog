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

lookbacks<-c(20, 50)

indexNames<-c("NIFTY MIDCAP 150 TR")

startDate<-as.Date("2005-04-01")
endDate<-as.Date("2019-10-31")
mfBuyLag <- 5 #gap between between sale and buy
mfSellLag <- 75 #minimum business day holding period

txCostPct<-0.2/100 #over transacted notional
txCostPctMf<-0.001/100 #for mutual funds. seller pays.

#################################################################################################################################

annRetDf<-data.frame(INDEX="", LB=0, BH=0.0, GROSS=0.0, NET=0.0)
for(indexName in indexNames){
	indexPx<-sqlQuery(lcon, sprintf("select PX_CLOSE, TIME_STAMP from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate, endDate))
	indexPxXts<-xts(indexPx[,1], as.Date(indexPx[,2]))

	for(lb in lookbacks){
		indexXts<-merge(indexPxXts, SMA(indexPxXts, lb))
		names(indexXts)<-c('INDEX', 'SMA')
		indexXts$DRET<-stats::lag(dailyReturn(indexXts$INDEX), -1)
		
		indexXts <- na.omit(indexXts)
		indexXts$L <- 0
		indexXts$L_T <- 0
		prevBuyIndex <- 1
		prevSellIndex <- 1
		isLong <- FALSE
		for(i in 1:nrow(indexXts)){
			if(coredata(indexXts$INDEX[i]) >= coredata(indexXts$SMA[i])){
				if(isLong){
					indexXts$L[i] <- coredata(indexXts$DRET[i])
				} else if ((i - prevSellIndex) >= mfBuyLag){
					indexXts$L[i] <- coredata(indexXts$DRET[i])
					prevBuyIndex <- i
					indexXts$L_T[i] <- 1
					isLong <- TRUE
				}
			}
			
			if(coredata(indexXts$INDEX[i]) < coredata(indexXts$SMA[i])){
				if(!isLong){
					indexXts$L[i] <- 0
				} else if ((i - prevBuyIndex) >= mfSellLag){
					indexXts$L[i] <- 0
					prevSellIndex <- i
					indexXts$L_T[i] <- -1
					isLong <- FALSE
				}
			}
		}

		indexXts$L_TT <- sign(indexXts$L_T) != sign(stats::lag(indexXts$L_T, -1)) #1 => a trade occurred
		indexXts<-na.omit(indexXts)
		
		#print(head(indexXts, 100))
		
		#wealth price series
		indexXts$WEALTH<-cumprod(1+indexXts$L)
		indexXts$WEALTH_TX<-cumprod(1+indexXts$L-txCostPctMf*indexXts$L_TT)

		#wealth daily return series
		indexXts$WEALTH_RET<-dailyReturn(indexXts$WEALTH)
		indexXts$WEALTH_TX_RET<-dailyReturn(indexXts$WEALTH_TX)

		toPlot<-indexXts[, c('DRET', 'WEALTH_RET', 'WEALTH_TX_RET')]
		names(toPlot)<-c('BH', 'Gross', 'Net')
		
		## plot cumulative returns
		Common.PlotCumReturns(toPlot, sprintf("%s SMA(%d) Returns (MF)", indexName, lb), sprintf("%.4f%% total one-way transaction cost %d/%d", 100*txCostPctMf, mfBuyLag, mfSellLag), sprintf("%s/LAG.%s.%d.cumulative.png", reportPath, indexName, lb))
		
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
			labs(x='', y='(%)', fill='', title=sprintf("%s SMA(%d) Annual Returns", indexName, lb), subtitle=sprintf("[%s:%s] %d/%d", first(index(toPlot)), last(index(toPlot)), mfBuyLag, mfSellLag)) +
			annotate("text", x=max(yearlyRetDf$Y), y=min(yearlyRets, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
				
		ggsave(sprintf("%s/LAG.%s-%d.annual.png", reportPath, indexName, lb), width=16, height=8, units="in")
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

ggsave(sprintf("%s/LAG.SMA.Strategy.Transaction.Cost.Analysis.png", reportPath), tt1, width=5.5, height=nrow(annRetDf)*0.75, units='in')

####################################################################################################

annRetDf<-data.frame(INDEX="", LB=0, BH=0.0, GROSS=0.0, NET=0.0)
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
		Common.PlotCumReturns(toPlot, sprintf("%s SMA(%d) Returns", indexName, lb), sprintf("%.2f%% total one-way transaction cost", 100*txCostPct), sprintf("%s/INDEX.%s.%d.cumulative.png", reportPath, indexName, lb))
		
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
				
		ggsave(sprintf("%s/INDEX.%s-%d.annual.png", reportPath, indexName, lb), width=16, height=8, units="in")
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

ggsave(sprintf("%s/INDEX.SMA.Strategy.Transaction.Cost.Analysis.png", reportPath), tt1, width=5.5, height=nrow(annRetDf)*0.32, units='in')



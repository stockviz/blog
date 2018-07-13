library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')


args = commandArgs(TRUE)
options(stringsAsFactors = FALSE)
source("d:/stockviz/r/config.r")

reportPath <- "D:/StockViz/public/blog/simple momentum"
transactionCost<-0.25/100
taxOnGains<-0.10

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexName<-'NIFTY 50'
if(!is.na(args[1])){
	indexName<-args[1]
} 

print(indexName)
tenor<-'5_10'

iPx<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE from BHAV_INDEX where INDEX_NAME='%s' and TIME_STAMP >= '2004-01-01'", indexName))
tPx<-sqlQuery(lcon, sprintf("select TIME_STAMP, TRI from INDEX_CCIL_TENOR where INDEX_NAME='%s' and TIME_STAMP >= '2004-01-01'", tenor))

iXts<-xts(iPx[,2], as.Date(iPx[,1]))
tXts<-xts(tPx[,2], as.Date(tPx[,1]))

tempXts<-merge(iXts, tXts)
tempXts<-na.locf(tempXts)

eqMRET<-monthlyReturn(tempXts[,1])
bMRET<-monthlyReturn(tempXts[,2])

allXts<-merge(eqMRET, bMRET)
names(allXts)<-c('EQUITY_MRET', 'BOND_MRET')
allXts$EQ_MRET12<-rollapply(allXts$EQUITY_MRET, 12, Return.cumulative)
allXts$B_MRET12<-rollapply(allXts$BOND_MRET, 12, Return.cumulative)

allXts$EQ_MRET_LAG1<-lag(allXts$EQUITY_MRET, -1)
allXts$B_MRET_LAG1<-lag(allXts$BOND_MRET, -1)

allXts<-na.omit(allXts)

allXts$STRAT<-ifelse(allXts$EQ_MRET12 > allXts$B_MRET12, allXts$EQ_MRET_LAG1, allXts$B_MRET_LAG1)

#keep track of when the switches occur
allXts$INST<-ifelse(allXts$EQ_MRET12 > allXts$B_MRET12, -1, 1)
allXts$TOGGLE<-allXts$INST+lag(allXts$INST,1)
allXts$TOGGLE[1]<-0

allXts$WEALTH<-cumprod(1+allXts$STRAT) #gross
allXts$WEALTH_TX<-cumprod(1- ifelse(allXts$TOGGLE == 0, transactionCost,0) + allXts$STRAT) #after transactionCost

#to be accurate, this should rebase WEALTH_TX with the after-tax value. However, since this gets divided by the current WEALTH_TX value, it shouldn't make a very big difference.
allXts$TAX_DRAG<-ifelse(allXts[allXts$TOGGLE==0,]$WEALTH_TX-lag(allXts[allXts$TOGGLE==0,]$WEALTH_TX, 1) > 0, allXts[allXts$TOGGLE==0,]$WEALTH_TX-lag(allXts[allXts$TOGGLE==0,]$WEALTH_TX, 1), 0)*taxOnGains/allXts$WEALTH_TX

allXts[is.na(allXts$TAX_DRAG),]$TAX_DRAG<-0
allXts$WEALTH_TX_TAX<-cumprod(1- ifelse(allXts$TOGGLE == 0, transactionCost,0) - allXts$TAX_DRAG + allXts$STRAT) #after transactionCost and tax

#convert wealth to returns.
allXts$STRAT_GROSS<-allXts$WEALTH/lag(allXts$WEALTH, 1)-1
allXts$STRAT_TX<-allXts$WEALTH_TX/lag(allXts$WEALTH_TX, 1)-1
allXts$STRAT_TX_TAX<-allXts$WEALTH_TX_TAX/lag(allXts$WEALTH_TX_TAX, 1)-1

toPlot<-na.omit(merge(allXts$STRAT_GROSS, allXts$STRAT_TX, allXts$STRAT_TX_TAX, allXts$EQ_MRET_LAG1))
names(toPlot)<-c('STRATEGY', 'STRATEGY after TX costs', 'STRATEGY after TX costs and taxes', indexName)

png(sprintf("%s/simple.momentum.cumulative.tx.%s.png", reportPath, indexName), bg = "white", width = 1200, height = 600)
par(family='Segoe UI')
charts.PerformanceSummary(toPlot, main=NA, cex.legend=1.5)
title(sprintf("Simple Momentum (%s/%s) @StockViz", indexName, tenor))
mtext(sprintf("%.2f%%/%.2f%%/%.2f%%/%.2f%%", 100*Return.cumulative(toPlot[,1]), 100*Return.cumulative(toPlot[,2]), 100*Return.cumulative(toPlot[,3]), 100*Return.cumulative(toPlot[,4])), family='Segoe UI')
mtext(sprintf("transaction cost: %.2f%%/tax: %.2f%%", 100*transactionCost, 100*taxOnGains), line=-1, family='Segoe UI', cex=0.8)
dev.off()

yearlyReturns<-rollapply(toPlot, 12, Return.cumulative)
yearlyReturns<-na.omit(yearlyReturns[.indexmon(yearlyReturns) == 11])
yearlyReturns<-100*yearlyReturns
yearlyReturns$Y<-year(index(yearlyReturns))
toPlotDf<-data.frame(yearlyReturns)

ggplot(data=melt(toPlotDf, id='Y'), aes(x=Y, y=value, fill=variable)) +
	theme_fivethirtyeight() +
	geom_bar(stat="identity", position=position_dodge()) + 
	scale_x_continuous(breaks=toPlotDf$Y) +
	geom_text(aes(label=round(value, 2)), vjust=1.6, color="black", position = position_dodge(0.9), size=2.5) +
	ylab("Returns (%)") +
	xlab("Year") +
	labs(fill="", title=sprintf("Simple Momentum Annual Returns (%s/%s)", indexName, tenor)) 
	
ggsave(sprintf('%s/simple.momentum.annual.tx.%s.png', reportPath, indexName), width=25, height=12, units="in", device="png")
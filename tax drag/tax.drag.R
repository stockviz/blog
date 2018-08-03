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

reportPath <- "D:/StockViz/public/blog/tax drag"
taxOnGains<-0.10

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexName<-'NIFTY 50'
if(!is.na(args[1])){
	indexName<-args[1]
} 

print(indexName)

iPx<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE from BHAV_INDEX where INDEX_NAME='%s' and TIME_STAMP >= '1996-01-01'", indexName))
iXts<-xts(iPx[,2], as.Date(iPx[,1]))

eqYRET<-annualReturn(iXts)
names(eqYRET)<-c('RET')

eqYRET$WEALTH<-cumprod(1+eqYRET$RET) #gross
eqYRET$TAX_DRAG<-ifelse(eqYRET$WEALTH-lag(eqYRET$WEALTH$WEALTH, 1) > 0, eqYRET$WEALTH-lag(eqYRET$WEALTH, 1), 0)*taxOnGains/eqYRET$WEALTH
eqYRET[is.na(eqYRET$TAX_DRAG),]$TAX_DRAG<-0

eqYRET$WEALTH_TAX<-cumprod(1 - eqYRET$TAX_DRAG + eqYRET$RET) #after tax

eqYRET$RET_GROSS<-eqYRET$WEALTH/lag(eqYRET$WEALTH, 1)-1
eqYRET$RET_TAX<-eqYRET$WEALTH_TAX/lag(eqYRET$WEALTH_TAX, 1)-1

toPlot<-na.omit(merge(eqYRET$RET_GROSS, eqYRET$RET_TAX))
names(toPlot)<-c('gross', 'after tax')

png(sprintf("%s/simple.after-tax.%s.png", reportPath, indexName), bg = "white", width = 1200, height = 600)
par(family='Segoe UI')
charts.PerformanceSummary(toPlot, main=NA, cex.legend=1.5)
title(sprintf("Gross vs. After-tax returns (%s) @StockViz", indexName))
mtext(sprintf("%.2f%%/%.2f%%", 100*Return.cumulative(toPlot[,1]), 100*Return.cumulative(toPlot[,2])), family='Segoe UI')
dev.off()

toPlot<-100*toPlot
toPlot$Y<-year(index(toPlot))
toPlotDf<-data.frame(toPlot)

ggplot(data=melt(toPlotDf, id='Y'), aes(x=Y, y=value, fill=variable)) +
	theme_fivethirtyeight() +
	geom_bar(stat="identity", position=position_dodge()) + 
	scale_x_continuous(breaks=toPlotDf$Y) +
	geom_text(aes(label=round(value, 2)), vjust=1.6, color="black", position = position_dodge(0.9), size=2.5) +
	ylab("Returns (%)") +
	xlab("Year") +
	labs(fill="", title=sprintf("Simple After-tax Annual Returns (%s)", indexName)) 
	
ggsave(sprintf('%s/simple.after-tax.annual.%s.png', reportPath, indexName), width=25, height=12, units="in", device="png")
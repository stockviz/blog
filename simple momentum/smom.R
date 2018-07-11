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
allXts$B_MRET1<-lag(allXts$BOND_MRET, -1)

allXts$STRAT<-ifelse(allXts$EQ_MRET12 > allXts$B_MRET12, allXts$EQ_MRET_LAG1, allXts$B_MRET1)

toPlot<-na.omit(merge(allXts$STRAT, allXts$EQ_MRET_LAG1))
names(toPlot)<-c('STRATEGY', indexName)

png(sprintf("%s/simple.momentum.cumulative.%s.png", reportPath, indexName), bg = "white", width = 1200, height = 600)
par(family='Segoe UI')
charts.PerformanceSummary(toPlot, main=NA, cex.legend=1.5)
title(sprintf("Simple Momentum (%s/%s) @StockViz", indexName, tenor))
mtext(sprintf("%.2f%%/%.2f%%", 100*Return.cumulative(toPlot[,1]), 100*Return.cumulative(toPlot[,2])), family='Segoe UI')
dev.off()

tdStrat<-table.Drawdowns(toPlot[,1], 10)
tdBh<-table.Drawdowns(toPlot[,2], 10)

mytheme <- ttheme_minimal(
		core = list(fg_params=list(fontfamily='Segoe UI')),
		colhead = list(fg_params=list(fontfamily='Segoe UI')),
		rowhead = list(fg_params=list(fontfamily='Segoe UI')))

t1<-arrangeGrob(tableGrob(tdStrat, rows=NULL, theme=mytheme), ncol=1, top = textGrob("STRATEGY",gp=gpar(fontsize=12, fontfamily='Segoe UI')))
t2<-arrangeGrob(tableGrob(tdBh, rows=NULL, theme=mytheme), ncol=1, top = textGrob(indexName,gp=gpar(fontsize=12, fontfamily='Segoe UI')))

tt1<-arrangeGrob(grobs=list(t1, t2), ncol=1, top=textGrob(sprintf("Simple Momentum Drawdowns (%s/%s)", indexName, tenor), gp=gpar(fontsize=14, fontface ='bold', fontfamily='Segoe UI')), bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))

ggplot2::ggsave(sprintf('%s/simple.momentum.drawdowns.%s.png', reportPath, indexName), tt1, width=7, height=8, units='in')

yearlyReturns<-rollapply(toPlot, 12, Return.cumulative)
yearlyReturns<-na.omit(yearlyReturns[.indexmon(yearlyReturns) == 11])
yearlyReturns<-100*yearlyReturns
yearlyReturns$Y<-year(index(yearlyReturns))
toPlotDf<-data.frame(yearlyReturns)

ggplot(data=melt(toPlotDf, id='Y'), aes(x=Y, y=value, fill=variable)) +
	theme_fivethirtyeight() +
	geom_bar(stat="identity", position=position_dodge()) + 
	scale_x_continuous(breaks=toPlotDf$Y) +
	geom_text(aes(label=round(value, 0)), vjust=1.6, color="black", position = position_dodge(0.9), size=2.5) +
	ylab("Returns (%)") +
	xlab("Year") +
	labs(fill="", title=sprintf("Simple Momentum Annual Returns (%s/%s)", indexName, tenor)) 
	
ggsave(sprintf('%s/simple.momentum.annual.%s.png', reportPath, indexName), width=25, height=12, units="in", device="png")
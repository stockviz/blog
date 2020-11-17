source("D:/stockviz/r/config.r")
reportPath <- "D:/StockViz/public/blog/volatility/vix-match"
library('RODBC')
library('tidyverse')
library('reshape2')
library('ggthemes')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggpubr')
library('grid')
library('gridExtra')
library('gtable')

source("D:/StockViz/public/blog/common/misc.R")

options(stringsAsFactors = FALSE)
options("scipen"=100)
pdf(NULL)

segLength <- 20

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

vixDf <- sqlQuery(lcon, "select time_stamp, px_close from vix_history")
vixXts <- xts(vixDf[,2], vixDf[,1])

lastX <- tail(vixXts, segLength)

distXts <- rollapply(vixXts, segLength, function(X) stats::dist(t(data.frame(a=coredata(X), b=coredata(lastX)))), by.column=F)

distDf <- data.frame(distXts)
distDf$T <- index(distXts)

smlDist <- head(distDf[order(distDf),])
names(smlDist) <- c('DIST', 'T')
tt1<-arrangeGrob(tableGrob(smlDist, rows=NULL, theme=tableTheme), ncol=1, top=textGrob("India VIX Euclidean Distance", gp=gpar(fontsize=12, fontface ='bold', fontfamily='Segoe UI')), bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))

ggsave(sprintf("%s/closest-euclidean.png", reportPath), tt1, width=3, height=nrow(smlDist)*0.5, units='in')

matchEnd <- as.Date("2010-11-02")

toPlotXts <- vixXts[sprintf("%s/%s", matchEnd-100, matchEnd+100),]
names(toPlotXts) <- c('VIX')
toPlotXts$C <- 0
toPlotXts[sprintf("%s/%s", index(first(tail(vixXts[sprintf("/%s", matchEnd)], segLength))), matchEnd),]$C <- 1

toPlot <- data.frame(toPlotXts)
toPlot$T <- index(toPlotXts)

p1 <- ggplot(toPlot, aes(x=T, y=VIX)) +
	theme_economist() +
	geom_line(aes(color=C), size=1) +
	guides(color=F) +
	scale_x_date(breaks='1 month') +
	labs(x='', y='VIX', color='', title="", subtitle=sprintf("[%s:%s]", first(index(toPlotXts)), last(index(toPlotXts)))) 
	
toPlotXts <- vixXts[sprintf("%s/%s", last(index(lastX))-200, last(index(lastX))+200),]
names(toPlotXts) <- c('VIX')
toPlotXts$C <- 0
toPlotXts[sprintf("%s/%s", index(first(tail(vixXts[sprintf("/%s", last(index(lastX)))], segLength))), last(index(lastX))),]$C <- 1

toPlot <- data.frame(toPlotXts)
toPlot$T <- index(toPlotXts)

p2 <- ggplot(toPlot, aes(x=T, y=VIX)) +
	theme_economist() +
	geom_line(aes(color=C), size=1) +
	guides(color=F) +
	scale_x_date(breaks='1 month') +
	labs(x='', y='VIX', color='', title="", subtitle=sprintf("[%s:%s]", first(index(toPlotXts)), last(index(toPlotXts)))) 
	
figure <- ggarrange(p1, p2, ncol=1, nrow=2)	
annotate_figure(figure, top = text_grob('INDIA VIX', face = "bold", size = 14, family='Segoe UI'), bottom = text_grob("@StockViz", face="bold", size=12, family="Segoe UI", color='grey'))
	
ggsave(sprintf("%s/closest-match-euclidean.png", reportPath), width=16, height=14, units="in")
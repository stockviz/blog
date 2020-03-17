library('RODBC')
library(tidyverse)

library(ggthemes)
library(quantmod)
library(PerformanceAnalytics)
library(reshape2)
library(ggrepel)
library(lubridate)

library('grid')
library('gridExtra')
library('gtable')

options("scipen"=999)
options(stringsAsFactors = FALSE)
source("d:/stockviz/r/config.R")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")
options(repr.plot.width=16, repr.plot.height=8)

reportPath <- "."
lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

#parameters

indexName <- 'NIFTY 50 TR'
smaLb <- 50 #number of days
tradeCost <- 0.001/100 + 0.05/100 #stt of 0.001% on the sell side and slippage of 0.05%
startDate <- as.Date("2005-04-01")

rfRate <- 0.1
rfRateMonthly = ((rfRate + 1)^(1/12)) - 1
rfRateDaily <- ((rfRate + 1)^(1/365)) - 1

args <- commandArgs(TRUE)
if(!is.na(args[1])){
	indexName <- args[1]
}

if(!is.na(args[2])){
	smaLb <- as.numeric(args[2])
}

sprintf("processing: %s/%d", indexName, smaLb)

#load data

indexDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s'", indexName, startDate))
pXts <- xts(indexDf[,2], indexDf[,1])
names(pXts) <- c('INDEX')
pXts$SMA <- SMA(pXts[,1], smaLb)
pXts$D_RET <- dailyReturn(pXts$INDEX)

pXts <- na.omit(pXts)

pXts$D_RET_LAG <- stats::lag(pXts$D_RET, -1)
pXts$D_RET_PCT <- pXts$D_RET * 100

yrs <- sort(unique(year(index(pXts))))

#Daily returns above and below SMA: averages and standard deviation by year

statByYear <- data.frame(Y = 0, N_ABOVE = 0, N_BELOW = 0, AVG_ABOVE = 0.0, AVG_BELOW = 0.0, SD_ABOVE = 0.0, SD_BELOW = 0.0)

for(yr in yrs){
    yrStr <- toString(yr)
    numAbove <- as.numeric(sum(ifelse(pXts[yrStr,'INDEX'] > pXts[yrStr,'SMA'], 1, 0)))
    numBelow <- as.numeric(sum(ifelse(pXts[yrStr,'INDEX'] <= pXts[yrStr,'SMA'], 1, 0)))
    
    avgAbove <- as.numeric(mean(pXts[pXts[yrStr,'INDEX'] > pXts[yrStr,'SMA'], 'D_RET_PCT']))
    avgBelow <- as.numeric(mean(pXts[pXts[yrStr,'INDEX'] <= pXts[yrStr,'SMA'], 'D_RET_PCT']))
    
    sdAbove <- as.numeric(sd(pXts[pXts[yrStr,'INDEX'] > pXts[yrStr,'SMA'], 'D_RET_PCT']))
    sdBelow <- as.numeric(sd(pXts[pXts[yrStr,'INDEX'] <= pXts[yrStr,'SMA'], 'D_RET_PCT']))
    
    statByYear <- rbind(statByYear, c(yr, numAbove, numBelow, avgAbove, avgBelow, sdAbove, sdBelow))
}

statByYear <- statByYear[-1,]
for(j in 4:ncol(statByYear)){
	statByYear[,j] <- round(as.numeric(statByYear[,j]), 2)
}

tt1<-arrangeGrob(tableGrob(statByYear, rows=NULL, theme=tableTheme), ncol=1, top=textGrob(sprintf("%s %d-SMA Daily Return Statistics", indexName, smaLb), gp=gpar(fontsize=12, fontface ='bold', fontfamily='Segoe UI')), bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))

ggsave(sprintf("%s/%s.%d.SMA.Yearly.Returns.png", reportPath, indexName, smaLb), tt1, width=ncol(statByYear)*1.10, height=nrow(statByYear)*0.32, units='in')

#

subsetPxts <- pXts

#Daily returns above and below SMA: density plot

abvBlw <- merge(subsetPxts[subsetPxts[,'INDEX'] > subsetPxts[,'SMA'], 'D_RET_LAG'],
                subsetPxts[subsetPxts[,'INDEX'] <= subsetPxts[,'SMA'], 'D_RET_LAG'])

names(abvBlw) <- c('above', 'below')
abvBlw <- merge(abvBlw, abvBlw['2010/',])
names(abvBlw) <- c('above', 'below', 'above2010', 'below2010')
abvBlw <- 100* abvBlw
toPlot <- melt(data.frame(abvBlw))
plotStart <- first(index(abvBlw))
plotEnd <- last(index(abvBlw))

ggplot(toPlot) +
    theme_economist() +
    stat_density(aes(x=value, color=variable), geom="line", position = "identity", na.rm=T) +
    labs(y='density', x='returns (%)', fill='', color='', 
         title = sprintf("%s Next-Day Return Density", indexName), 
         subtitle = sprintf("%d-SMA [%s:%s]", smaLb, plotStart, plotEnd)) +
    annotate("text", x=min(toPlot$value, na.rm=T), y=0, label = "@StockViz", 
             hjust=0, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.9)

ggsave(sprintf("%s/%s.%d.SMA.daily-return.Density.png", reportPath, indexName, smaLb), width=16, height=8, units='in')
			 
#Rolling returns above and below SMA and their standard deviation

rollN <- 200

stat200 <- rollapply(subsetPxts, rollN, function(X){
    numAbove <- as.numeric(sum(ifelse(X[,'INDEX'] > X[,'SMA'], 1, 0)))
    numBelow <- as.numeric(sum(ifelse(X[,'INDEX'] <= X[,'SMA'], 1, 0)))
    
    avgAbove <- as.numeric(mean(X[X[,'INDEX'] > X[,'SMA'], 'D_RET_PCT']))
    avgBelow <- as.numeric(mean(X[X[,'INDEX'] <= X[,'SMA'], 'D_RET_PCT']))
    
    sdAbove <- as.numeric(sd(X[X[,'INDEX'] > X[,'SMA'], 'D_RET_PCT']))
    sdBelow <- as.numeric(sd(X[X[,'INDEX'] <= X[,'SMA'], 'D_RET_PCT']))
    
    xts(matrix(c(numAbove, numBelow, avgAbove, avgBelow, sdAbove, sdBelow), nrow=1), as.Date(last(index(X))))
}, by.column = FALSE)

stat200 <- na.omit(stat200)
names(stat200) <- c('N_ABOVE', 'N_BELOW', 'AVG_ABOVE', 'AVG_BELOW', 'SD_ABOVE', 'SD_BELOW')

stat200Df <- data.frame(stat200)
stat200Df$T <- as.Date(index(stat200))

plotStart <- first(index(stat200))
plotEnd <- last(index(stat200))
xAxisTicks<-seq(plotStart, plotEnd, length.out=10)

ggplot(stat200Df, aes(x=T)) +
    theme_economist() +
    geom_line(aes(y=AVG_ABOVE, color='a')) +
    geom_ribbon(aes(ymin = AVG_ABOVE - SD_ABOVE, ymax = AVG_ABOVE + SD_ABOVE), fill='springgreen', alpha=0.3) +
    geom_line(aes(y=AVG_BELOW, color='b')) +
    geom_ribbon(aes(ymin = AVG_BELOW - SD_BELOW, ymax = AVG_BELOW + SD_BELOW), fill='darkorange', alpha=0.3) +
    scale_x_date(breaks = xAxisTicks) +
    labs(x = "", y="returns(%)", fill="", color="", 
         title = sprintf("%s Rolling %d-day Returns Above and Below %d-SMA", indexName, rollN, smaLb),
         subtitle = sprintf("[%s:%s]", smaLb, plotStart, plotEnd)) +
    scale_colour_manual(name = '', values =c('a'='olivedrab','b'='maroon'), labels = c('above','below')) +
    annotate("text", x=plotEnd, y=min(stat200Df$AVG_ABOVE - stat200Df$SD_ABOVE), label = "@StockViz", 
             hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)

ggsave(sprintf("%s/%s.%d.SMA.%d-day.Rolling.Returns.png", reportPath, indexName, smaLb, rollN), width=16, height=8, units="in")

#stay long the index only if it is trading above SMA

longOnly50 <- merge(ifelse(subsetPxts[,'INDEX'] > subsetPxts[,'SMA'], subsetPxts[,'D_RET_LAG'], 0), 
                    ifelse(subsetPxts[,'INDEX'] > subsetPxts[,'SMA'], 1, 0), 
                    subsetPxts[,'D_RET_LAG'])
names(longOnly50) <- c('SMA_L', 'L', 'BH')
longOnly50$TRADE <- longOnly50$L - stats::lag(longOnly50$L, -1)
longOnly50 <- na.omit(longOnly50)
longOnly50$CUM_L_NET <- cumprod(1 + longOnly50$SMA_L - ifelse(longOnly50$TRADE == 1, tradeCost, 0))
longOnly50$SMA_L_NET <- dailyReturn(longOnly50$CUM_L_NET)

#print(head(longOnly50, 200))
longOnly50 <- longOnly50[-1,]

#plot trades over the index

temp <- merge(subsetPxts[,'INDEX'], longOnly50$TRADE)
temp$BUY <- ifelse(temp[,2] == -1, temp[,1], NA)
temp$SELL <- ifelse(temp[,2] == 1, temp[,1], NA)

tradePlot <- data.frame(temp[, c('INDEX', 'BUY', 'SELL')]) 
tradePlot$T <- index(temp)

firstDate<-first(index(temp))
lastDate<-last(index(temp))
xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=20)
	
ggplot(tradePlot, aes(x=T)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_line(aes(y=INDEX)) +
	geom_point(aes(y=BUY), color='darkgreen', fill='darkgreen', shape=24) +
	geom_point(aes(y=SELL), color='maroon', fill='maroon', shape=25) + 
	scale_x_date(breaks = xAxisTicks) +
	scale_y_log10() +
	labs(x='', y=sprintf('log(%s)', indexName), color='', 
		title=sprintf("%s %d-SMA Trades", indexName, smaLb), 
		subtitle=sprintf("[%s:%s]", firstDate, lastDate)) +
	annotate("text", x=lastDate, y=min(tradePlot$INDEX), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)

ggsave(sprintf("%s/%s.%d.SMA.Trades.png", reportPath, indexName, smaLb), width=16, height=8, units="in")			 	

#####

longOnly50 <- na.omit(longOnly50[, c('SMA_L','SMA_L_NET','BH')])

Common.PlotCumReturns(longOnly50, sprintf("%s %d-SMA Regime Returns", indexName, smaLb), 'Long-Only', sprintf("%s/%s.%d.SMA.long-only.backtest.png", reportPath, indexName, smaLb))
Common.PlotCumReturns(longOnly50["2010/",], sprintf("%s %d-SMA Regime Returns", indexName, smaLb), 'Long-Only', sprintf("%s/%s.%d.SMA.long-only.backtest.2010.png", reportPath, indexName, smaLb))

#

returnsByYear <- data.frame(Y = 0, SMA_L = 0.0, SMA_L_NET = 0.0, BH = 0.0)

for(yr in yrs){
    yrStr <- toString(yr)
    if (length(longOnly50[yrStr, 1]) < 50) next
    returnsByYear <- rbind(returnsByYear, c(yr, as.numeric(Return.cumulative(longOnly50[yrStr, 'SMA_L'])), 
												as.numeric(Return.cumulative(longOnly50[yrStr, 'SMA_L_NET'])),
                                                as.numeric(Return.cumulative(longOnly50[yrStr, 'BH']))))
}

returnsByYear <- returnsByYear[-1,]
returnsByYear[,1] <- as.numeric(returnsByYear[,1])
returnsByYear[,2] <- as.numeric(returnsByYear[,2])*100.0
returnsByYear[,3] <- as.numeric(returnsByYear[,3])*100.0
returnsByYear[,4] <- as.numeric(returnsByYear[,4])*100.0

mDf <- melt(returnsByYear, id='Y')
ggplot(mDf, aes(x=Y, y=value, fill=variable)) +
    theme_economist() +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_x_continuous(labels=returnsByYear$Y, breaks=returnsByYear$Y) +
    geom_text_repel(aes(label= round(value, 2)), position = position_dodge(0.9)) +
    labs(x='', y='(%)', fill='', title=sprintf("%s %d-SMA Regime Returns", indexName, smaLb), 
         subtitle="Long-Only") +
    annotate("text", x=max(returnsByYear$Y), y=min(mDf$value), 
             label = "@StockViz", hjust=1.1, vjust=-1.1, 
             col="white", cex=6, fontface = "bold", alpha = 0.8)  

ggsave(sprintf("%s/%s.%d.SMA.long-only.backtest.annual-returns.png", reportPath, indexName, smaLb, rollN), width=16, height=8, units="in")	

## CALCULATE VaR

metricDf <- rbind(PainIndex(longOnly50[, c('SMA_L_NET', 'BH')]),
VaR(longOnly50[, c('SMA_L_NET', 'BH')], invert=F),
StdDev.annualized(longOnly50[, c('SMA_L_NET', 'BH')]))

metricDf <- data.frame(metricDf)

for(j in 1:ncol(metricDf)){
	metricDf[,j] <- round(as.numeric(metricDf[,j]), 5)
}

metricDf <- tibble::rownames_to_column(metricDf, "VALUE")

tt1<-arrangeGrob(tableGrob(metricDf, rows=NULL, theme=tableTheme), ncol=1, top=textGrob(sprintf("%s %d-SMA Metrics", indexName, smaLb), gp=gpar(fontsize=12, fontface ='bold', fontfamily='Segoe UI')), bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))

ggsave(sprintf("%s/%s.%d.SMA.metrics.png", reportPath, indexName, smaLb), tt1, width=5, height=nrow(metricDf)*0.75, units='in')

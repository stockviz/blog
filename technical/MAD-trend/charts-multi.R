library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')	

library('tidyverse')
library('tidyquant')

library('lubridate')
library('reshape2')
library('viridis')
library('ggthemes')

library(gridExtra)

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")

reportPath <- "."
pdf(NULL)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

load(sprintf("%s/multi.monthly-returns-vol.Rdata", reportPath)) #retDf


sharpePre <- retDf %>% filter(TIME_STAMP <= as.Date("2019-12-31")) %>% group_by(LB_SMALL, LB_LARGE) %>% 
			tq_performance(RET, performance_fun=SharpeRatio.annualized) %>% rename(SHARPE = 3)
			
annPerfPre <- retDf %>% filter(TIME_STAMP <= as.Date("2019-12-31")) %>% group_by(LB_SMALL, LB_LARGE) %>% 
			tq_performance(RET, performance_fun=Return.annualized) %>% rename(RET = 3)
			
statsPre <- sharpePre %>% left_join(annPerfPre, by=c('LB_SMALL', 'LB_LARGE'))

bestStatsPre <- statsPre %>% ungroup() %>% slice_max(RET, n=10) %>% slice_max(SHARPE, n=5) %>% mutate(RET = round(RET*100, 2), SHARPE = round(SHARPE, 2))

png(sprintf("%s/mad-multi-best.pre.png", reportPath), width = 4, height = 3, units='in', res=72)
p <- tableGrob(bestStatsPre)
grid.arrange(top = "Top MAD Trend Returns/Sharpe", p, bottom='@StockViz')
dev.off()

statsPre$color <- NA

medianRetPre <- median(statsPre$RET)
reds <- colorRampPalette(c("darkred", "#FFFDD0"))(nrow(statsPre[!is.na(statsPre$RET) & statsPre$RET < medianRetPre,]))
greens <- colorRampPalette(c("#FFFDD0", "darkgreen"))(nrow(statsPre[!is.na(statsPre$RET) & statsPre$RET >= medianRetPre,]))
statsPre[order(statsPre$RET),]$color <- c(reds, greens, rep(NA, nrow(statsPre) - length(reds) - length(greens)))

statsPre %>% ggplot(aes(x=factor(LB_SMALL), y=factor(LB_LARGE), fill=color)) +
	theme_economist() +
	geom_tile() +
	geom_text(aes(label=sprintf("%.2f/%.2f", RET*100, SHARPE)), hjust = "center", size=3.5) +
	scale_fill_identity() +
	guides(fill="none") +
	labs(x='small', y='large', title="MAD Trend Returns/Sharpe", subtitle="pre-COVID", caption='@StockViz') 

ggsave(sprintf("%s/mad-multi-ret.pre.png", reportPath), width=16, height=8, units="in")
	
			
sharpePost <- retDf %>% filter(TIME_STAMP >= as.Date("2020-05-01")) %>% group_by(LB_SMALL, LB_LARGE) %>% 
			tq_performance(RET, performance_fun=SharpeRatio.annualized) %>% rename(SHARPE = 3)
			
annPerfPost <- retDf %>% filter(TIME_STAMP >= as.Date("2020-05-01")) %>% group_by(LB_SMALL, LB_LARGE) %>% 
			tq_performance(RET, performance_fun=Return.annualized) %>% rename(RET = 3)
			
statsPost <- sharpePost %>% left_join(annPerfPost, by=c('LB_SMALL', 'LB_LARGE'))

bestStatsPost <- statsPost %>% ungroup() %>% slice_max(RET, n=10) %>% slice_max(SHARPE, n=5) %>% mutate(RET = round(RET*100, 2), SHARPE = round(SHARPE, 2))

png(sprintf("%s/mad-multi-best.post.png", reportPath), width = 4, height = 3, units='in', res=72)
p <- tableGrob(bestStatsPost)
grid.arrange(top = "Top MAD Trend Returns/Sharpe", p, bottom='@StockViz')
dev.off()

statsPost$color <- NA

medianRetPre <- median(statsPost$RET)
reds <- colorRampPalette(c("darkred", "#FFFDD0"))(nrow(statsPost[!is.na(statsPost$RET) & statsPost$RET < medianRetPre,]))
greens <- colorRampPalette(c("#FFFDD0", "darkgreen"))(nrow(statsPost[!is.na(statsPost$RET) & statsPost$RET >= medianRetPre,]))
statsPost[order(statsPost$RET),]$color <- c(reds, greens, rep(NA, nrow(statsPost) - length(reds) - length(greens)))

statsPost %>% ggplot(aes(x=factor(LB_SMALL), y=factor(LB_LARGE), fill=color)) +
	theme_economist() +
	geom_tile() +
	geom_text(aes(label=sprintf("%.2f/%.2f", RET*100, SHARPE)), hjust = "center", size=3.5) +
	scale_fill_identity() +
	guides(fill="none") +
	labs(x='small', y='large', title="MAD Trend Returns/Sharpe", subtitle="post-COVID", caption='@StockViz') 

ggsave(sprintf("%s/mad-multi-ret.post.png", reportPath), width=16, height=8, units="in")

###############################################################

topLbSmall <- bestStatsPre$LB_SMALL[1]
topLbLarge <- bestStatsPre$LB_LARGE[1]

symRets <- retDf %>% filter(LB_SMALL == topLbSmall & LB_LARGE == topLbLarge) %>% select(RET, TIME_STAMP) 
symRets <- xts(symRets[,1], symRets[,2])

nDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", "NIFTY 200 TR", 
								first(index(symRets)) - 10, last(index(symRets))))
n100Xts <- xts(nDf[,2], nDf[,1])

nDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", "NIFTY MIDCAP 150 TR", 
								first(index(symRets)) - 10, last(index(symRets))))
m150Xts <- xts(nDf[,2], nDf[,1])

nDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", "NIFTY SMALLCAP 50 TR", 
								first(index(symRets)) - 10, last(index(symRets))))
s50Xts <- xts(nDf[,2], nDf[,1])

benchXts <- na.omit(merge(symRets, n100Xts, m150Xts, s50Xts))
benchXts[,2] <- benchXts[,2]/stats::lag(benchXts[,2], 1)-1
benchXts[,3] <- benchXts[,3]/stats::lag(benchXts[,3], 1)-1
benchXts[,4] <- benchXts[,4]/stats::lag(benchXts[,4], 1)-1

names(benchXts) <- c('RET', 'N50', 'M150', 'S50')
			
sharpe <- SharpeRatio.annualized(benchXts)
Common.PlotCumReturns(benchXts, "MAD Trend", sprintf("%dx%d; SR: %s", topLbSmall, topLbLarge, paste(round(sharpe,2), collapse="/")), 
		sprintf("%s/mad-top-cum-ret.%d.%d.ALL.png", reportPath, topLbSmall, topLbLarge), NULL)

sharpe <- SharpeRatio.annualized(benchXts["/2019",])
Common.PlotCumReturns(benchXts["/2019",], "MAD Trend", sprintf("%dx%d; SR: %s", topLbSmall, topLbLarge, paste(round(sharpe,2), collapse="/")), 
		sprintf("%s/mad-top-cum-ret.%d.%d.pre-2020.png", reportPath, topLbSmall, topLbLarge), NULL)

sharpe <- SharpeRatio.annualized(benchXts["2020-05-01/",])
Common.PlotCumReturns(benchXts["2020-05-01/",], "MAD Trend", sprintf("%dx%d; SR: %s", topLbSmall, topLbLarge, paste(round(sharpe,2), collapse="/")), 
		sprintf("%s/mad-top-cum-ret.%d.%d.post-2020.png", reportPath, topLbSmall, topLbLarge), NULL)


library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
startDate<-as.Date("1970-01-01")
endDate<-as.Date("2018-11-30")
index1<-'^GSPC'

usdDf1<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO 
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and symbol='%s'", startDate, endDate, index1))
usdXts1<-xts(usdDf1$ac, as.Date(usdDf1$time_stamp))
usdXts1<-merge(usdXts1, dailyReturn(usdXts1))
usdXts1<-merge(usdXts1, SMA(usdXts1[,1], 50))
usdXts1<-merge(usdXts1, SMA(usdXts1[,1], 100))
usdXts1<-merge(usdXts1, SMA(usdXts1[,1], 200))

pXts<-na.omit(usdXts1)
names(pXts)<-c('INDEX', 'RET', 'S50', 'S100', 'S200')

btDf<-data.frame(100*merge(pXts[pXts$INDEX > pXts$S50]$RET, pXts[pXts$INDEX < pXts$S50]$RET, 
	pXts[pXts$INDEX > pXts$S100]$RET, pXts[pXts$INDEX < pXts$S100]$RET, 
	pXts[pXts$INDEX > pXts$S200]$RET, pXts[pXts$INDEX < pXts$S200]$RET))
names(btDf)<-c('> SMA 50', '< SMA 50', '> SMA 100', '< SMA 100', '> SMA 200', '< SMA 200')
meltedDf<-melt(btDf)

plotStart<-first(index(pXts))
plotEnd<-last(index(pXts))

pdf(NULL)
ggplot(meltedDf, aes(x=value, color=variable)) +
	theme_economist() +
	stat_density(geom="line", position = "identity", na.rm=T, size=0.8) +
	labs(y='density', x='daily returns(%)', fill='', color='', title="S&P 500 SMA Regimes", subtitle=sprintf("%s:%s", plotStart, plotEnd)) +
	annotate("text", x=min(btDf, na.rm=T), y=0, label = "@StockViz", hjust=-.5, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.9)
ggsave(sprintf("%s/sp500.sma.regime.density.png", reportPath), width=12, height=6, units="in")

pXts$BH_LAG_1<-stats::lag(pXts$RET, -1)
pXts$L50<-ifelse(pXts$INDEX > pXts$S50, pXts$BH_LAG_1, 0)
pXts$L100<-ifelse(pXts$INDEX > pXts$S100, pXts$BH_LAG_1, 0)
pXts$L200<-ifelse(pXts$INDEX > pXts$S200, pXts$BH_LAG_1, 0)

pXts$LS50<-ifelse(pXts$INDEX > pXts$S50, pXts$BH_LAG_1, -pXts$BH_LAG_1)
pXts$LS100<-ifelse(pXts$INDEX > pXts$S100, pXts$BH_LAG_1, -pXts$BH_LAG_1)
pXts$LS200<-ifelse(pXts$INDEX > pXts$S200, pXts$BH_LAG_1, -pXts$BH_LAG_1)

Common.PlotCumReturns(pXts[, c('BH_LAG_1','L50', 'L100', 'L200')], "S&P 500 Long-only SMA Regime Switch", sprintf("%s/sp500.long-only.SMA.regime.cumulative.png", reportPath))
Common.PlotCumReturns(pXts[, c('BH_LAG_1','LS50', 'LS100', 'LS200')], "S&P 500 Long-short SMA Regime Switch", sprintf("%s/sp500.long-short.SMA.regime.cumulative.png", reportPath))

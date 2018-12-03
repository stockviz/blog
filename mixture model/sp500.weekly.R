library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('mixtools')

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
usdWeeklyRets<-weeklyReturn(usdXts1)
usdWeeklyRets<-usdWeeklyRets[-1]
usdWeeklyRets<-usdWeeklyRets[-nrow(usdWeeklyRets)]

weeklyXts<-rollapply(usdWeeklyRets, 200, function(X){
	tryCatch({
		mix_mod <- normalmixEM(X, k=2, lambda = c(0.2, 0.8))
		xts(matrix(c(mix_mod$sigma[1], mix_mod$sigma[2], last(mix_mod$posterior[,1])), ncol=3), index(last(X)))
	}, error=function(e){
		xts(matrix(c(NA, NA, NA), ncol=3), index(last(X)))
	})
}, by.column=F)

weeklyXtsCp<-na.omit(weeklyXts)
names(weeklyXtsCp)<-c('SIGMA_1', 'SIGMA_2', 'PROB_1')

#lower sigma => stable. 
weeklyXtsCp$PROB_STABLE<-ifelse(weeklyXtsCp$SIGMA_1 < weeklyXtsCp$SIGMA_2, weeklyXtsCp$PROB_1, 1-weeklyXtsCp$PROB_1) 

#apply a threshold to the probability to make a binary classification
weeklyXtsCp$REGIME<-ifelse(weeklyXtsCp$PROB_STABLE > 0.5, 1, 0)

#use the non-smoothed regime to create a density plot	
btXts<-na.omit(merge(usdWeeklyRets, weeklyXtsCp$REGIME))	
names(btXts)<-c('BH', 'REGIME')

plotStart<-first(index(btXts))
plotEnd<-last(index(btXts))

btDf<-data.frame(100*merge(btXts[btXts$REGIME == 0]$BH, btXts[btXts$REGIME == 1]$BH))
names(btDf)<-c('Unstable', 'Stable')
meltedDf<-melt(btDf)

pdf(NULL)
ggplot(meltedDf, aes(x=value, color=variable)) +
	theme_economist() +
	stat_density(geom="line", position = "identity", na.rm=T, size=0.8) +
	labs(y='density', x='weekly returns(%)', fill='', color='', title="S&P 500 Regime", subtitle=sprintf("%s:%s", plotStart, plotEnd)) +
	annotate("text", x=min(btDf, na.rm=T), y=0, label = "@StockViz", hjust=-.5, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.9)
ggsave(sprintf("%s/sp500.regime.density.weekly.png", reportPath), width=12, height=6, units="in")

#use the non-smoothed regime for a long-only back-test
btXts$BH_LAG_1<-stats::lag(btXts$BH, -1)
btXts$L<-ifelse(btXts$REGIME == 1, btXts$BH_LAG_1, 0)
Common.PlotCumReturns(btXts[, c('BH_LAG_1','L')], "S&P 500 Regime Switch (weekly)", sprintf("%s/sp500.regime.cumulative.weekly.png", reportPath))

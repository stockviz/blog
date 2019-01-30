library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('ggrepel')
library('dplyr')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")
reportPath <- "."

startDate<-as.Date('1991-01-01')
endDate<-as.Date('2018-12-31')

investment<-100000

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

niftyDf<-sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where time_stamp >= '%s' and time_stamp <= '%s' and index_name='%s'", startDate, endDate, 'NIFTY 50'))
niftyXts<-xts(niftyDf$px_close, as.Date(niftyDf$time_stamp))
niftyYearly<-to.period(niftyXts, "years")
niftyYearly<-Common.NormalizeMonthlyDates(niftyYearly)

yearlyBuyPx<-data.frame(Y=0, H=0.0, L=0.0, R=0.0)
yearlyAsset<-data.frame(Y=0, H=0.0, L=0.0, R=0.0)

for(i in 1:nrow(niftyYearly)){
	highPx<-niftyYearly[i, 2]
	lowPx<-niftyYearly[i, 3]
	yr<-year(niftyYearly[i])
	rndPx<-sample(niftyXts[sprintf("%d", yr)], 1)
	yearlyBuyPx<-rbind(yearlyBuyPx, c(yr, highPx, lowPx, rndPx))
	yearlyAsset<-rbind(yearlyAsset, c(yr, investment/highPx, investment/lowPx, investment/rndPx))
}

yearlyBuyPx<-yearlyBuyPx[-1,]
yearlyAsset<-yearlyAsset[-1,]

yearlyAsset10<-cbind(yearlyAsset[10:nrow(yearlyAsset),1], rollapply(yearlyAsset[, 2:4], 10, sum))
colnames(yearlyAsset10)<-colnames(yearlyAsset)

terminal10<-xts()
for(j in 2:4){
	terminal10<-merge(terminal10, niftyYearly[sprintf("%d-12-20", yearlyAsset10[,1]), 4]*yearlyAsset10[,j])
}

names(terminal10)<-c('H', 'L', 'R')
terminal10<-terminal10/investment

aRetDf<-data.frame(terminal10)
retYears<-year(index(terminal10))
aRetDf$Y<-factor(retYears, levels=retYears)

pdf(NULL)
ggplot(data=melt(aRetDf, id='Y'), aes(x=Y, y=value, fill=variable)) +
  theme_economist() +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text_repel(aes(label= round(value, 2)), position = position_dodge(0.9)) +
  labs(x = "", y="Rs. lakhs", fill="", color="", title="NIFTY 50: 10-year rolling terminal wealth", subtitle="lumpsum investment of Rs. 1 lakh every year") +
  annotate("text", x=length(retYears), y=min(terminal10), label = "@StockViz", hjust=1.1, vjust=-.5, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/nifty.market-timing.annual.png", reportPath), width=20, height=8, units="in")  




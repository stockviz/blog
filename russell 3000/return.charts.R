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
startDate<-as.Date("2003-01-01")
endDate<-as.Date("2018-11-30")
indices<-c('^RUA', '^RAG', '^RAV')

usdXts1<-xts()

for(i in indices){
	usdDf1<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO 
								where time_stamp >= '%s' and time_stamp <= '%s' 
								and ac > 0
								and symbol='%s'", startDate, endDate, i))
	usdXts1<-merge(usdXts1, xts(usdDf1$ac, as.Date(usdDf1$time_stamp)))
}

monthlyXts<-merge(monthlyReturn(usdXts1[,1]), monthlyReturn(usdXts1[,2]), monthlyReturn(usdXts1[,3]))
annualXts<-100*merge(annualReturn(usdXts1[,1]), annualReturn(usdXts1[,2]), annualReturn(usdXts1[,3]))
names(monthlyXts)<-indices
names(annualXts)<-indices

toPlotDf<-data.frame(annualXts)
toPlotDf$T<-year(index(annualXts))

y1<-min(toPlotDf$T)
y2<-max(toPlotDf$T)

meltedDf<-melt(toPlotDf, id='T')
meltedDf$T<-factor(meltedDf$T, levels=sort(unique(meltedDf$T)))

pdf(NULL)	
ggplot(meltedDf, aes(x=T, y=value, fill=variable)) + 
	theme_economist() +
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text(aes(label=round(value,2)), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5) +
	labs(x = "", y="returns(%)", fill="", color="", title="Russell 3000", subtitle=sprintf("%s:%s", y1, y2)) +
	annotate("text", x=y2-y1, y=min(meltedDf$value), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
ggsave(sprintf("%s/r3000.annual.png", reportPath), width=12, height=6, units="in")

Common.PlotCumReturns(monthlyXts, "Russell 3000", sprintf("%s/r3000.cumulative.png", reportPath))
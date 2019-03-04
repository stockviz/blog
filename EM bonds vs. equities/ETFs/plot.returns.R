library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('ggrepel')
library('reshape2')

#etfTickers<-c("EMB", "AGG")
#startDate<-as.Date("2006-01-01")
#endDate<-as.Date("2019-02-25")

args = commandArgs(TRUE)
etfTickers<-unlist(strsplit(args[1], ","))
startDate<-as.Date(args[2])
endDate<-as.Date(args[3])

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

retDaily<-xts()
retAnnual<-xts()
for(i in 1:length(etfTickers)){
	pDf<-sqlQuery(lconUs2, sprintf("select C, TIME_STAMP from BHAV_EQ_TD where SYMBOL='%s' and time_stamp >= '%s' and time_stamp <= '%s'", etfTickers[i], startDate, endDate))
	pXts<-xts(pDf[,1], as.Date(pDf[,2]))
	retDaily<-merge(retDaily, dailyReturn(pXts))
	retAnnual<-merge(retAnnual, annualReturn(pXts))
}
retDaily<-na.omit(retDaily[-1,])
names(retDaily)<-etfTickers

retAnnual<-retAnnual[-1,]
retAnnual<-retAnnual[-nrow(retAnnual),]
retAnnual<-na.omit(retAnnual)
names(retAnnual)<-etfTickers

Common.PlotCumReturns(retDaily, "ETF Returns", sprintf("%s/ETF.%s.cumulative.%s.%s.png", reportPath, paste(etfTickers, collapse="-"), startDate, endDate))

retAnnualDf<-data.frame(retAnnual*100)
retAnnualDf$Y<-year(index(retAnnual))
retAnnualDf$Y<-factor(retAnnualDf$Y, levels=retAnnualDf$Y)

toPlot<-melt(retAnnualDf, id='Y')

pdf(NULL)	
ggplot(toPlot, aes(x=Y, y=value, fill=variable)) +
	theme_economist() +
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text_repel(aes(label=round(value,2)), position = position_dodge(0.9)) +
	labs(x = "", y="returns(%)", fill="", color="", title="ETF Returns") +
	annotate("text", x=0, y=max(retAnnual), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.5)
ggsave(sprintf("%s/ETF.%s.annual.%s.%s.png", reportPath, paste(etfTickers, collapse="-"), startDate, endDate), width=12, height=8, units="in")
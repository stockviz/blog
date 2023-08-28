library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")
reportPath <- "."

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indiaId<-935600
emId<-891800

startDate<-as.Date('1992-12-31')
endDate<-as.Date('2018-12-31')

downloadMsci<-function(indexId){
	msciDf<-sqlQuery(lconUs2, sprintf("select time_stamp, val from MSCI_DATA where time_stamp >= '%s' and time_stamp <= '%s' and id=%d", startDate, endDate, indexId))
	msciDf$time_stamp<-as.Date(msciDf$time_stamp)
	msciDf$date_diff<-c(30,diff(msciDf$time_stamp))

	monthlyMsci<-msciDf[msciDf$date_diff > 15,]
	monthlyMsciXts<-xts(monthlyMsci$val, monthlyMsci$time_stamp)

	dailyMsci<-msciDf[msciDf$date_diff < 15,]

	dailyMsciXts<-xts(dailyMsci$val, dailyMsci$time_stamp)
	x1<-to.period(dailyMsciXts, 'months')
	
	if (month(first(x1)) == month(last(monthlyMsciXts))){
		monthlyMsciXts<-monthlyMsciXts[-nrow(monthlyMsciXts)]
	}

	momXts2<-rbind(monthlyMsciXts, x1[,4])
	return(momXts2)
}

indiaXts2<-downloadMsci(indiaId)
emXts2<-downloadMsci(emId)

indiaXts2<-Common.NormalizeMonthlyDates(indiaXts2)
emXts2<-Common.NormalizeMonthlyDates(emXts2)

monthlyReturnXts<-merge(diff(indiaXts2)/stats::lag(indiaXts2,1), diff(emXts2)/stats::lag(emXts2,1))
namesPrefix<-c('IN', 'EM')
names(monthlyReturnXts)<-namesPrefix
monthlyReturnXts<-na.omit(monthlyReturnXts)

Common.PlotCumReturns(monthlyReturnXts, "India vs. EM MSCI Indices", sprintf("%s/INDIA-EM.cumulative.png", reportPath))

aRet1<-rollapply(monthlyReturnXts[,1], 12, function(X) if(month(xts::last(zoo::index(X)))==12) Return.cumulative(X) else xts(NA, xts::last(zoo::index(X))))
aRet2<-rollapply(monthlyReturnXts[,2], 12, function(X) if(month(xts::last(zoo::index(X)))==12) Return.cumulative(X) else xts(NA, xts::last(zoo::index(X))))

aRet1<-na.omit(aRet1)
aRet2<-na.omit(aRet2)

annualReturnsXts<-merge(aRet1, aRet2)
names(annualReturnsXts)<-c('IN', 'EM')

annualReturnsXts<-100*annualReturnsXts
aRetDf<-data.frame(annualReturnsXts)
retYears<-year(index(annualReturnsXts))
aRetDf$Y<-factor(retYears, levels=retYears)

pdf(NULL)
ggplot(data=melt(aRetDf, id='Y'), aes(x=Y, y=value, fill=variable)) +
  theme_economist() +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text_repel(aes(label= round(value, 2)), position = position_dodge(0.9)) +
  labs(x = "", y="returns(%)", fill="", color="", title="India vs. EM MSCI Indices") +
  annotate("text", x=length(retYears), y=min(annualReturnsXts), label = "@StockViz", hjust=1.1, vjust=-.5, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/INDIA-EM.annual.png", reportPath), width=20, height=8, units="in")  





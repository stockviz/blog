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

msciIndex<-'USA MOMENTUM'
yahooIndex<-'^GSPC'

endDate<-as.Date("2018-12-31")

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

msciId<-sqlQuery(lconUs2, sprintf("select index_code from MSCI_META where index_name='%s'", msciIndex))[[1]]
msciDf<-sqlQuery(lconUs2, sprintf("select time_stamp, val from MSCI_DATA where id=%d", msciId))
msciDf$time_stamp<-as.Date(msciDf$time_stamp)
msciDf$date_diff<-c(30,diff(msciDf$time_stamp))

monthlyMsci<-msciDf[msciDf$date_diff > 15,]
monthlyMsciXts<-xts(monthlyMsci$val, monthlyMsci$time_stamp)
monthlyMsciXts<-monthlyMsciXts[-nrow(monthlyMsciXts)]

dailyMsci<-msciDf[msciDf$date_diff < 15,]

dailyMsciXts<-xts(dailyMsci$val, dailyMsci$time_stamp)
x1<-to.period(dailyMsciXts, 'months')

monthlyMsciXts<-rbind(monthlyMsciXts, x1[,4])

startDate<-first(index(monthlyMsciXts))-45

df1<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO where time_stamp >= '%s' and time_stamp <= '%s' and symbol='%s'", startDate, endDate, '^GSPC'))
xts2<-xts(df1$ac, as.Date(df1$time_stamp))
x2<-to.period(xts2, 'months')

msciXts2<-Common.NormalizeMonthlyDates(monthlyMsciXts)
yahooXts2<-Common.NormalizeMonthlyDates(x2[,4])

monthlyReturnXts<-merge(diff(msciXts2)/stats::lag(msciXts2,1), diff(yahooXts2)/stats::lag(yahooXts2,1))
monthlyReturnXts<-na.omit(monthlyReturnXts)

names(monthlyReturnXts)<-c('USA MOMENTUM', 'SP 500')

Common.PlotCumReturns(monthlyReturnXts, "MSCI USA Momentum vs. S&P 500", sprintf("%s/msci-usa-mom.sp500.cumulative.png", reportPath))

aRet1<-annualReturn(msciXts2)
aRet2<-annualReturn(yahooXts2)
annualReturnsXts<-na.omit(merge(aRet1[-1], aRet2[-1]))
names(annualReturnsXts)<-c('MOM', 'SP500')

annualReturnsXts<-100*annualReturnsXts
aRetDf<-data.frame(annualReturnsXts)
retYears<-year(index(annualReturnsXts))
aRetDf$Y<-factor(retYears, levels=retYears)

pdf(NULL)
ggplot(data=melt(aRetDf, id='Y'), aes(x=Y, y=value, fill=variable)) +
  theme_economist() +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text_repel(aes(label= round(value, 2)), position = position_dodge(0.9), segment.color='orange') +
  labs(x = "", y="returns(%)", fill="", color="", title="MSCI USA Momentum vs. S&P 500") +
  annotate("text", x=length(retYears), y=min(annualReturnsXts), label = "@StockViz", hjust=1.1, vjust=-.5, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/msci-usa-mom.sp500.annual.png", reportPath), width=20, height=8, units="in")  



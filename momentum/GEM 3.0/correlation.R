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

sp500Index <- 'SP 500'
sp500IndexId <- '^GSPC'

exUsIndex <- 'WORLD ex USA'
exUsIndexId <- 991000

sp500MomIndex <- 'USA MOMENTUM'
sp500MomIndexId <- 703025

exUsMomIndex <- 'WORLD ex USA MOMENTUM'
exUsMomIndexId <- 703841

startDate<-as.Date('1990-12-31')
endDate<-as.Date('2018-12-31')

lconUs <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

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

#from: https://stackoverflow.com/a/7549819/644883
lm_eqn <- function(df, x, y){
    m <- lm(as.formula(sprintf("%s ~ %s", y, x)), df)
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
         list(a = format(as.numeric(coef(m)[1]), digits = 2), 
              b = format(as.numeric(coef(m)[2]), digits = 2), 
             r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq))                 
}

sp500Df<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO where time_stamp >= '%s' and time_stamp <= '%s' and symbol='%s'", startDate, endDate, '^GSPC'))
sp500Xts<-xts(sp500Df$ac, as.Date(sp500Df$time_stamp))
sp500Xts2<-to.period(sp500Xts, 'months')[,4]

exUsXts2<-downloadMsci(exUsIndexId)
sp500MomXts2<-downloadMsci(sp500MomIndexId)
exUsMomXts2<-downloadMsci(exUsMomIndexId)

sp500Xts2<-Common.NormalizeMonthlyDates(sp500Xts2)
exUsXts2<-Common.NormalizeMonthlyDates(exUsXts2)
sp500MomXts2<-Common.NormalizeMonthlyDates(sp500MomXts2)
exUsMomXts2<-Common.NormalizeMonthlyDates(exUsMomXts2)

monthlyReturnXts<-merge(diff(sp500Xts2)/stats::lag(sp500Xts2,1), 
						diff(exUsXts2)/stats::lag(exUsXts2,1), 	 
						diff(sp500MomXts2)/stats::lag(sp500MomXts2,1),
						diff(exUsMomXts2)/stats::lag(exUsMomXts2,1)) 
						
monthlyReturnXts<-na.omit(monthlyReturnXts)
names(monthlyReturnXts)<-c(sp500Index, exUsIndex, sp500MomIndex, exUsMomIndex)
mRetDf<-data.frame(monthlyReturnXts)

namesDf<-data.frame(ORIG=c(sp500Index, exUsIndex, sp500MomIndex, exUsMomIndex), DF=names(mRetDf))
sp500Index2<-namesDf[namesDf$ORIG==sp500Index,]$DF[1]
exUsIndex2<-namesDf[namesDf$ORIG==exUsIndex,]$DF[1]
sp500MomIndex2<-namesDf[namesDf$ORIG==sp500MomIndex,]$DF[1]
exUsMomIndex2<-namesDf[namesDf$ORIG==exUsMomIndex,]$DF[1]

plotStart<-first(index(monthlyReturnXts))
plotEnd<-last(index(monthlyReturnXts))

lmEq<-lm_eqn(mRetDf, sp500Index2, sp500MomIndex2)
pdf(NULL)
ggplot(mRetDf, aes_string(x=sp500Index2, y=sp500MomIndex2)) +
	theme_economist() +
	geom_point() +
	geom_smooth() +
	geom_text(x = 0.9*min(monthlyReturnXts[,sp500Index], na.rm=T), y = 0.9*max(monthlyReturnXts[,sp500MomIndex], na.rm=T), label = lmEq, parse = TRUE) +
	labs(x = sp500Index, y=sp500MomIndex, fill="", color="", title=sprintf("Correlation: %s vs %s", sp500Index, sp500MomIndex), subtitle=sprintf("monthly returns [%s:%s]", plotStart, plotEnd)) +
	annotate("text", x=max(monthlyReturnXts[,sp500Index], na.rm=T), y=min(monthlyReturnXts[,sp500MomIndex], na.rm=T), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)
ggsave(sprintf("%s/%s.%s.correlation.png", reportPath, sp500Index, sp500MomIndex), width=20, height=8, units="in")  

lmEq<-lm_eqn(mRetDf, exUsIndex2, exUsMomIndex2)	
ggplot(mRetDf, aes_string(x=exUsIndex2, y=exUsMomIndex2)) +
	theme_economist() +
	geom_point() +
	geom_smooth() +
	geom_text(x = 0.9*min(monthlyReturnXts[,exUsIndex], na.rm=T), y = 0.9*max(monthlyReturnXts[,exUsMomIndex], na.rm=T), label = lmEq, parse = TRUE) +
	labs(x = exUsIndex, y=exUsMomIndex, fill="", color="", title=sprintf("Correlation: %s vs %s", exUsIndex, exUsMomIndex), subtitle=sprintf("monthly returns [%s:%s]", plotStart, plotEnd)) +
	annotate("text", x=max(monthlyReturnXts[,exUsIndex], na.rm=T), y=min(monthlyReturnXts[,exUsMomIndex], na.rm=T), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)
ggsave(sprintf("%s/%s.%s.correlation.png", reportPath, exUsIndex, exUsMomIndex), width=20, height=8, units="in")   
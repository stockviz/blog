library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('ggrepel')
library('grid')
library('gridExtra')
library('gtable')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")
reportPath <- "."

sp500Index <- 'SP 500'
sp500IndexId <- '^GSPC'

momIndex <- 'USA MOMENTUM'
momIndexId <- 703025

worldIndex <- 'WORLD ex USA'
worldIndexId <- 991000

worldMomIndex <- 'WORLD ex USA MOMENTUM'
worldMomIndexId <- 703841

tbilIndex <- 'Barclays 3 month T Bill'
tbilIndexId <- 'BCC23MTB'

bondIndex <- 'ICE BofAML US Corp Master TR'
bondIndexId <- -2147171415

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


saveDd<-function(retCumXts, ddTitle, ddFileName){
	tdn<-table.Drawdowns(retCumXts, 10)
	tdn<-tdn[tdn$Depth < -0.05,]
	tdn$Depth<-round(100*tdn$Depth, 2)

	tt1<-arrangeGrob(tableGrob(tdn, rows=NULL, theme=tableTheme), ncol=1, top=textGrob(ddTitle, gp=gpar(fontsize=12, fontface ='bold', fontfamily='Segoe UI')), bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))

	ggsave(ddFileName, tt1, width=7, height=nrow(tdn)*0.5, units='in')
}


###########################################

sp500Df<-sqlQuery(lconUs2, sprintf("select ac, time_stamp from BHAV_YAHOO where time_stamp >= '%s' and time_stamp <= '%s' and symbol='%s'", startDate, endDate, '^GSPC'))
sp500Xts<-xts(sp500Df$ac, as.Date(sp500Df$time_stamp))
sp500Xts2<-to.period(sp500Xts, 'months')[,4]

###########################################

momXts2<-downloadMsci(momIndexId)
worldXts2<-downloadMsci(worldIndexId)
worldMomXts2<-downloadMsci(worldMomIndexId)

###########################################

tbilDf<-sqlQuery(lconUs2, sprintf("select time_stamp, val from BARCLAYS_DATA where time_stamp >= '%s' and time_stamp <= '%s' and ticker='%s'", startDate, endDate, tbilIndexId))
tbilXts<-xts(tbilDf$val, as.Date(tbilDf$time_stamp))
tbilXts2<-to.period(tbilXts, 'months')[,4]

###########################################

bondDf<-sqlQuery(lconUs, sprintf("select time_stamp, val from FRED_OBSERVATION where time_stamp >= '%s' and time_stamp <= '%s' and SERIES_ID=%d", startDate, endDate, bondIndexId))
bondXts<-xts(bondDf$val, as.Date(bondDf$time_stamp))
bondXts2<-to.period(bondXts, 'months')[,4]

###########################################

sp500Xts2<-Common.NormalizeMonthlyDates(sp500Xts2)
worldXts2<-Common.NormalizeMonthlyDates(worldXts2)
worldMomXts2<-Common.NormalizeMonthlyDates(worldMomXts2)
momXts2<-Common.NormalizeMonthlyDates(momXts2)
tbilXts2<-Common.NormalizeMonthlyDates(tbilXts2)
bondXts2<-Common.NormalizeMonthlyDates(bondXts2)

monthlyReturnXts<-merge(diff(sp500Xts2)/stats::lag(sp500Xts2,1), #1
						diff(momXts2)/stats::lag(momXts2,1),     #2
						diff(worldXts2)/stats::lag(worldXts2,1), #3
						diff(worldMomXts2)/stats::lag(worldMomXts2,1), #4
						diff(tbilXts2)/stats::lag(tbilXts2,1),   #5
						diff(bondXts2)/stats::lag(bondXts2,1))   #6

monthlyReturnXts<-na.omit(monthlyReturnXts)						
m12ReturnXts<-xts()
for(i in 1:ncol(monthlyReturnXts)){
	m12ReturnXts<-merge(m12ReturnXts, rollapply(monthlyReturnXts[,i], 12, Return.cumulative))
}
m12ReturnXts<-na.omit(m12ReturnXts)

monthlyReturnXts2<-xts()
for(i in 1:ncol(monthlyReturnXts)){
	monthlyReturnXts2<-merge(monthlyReturnXts2, stats::lag(monthlyReturnXts[,i],-1))
}
monthlyReturnXts2<-na.omit(monthlyReturnXts2)

allXts<-merge(m12ReturnXts, monthlyReturnXts2)
allXts<-na.omit(allXts)

gemSp500Xts<- ifelse(allXts[,1] > allXts[,5], ifelse(allXts[, 1] > allXts[, 3], allXts[, 7], allXts[, 9]), allXts[,12])
gemSpMomXts<- ifelse(allXts[,1] > allXts[,5], ifelse(allXts[, 1] > allXts[, 3], allXts[, 8], allXts[, 9]), allXts[,12])
gemMomXts<- ifelse(allXts[,2] > allXts[,5], ifelse(allXts[, 2] > allXts[, 3], allXts[, 8], allXts[, 9]), allXts[,12])

gemSpMom2Xts<- ifelse(allXts[,1] > allXts[,5], ifelse(allXts[, 1] > allXts[, 3], allXts[, 8], allXts[, 10]), allXts[,12])
gemMom2Xts<- ifelse(allXts[,2] > allXts[,5], ifelse(allXts[, 2] > allXts[, 3], allXts[, 8], allXts[, 10]), allXts[,12])

indexClass(gemSp500Xts) <- "Date"
indexClass(gemSpMomXts) <- "Date"
indexClass(gemMomXts) <- "Date"
indexClass(gemSpMom2Xts) <- "Date"
indexClass(gemMom2Xts) <- "Date"

spMom<-na.omit(merge(gemSp500Xts, gemSpMomXts, gemMomXts))
names(spMom)<-c('SP 500', 'SP 500x1', 'MOM')
Common.PlotCumReturns(spMom, "S&P 500/MSCI USA Momentum GEM", sprintf("%s/sp500.mom.GEM.cumulative.png", reportPath))

spMom<-na.omit(merge(gemSp500Xts, gemSpMomXts, gemMomXts, gemSpMom2Xts, gemMom2Xts))
names(spMom)<-c('SP 500', 'SP 500x1', 'MOM', 'SP 500x2', 'MOMx2')
Common.PlotCumReturns(spMom, "S&P 500/MSCI USA and World Momentum GEM", sprintf("%s/sp500.mom.world.GEM.cumulative.png", reportPath))

saveDd(gemSp500Xts, "S&P 500 GEM Drawdowns", sprintf("%s/SP500.GEM.dd.png", reportPath))
saveDd(gemSpMomXts, "S&P 500 x1 GEM Drawdowns", sprintf("%s/SP500x.GEM.dd.png", reportPath))
saveDd(gemMomXts, "Momentum GEM Drawdowns", sprintf("%s/MTUM.GEM.dd.png", reportPath))
saveDd(gemSpMom2Xts, "S&P 500 x2 GEM Drawdowns", sprintf("%s/SP500x2.GEM.dd.png", reportPath))
saveDd(gemMom2Xts, "Momentum x2 GEM Drawdowns", sprintf("%s/MTUMx2.GEM.dd.png", reportPath))

###########################################

aRet1<-rollapply(gemSp500Xts, 12, function(X) if(month(xts::last(zoo::index(X)))==12) Return.cumulative(X) else xts(NA, xts::last(zoo::index(X))))
aRet2<-rollapply(gemSpMomXts, 12, function(X) if(month(xts::last(zoo::index(X)))==12) Return.cumulative(X) else xts(NA, xts::last(zoo::index(X))))
aRet3<-rollapply(gemMomXts, 12, function(X) if(month(xts::last(zoo::index(X)))==12) Return.cumulative(X) else xts(NA, xts::last(zoo::index(X))))
aRet4<-rollapply(gemSpMom2Xts, 12, function(X) if(month(xts::last(zoo::index(X)))==12) Return.cumulative(X) else xts(NA, xts::last(zoo::index(X))))
aRet5<-rollapply(gemMom2Xts, 12, function(X) if(month(xts::last(zoo::index(X)))==12) Return.cumulative(X) else xts(NA, xts::last(zoo::index(X))))

aRet1<-na.omit(aRet1)
aRet2<-na.omit(aRet2)
aRet3<-na.omit(aRet3)
aRet4<-na.omit(aRet4)
aRet5<-na.omit(aRet5)
annualReturnsXts<-merge(aRet1, aRet2, aRet3, aRet4, aRet5)
names(annualReturnsXts)<-c('SP500', 'SP500x', 'MOM', 'SP500x2', 'MOMx2')

annualReturnsXts<-100*annualReturnsXts
aRetDf<-data.frame(annualReturnsXts)
retYears<-year(index(annualReturnsXts))
aRetDf$Y<-factor(retYears, levels=retYears)

pdf(NULL)
ggplot(data=melt(aRetDf, id='Y'), aes(x=Y, y=value, fill=variable)) +
  theme_economist() +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text_repel(aes(label= round(value, 2)), position = position_dodge(0.9)) +
  labs(x = "", y="returns(%)", fill="", color="", title="Global Equities Momentum") +
  annotate("text", x=length(retYears), y=min(annualReturnsXts), label = "@StockViz", hjust=1.1, vjust=-.5, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/sp500.mtum.x2.GEM.annual.png", reportPath), width=20, height=8, units="in")  



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

args = commandArgs(TRUE)

baseIndex <- args[1]
worldIndex <- args[2]

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")
reportPath <- "."

sp500Index <- 'SP 500'
sp500IndexId <- '^GSPC'

#baseIndex <- 'USA IMI'
#baseIndexId <- 664187

#worldIndex <- 'ACWI ex USA'
#worldIndexId <- 991000

tbilIndex <- 'Barclays 3 month T Bill'
tbilIndexId <- 'BCC23MTB'

bondIndex <- 'ICE BofAML US Corp Master TR'
bondIndexId <- -2147171415

startDate<-as.Date('1990-12-31')
endDate<-as.Date('2018-12-31')

lconUs <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

baseIndexId<-sqlQuery(lconUs2, sprintf("select index_code from MSCI_META where index_name='%s'", baseIndex))[[1]]
worldIndexId<-sqlQuery(lconUs2, sprintf("select index_code from MSCI_META where index_name='%s'", worldIndex))[[1]]

print(paste(baseIndex, baseIndexId, worldIndex, worldIndexId))

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

baseXts2<-downloadMsci(baseIndexId)
worldXts2<-downloadMsci(worldIndexId)

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
baseXts2<-Common.NormalizeMonthlyDates(baseXts2)
worldXts2<-Common.NormalizeMonthlyDates(worldXts2)
tbilXts2<-Common.NormalizeMonthlyDates(tbilXts2)
bondXts2<-Common.NormalizeMonthlyDates(bondXts2)

monthlyReturnXts<-merge(diff(sp500Xts2)/stats::lag(sp500Xts2,1), #1
						diff(baseXts2)/stats::lag(baseXts2,1),   #2
						diff(worldXts2)/stats::lag(worldXts2,1), #3
						diff(tbilXts2)/stats::lag(tbilXts2,1),   #4
						diff(bondXts2)/stats::lag(bondXts2,1))   #5

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

gemSp500Xts<- ifelse(allXts[,1] > allXts[,4], ifelse(allXts[, 2] > allXts[, 3], allXts[, 7], allXts[, 8]), allXts[, 10])

indexClass(gemSp500Xts) <- "Date"

spMom<-na.omit(merge(gemSp500Xts, allXts[,7], allXts[,8]))
names(spMom)<-c('GEM', baseIndex, worldIndex)
Common.PlotCumReturns(spMom, sprintf("%s/%s GEM", baseIndex, worldIndex), sprintf("%s/%s.%s.GEM.cumulative.png", reportPath, baseIndex, worldIndex))

saveDd(spMom[,1], sprintf("%s/%s GEM Drawdowns", baseIndex, worldIndex), sprintf("%s/%s.%s.GEM.dd.png", reportPath, baseIndex, worldIndex))
saveDd(spMom[,2], sprintf("%s Drawdowns", baseIndex), sprintf("%s/%s.dd.png", reportPath, baseIndex))
saveDd(spMom[,3], sprintf("%s Drawdowns", worldIndex), sprintf("%s/%s.dd.png", reportPath, worldIndex))

###########################################

aRet1<-rollapply(spMom[,1], 12, function(X) if(month(xts::last(zoo::index(X)))==12) Return.cumulative(X) else xts(NA, xts::last(zoo::index(X))))
aRet2<-rollapply(spMom[,2], 12, function(X) if(month(xts::last(zoo::index(X)))==12) Return.cumulative(X) else xts(NA, xts::last(zoo::index(X))))
aRet3<-rollapply(spMom[,3], 12, function(X) if(month(xts::last(zoo::index(X)))==12) Return.cumulative(X) else xts(NA, xts::last(zoo::index(X))))

aRet1<-na.omit(aRet1)
aRet2<-na.omit(aRet2)
aRet3<-na.omit(aRet3)
annualReturnsXts<-merge(aRet1, aRet2, aRet3)
names(annualReturnsXts)<-c('GEM', baseIndex, worldIndex)

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

ggsave(sprintf("%s/%s.%s.GEM.annual.png", reportPath, baseIndex, worldIndex), width=20, height=8, units="in")  



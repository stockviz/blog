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
MSCI_CSV <- "../COUNTRY_MSCI_BEFORE_1993.csv"

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date('1992-12-31')
endDate<-as.Date('2019-01-31')

rollPeriod<-5 #years

downloadMsci<-function(indexId){
	msciDf<-sqlQuery(lconUs2, sprintf("select time_stamp, val from MSCI_DATA where time_stamp >= '%s' and time_stamp <= '%s' and id=%d and index_type='p'", startDate, endDate, indexId))
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

msciIndices<-read.csv(MSCI_CSV)

monthlyRetsXts<-xts()
retDf<-data.frame(INDEX="", YEAR=0, RET=0.0)
for(i in 2:nrow(msciIndices)){
	monthlyVals<-downloadMsci(msciIndices[i, 2])
	mRets<-diff(monthlyVals)/stats::lag(monthlyVals,1)
	mRets<-Common.NormalizeMonthlyDates(mRets)
	monthlyRetsXts<-merge(monthlyRetsXts, mRets)
	
	annRets<-annualReturn(monthlyVals)
	annRets<-annRets[-1]
	rets<-data.frame(INDEX=rep(msciIndices[i, 1], nrow(annRets)), YEAR=year(index(annRets)), RET=as.numeric(100*coredata(annRets)))
	retDf<-rbind(retDf, rets)
}

names(monthlyRetsXts)<-msciIndices[2:nrow(msciIndices),1]
monthlyRetsXts<-na.omit(monthlyRetsXts)

retDf<-retDf[-1,]
retDf$YEAR<-as.numeric(retDf$YEAR)
retDf$RET<-as.numeric(retDf$RET)

rollingCorMatrix<-rollapply(monthlyRetsXts, rollPeriod*12, function(X) {
	cor(data.frame(X), use='all.obs', method='spearman')[,'INDIA']
}, by.column=F) 	

rollingCor<-na.omit(rollingCorMatrix[, colnames(rollingCorMatrix) != 'INDIA'])
rollingCorMatrixDf<-data.frame(rollingCor)

medianCor <- apply(rollingCorMatrixDf, 2, median) 
medianCor <- sort(medianCor)

allNames<-names(medianCor)

pdf(NULL)

st<-1
while(st<=(length(allNames))){
	indexNames<-na.omit(allNames[st:(st+5)])
	print(indexNames)
	st<-st+6
	
	tp<-rollingCorMatrixDf[, indexNames]
	tp$T<-as.Date(row.names(rollingCorMatrixDf))
	plotStart<-first(tp$T)
	plotEnd<-last(tp$T)
	xAxisTicks<-seq(from=plotStart, to=plotEnd, length.out=10)
	tpMelt<-melt(tp, id='T')
	
	ggplot(tpMelt, aes(x=T, y=value, color=variable))+
		theme_economist() +
		geom_line() +
		scale_x_date(breaks = xAxisTicks) +
		labs(x = "", y="correlation", fill="", color="", title="Correlation with MSCI INDIA", subtitle=sprintf("rolling %d year %s:%s", rollPeriod, plotStart, plotEnd)) +
		annotate("text", x=plotEnd, y=min(tpMelt$value), label = "@StockViz", hjust=1.1, vjust=-.5, col="white", cex=6, fontface = "bold", alpha = 0.5)
		
	ggsave(sprintf("%s/MSCI-INDIA.correlations.%02d.%d-year.%s.png", reportPath, st, rollPeriod, paste(indexNames, collapse="_")), width=12, height=6, units="in")		
}

##### JORDAN is least correlated with INDIA 
##### HONG.KONG is least correlated with INDIA

v2<-retDf[retDf$INDEX %in% c("INDIA", "JORDAN", "HONG KONG"),]
v2$INDEX<-factor(v2$INDEX, levels=sort(unique(v2$INDEX)))
v2$YEAR<-factor(v2$YEAR, levels=sort(unique(v2$YEAR)))

ggplot(data=v2, aes(x=YEAR, y=RET, fill=INDEX)) +
	theme_economist() +
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text_repel(aes(label= round(RET, 2)), position = position_dodge(0.9)) +
	labs(x = "", y="returns(%)", fill="", color="", title="Annual Returns", subtitle=sprintf("%d:%d", min(retDf$YEAR), max(retDf$YEAR))) +
	annotate("text", x=0, y=min(v2$RET, na.rm=T), label = "@StockViz", hjust=0, vjust=-.5, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/MSCI.INDIA-JORDAN-HONG.KONG.annual.returns.png", reportPath), width=14, height=8, units="in")  

########## 50/50 INDIA/JORDAN

port50<-merge(monthlyRetsXts[, 'INDIA']/2 + monthlyRetsXts[, 'JORDAN']/2, monthlyRetsXts[, 'INDIA'], monthlyRetsXts[, 'JORDAN'])
names(port50)<-c('INDIA+JORDAN', 'INDIA', 'JORDAN')
Common.PlotCumReturns(port50, "50/50 INDIA/JORDAN", sprintf("%s/MSCI.INDIA-JORDAN.cumulative.png", reportPath))

########## 50/50 INDIA/HONG.KONG

port50<-merge(monthlyRetsXts[, 'INDIA']/2 + monthlyRetsXts[, 'HONG KONG']/2, monthlyRetsXts[, 'INDIA'], monthlyRetsXts[, 'HONG KONG'])
names(port50)<-c('INDIA+HONG.KONG', 'INDIA', 'HONG.KONG')
Common.PlotCumReturns(port50, "50/50 INDIA/HONG.KONG", sprintf("%s/MSCI.INDIA-HONG.KONG.cumulative.png", reportPath))
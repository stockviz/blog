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
MSCI_CSV <- "indices.csv"

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

endDate<-as.Date("2018-12-31")
msciIndices<-read.csv(MSCI_CSV)

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

msciData<-data.frame(INDEX="", ID=0, ST_DATE="", ED_DATE="")

for(i in 1:nrow(msciIndices)){
	for(j in 1:ncol(msciIndices)){
		msciId<-sqlQuery(lconUs2, sprintf("select index_code from MSCI_META where index_name='%s'", msciIndices[i, j]))[[1]]
		dates<-sqlQuery(lconUs2, sprintf("select min(time_stamp), max(time_stamp) from MSCI_DATA where id=%d", msciId))
		
		msciData<-rbind(msciData, c(msciIndices[i, j], msciId, toString(dates[1,1]), toString(dates[1,2])))
	}
}
msciData<-msciData[-1,]
msciData$ID<-as.numeric(msciData$ID)
msciData$ST_DATE<-as.Date(msciData$ST_DATE)
msciData$ED_DATE<-as.Date(msciData$ED_DATE)

startDate<-max(msciData$ST_DATE)
startYear<-year(startDate)+1

monthlyRetsXts<-xts()
retDf<-data.frame(INDEX="", YEAR=0, RET=0.0)
for(i in 1:nrow(msciData)){
	monthlyVals<-downloadMsci(msciData$ID[i])
	
	mRets<-diff(monthlyVals)/stats::lag(monthlyVals,1)
	mRets<-Common.NormalizeMonthlyDates(mRets)
	monthlyRetsXts<-merge(monthlyRetsXts, mRets)
	
	annRets<-annualReturn(monthlyVals)
	
	rets<-data.frame(INDEX=rep(msciData$INDEX[i], nrow(annRets)), YEAR=year(index(annRets)), RET=as.numeric(100*coredata(annRets)))
	retDf<-rbind(retDf, rets)
}
names(monthlyRetsXts)<-msciData$INDEX
monthlyRetsXts<-na.omit(monthlyRetsXts)

retDf<-retDf[-1,]
retDf$YEAR<-as.numeric(retDf$YEAR)
retDf$RET<-as.numeric(retDf$RET)

pdf(NULL)

for(i in 1:nrow(msciIndices)){
	tok1<-strsplit(msciIndices[i,1], ' ')[[1]][1]
	colNames<-as.character(as.vector(msciIndices[i,]))
	Common.PlotCumReturns(monthlyRetsXts[, colNames], sprintf("%s Value vs. Momentum", tok1), sprintf("%s/MSCI.%s.prime.momentum.cumulative.%s.%s.png", reportPath, tok1, startDate, endDate))
	
	v2<-retDf[retDf$YEAR >= startYear & retDf$INDEX %in% colNames,]
	v2$INDEX<-factor(v2$INDEX, levels=sort(unique(v2$INDEX)))
	v2$YEAR<-factor(v2$YEAR, levels=sort(unique(v2$YEAR)))

	lts<-c(median(v2$RET)-sd(v2$RET),median(v2$RET)+sd(v2$RET))

	ggplot(data=v2, aes(x=YEAR, y=RET, fill=INDEX)) +
		theme_economist() +
		geom_bar(stat="identity", position=position_dodge()) +
		geom_text_repel(aes(label= round(RET, 2)), position = position_dodge(0.9)) +
		labs(x = "", y="returns(%)", fill="", color="", title=sprintf("%s Value vs. Momentum", tok1), subtitle=sprintf("%s:%s", startDate, endDate)) +
		annotate("text", x=0, y=min(v2$RET), label = "@StockViz", hjust=1.1, vjust=-.5, col="white", cex=6, fontface = "bold", alpha = 0.5)

	ggsave(sprintf("%s/MSCI.%s.prime.momentum.yearwise.%s.%s.png", reportPath, tok1, startDate, endDate), width=14, height=8, units="in")  
}


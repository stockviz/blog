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

endDate<-as.Date('2018-12-31')

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

msciIndices<-data.frame(NAME=c('INDIA MOMENTUM',
	'EM (EMERGING MARKETS) MOMENTUM',
	'EUROPE MOMENTUM',
	'USA MOMENTUM',
	'WORLD MOMENTUM'))

msciIndices$ID<-NA	
msciIndices$ST_DATE<-NA
msciIndices$ED_DATE<-NA

for(i in 1:nrow(msciIndices)){
	msciId<-sqlQuery(lconUs2, sprintf("select index_code from MSCI_META where index_name='%s'", msciIndices$NAME[i]))[[1]]
	msciIndices$ID[i]<-msciId
	
	dates<-sqlQuery(lconUs2, sprintf("select min(time_stamp), max(time_stamp) from MSCI_DATA where id=%d", msciId))
	
	msciIndices$ST_DATE[i]<-toString(dates[1,1])
	msciIndices$ED_DATE[i]<-toString(dates[1,2])
}

msciIndices$ST_DATE<-as.Date(msciIndices$ST_DATE)
msciIndices$ED_DATE<-as.Date(msciIndices$ED_DATE)

startDate<-max(msciIndices$ST_DATE)
startYear<-year(startDate)+1

retDf<-data.frame(INDEX="", YEAR=0, RET=0.0)
for(i in 1:nrow(msciIndices)){
	monthlyVals<-downloadMsci(msciIndices$ID[i])
	annRets<-annualReturn(monthlyVals)
	
	rets<-data.frame(INDEX=rep(msciIndices$NAME[i], nrow(annRets)), YEAR=year(index(annRets)), RET=as.numeric(100*coredata(annRets)))
	retDf<-rbind(retDf, rets)
}

retDf<-retDf[-1,]
retDf$YEAR<-as.numeric(retDf$YEAR)
retDf$RET<-as.numeric(retDf$RET)

colourCount<-length(nrow(msciIndices))
getPalette = colorRampPalette(stata_pal("s2color")(14))

v2<-retDf[retDf$YEAR >= startYear,]
v2$INDEX<-factor(v2$INDEX, levels=sort(unique(v2$INDEX)))
v2$YEAR<-factor(v2$YEAR, levels=sort(unique(v2$YEAR)))

pdf(NULL)
ggplot(data=v2, aes(x=INDEX, y=RET, fill=YEAR)) +
  theme_economist() +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "", y="returns(%)", fill="", color="", title="MSCI Country Momentum Index Returns", subtitle=sprintf("%s:%s", startDate, endDate)) +
  annotate("text", x=0, y=min(v2$RET), label = "@StockViz", hjust=1.1, vjust=-.5, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/MSCI.sub-country.momentum.yearwise.%s.%s.png", reportPath, startDate, endDate), width=16, height=8, units="in")  

###
lts<-c(median(v2$RET)-sd(v2$RET),median(v2$RET)+sd(v2$RET))
ggplot(data=v2, aes(x=YEAR, y=INDEX)) + 
	theme_economist() +
	geom_tile(aes(fill = RET)) +
	geom_text(aes(label= sprintf("%.2f", RET)), hjust = 1) +
	scale_fill_gradientn(colors=c("red", "white", "green"), limits=lts) +
	guides(fill=F) +
	labs(x = "", y="", fill="", color="", title="MSCI Country Momentum Index Returns", subtitle=sprintf("%s:%s", startDate, endDate)) +
	annotate("text", x=1, y=0, label = "@StockViz", hjust=0.5, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/MSCI.sub-country.momentum.yearwise.heat.%s.%s.png", reportPath, startDate, endDate), width=25, height=6, units="in")

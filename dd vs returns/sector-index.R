library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('ggrepel')

options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
reportPath <- "d:/StockViz/report/"
lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("2007-01-01")
indexNames<-c('NIFTY AUTO',
	'NIFTY BANK',
	'NIFTY COMMODITIES',
	'NIFTY CONSUMPTION',
	'NIFTY ENERGY',
	'NIFTY FIN SERVICE',
	'NIFTY FMCG',
	'NIFTY INFRA',
	'NIFTY IT',
	'NIFTY MEDIA',
	'NIFTY METAL',
	'NIFTY PHARMA',
	'NIFTY PSU BANK',
	'NIFTY PVT BANK',
	'NIFTY REALTY',
	'NIFTY SERV SECTOR')
	
pXts<-xts()
for(i in indexNames){
	data<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE from BHAV_INDEX where index_name='%s' and time_stamp >= '%s'", i, startDate))
	dataXts<-xts(data[,-1], as.Date(data[,1]))
	pXts<-merge(pXts, dailyReturn(dataXts))
}

names(pXts)<-indexNames

retDf<-data.frame(INDEX="", CUM_RET=0.0, MAX_DD=0.0)
for(j in 1:length(pXts[1,])){
	retDf<-rbind(retDf, c(toString(indexNames[j]), 100.0*Return.cumulative(pXts[,j]), 100.0*as.numeric(table.Drawdowns(pXts[,j], 1)$Depth[1])))
}
retDf<-retDf[-1,]
retDf$CUM_RET<-as.numeric(retDf$CUM_RET)
retDf$MAX_DD<-as.numeric(retDf$MAX_DD)

ggplot(data=retDf, aes(x=CUM_RET, y=MAX_DD)) +
  theme_economist() +
  geom_point() + 
  geom_smooth() +
  geom_text_repel(aes(label=retDf$INDEX)) +
  ylab("Max Drawdown (%)") + 
  xlab("Cumulative Return (%s)") +
  ggtitle(sprintf("NIFTY Sector Index Drawdowns vs. Cumulative Returns [%s - %s] @StockViz", as.Date(index(first(pXts))), as.Date(index(last(pXts))))) 
  
ggsave(sprintf("%s/dd.vs.returns.png", reportPath))  
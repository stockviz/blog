library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
reportPath <- "."
COUNTRY_ETF_CSV <- "../country etfs/COUNTRY_NQ.csv"
allWorldEquityTicker<-'NQGIT'
indiaTrTicker<-'NQINT'

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("2004-03-01")
endDate<-as.Date("2018-12-31")

indices<-read.csv(COUNTRY_ETF_CSV)
indexNames<-c(sort(indices$CODE), allWorldEquityTicker)

pXts<-xts()
for(i in indexNames){
	ticker<-toString(i)
	iId<-as.numeric(sqlQuery(lconUs2, sprintf("select ID from QUANDL_META_V3 where DATASET_CODE='%s'", ticker))[[1]])
	
	data<-sqlQuery(lconUs2, sprintf("select trade_date, index_value from QUANDL_DATA_V3 where id=%d and trade_date >= '%s' and trade_date <= '%s'", iId, startDate, endDate))
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

colourCount<-nrow(retDf)
getPalette = colorRampPalette(stata_pal("s2color")(14))

pdf(NULL)
ggplot(data=retDf, aes(x=CUM_RET, y=MAX_DD)) +
  theme_economist() +
  geom_point(color=ifelse(retDf$INDEX == indiaTrTicker, 'red', ifelse(retDf$INDEX == allWorldEquityTicker, 'black', getPalette(colourCount))), 
			shape=ifelse(retDf$INDEX == indiaTrTicker, 15, ifelse(retDf$INDEX == allWorldEquityTicker, 17,20)), 
			size=ifelse(retDf$INDEX == indiaTrTicker, 3, ifelse(retDf$INDEX == allWorldEquityTicker, 3, 2))) + 
  geom_smooth() +
  guides(color=F) +
  geom_text_repel(aes(label=retDf$INDEX, color=getPalette(colourCount))) +
  labs(x = "Returns(%)", y="Max Drawdown (%)", fill="", color="", title="NASDAQOMX Country TR Index Drawdowns vs. Cumulative Returns", subtitle=sprintf("in USD, %s:%s", startDate, endDate)) +
  annotate("text", x=max(retDf$CUM_RET, na.rm=T), y=min(retDf$MAX_DD, na.rm=T), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/NASDAQOMX.dd.vs.returns.%s.%s.png", reportPath, startDate, endDate), width=14, height=8, units="in")  
library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
COUNTRY_ETF_CSV <- "../COUNTRY_NQ.csv"

startDate<-as.Date("2004-03-01")
endDate<-as.Date("2018-12-31")

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indices<-read.csv(COUNTRY_ETF_CSV)
indices$CODE<-sort(indices$CODE)

indices$NAME<-NA

indices$S_DATE<-NA
indices$E_DATE<-NA
indices$S_PX<-NA
indices$E_PX<-NA

for(i in 1:nrow(indices)){
	ticker<-indices$CODE[i]
	metaData<-sqlQuery(lconUs2, sprintf("select ID, NAME from QUANDL_META_V3 where DATASET_CODE='%s'", ticker))
	indices$NAME[i]<-toString(metaData$NAME[1])
	iId<-metaData$ID[1]
	
	sDate<-as.Date(sqlQuery(lconUs2, sprintf("select min(trade_date) from QUANDL_DATA_V3 where id=%d and trade_date >='%s'", iId, startDate))[[1]])
	eDate<-as.Date(sqlQuery(lconUs2, sprintf("select max(trade_date) from QUANDL_DATA_V3 where id=%d and trade_date <='%s'", iId, endDate))[[1]])
	
	sPx<-as.numeric(sqlQuery(lconUs2, sprintf("select index_value from QUANDL_DATA_V3 where id=%d and trade_date ='%s'", iId, sDate))[[1]])
	ePx<-as.numeric(sqlQuery(lconUs2, sprintf("select index_value from QUANDL_DATA_V3 where id=%d and trade_date ='%s'", iId, eDate))[[1]])
	
	indices$S_DATE[i]<-toString(sDate)
	indices$E_DATE[i]<-toString(eDate)
	indices$S_PX[i]<-sPx
	indices$E_PX[i]<-ePx
}

indices$RET<-indices$E_PX/indices$S_PX-1
indices$RET<-indices$RET*100.0

indices$SYMBOL<-factor(indices$CODE, levels=indices$CODE) 

colourCount<-nrow(indices)
getPalette = colorRampPalette(stata_pal("s2color")(14))

pdf(NULL)	
ggplot(indices, aes(x=SYMBOL, y=RET, fill=SYMBOL)) + 
	theme_economist() +
	scale_fill_manual(values = getPalette(colourCount)) +
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text(aes(label=round(RET,2)), hjust=-sign(indices$RET)*1.01, color="black", size=2) +
	coord_flip() +
	guides(fill=F) +
	labs(x = "", y="returns(%)", fill="", color="", title="NASDAQ Country TR Index Absolute Returns", subtitle=sprintf("in USD, %s:%s", startDate, endDate)) +
	annotate("text", x=0, y=max(indices$RET), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.5)
ggsave(sprintf("%s/INDEX-NQ.absolute.%s.%s.png", reportPath, startDate, endDate), width=9, height=12, units="in")
	
#################
	
allWorldEquityTicker<-'NQGIT'
metaData<-sqlQuery(lconUs2, sprintf("select ID, NAME from QUANDL_META_V3 where DATASET_CODE='%s'", allWorldEquityTicker))
iName<-toString(metaData$NAME[1])
iId<-metaData$ID[1]

sDate<-as.Date(sqlQuery(lconUs2, sprintf("select min(trade_date) from QUANDL_DATA_V3 where id=%d and trade_date >='%s'", iId, startDate))[[1]])
eDate<-as.Date(sqlQuery(lconUs2, sprintf("select max(trade_date) from QUANDL_DATA_V3 where id=%d and trade_date <='%s'", iId, endDate))[[1]])
	
sPx<-as.numeric(sqlQuery(lconUs2, sprintf("select index_value from QUANDL_DATA_V3 where id=%d and trade_date ='%s'", iId, sDate))[[1]])
ePx<-as.numeric(sqlQuery(lconUs2, sprintf("select index_value from QUANDL_DATA_V3 where id=%d and trade_date ='%s'", iId, eDate))[[1]])
	
awRet<-ePx/sPx-1	
indices$RET_REL<-indices$RET-100.0*awRet

ggplot(indices, aes(x=SYMBOL, y=RET_REL, fill=SYMBOL)) + 
	theme_economist() +
	scale_fill_manual(values = getPalette(colourCount)) +
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text(aes(label=round(RET_REL,2)), hjust=-sign(indices$RET_REL)*1.01, color="black", size=2) +
	coord_flip() +
	guides(fill=F) +
	labs(x = "", y="returns(%)", fill="", color="", title="NASDAQ Country TR Index Returns over NASDAQ Global TR Index (NQGIT)", subtitle=sprintf("in USD, %s:%s", startDate, endDate)) +
	annotate("text", x=0, y=max(indices$RET_REL), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.5)
ggsave(sprintf("%s/INDEX-NQ.NQGIT.%s.%s.png", reportPath, startDate, endDate), width=9, height=12, units="in")
	
#################

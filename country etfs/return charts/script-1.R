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
COUNTRY_ETF_CSV <- "../COUNTRY_ETFS.csv"

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

etfs<-read.csv(COUNTRY_ETF_CSV)
etfs$TICKER<-sort(etfs$TICKER)

etfs$NAME<-NA
etfs$START<-NA
etfs$END<-NA

for(i in 1:nrow(etfs)){
	ticker<-etfs$TICKER[i]
	etfName<-toString(sqlQuery(lcon, sprintf("select FUND from ETF_META where SYMBOL='%s'", ticker))$FUND[1])
	minMaxTime<-sqlQuery(lconUs2, sprintf("select min(time_stamp), max(time_stamp) from bhav_eq_iex where symbol='%s'", ticker))
	
	etfs$NAME[i]<-etfName
	etfs$START[i]<-minMaxTime[1,1]
	etfs$END[i]<-minMaxTime[1,2]
}

etfs$START<-as.Date(etfs$START)
etfs$END<-as.Date(etfs$END)

maxDate<-max(etfs$END)
etfs$LAST_DATE_DIFF<-difftime(maxDate, etfs$END, units='days')

etfSubset<-etfs[etfs$LAST_DATE_DIFF < 5,]
endDate<-min(etfSubset$END)
startDate<-ymd(endDate)-365

etfSubset$START_DATE_DIFF<-difftime(etfs$START, startDate, units='days')
etfSubset<-etfSubset[etfSubset$START_DATE_DIFF < 0,]

etfSubset$S_DATE<-NA
etfSubset$S_PX<-NA
etfSubset$E_PX<-NA

for(i in 1:nrow(etfSubset)){
	ticker<-etfSubset$TICKER[i]
	sDate<-as.Date(sqlQuery(lconUs2, sprintf("select max(time_stamp) from bhav_eq_iex where symbol='%s' and time_stamp <= '%s'", ticker, startDate))[[1]])
	sPx<-as.numeric(sqlQuery(lconUs2, sprintf("select c from bhav_eq_iex where symbol='%s' and time_stamp='%s'", ticker, sDate))[[1]])
	ePx<-as.numeric(sqlQuery(lconUs2, sprintf("select c from bhav_eq_iex where symbol='%s' and time_stamp='%s'", ticker, endDate))[[1]])
	
	etfSubset$S_DATE[i]<-toString(sDate)
	etfSubset$S_PX[i]<-sPx
	etfSubset$E_PX[i]<-ePx
}

etfSubset$RET<-etfSubset$E_PX/etfSubset$S_PX-1
etfSubset$RET<-etfSubset$RET*100.0

etfSubset$SYMBOL<-factor(etfSubset$TICKER, levels=etfSubset$TICKER) 

colourCount<-nrow(etfSubset)
getPalette = colorRampPalette(stata_pal("s2color")(14))

pdf(NULL)	
ggplot(etfSubset, aes(x=SYMBOL, y=RET, fill=SYMBOL)) + 
	theme_economist() +
	scale_fill_manual(values = getPalette(colourCount)) +
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text(aes(label=round(RET,2)), hjust=-sign(etfSubset$RET)*1.01, color="black", size=2) +
	coord_flip() +
	guides(fill=F) +
	labs(x = "", y="returns(%)", fill="", color="", title="Country ETF Absolute Returns", subtitle=sprintf("in USD, %s:%s", startDate, endDate)) +
	annotate("text", x=0, y=max(etfSubset$RET), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.5)
ggsave(sprintf("%s/ETF.absolute.png", reportPath), width=9, height=12, units="in")
	
#################
	
allWorldEquityEtfTicker<-'VT'
sDate<-as.Date(sqlQuery(lconUs2, sprintf("select max(time_stamp) from bhav_eq_iex where symbol='%s' and time_stamp <= '%s'", allWorldEquityEtfTicker, startDate))[[1]])
sPx<-as.numeric(sqlQuery(lconUs2, sprintf("select c from bhav_eq_iex where symbol='%s' and time_stamp='%s'", allWorldEquityEtfTicker, sDate))[[1]])
ePx<-as.numeric(sqlQuery(lconUs2, sprintf("select c from bhav_eq_iex where symbol='%s' and time_stamp='%s'", allWorldEquityEtfTicker, endDate))[[1]])
awRet<-ePx/sPx-1	
etfSubset$RET_REL<-etfSubset$RET-100.0*awRet

ggplot(etfSubset, aes(x=SYMBOL, y=RET_REL, fill=SYMBOL)) + 
	theme_economist() +
	scale_fill_manual(values = getPalette(colourCount)) +
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text(aes(label=round(RET_REL,2)), hjust=-sign(etfSubset$RET_REL)*1.01, color="black", size=2) +
	coord_flip() +
	guides(fill=F) +
	labs(x = "", y="returns(%)", fill="", color="", title="Country ETF Returns over VT (Vanguard Total World Stock ETF)", subtitle=sprintf("in USD, %s:%s", startDate, endDate)) +
	annotate("text", x=0, y=max(etfSubset$RET_REL), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.5)
ggsave(sprintf("%s/ETF.vt.png", reportPath), width=9, height=12, units="in")
	
#################

tt1<-arrangeGrob(grobs=list(tableGrob(etfSubset[, c('TICKER', 'NAME')], rows=NULL, theme=tableTheme)), ncol=1, top=textGrob("Country ETFs", gp=gpar(fontsize=14, fontface ='bold', fontfamily='Segoe UI')), bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))

ggsave(sprintf("%s/ETF.list.png", reportPath), tt1, width=6, height=nrow(etfSubset)*0.3, units='in')	
	
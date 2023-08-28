library('RODBC')
library('mongolite')
library('quantmod')
library('PerformanceAnalytics')

library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('grid')
library('gridExtra')
library('gtable')
library('reshape2')
library('ggpubr')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
COUNTRY_ETF_CSV <- "../COUNTRY_ETFS.csv"

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/misc.R")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

mcon <- mongo('ycharts', url="mongodb://holland/EtfPortfolioDb")

etfs<-read.csv(COUNTRY_ETF_CSV)
etfs<-rbind(etfs, c('EEM'))
etfs$NAME<-NA

for(i in 1:nrow(etfs)){
	ticker<-etfs$TICKER[i]
	etfName<-toString(sqlQuery(lcon, sprintf("select FUND from ETF_META where SYMBOL='%s'", ticker))$FUND[1])
	etfs$NAME[i]<-etfName
}

etfLastUpdate<-mcon$aggregate('[{"$group": { "_id": "$etfTicker" , "updated": { "$max": "$downloadDate" } } }]')

fundaDf<-data.frame()
for(i in 1:nrow(etfs)){
	ticker<-etfs$TICKER[i]
	
	lastUpdate<-as.Date(etfLastUpdate[etfLastUpdate[,1] == ticker, 2])
	
	print(paste(ticker, lastUpdate))
	
	info<-mcon$find(query = sprintf('{ "etfTicker": "%s", "download.title": "Fundamentals" }', ticker, lastUpdate-1), fields = '{"downloadDate": 1, "download.title.$": 1}', sort = '{"downloadDate": -1}', limit=1)
	funda<-info$download[[1]]$info[[1]]
	funda$k<-trimws(gsub("\\(.*?\\)", "", funda$k))
	
	if(nrow(fundaDf) == 0){
		fundaDf<-data.frame(matrix(funda$v, nrow=1))
		names(fundaDf)<-funda$k
		fundaDf$TICKER<-ticker
		fundaDf$AS_OF<-as.Date(info$downloadDate)
	} else {
		tempDf<-data.frame(matrix(funda$v, nrow=1))
		names(tempDf)<-funda$k
		tempDf$TICKER<-ticker
		tempDf$AS_OF<-as.Date(info$downloadDate)
		
		tryCatch({
			fundaDf<-rbind(fundaDf, tempDf)
		}, error=function(e){
			print(ticker)
			print(e)
		})
	}
}

fundaDf2<-subset(fundaDf, select=c(-`Dividend Yield TTM`, -`30-Day SEC Yield`, -`7-Day SEC Yield`, -`Number of Holdings`, -`AS_OF`))
fundaDf2<-fundaDf2[order(fundaDf2$TICKER),]

valNames<-names(fundaDf2)[names(fundaDf2) != 'TICKER']

fundaDf3<-NULL
for(j in 1:length(valNames)){
	fundaDf3<-cbind(fundaDf3, as.numeric(scale(fundaDf2[,valNames[j]])))
}
colnames(fundaDf3)<-valNames
fundaDf3<-data.frame(fundaDf3)

fundaDf3$TICKER <- fundaDf2$TICKER

v2 <- melt(data.frame(fundaDf2), id='TICKER')
v2$variable <- factor(v2$variable, levels=unique(v2$variable))

v3 <- melt(fundaDf3, id='TICKER')
v3$variable <- factor(v3$variable, levels=unique(v3$variable))

v4<-merge(v2, v3, by=c('TICKER', 'variable'))
colnames(v4)<-c('TICKER', 'METRIC', 'raw', 'scaled')
v4$METRIC <- factor(v4$METRIC, levels=unique(v4$METRIC))

pdf(NULL)
p<-ggplot(data=v4, aes(x=METRIC, y=TICKER, fill=scaled)) + 
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_tile() +
	geom_text(aes(label= sprintf("%.2f", raw)), hjust = 1) +
	scale_fill_gradientn(colors=c("green", "white", "red")) +
	guides(fill=F) +
	labs(x = "", y="", fill="", color="", title="ETF Fundamental Metrics") 
annotate_figure(p, bottom = text_grob("source: ycharts. created: @StockViz", size=12, family="Segoe UI", color='grey'))	
	
ggsave(sprintf("%s/ETF.fundamental.heat.%s.png", reportPath, Sys.Date()), width=18, height=12, units="in")

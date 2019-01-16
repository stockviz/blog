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
reportPath <- "."
COUNTRY_ETF_CSV <- "../COUNTRY_NQ.csv"
indiaTrTicker<-'NQINT'

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("2004-03-01")
endDate<-as.Date("2018-12-31")

indices<-read.csv(COUNTRY_ETF_CSV)
indexNames<-sort(indices$CODE)

pXts<-xts()
for(i in indexNames){
	ticker<-toString(i)
	iId<-as.numeric(sqlQuery(lconUs2, sprintf("select ID from QUANDL_META_V3 where DATASET_CODE='%s'", ticker))[[1]])
	
	data<-sqlQuery(lconUs2, sprintf("select trade_date, index_value from QUANDL_DATA_V3 where id=%d and trade_date >= '%s' and trade_date <= '%s'", iId, startDate, endDate))
	dataXts<-xts(data[,-1], as.Date(data[,1]))
	pXts<-merge(pXts, dailyReturn(dataXts))
}

names(pXts)<-indexNames

yrs<-sort(unique(year(index(pXts))))
retDf<-data.frame(INDEX="", YEAR=0, VOL=0.0)

for(j in 1:length(pXts[1,])){
	iName<-toString(names(pXts)[j])
	allVol<-sd(coredata(pXts[,j]), na.rm=T)
	retDf<-rbind(retDf, c(iName, 0, allVol))
	for(yr in yrs){
		yrVol<-sd(coredata(pXts[sprintf("%d", yr),j]), na.rm=T)
		retDf<-rbind(retDf, c(iName, yr, yrVol))
	}
}
retDf<-retDf[-1,]
retDf$YEAR<-as.numeric(retDf$YEAR)
retDf$VOL<-as.numeric(retDf$VOL)*100

colourCount<-length(indexNames)
getPalette = colorRampPalette(stata_pal("s2color")(14))

## plot all-years volatility

v1<-retDf[retDf$YEAR ==0, c('INDEX','VOL')]
v1$INDEX<-factor(v1$INDEX, levels=sort(v1$INDEX))
pdf(NULL)
ggplot(data=v1, aes(x=INDEX, y=VOL, fill=INDEX)) +
  theme_economist() +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  guides(fill=F) +
  labs(x = "", y="standard deviation", fill="", color="", title="NASDAQOMX Country TR Index", subtitle=sprintf("close-close standard deviation, %s:%s", startDate, endDate)) +
  annotate("text", x=0, y=max(v1$VOL), label = "@StockViz", hjust=1.1, vjust=-.5, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/NASDAQOMX.volatility.%s.%s.png", reportPath, startDate, endDate), width=8, height=14, units="in")  

## plot year-wise volatility

v2<-retDf[retDf$YEAR != 0,]
v2$INDEX<-factor(v2$INDEX, levels=sort(unique(v2$INDEX)))
v2$YEAR<-factor(v2$YEAR, levels=sort(unique(v2$YEAR)))

pdf(NULL)
ggplot(data=v2, aes(x=INDEX, y=VOL, fill=YEAR)) +
  theme_economist() +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  labs(x = "", y="standard deviation", fill="", color="", title="NASDAQOMX Country TR Index", subtitle=sprintf("close-close standard deviation, %s:%s", startDate, endDate)) +
  annotate("text", x=0, y=max(v2$VOL), label = "@StockViz", hjust=1.1, vjust=-.5, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/NASDAQOMX.volatility.yearwise.%s.%s.png", reportPath, startDate, endDate), width=8, height=20, units="in")  

###
lts<-c(median(v2$VOL)-sd(v2$VOL),median(v2$VOL)+sd(v2$VOL))
ggplot(data=v2, aes(x=YEAR, y=INDEX)) + 
	theme_economist() +
	geom_tile(aes(fill = VOL)) +
	geom_text(aes(label= sprintf("%.2f", VOL)), hjust = 1) +
	scale_fill_gradientn(colors=c("green", "white", "red"), limits=lts) +
	guides(fill=F) +
	labs(x = "", y="", fill="", color="", title="NASDAQOMX Country TR Index", subtitle=sprintf("close-close standard deviation, %s:%s", startDate, endDate)) +
	annotate("text", x=1, y=0, label = "@StockViz", hjust=0.5, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/NASDAQOMX.volatility.yearwise.heat.%s.%s.png", reportPath, startDate, endDate), width=14, height=12, units="in")

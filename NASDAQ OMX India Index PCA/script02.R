library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('lubridate')
library('reshape2')
library('ggrepel')

options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
reportPath <- "."
lconUS2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUS2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

#NASDAQ India TR Index (NQINT)
trId<-13121272 

#NASDAQ India Basic Matls TR Index (NQIN1000T)
#NASDAQ India Cnsmr Goods TR Index (NQIN3000T)
#NASDAQ India Financials TR Index (NQIN8000T)
#NASDAQ India Health Care TR Index (NQIN4000T)
#NASDAQ India Inds TR Index (NQIN2000T)
#NASDAQ India Tech TR Index (NQIN9000T)
compIds<-c(13120874, 13120916, 13120986, 13121029, 13121072, 13121253)
compNames<-c()
for(id in compIds){
	cName<-sqlQuery(lconUS2, sprintf("select name from QUANDL_META_V3 where id=%d", id))[[1]]
	compNames<-c(compNames, toString(cName))
}

startDate<-as.Date("2001-03-30")
endDate<-as.Date("2017-12-31")
smaLb<-c(50, 100, 200)
rollingWindow<-20 #days

#index levels
allData<-xts()
for(id in c(trId, compIds)){
	idd<-as.numeric(id)
	data<-sqlQuery(lconUS2, sprintf("select TRADE_DATE, INDEX_VALUE from QUANDL_DATA_V3 where id = %d and trade_date >= '%s' and trade_date <= '%s'", idd, startDate, endDate))
	allData<-merge(allData,xts(data[,-1], as.Date(data[,1])))
}

valNames<-sapply(c(trId, compIds), function(x) sprintf("VAL_%d", x))
names(allData)<-valNames

#daily returns
for(id in valNames){
	idd<-toString(id)
	allData<-merge(allData, dailyReturn(allData[,idd]))
}

retNames<-sapply(c(trId, compIds), function(x) sprintf("%d_RET", x))
names(allData)<-c(valNames, retNames)

#calculate rolling returns
rRets<-rollapply(allData[,retNames], rollingWindow, function(X) Return.cumulative(X))
rRets<-lag(rRets, -rollingWindow)
allData<-merge(allData, rRets)
rRetNames<-sapply(c(trId, compIds), function(x) sprintf("%d_RRET", x))
names(allData)<-c(valNames, retNames, rRetNames)

#various SMAs of indices
for(sma in smaLb){
	allData<-merge(allData, SMA(allData[, valNames[1]], sma))
}

smaNames<-sapply(smaLb, function(x) sprintf("SMA_%d", x))
names(allData)<-c(valNames, retNames, rRetNames, smaNames)

allData<-na.omit(allData)
firstDate<-as.Date(first(index(allData)))
lastDate<-as.Date(last(index(allData)))
for(i in smaNames){
	toPlot<-data.frame(allData[,1], index(allData[,1]), ifelse(allData[,1] > allData[,i], 'above', 'below'))
	names(toPlot)<-c('NQINT', 'DAY', 'SMA')
	toPlot$DAY<-as.Date(toPlot$DAY)
	xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)
	
	pdf(NULL)
	ggplot(data=toPlot, aes(x=DAY, y=NQINT)) +
		theme_economist() +
		geom_line(aes(color=SMA, group=1)) + 
		scale_x_date(breaks = xAxisTicks) +
		scale_y_log10() +
		labs(x='', y='log()', color='', title="NQINT", subtitle=sprintf("%s [%s:%s]", i, firstDate, lastDate)) +
		annotate("text", x=lastDate, y=100, label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/NQINT.%s.png", reportPath, i), dpi=600, width=12, height=6, units="in")	
}

yrStart<-year(index(first(allData)))+1
yrEnd<-year(index(last(allData)))
yrInc<-4 #5 year sliding window of 1 year

pc1Loadings<-data.frame(matrix(nrow=1, ncol=length(compIds)+4))
loadingNames<-sapply(compIds, function(x) sprintf("VAL_%d", x))
names(pc1Loadings)<-c("Y1", "Y2", "SMA", "AB", loadingNames)

for(yrs in seq(from=yrStart, to=yrEnd-yrInc, by=1)){
	y1<-yrs
	y2<-yrs + yrInc
	yrRange<-sprintf("%d/%d", y1, y2)
	print(yrRange)
	
	allSubset<-allData[, rRetNames]
	allSubset<-allSubset[yrRange,-1]
	prRet<-prcomp(allSubset)
	pc1Loadings<-rbind(pc1Loadings, c(y1, y2, "ALL", "ALL", prRet$rotation[,'PC1']))
	
	for(i in 1:length(smaNames)){
		smaName<-toString(smaNames[i])

		asma<-allData[allData[,1] > allData[,smaName], rRetNames]
		bsma<-allData[allData[,1] < allData[,smaName], rRetNames]
		
		#remove the TR column
		asma<-asma[yrRange,-1]
		bsma<-bsma[yrRange,-1]
		
		#break it down
		prRetAsma<-prcomp(asma)
		prRetBsma<-prcomp(bsma)
		
		pc1Loadings<-rbind(pc1Loadings, c(y1, y2, smaName, "A", prRetAsma$rotation[,'PC1']))
		pc1Loadings<-rbind(pc1Loadings, c(y1, y2, smaName, "B", prRetBsma$rotation[,'PC1']))
	}
}

pc1Loadings<-pc1Loadings[-1,]
pc1Loadings[, c("Y1", "Y2", loadingNames)]<- apply(pc1Loadings[,c("Y1", "Y2", loadingNames)], 2, function(x) as.numeric(x))
pc1Loadings[pc1Loadings[, loadingNames[1]] < 0,loadingNames] <- -pc1Loadings[pc1Loadings[, loadingNames[1]] < 0, loadingNames]
 
oldNames<-names(pc1Loadings) 
pc1Loadings<-cbind(pc1Loadings, pc1Loadings[, loadingNames]/rowSums(pc1Loadings[, loadingNames]))

names(pc1Loadings)<-c(oldNames, compNames)

toPlot<-pc1Loadings[pc1Loadings$AB=="ALL", c("Y2", compNames)]
toPlot[, compNames]<-100.0*toPlot[, compNames]

pdf(NULL)
ggplot(data=melt(toPlot, id='Y2'), aes(x=Y2, y=value, fill=variable)) +
	  theme_economist() +
	  theme(legend.text = element_text(size=10, family='Segoe UI'))+
	  geom_line(aes(color=variable), size=1) + 
	  scale_x_continuous(breaks=toPlot$Y2) +
	  geom_text_repel(aes(label=round(value, 0)), size=2.5) +
	  scale_color_discrete(name = "")+
	  labs(x="", y="Loading", title="PCA - NASDAQOMX India TR Indices", subtitle="5yr sliding window of 20-day returns (ALL DATA)") +
	  annotate("text", x=max(toPlot$Y2), y=0, label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	  
ggsave(sprintf("%s/factor-loadings.ROLLING.ALL.png", reportPath), dpi=600, width=12, height=6, units="in")  

for(i in smaNames){
	for(j in c('A', 'B')){
		toPlot<-pc1Loadings[pc1Loadings$AB==j & pc1Loadings$SMA == i, c("Y2", compNames)]
		toPlot[, compNames]<-100.0*toPlot[, compNames]
		
		pdf(NULL)
		ggplot(data=melt(toPlot, id='Y2'), aes(x=Y2, y=value, fill=variable)) +
			theme_economist() +
			ylim(0, 100*max(pc1Loadings[, compNames])) +
			theme(legend.text = element_text(size=10, family='Segoe UI'))+
 			geom_line(aes(color=variable), size=1) + 
			scale_x_continuous(breaks=toPlot$Y2) +
			geom_text_repel(aes(label=round(value, 0)), size=2.5) +
			scale_color_discrete(name = "")+
			labs(x="", y= "Loading", title="PCA - NASDAQOMX India TR Indices", subtitle=sprintf("5yr sliding window of 20-day returns (%s %s)", j, i)) +
			annotate("text", x=max(toPlot$Y2), y=0, label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
			
		ggsave(sprintf("%s/factor-loadings.ROLLING.%s-%s.png", reportPath, j, i), dpi=600, width=12, height=6, units="in")  
	}
}
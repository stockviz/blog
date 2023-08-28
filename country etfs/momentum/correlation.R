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
source("D:/StockViz/public/blog/common/msci.R")
reportPath <- "."

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date('1996-05-31')
endDate<-as.Date('2019-12-31')

keyIndexName <- "INDIA MOMENTUM"

momIndices <- sqlQuery(lconUs2, "select m.INDEX_NAME, m.INDEX_CODE, min(d.time_stamp) s, max(d.time_stamp) e
						from MSCI_META m, MSCI_DATA d 
						where INDEX_NAME like '%momentum%'
						and INDEX_NAME not like '%tilt%'
						and INDEX_NAME not like '%barra%'
						and INDEX_NAME not like '%imi%'
						and m.INDEX_CODE = d.ID
						and d.INDEX_TYPE='g'
						group by m.INDEX_NAME, m.INDEX_CODE
						order by e")

momIndices <- momIndices[momIndices$s <= startDate,]

keyIndexId <- momIndices[momIndices$INDEX_NAME == keyIndexName,]$INDEX_CODE[1]

keyIndexTs <- Common.DownloadMsci(keyIndexId, "G", startDate, endDate)
keyIndexRet <- na.omit(keyIndexTs/lag(keyIndexTs,1)-1)

doStuff <- function(lookback){
	rollingCors <- NULL
	for(i in 1:nrow(momIndices)){
		indexId <- momIndices[i, 'INDEX_CODE']
		if (indexId == keyIndexId) next
		
		indexTs <- Common.DownloadMsci(indexId, "G", startDate, endDate)
		indexRet <- na.omit(indexTs/lag(indexTs,1)-1)

		corData <- na.omit(merge(keyIndexRet, indexRet))
		rollingCors <- merge.xts(rollingCors, na.omit(runCor(corData[,1], corData[,2], lookback)))
	}

	names(rollingCors) <- momIndices[momIndices$INDEX_NAME != keyIndexName,]$INDEX_NAME

	medianCors <- melt(apply(data.frame(rollingCors), 2, function(X) median(X, na.rm=T)))
	medianCors$INDEX_NAME<-row.names(medianCors)
	top5 <- medianCors[order(medianCors[,1]),]

	toPlot <- data.frame(rollingCors)
	toPlotDf <- melt(toPlot)

	ggplot(toPlotDf, aes(x=variable, y=value, color=variable)) +
		theme_economist() +
		geom_violin(draw_quantiles = c(0.5)) +
		theme(axis.text.x=element_text(angle=90, hjust=1)) +
		labs(x='', y='', fill='', color='',
			 title=sprintf("Rolling Correlations of Momentum Indices with %s", keyIndexName),
			 subtitle=sprintf("%d-months [%s:%s]", lookback, startDate, endDate)) +
		guides(fill=F, color=F) +
		annotate("text", x=1, y=0, 
				 label = "@StockViz", hjust=1.1, vjust=-1.1, 
				 col="white", cex=6, fontface = "bold", alpha = 0.8)
	ggsave(sprintf("%s/correlations-%s-%d.png", reportPath, keyIndexName, lookback), width=16, height=8, units="in")
	
	return(top5)
}	

t51 <- doStuff(12*3)
t52 <- doStuff(12*5)		 

print(t51)
print(t52)
library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
dataPath <- "."

#reportPath <- "D:/StockViz/public/blog/mutual funds"
#dataPath <- "D:/StockViz/public/blog/mutual funds"

source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

mfEtfMap<-read.csv(file=sprintf("%s/mf.etf.map.csv", dataPath))

mfEtfMap$PX_FIRST_DT<-NA
mfEtfMap$PX_LAST_DT<-NA

for(i in 1:length(mfEtfMap[,1])){
	pxFirstLast<-sqlQuery(lcon, sprintf("select min(time_stamp), max(time_stamp) from px_history where symbol='%s'", mfEtfMap$NSE_SYMBOL[i]))
	mfEtfMap$PX_FIRST_DT[i]<-toString(pxFirstLast[1,1])
	mfEtfMap$PX_LAST_DT[i]<-toString(pxFirstLast[1,2])
}

mfEtfMap$PX_FIRST_DT<-as.Date(mfEtfMap$PX_FIRST_DT)
mfEtfMap$PX_LAST_DT<-as.Date(mfEtfMap$PX_LAST_DT)

lastDate<-max(mfEtfMap$PX_LAST_DT, na.rm=T)
mfEtfMapFinal<-mfEtfMap[mfEtfMap$PX_LAST_DT == lastDate,]
mfEtfMapFinal<-mfEtfMapFinal[mfEtfMapFinal$PX_FIRST_DT < lastDate-365,]
mfEtfMapFinal<-na.omit(mfEtfMapFinal)

mfEtfMapFinal$PX_CLOSE<-0.0
mfEtfMapFinal$PX_LAST<-0.0
mfEtfMapFinal$NAV<-0.0

navDate<-as.Date(sqlQuery(lcon, sprintf("select max(as_of) from MF_NAV_HISTORY where scheme_code in (%s)", paste(mfEtfMapFinal$SCHEME_CODE, collapse=',')))[[1]])

dataDf<-data.frame(SYMBOL="", TIME_STAMP="", C=0.0, L=0.0)

for(i in 1:length(mfEtfMapFinal[,1])){
	premDisc<-sqlQuery(lcon, sprintf("select SYMBOL, TIME_STAMP, PX_CLOSE/NAV-1 C, PX_LAST/NAV-1 L from PX_HISTORY P, MF_NAV_HISTORY M
									WHERE symbol='%s' and (series='eq' or series='be')
									AND scheme_code=%d and as_of=time_stamp
									and time_stamp >= '%s'", mfEtfMapFinal$NSE_SYMBOL[i], mfEtfMapFinal$SCHEME_CODE[i], lastDate-365))
	
	dataDf<-rbind(dataDf, premDisc[, c('SYMBOL', 'TIME_STAMP', 'C', 'L')])
}
dataDf<-dataDf[-1,]
dataDf$C<-dataDf$C*100.0
dataDf$L<-dataDf$L*100.0
dataDf$TIME_STAMP<-as.Date(dataDf$TIME_STAMP)
firstDate<-min(dataDf$TIME_STAMP)

#chart the boxplots of premium/discount
doBoxPlots<-function(plotDf){
	pdf(NULL)
	ggplot(plotDf, aes(SYMBOL, C)) +
		theme_economist() +
		geom_boxplot(outlier.colour = "red", outlier.shape = 1, varwidth = TRUE) +
		geom_jitter(alpha=0.1, width = 0.2) +
		coord_flip() +
		labs(x='', y='premium/discount (%)', color='', title="Premium/Discount of ETF over MF", subtitle=sprintf("Closing Price vs. NAV [%s:%s]", firstDate, lastDate)) +
		annotate("text", x=0, y=max(plotDf$C), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=4, fontface = "bold")

	ggsave(sprintf("%s/mf.etf.prem.disc.closing.BOX.png", reportPath), width=7, height=12, units="in")
		
	ggplot(plotDf, aes(SYMBOL, L)) +
		theme_economist() +
		geom_boxplot(outlier.colour = "red", outlier.shape = 1, varwidth = TRUE) +
		geom_jitter(alpha=0.1, width = 0.2) +
		coord_flip() +
		labs(x='', y='premium/discount (%)', color='', title="Premium/Discount of ETF over MF", subtitle=sprintf("Last Price vs. NAV [%s:%s]", firstDate, lastDate)) +
		annotate("text", x=0, y=max(plotDf$L), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=4, fontface = "bold")

	ggsave(sprintf("%s/mf.etf.prem.disc.last.BOX.png", reportPath), width=7, height=12, units="in")
}

#chart the time-series of premium/discount
doTsCharts<-function(plotDf){
	pdf(NULL)
	ggplot(plotDf, aes(x=TIME_STAMP, y=C, color=SYMBOL)) +
		theme_economist() +
		geom_line() +
		labs(x='', y='premium/discount (%)', color='', title="Premium/Discount of ETF over MF", subtitle=sprintf("Closing Price vs. NAV [%s:%s]", firstDate, lastDate)) +
		annotate("text", x=max(plotDf$TIME_STAMP), y=min(plotDf$C), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold")
	ggsave(sprintf("%s/mf.etf.prem.disc.closing.TS.png", reportPath), dpi=600, width=12, height=6, units="in")
	
	ggplot(plotDf, aes(x=TIME_STAMP, y=L, color=SYMBOL)) +
		theme_economist() +
		geom_line() +
		labs(x='', y='premium/discount (%)', color='', title="Premium/Discount of ETF over MF", subtitle=sprintf("Last Price vs. NAV [%s:%s]", firstDate, lastDate)) +
		annotate("text", x=max(plotDf$TIME_STAMP), y=min(plotDf$L), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold")
	ggsave(sprintf("%s/mf.etf.prem.disc.last.TS.png", reportPath), dpi=600, width=12, height=6, units="in")
}

doBoxPlots(dataDf)
doTsCharts(dataDf[dataDf$SYMBOL %in% c('M100', 'N100', 'RRSLGETF'),])   #for the The EQUAL-III Theme ETF portfolio
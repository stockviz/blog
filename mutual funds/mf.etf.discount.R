library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
#reportPath <- "D:/StockViz/public/blog/mutual funds"


source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

lastDate <- sqlQuery(lcon, "select max(time_stamp) from px_history")[[1]]
cutoff <- lastDate - 3*365
etfFunds <- sqlQuery(lcon, sprintf("select t.SYMBOL, n.SCHEME_CODE, n.SCHEME_NAME from ETF_TICKER t, MF_NAV_HISTORY n 
									where t.ISIN=n.ISIN_PAYOUT_GROWTH 
									and SCHEME_NAME like '%%etf%%' 
									and AS_OF='%s'
									and DATE_LISTING <= '%s'", lastDate, cutoff))

etfDates <- sqlQuery(lcon, sprintf("select SYMBOL, min(time_stamp) stdt, max(time_stamp) eddt from px_history where symbol in ('%s') group by symbol", paste(etfFunds$SYMBOL, collapse="','")))
etfDates <- etfDates[etfDates$eddt == lastDate,]

mfEtfMap <- etfDates %>% inner_join(etfFunds, by='SYMBOL')

dataDf<-data.frame(SYMBOL="", TIME_STAMP=Sys.Date(), C=0.0, L=0.0)

for(i in 1:nrow(mfEtfMap)){
	premDisc<-sqlQuery(lcon, sprintf("select SYMBOL, TIME_STAMP, PX_CLOSE/NAV-1 C, PX_LAST/NAV-1 L from PX_HISTORY P, MF_NAV_HISTORY M
									WHERE symbol='%s' and (series='eq' or series='be')
									AND scheme_code=%d and as_of=time_stamp
									and time_stamp >= '%s'", mfEtfMap$SYMBOL[i], mfEtfMap$SCHEME_CODE[i], cutoff))
	
	dataDf<-rbind(dataDf, premDisc[, c('SYMBOL', 'TIME_STAMP', 'C', 'L')])
}
dataDf<-dataDf[-1,]
dataDf$C<-dataDf$C*100.0
dataDf$L<-dataDf$L*100.0
dataDf$TIME_STAMP<-as.Date(dataDf$TIME_STAMP)
firstDate<-min(dataDf$TIME_STAMP)

dataDf2 <- dataDf[abs(dataDf$C) < 50,]

#chart the boxplots of premium/discount
doBoxPlots<-function(plotDf, nameFix){
	pdf(NULL)
	ggplot(plotDf, aes(SYMBOL, C)) +
		theme_economist() +
		geom_boxplot(outlier.colour = "red", outlier.shape = 1, varwidth = TRUE) +
		geom_jitter(alpha=0.1, width = 0.2) +
		coord_flip() +
		labs(x='', y='premium/discount (%)', color='', title="Premium/Discount of ETF over MF", subtitle=sprintf("Closing Price vs. NAV [%s:%s]", firstDate, lastDate)) +
		annotate("text", x=0, y=max(plotDf$C), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=4, fontface = "bold")

	ggsave(sprintf("%s/mf.etf.prem.disc.closing.BOX.%s.png", reportPath, nameFix), width=7, height=14, units="in")
		
	ggplot(plotDf, aes(SYMBOL, L)) +
		theme_economist() +
		geom_boxplot(outlier.colour = "red", outlier.shape = 1, varwidth = TRUE) +
		geom_jitter(alpha=0.1, width = 0.2) +
		coord_flip() +
		labs(x='', y='premium/discount (%)', color='', title="Premium/Discount of ETF over MF", subtitle=sprintf("Last Price vs. NAV [%s:%s]", firstDate, lastDate)) +
		annotate("text", x=0, y=max(plotDf$L), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=4, fontface = "bold")

	ggsave(sprintf("%s/mf.etf.prem.disc.last.BOX.%s.png", reportPath, nameFix), width=7, height=14, units="in")
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

doBoxPlots(dataDf, "RAW")
doBoxPlots(dataDf2, "50")
#doTsCharts(dataDf[dataDf$SYMBOL %in% c('M100', 'N100', 'RRSLGETF'),])   #for the The EQUAL-III Theme ETF portfolio
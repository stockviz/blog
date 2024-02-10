source("d:/stockviz/r/config.r")
reportPath<-"."

library('RODBC')
library('metrica')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('patchwork')

pdf(NULL)
options(stringsAsFactors = FALSE)
options("scipen"=100)

indices <- c("NIFTY 50", "NIFTY MIDCAP 150")

volLbs <- seq(10, 500, by=10)

startDate <- as.Date("2005-04-01")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

volDf = data.frame(INAME = "", LB = 0, MIN = 0.0, Q1 = 0.0, MEDIAN = 0.0, MEAN = 0.0, Q3 = 0.0, MAX = 0.0)
for(indexName in indices){
	indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_open as [Open], px_high as [High], px_low as [Low], px_close as [Close] from BHAV_INDEX where index_name='%s' and time_stamp >= '%s'", indexName, startDate))
	pXts <- xts(indexPx[,-1], indexPx[,1])
	
	for(volLb in volLbs){
		ivol <- volatility(pXts, n=volLb, calc = "yang.zhang")
		ivolDf <- as.data.frame.matrix(summary(coredata(ivol)))
		ivolRow <- as.numeric(gsub(" ", "", (gsub(".*:","", ivolDf[,1]))))
		volDf <- rbind(volDf, c(indexName, volLb, ivolRow))
	}
}

volDf <- volDf[-1,]

toPlot <- volDf
toPlot$INAME <- factor(toPlot$INAME, levels = unique(toPlot$INAME))
toPlot$LB <- factor(toPlot$LB, levels = unique(toPlot$LB))

ggplot(toPlot, aes(x=LB, group = interaction(LB, INAME), color = INAME)) +
	theme_economist() +
	#scale_color_viridis_d() +
	geom_boxplot(stat="identity", aes(lower = as.numeric(Q1), upper = as.numeric(Q3), middle = as.numeric(MEDIAN), ymin = as.numeric(MIN), ymax = as.numeric(MAX))) +
	labs(x="lookback (days)", y="volatility", color="", title = "Volatility over lookbacks", subtitle=sprintf("%s:%s", first(index(pXts)), last(index(pXts)))) +
	annotate("text", x=nlevels(toPlot$LB), y=max(as.numeric(toPlot$MAX)), label = "@StockViz", hjust='right', vjust='top', col="white", cex=5, fontface = "bold", alpha = 0.5)
	
ggsave(sprintf("%s/volatility-signature.png", reportPath), width=18, height=6, units="in")	
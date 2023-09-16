source("d:/stockviz/r/config.r")

reportPath<-"."

library('RODBC')
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

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate <- as.Date("2011-01-01")
indexName <- "NIFTY 50"
lb <- 50 #days

indexDf <- sqlQuery(lcon, sprintf("select time_stamp, px_open as [Open], px_high as [High], px_low as [Low], px_close as [Close] from bhav_index where index_name = '%s' and time_stamp >= '%s'", indexName, startDate))
iXts <- xts(indexDf[,-1], indexDf[,1])

rets <- merge(iXts$Open/stats::lag(iXts$Close, 1) - 1, iXts$Close/stats::lag(iXts$Close, 1) - 1, iXts$Close/iXts$Open - 1)
names(rets) <- c('CO', 'CC', 'OC')

rets <- rets[-1,]

sds <- rollapply(rets, lb, function(X) sd(X))

sds <- na.omit(sds)

toPlot <- data.frame(sds)
toPlot$T <- index(sds)
toPlot <- melt(toPlot, id='T')

ggplot(toPlot, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_line() +
	scale_x_date(date_breaks='6 months', date_labels='%Y %b %d') +
	labs(x = "", y="", fill="", color="", title=sprintf("%s Volatility", indexName), subtitle=sprintf("%d-day std. dev. [%s:%s]", lb, first(index(iXts)), last(index(iXts)))) +
	annotate("text", x=min(toPlot$T), y=max(toPlot$value), label = "@StockViz", hjust='left', vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8) 
	
ggsave(sprintf("%s/%s-index.png", reportPath, indexName), width=12, height=6, units="in")		


sds2 <- sds["2020-09-01/"]

toPlot <- data.frame(sds2)
toPlot$T <- index(sds2)
toPlot <- melt(toPlot, id='T')

ggplot(toPlot, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	geom_line() +
	scale_x_date(date_breaks='6 months', date_labels='%Y %b %d') +
	labs(x = "", y="", fill="", color="", title=sprintf("%s Volatility", indexName), subtitle=sprintf("%d-day std. dev. [%s:%s]", lb, first(index(sds2)), last(index(sds2)))) +
	annotate("text", x=min(toPlot$T), y=max(toPlot$value), label = "@StockViz", hjust='left', vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8) 
	
ggsave(sprintf("%s/%s-index.post.png", reportPath, indexName), width=12, height=6, units="in")		
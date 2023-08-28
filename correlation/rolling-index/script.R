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

indexName1a <- "NIFTY 50 TR"
indexName2a <- "NIFTY MIDCAP 150 TR"

indexName1b <- "NIFTY200 MOMENTUM 30 TR"
indexName2b <- "NIFTY MIDCAP150 MOMENTUM 50 TR"


startDate <- as.Date("2005-04-01")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s'", indexName1a, startDate))
index1aXts <- xts(indexPx[,2], indexPx[,1])

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s'", indexName2a, startDate))
index2aXts <- xts(indexPx[,2], indexPx[,1])

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s'", indexName1b, startDate))
index1bXts <- xts(indexPx[,2], indexPx[,1])

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s'", indexName2b, startDate))
index2bXts <- xts(indexPx[,2], indexPx[,1])


retXts <- merge(weeklyReturn(index1aXts), weeklyReturn(index2aXts))

corPearson <- rollapply(retXts, 50, function(X) cor(coredata(X[,1]), coredata(X[,2]), method="pearson"), by.column = F)
corDist <- rollapply(retXts, 50, function(X) dcorr(obs=coredata(X[,1]), pred=coredata(X[,2])), by.column = F)

corXts <- na.omit(merge(corPearson, corDist))
names(corXts) <- c('Pearson', 'Distance')

toPlot <- data.frame(corXts)
toPlot$T <- index(corXts)
toPlot <- melt(toPlot, id='T')

p1 <- toPlot %>% {
	ggplot(., aes(x=T, y=value, color=variable)) +
		theme_economist() +
		geom_line() +
		scale_x_date(breaks='1 year') +
		labs(x="", y="correlation", color="", title = sprintf("%s vs. %s Correlation", indexName1a, indexName2a)) 
}

retXts <- merge(weeklyReturn(index1bXts), weeklyReturn(index2bXts))

corPearson <- rollapply(retXts, 50, function(X) cor(coredata(X[,1]), coredata(X[,2]), method="pearson"), by.column = F)
corDist <- rollapply(retXts, 50, function(X) dcorr(obs=coredata(X[,1]), pred=coredata(X[,2])), by.column = F)

corXts <- na.omit(merge(corPearson, corDist))
names(corXts) <- c('Pearson', 'Distance')

toPlot <- data.frame(corXts)
toPlot$T <- index(corXts)
toPlot <- melt(toPlot, id='T')

p2 <- toPlot %>% {
	ggplot(., aes(x=T, y=value, color=variable)) +
		theme_economist() +
		geom_line() +
		scale_x_date(breaks='1 year') +
		labs(x="", y="correlation", color="", title = sprintf("%s vs. %s Correlation", indexName1b, indexName2b))
}

p1 + p2 + plot_layout(ncol=1) + 
plot_annotation(
  title = "Index Correlations",
  subtitle = sprintf('rolling %d-week', 50),
  caption = '@StockViz'
)
ggsave(sprintf("%s/correlations.png", reportPath), width=18, height=12, units="in")
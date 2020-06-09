library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('lubridate')
library('reshape2')

library('ggthemes')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
pdf(NULL)
reportPath <- "D:/StockViz/public/blog/allocation vs tactical"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexName <- "NIFTY 50 TR"
startDate <- as.Date("1999-06-30")
endDate <- as.Date("2020-05-31")

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate, endDate))
dXts <- monthlyReturn(xts(pDf[,2], pDf[,1]))

dXts <- dXts[-1,]
names(dXts) <- c("INDEX")

######################################

#### plot monthly returns

dDf <- data.frame(100*dXts)
dDf$T <- index(dXts)

meanDst <- mean(dDf$INDEX)
normDst <- data.frame(INDEX=rnorm(nrow(dDf)*1000, mean=meanDst,sd=sd(dDf$INDEX)))

pstart <- first(index(dXts))
pend <- last(index(dXts))

ggplot(dDf, aes(x=INDEX)) +
	theme_economist() +
	geom_histogram(aes(y=..density..), binwidth=0.5, color="black", fill="lightblue") +
	geom_density(data=normDst, color='maroon', size=2) +
	labs(x='returns (%)', y='density', color='', title=sprintf("%s Monthly Return Density", indexName), subtitle=sprintf("%s:%s; %d months", pstart, pend, nrow(dDf))) +
	annotate("text", x=0, y=0, label = "@StockViz", hjust=0.5, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)
	
ggsave(sprintf("%s/%s.monthly-return-histogram.png", reportPath, indexName), width=12, height=8, units="in", device="png")

######################################

#### plot cumulative returns

Common.PlotCumReturns(dXts, indexName, sprintf("%d months", nrow(dDf)), sprintf("%s/%s.cumulative-return-actual.png", reportPath, indexName), NULL)

######################################

#### simulate returns

numMonths <- 10*12
numSims <- 100
actualRets <- as.numeric(dXts)
simDf <- NULL
simDf2 <- NULL
for(i in 1:numSims){
	sRets <- sample(actualRets, numMonths)
	cumRet <- cumprod(1+sRets)
	simDf <- cbind(simDf, cumRet)
	simDf2 <- cbind(simDf2, sRets)
}
colnames(simDf) <- sapply(1:numSims, function(X) paste0('s', X))
colnames(simDf2) <- sapply(1:numSims, function(X) paste0('s', X))

toPlot <- melt(simDf)

greys <- colorRampPalette(c("#FFFDD0", "black"))(numSims)
toPlot$color <- greys[as.integer(gsub('s', '', toPlot$Var2))]

actualTp <- data.frame(x=1:numMonths, y=cumprod(1+actualRets[1:numMonths]), Var2=rep("S", numMonths))
meanTp <- data.frame(x=1:numMonths, y=cumprod(1+rep(meanDst/100, numMonths)), Var2=rep("M", numMonths))

ggplot(toPlot, aes(x=Var1, y=value, group=Var2)) +
	theme_economist() +
	geom_line(aes(color=color)) + 
	#geom_line(data=actualTp, aes(x=x, y=y), color='maroon', size=2) +
	geom_line(data=meanTp, aes(x=x, y=y), color='blue', size=1) +
	scale_color_identity() +
	guides(color=F) +
	labs(x='', y='growth of Rs.1', title=sprintf("Cumulative Returns of Re-sampling Monthly %s Returns", indexName), subtitle=sprintf("%s:%s; %d months", pstart, pend, numMonths)) +
	annotate("text", x=0, y=max(toPlot$value, na.rm=T), label = "@StockViz", hjust=0, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
	
ggsave(sprintf("%s/%s.cumulative-return-simulated.png", reportPath, indexName), width=16, height=8, units="in", device="png")	

######################################

#### plot simulated annual returns distributions

actualAnn <- as.numeric(Return.annualized(dXts))*100
simAnn <- data.frame(apply(simDf2, 2, function(X) Return.annualized(xts(X, index(dXts[1:numMonths])))))
colnames(simAnn) <- c('ANN')
toPlot <- simAnn*100
ggplot(toPlot, aes(x=ANN)) +
	theme_economist() +
	geom_histogram(binwidth=0.5, color="black", fill="lightblue") +
	geom_vline(xintercept=actualAnn, color='maroon', size=2) +
	geom_text(aes(x=actualAnn, label=sprintf("actual annualized returns\n%.2f%%", actualAnn), y=5), colour="blue", angle=90) +
	labs(x='returns (%)', y='count', color='', title=sprintf("%s Simulated Annualized Returns Histogram", indexName), subtitle=sprintf("%s:%s; %d months", pstart, pend, numMonths)) +
	annotate("text", x=max(toPlot$ANN), y=1, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/%s.annualized-return-simulated.png", reportPath, indexName), width=16, height=8, units="in", device="png")		
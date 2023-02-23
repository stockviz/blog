library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('lubridate')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('viridis')
library('ggpmisc')

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

endDate <- as.Date("2023-01-31")

indices <- c("NIFTY 100 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR")

beginDates <- sqlQuery(lcon, sprintf("select index_name, min(time_stamp) from bhav_index where index_name in ('%s') group by index_name", paste(indices, collapse="','")))

beginDate <- max(beginDates[,2])

retXts <- NULL

for(iName in indices){
	idf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", iName, beginDate, endDate))
	iXts <- xts(idf[,1], idf[,2])
	retXts <- merge.xts(retXts, monthlyReturn(iXts))
}

retXts <- retXts[-1,]
names(retXts) <- indices

ytm05 <- sqlQuery(lcon, sprintf("select ytm, time_stamp from INDEX_CCIL_TENOR where index_name='0_5' and time_stamp >= '%s' and time_stamp <= '%s' order by time_stamp", beginDate, endDate))
ytm05Xts <- xts(ytm05[,1], ytm05[,2])
ytm05mXts <- to.monthly(ytm05Xts)[,1] #open
names(ytm05mXts) <- c("YTM")

tXts <- merge(retXts, ytm05mXts)
tXts$YTM <- na.locf(tXts$YTM)
tXts <- na.omit(tXts)

tXts$Rf <- (1 + tXts$YTM/100)^(1/12) - 1

rollSRa <- do.call(merge.xts, lapply(1:length(indices), function(x) rollapply(tXts[,c(x, ncol(tXts))], 60, function(X) SharpeRatio.annualized(X[,1], X[,2]), by.column = F)))
rollSRa <- na.omit(rollSRa)
names(rollSRa) <- indices

avgStatDf <- data.frame(matrix(unlist(lapply(1:length(indices), function(X) median(tXts[,X]))), nrow=1))
avgStatDf <- rbind(avgStatDf, unlist(lapply(1:length(indices), function(X) mean(tXts[,X]))))
avgStatDf <- rbind(avgStatDf, unlist(lapply(1:length(indices), function(X) max(tXts[,X]))))
avgStatDf <- rbind(avgStatDf, unlist(lapply(1:length(indices), function(X) min(tXts[,X]))))
colnames(avgStatDf) <- indices
avgStatDf$STAT <- c('MEDIAN', 'MEAN', 'MAX', 'MIN')

avgStatDf <- rbind(avgStatDf, c(unlist(lapply(1:length(indices), function(X) ecdf(coredata(tXts[,X]))(avgStatDf[avgStatDf$STAT == "MEAN", X]))), "CDF < MEAN"))

avgStatDf[,1] <- round(as.numeric(avgStatDf[,1]), 4)
avgStatDf[,2] <- round(as.numeric(avgStatDf[,2]), 4)
avgStatDf[,3] <- round(as.numeric(avgStatDf[,3]), 4)

toPlot <- data.frame(rollSRa)
toPlot$T <- index(rollSRa)
toPlot <- melt(toPlot, id='T')
toPlot$T <- as.Date(toPlot$T)
#toPlot$T <- factor(toPlot$T, levels=as.character(unique(toPlot$T)))

plotStart <- first(index(tXts))
plotEnd <- last(index(tXts))

ggplot(toPlot, aes(x=value, color=variable)) +
	theme_economist() +
	stat_density(size=1.25, geom="line", position="identity") +
	scale_color_viridis_d() +
	labs(x = "", y="", fill="", color="", title="Rolling Annualized Sharpe Ratio (Actual Rf)", subtitle=sprintf("%d-month rolling [%s:%s]", 60, plotStart, plotEnd)) +
	annotate("text", x=min(toPlot$value), y=0, label = "@StockViz", hjust='left', vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8) +
	annotate("table", x=max(toPlot$value), y=1.5, vjust='bottom', label=list(avgStatDf))

ggsave(sprintf("%s/sharpe-actual-Rf.rolling.density.png", reportPath), width=16, height=8, units="in")
	
ggplot(toPlot, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_line(size=1.25) +
	scale_x_date(date_breaks = "6 months", date_labels="%b-%Y") +
	scale_color_viridis_d() +
	labs(x = "", y="", fill="", color="", title="Rolling Annualized Sharpe Ratio (Actual Rf)", subtitle=sprintf("%d-month rolling [%s:%s]", 60, plotStart, plotEnd)) +
	annotate("text", x=min(toPlot$T), y=min(toPlot$value), label = "@StockViz", hjust='left', vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8) 

ggsave(sprintf("%s/sharpe-actual-Rf.rolling.png", reportPath), width=12, height=6, units="in")
	
########################################################################

pSRaDf <- do.call(rbind, lapply(0:10, function(aRf) unlist(lapply(1:length(indices), function(x) SharpeRatio.annualized(tXts[,x], (1 + aRf/100)^(1/12) - 1)))))
colnames(pSRaDf) <- indices
pSRaDf <- data.frame(pSRaDf)
pSRaDf$Rf <- c(0:10)

toPlot <- melt(pSRaDf, id='Rf')

ggplot(toPlot, aes(x=Rf, y=value, color=variable)) +
	theme_economist() +
	geom_line(size=1.25) +
	scale_color_viridis_d() +
	labs(x = "Risk Free Rate", y="Annualized Sharpe Ratio", fill="", color="", title="Sharpe Sensitivity to Rf", subtitle=sprintf("[%s:%s]", plotStart, plotEnd)) +
	annotate("text", x=0, y=min(toPlot$value), label = "@StockViz", hjust='left', vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8) 

ggsave(sprintf("%s/sharpe-sensitivity-Rf.png", reportPath), width=12, height=6, units="in")
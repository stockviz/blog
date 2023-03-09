library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('lubridate')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('viridis')
library('ggpmisc')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("d:/stockviz/r/config.r")

startDate <- as.Date("1999-07-01")
endDate <- as.Date("2023-02-28")

indexName <- "NIFTY 50 TR"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

pDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate, endDate))
pXts <- xts(pDf[,1], pDf[,2])
names(pXts) <- c('INDEX')

dretXts <- dailyReturn(pXts)
dretXts <- dretXts[-1,]
names(dretXts) <- c('INDEX')

sdAll <- sd(dretXts)
avgAll <- mean(dretXts)
	
doCharts <- function(dXts, suffixId){
	names(dXts) <- c("INDEX")
	dDf <- data.frame(dXts*100)

	plotStart <- first(index(dXts))
	plotEnd <- last(index(dXts))

	summ <- summary(dXts*100, digits=1)
	avgStatDf <- data.frame(matrix(c(summ[,1], summ[,2]), ncol=2))

	colnames(avgStatDf) <- c("DAY", "RETURN")

	ggplot(dDf, aes(x=INDEX)) +
		theme_economist() +
		geom_density(size=1) +
		labs(x = "", y="", fill="", color="", title=sprintf("%s Daily Return Density Plot (%s)", indexName, suffixId), subtitle=sprintf("[%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=max(dDf$INDEX, na.rm=T), y=0, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8) +
		annotate("table", x=max(dDf$INDEX), y=0.25, hjust = 'right', vjust='bottom', label=list(avgStatDf), family='mono') +
		annotate("text", x=max(dDf$INDEX), y=0.24, hjust = 'right', vjust='top', label=sprintf("Cumulative Returns: %.2f", Return.cumulative(dXts)), family='mono')
		
	ggsave(sprintf("%s/%s.daily-return-density.%s.png", reportPath, indexName, suffixId), width=12, height=6, units="in")
		
	#sdAll <- sd(dXts)
	#avgAll <- mean(dXts)

	sigma1 <- rbind(dXts[dXts >= (avgAll + 1*sdAll) & dXts < (avgAll + 2*sdAll)], dXts[dXts <= (avgAll - 1*sdAll) & dXts > (avgAll - 2*sdAll)])*100
	sigma2 <- rbind(dXts[dXts >= (avgAll + 2*sdAll) & dXts < (avgAll + 3*sdAll)], dXts[dXts <= (avgAll - 2*sdAll) & dXts > (avgAll - 3*sdAll)])*100
	sigma3 <- rbind(dXts[dXts >= (avgAll + 3*sdAll)], dXts[dXts <= (avgAll - 3*sdAll)])*100

	sigma1$S <- 1
	sigma2$S <- 2
	sigma3$S <- 3

	sigmas <- rbind(sigma1, sigma2, sigma3)
	names(sigmas) <- c('S_RET', 'S')

	dates <- data.frame(X=index(sigmas))
	dates$Y <- c(dates$X[-1], NA)
	dates$diff <- dates$Y - dates$X

	sigmas$DAYS <- as.numeric(dates$diff)
	sigmas$DAYS <- stats::lag(sigmas$DAYS, 1)

	sigmasDf <- data.frame(sigmas)
	sigmasDf$T <- index(sigmas)
	
	toPlot <- sigmasDf %>% mutate(UD = ifelse(S_RET > 0, "UP", "DN"), Y = year(T)) %>% 
				mutate(UD = factor(UD), Y = factor(Y), S = factor(S)) %>% group_by(Y, S, UD, .drop=F) %>% 
				summarize(CTR = n()) %>% arrange(Y, S, UD) %>% 
				as.data.frame()
	
	ggplot(toPlot, aes(x=factor(Y), fill=interaction(S, UD), y=CTR)) +
		theme_economist() +
		geom_bar(stat = "identity", position = position_dodge2(preserve = "single"), alpha=0.9) +
		geom_text_repel(aes(label = ifelse(CTR > 0, CTR, "")), position = position_dodge(0.9)) +
		scale_fill_viridis_d() +
		labs(x = "Year", y="count", fill="\u03C3.Up/Dn", color="", size="", title=sprintf("%s \u03C3-Returns Count (%s)", indexName, suffixId), subtitle=sprintf("[%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=1, y=-1, label = "@StockViz", hjust="left", vjust="top", col="white", cex=6, fontface = "bold", alpha = 0.8)
	
	ggsave(sprintf("%s/%s.outliers.annual-count.%s.png", reportPath, indexName, suffixId), width=16, height=6, units="in")	
	
	toPlot <- sigmasDf
	toPlot$S <- factor(toPlot$S, levels=unique(toPlot$S))
	buckets <- c(1, 5, 10, 20)

	ggplot(toPlot, aes(x=DAYS, fill=S)) +
		theme_economist() +
		geom_histogram(breaks=buckets, alpha=0.7, position='identity') +
		stat_bin(aes(y=..count.., label=..count.., color=S), geom="label_repel", position='identity', breaks=buckets, fontface="bold", vjust = "outward") +
		scale_fill_viridis_d(option='D') +
		scale_color_viridis_d(option='A') +
		scale_x_continuous(breaks=buckets) +
		guides(color="none") +
		labs(x = "days between", y="count", fill="\u03C3", color="", title=sprintf("%s +/- \u03C3-Returns Cluster (%s)", indexName, suffixId), subtitle=sprintf("[%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=max(buckets), y=0, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8) 
		
	ggsave(sprintf("%s/%s.outliers.histogram.%s.png", reportPath, indexName, suffixId), width=12, height=6, units="in")	

	ranges <- c("2000/2010", "2011/2019", "2020/")

	for(pRange in ranges){
		toPlot <- data.frame(merge(log(pXts[pRange,]), rbind(sigma2[pRange, 1], sigma3[pRange, 1]), ifelse(rbind(sigma2[pRange, 1], sigma3[pRange, 1]) > 0, 1, -1)))
		names(toPlot) <- c('INDEX', 'RET', 'UD')
		toPlot$UD <- ifelse(toPlot$UD > 0, 'UP', 'DN')
		toPlot$UD <- factor(toPlot$UD, levels=unique(toPlot$UD))
		toPlot$T <- index(pXts[pRange,])

		ggplot(toPlot, aes(x=T)) +
			theme_economist() +
			geom_line(aes(y=INDEX)) +
			geom_point(aes(y=INDEX, fill=UD, color=UD, size=abs(RET)), alpha=0.7) +
			scale_color_viridis_d(na.translate = F) +
			scale_fill_viridis_d(na.translate = F) +
			guides(fill='none', size='none') +
			labs(x = "", y="log(INDEX)", color="Up/Dn", fill="", title=sprintf("%s +/- 2&3 \u03C3-Returns Cluster (%s)", indexName, suffixId), subtitle=pRange) +
			annotate("text", x=max(toPlot$T), y=min(toPlot$INDEX), label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8) 
			
		ggsave(sprintf("%s/%s.outliers.%s.%s.png", reportPath, indexName, gsub('/', '-', pRange), suffixId), width=12, height=6, units="in")
		
	}
}	

lookbacks <- c(20, 50, 100)
smaXts <- do.call(merge, lapply(lookbacks, function(lb) SMA(pXts, lb)))

allXts <- merge(pXts, smaXts, stats::lag(dretXts, -1))

smaRets <- do.call(merge, lapply(1:length(lookbacks), function(i) ifelse(allXts[,1] > allXts[, i+1], allXts[, 2+length(lookbacks)], 0)))
allRets <- na.omit(merge(dretXts, smaRets))

doCharts(allRets[,1], "BH")
lapply(1:length(lookbacks), function(x) doCharts(allRets[,x+1], paste0('SMA', lookbacks[x])))

library('tidyverse')

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('extrafont')
library('lubridate')
library('ggthemes')
library('ggpubr')
library('reshape2')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/misc.R")
source("D:/StockViz/public/blog/common/msci.R")
source("D:/StockViz/public/blog/common/plot.common.R")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

fredUsdInr <- "DEXINUS"
fredSeries <- "GOLDPMGBD228NLBM"
trBench <- "NIFTY 50 TR"
msciSeries <- c("USA", "INDIA")

aggMonths <- 8*12

startDateFred <- as.Date('1993-01-01')
startDateTr <- as.Date('2000-01-01')

fredId <- sqlQuery(lconUs, sprintf("select id from FRED_SERIES where series_id='%s'", fredSeries))[[1]]
fredDf <- sqlQuery(lconUs, sprintf("select val, time_stamp from FRED_OBSERVATION where series_id=%d and time_stamp >= '%s'", fredId, startDateFred))
fredXts <- xts(fredDf$val, fredDf$time_stamp)

fredId <- sqlQuery(lconUs, sprintf("select id from FRED_SERIES where series_id='%s'", fredUsdInr))[[1]]
fredDf <- sqlQuery(lconUs, sprintf("select val, time_stamp from FRED_OBSERVATION where series_id=%d and time_stamp >= '%s'", fredId, startDateFred))
fredUsdXts <- xts(fredDf$val, fredDf$time_stamp)

fredGold <- merge(fredXts, fredUsdXts)
fredGold <- na.locf(fredGold)
fredGold$INR <- fredGold[,1]*fredGold[,2]

mFredRetInr <- monthlyReturn(fredGold$INR)
mFredRetAggInr <- rollapply(mFredRetInr, aggMonths, Return.annualized)

mFredRetUsd <- monthlyReturn(fredGold[,1])
mFredRetAggUsd <- rollapply(mFredRetUsd, aggMonths, Return.annualized)

##############################

msciId <- sqlQuery(lconUs2, sprintf("select index_code from MSCI_META where index_name='%s'", msciSeries[1]))[[1]]
msciMonthly <- Common.DownloadMsci(msciId, msciType='G', startDateFred-40, Sys.Date())
mMsciRet <- msciMonthly/stats::lag(msciMonthly, 1)-1
mMsciRet <- mMsciRet[sprintf('%s/', startDateFred)]
mMsciRet <- mMsciRet[-nrow(mMsciRet)] #remove the last row
mMsciRetAgg <- rollapply(mMsciRet, aggMonths, Return.annualized)

msciId <- sqlQuery(lconUs2, sprintf("select index_code from MSCI_META where index_name='%s'", msciSeries[2]))[[1]]
msciMonthly <- Common.DownloadMsci(msciId, msciType='G', startDateFred-40, Sys.Date())
mMsciRet2 <- msciMonthly/stats::lag(msciMonthly, 1)-1
mMsciRet2 <- mMsciRet2[sprintf('%s/', startDateFred)]
mMsciRet2<- mMsciRet2[-nrow(mMsciRet2)] #remove the last row
mMsciRetAgg2 <- rollapply(mMsciRet2, aggMonths, Return.annualized)

##############################
trDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s' and time_stamp >= '%s'", trBench, startDateTr))
trXts <- xts(trDf$px_close, trDf$time_stamp)

mTrRet <- monthlyReturn(trXts)
mTrRetAgg <- rollapply(mTrRet, aggMonths, Return.annualized)

##############################

mFredRetAggInr <- na.omit(mFredRetAggInr)
mFredRetAggUsd <- na.omit(mFredRetAggUsd)
mMsciRetAgg <- na.omit(mMsciRetAgg)
mMsciRetAgg2 <- na.omit(mMsciRetAgg2)

mFredRetAggInr <- Common.NormalizeMonthlyDates(mFredRetAggInr)
mFredRetAggUsd <- Common.NormalizeMonthlyDates(mFredRetAggUsd)
mTrRetAgg <- Common.NormalizeMonthlyDates(mTrRetAgg)
mMsciRetAgg <- Common.NormalizeMonthlyDates(mMsciRetAgg)
mMsciRetAgg2 <- Common.NormalizeMonthlyDates(mMsciRetAgg2)

##############################

inrNames <- c('GOLD-INR', trBench)
usdNames <- c('GOLD-USD', msciSeries)
retAgg <- na.omit(merge(mFredRetInr, mTrRet))
names(retAgg) <- inrNames
Common.PlotCumReturns(retAgg, sprintf('Gold vs. %s', trBench), "in INR", sprintf("%s/gold-cum-returns-INR.png", reportPath))

retAgg <- na.omit(merge(mFredRetUsd, mMsciRet, mMsciRet2))
names(retAgg) <- usdNames
Common.PlotCumReturns(retAgg, sprintf('Gold vs. MSCI %s', paste(msciSeries, collapse=',')), "in USD", sprintf("%s/gold-cum-returns-USD.png", reportPath))

##############################
retAgg <- 100*merge(mFredRetAggUsd, mMsciRetAgg, mMsciRetAgg2)
pltStart <- first(index(retAgg))
pltEnd <- last(index(retAgg))

toPlot <- data.frame(retAgg)

seriesNames <- c('GOLD-USD', msciSeries)
names(toPlot) <- seriesNames

toPlot$T <- index(retAgg)
toPlot <- melt(toPlot, id='T')

toPlot <- toPlot %>% filter(!is.na(value)) %>% group_by(variable) %>% summarize(mint = min(T), maxt = max(T)) %>% ungroup() %>% mutate(variable = as.character(variable)) %>%
	right_join(toPlot, by=c('variable')) %>% mutate(label = if_else(mint == T, variable, NA_character_), label2 = if_else(maxt == T, round(value, 2), NA_real_)) %>%
	as.data.frame()

pdf(NULL)
plt <- ggplot(toPlot, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_line(aes(size=variable)) +
	scale_size_manual(breaks=seriesNames, values=c(0.5, 0.5, 1)) +
	scale_x_date(date_breaks = '1 year', date_labels="%Y-%m-%d") +
	guides(size=F, color = F) +
	geom_label_repel(aes(label = label), alpha=0.8) +
	geom_label_repel(aes(label = label2)) +
	labs(x='', y='(%)', color='', title=sprintf('Gold vs. MSCI %s', paste(msciSeries, collapse=',')), 
								subtitle=sprintf("Rolling %d-month (%d-year) returns [%s:%s]", aggMonths, aggMonths/12, pltStart, pltEnd)) +
	annotate("text", x=max(toPlot$T), y=max(toPlot$value, na.rm=T), label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/gold-rolling-returns-msci.png", reportPath), plt, width=16, height=8, units="in")

##############################
retAgg <- 100*merge(mFredRetAggInr, mTrRetAgg, mFredRetAggUsd, mMsciRetAgg)
pltStart <- first(index(retAgg))
pltEnd <- last(index(retAgg))

toPlot <- data.frame(retAgg)

seriesNames <- c(inrNames, 'GOLD-USD', msciSeries[1])
names(toPlot) <- seriesNames

toPlot$T <- index(retAgg)
toPlot <- melt(toPlot, id='T')

toPlot <- toPlot %>% filter(!is.na(value)) %>% group_by(variable) %>% summarize(mint = min(T), maxt = max(T)) %>% ungroup() %>% mutate(variable = as.character(variable)) %>%
	right_join(toPlot, by=c('variable')) %>% mutate(label = if_else(mint == T, variable, NA_character_), label2 = if_else(maxt == T, round(value, 2), NA_real_)) %>%
	as.data.frame()

pdf(NULL)
plt <- ggplot(toPlot, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_line(aes(size=variable)) +
	scale_size_manual(breaks=seriesNames, values=c(0.5, 0.5, 1, 1)) +
	scale_x_date(date_breaks = '1 year', date_labels="%Y-%m-%d") +
	guides(size=F, color = F) +
	geom_label_repel(aes(label = label), alpha=0.8) +
	geom_label_repel(aes(label = label2)) +
	labs(x='', y='(%)', color='', title=sprintf('Gold vs. %s, MSCI %s', trBench, msciSeries[1]), 
								subtitle=sprintf("Rolling %d-month (%d-year) returns [%s:%s]", aggMonths, aggMonths/12, pltStart, pltEnd)) +
	annotate("text", x=max(toPlot$T), y=max(toPlot$value, na.rm=T), label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/gold-rolling-returns.png", reportPath), plt, width=16, height=8, units="in")

pdf(NULL)
plt <- ggplot(toPlot, aes(x=value, color=variable)) +
	theme_economist() +
	stat_density(aes(size=variable), geom="line", position="identity") +
	scale_size_manual(breaks=seriesNames, values=c(0.5, 0.5, 1, 1)) +
	guides(variable=F, size=F, color=guide_legend(override.aes=list(size=3))) +
	labs(x='', y='density', variable='', color='', title=sprintf('Gold vs. %s, MSCI %s', trBench, msciSeries[1]), 
								subtitle=sprintf("Rolling %d-month (%d-year) returns [%s:%s]", aggMonths, aggMonths/12, pltStart, pltEnd)) +
	annotate("text", x=0, y=0, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/gold-rolling-returns-DENSITY.png", reportPath), plt, width=16, height=8, units="in")


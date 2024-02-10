source("d:/stockviz/r/config.r")

reportPath<-"."

library('RODBC')
library('quantmod')
library('feasts')
library('lubridate')
library('extrafont')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('ggpmisc')

pdf(NULL)
options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexDf <- sqlQuery(lcon, "select px_close, time_stamp from VIX_HISTORY")
iXts <- xts(indexDf[,1], indexDf[,2])
names(iXts) <- c("VIX")

startDate <- first(index(iXts))
endDate <- last(index(iXts))

iXts$VIX_1 <- stats::lag(iXts$VIX, -1)

iXts$DIFF_PCT <- 100*(iXts$VIX/iXts$VIX_1 - 1)

iXts <- na.omit(iXts)

iDf <- data.frame(iXts)
iDf$T <- index(iXts)

ggplot(iDf, aes(x=VIX_1, y=VIX)) + 
	theme_economist() +
	geom_point() +
	geom_smooth(method = "lm") +
	labs(x="Actual", y="Forecast", title="INDIA VIX Forecast t+1 = t", subtitle = sprintf("%s:%s", startDate, endDate)) 
	
ggsave(sprintf("%s/vix-forecast.simple.png", reportPath), width=12, height=6, units="in")

diffStat <- summary(iDf$DIFF_PCT)
diffStat <- data.frame(stat=names(diffStat), value=matrix(diffStat))
diffStat[,2] <- round(diffStat[,2], 2)
colnames(diffStat) <- c("", "")

ggplot(iDf, aes(x=DIFF_PCT)) + 
	theme_economist() +
	stat_density(size=1.25, geom="line", position="identity") +
	labs(x="Forecast Error", y="", title="INDIA VIX Forecast t+1 = t Errors", subtitle = sprintf("%s:%s", startDate, endDate)) +
	annotate("table", x=min(iDf$DIFF_PCT), y=0.1, vjust='top', hjust='left', label=list(diffStat), cex=4)
	
ggsave(sprintf("%s/vix-forecast.simple.error.png", reportPath), width=12, height=6, units="in")

##################

lb <- 5

iXts <- xts(indexDf[,1], indexDf[,2])
names(iXts) <- c("VIX")

iXts$VIX_EMA <- EMA(iXts$VIX, lb)
iXts$VIX_1 <- stats::lag(iXts$VIX, -1)
iXts$DIFF_PCT <- 100*(iXts$VIX_EMA/iXts$VIX_1 - 1)
iXts <- na.omit(iXts)

iDf <- data.frame(iXts)
iDf$T <- index(iXts)

ggplot(iDf, aes(x=VIX_1, y=VIX_EMA)) + 
	theme_economist() +
	geom_point() +
	geom_smooth(method = "lm") +
	labs(x="Actual", y="Forecast", title=sprintf("INDIA VIX Forecast EMA(%d)", lb), subtitle = sprintf("%s:%s", startDate, endDate)) 
	
ggsave(sprintf("%s/vix-forecast.ema.%d.png", reportPath, lb), width=12, height=6, units="in")

diffStat <- summary(iDf$DIFF_PCT)
diffStat <- data.frame(stat=names(diffStat), value=matrix(diffStat))
diffStat[,2] <- round(diffStat[,2], 2)
colnames(diffStat) <- c("", "")

ggplot(iDf, aes(x=DIFF_PCT)) + 
	theme_economist() +
	stat_density(size=1.25, geom="line", position="identity") +
	labs(x="Forecast Error", y="", title=sprintf("INDIA VIX Forecast EMA(%d) Errors", lb), subtitle = sprintf("%s:%s", startDate, endDate)) +
	annotate("table", x=min(iDf$DIFF_PCT), y=0.1, vjust='top', hjust='left', label=list(diffStat), cex=4)
	
ggsave(sprintf("%s/vix-forecast.ema.%d.error.png", reportPath, lb), width=12, height=6, units="in")

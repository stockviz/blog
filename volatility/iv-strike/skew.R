source("d:/stockviz/r/config.r")

reportPath<-"."

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('viridis')

pdf(NULL)
options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

inInst <- "NIFTY 50 TR"
usInst <- "SPY"

inStDt <- sqlQuery(lcon, sprintf("select min(time_stamp) from bhav_index where index_name='%s'", inInst))[[1]]
usStDt <- sqlQuery(lconUs, sprintf("select min(time_stamp) from BHAV_EQ_TD where symbol='%s'", usInst))[[1]]

startDate <- max(inStDt, usStDt)

pxDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s'", inInst, startDate))
inPx <- xts(pxDf[,-1], pxDf[,1])

pxDf <- sqlQuery(lconUs, sprintf("select time_stamp, c from BHAV_EQ_TD where symbol='%s' and time_stamp >= '%s'", usInst, startDate))
usPx <- xts(pxDf[,-1], pxDf[,1])

endDate <- min(last(index(inPx)), last(index(usPx)))

inPx <- inPx[paste0('/', endDate)]
usPx <- usPx[paste0('/', endDate)]

inDaily <- dailyReturn(inPx)
usDaily <- dailyReturn(usPx)

inDaily <- inDaily[-1]
usDaily <- usDaily[-1]

inWeekly <- weeklyReturn(inPx)
usWeekly <- weeklyReturn(usPx)

inWeekly <- inWeekly[-1]
usWeekly <- usWeekly[-1]

inSkewDaily <- skewness(inDaily, method = "sample")
inSkewDailyR100 <- rollapply(inDaily, 100, skewness, method = "sample")

usSkewDaily <- skewness(usDaily, method = "sample")
usSkewDailyR100 <- rollapply(usDaily, 100, skewness, method = "sample")

inSkewWeekly <- skewness(inWeekly, method = "sample")
inSkewWeeklyR50 <- rollapply(inWeekly, 50, skewness, method = "sample")

usSkewWeekly <- skewness(usWeekly, method = "sample")
usSkewWeeklyR50 <- rollapply(usWeekly, 50, skewness, method = "sample")

skewDailyDf <- data.frame(merge(inSkewDailyR100, usSkewDailyR100))
colnames(skewDailyDf) <- c("IN", "US")

skewDailyDf <- melt(skewDailyDf)

vColors <- viridis_pal()(2)
plotColors <- c("IN" = vColors[1], "US" = vColors[2])

ggplot(skewDailyDf, aes(x=value, color=variable)) +
	theme_economist() +
	stat_density(geom = "line", position = "identity", linewidth = 1) +
	geom_vline(xintercept = coredata(inSkewDaily), color = plotColors["IN"]) +
	geom_vline(xintercept = coredata(usSkewDaily), color = plotColors["US"]) +
	geom_text(aes(x = coredata(inSkewDaily), label=round(coredata(inSkewDaily), 2), y = 0.5), color='black', angle=90) +
	geom_text(aes(x = coredata(usSkewDaily), label=round(coredata(usSkewDaily), 2), y = 0.5), color='black', angle=90) +
	scale_color_manual(values = plotColors) +
	labs(x="skew", y="density", color="", title=sprintf("%s/%s Daily Return Skew", inInst, usInst), subtitle = sprintf("rolling %d-day; %s:%s", 100, startDate, endDate), caption='@StockViz') 

ggsave(sprintf("%s/daily.skew.density.png", reportPath), width=12, height=6, units="in")			
	
skewWeeklyDf <- data.frame(merge(inSkewWeeklyR50, usSkewWeeklyR50))
colnames(skewWeeklyDf) <- c("IN", "US")

skewWeeklyDf <- melt(skewWeeklyDf)
	
ggplot(skewWeeklyDf, aes(x=value, color=variable)) +
	theme_economist() +
	stat_density(geom = "line", position = "identity", linewidth = 1) +
	geom_vline(xintercept = coredata(inSkewWeekly), color = plotColors["IN"]) +
	geom_vline(xintercept = coredata(usSkewWeekly), color = plotColors["US"]) +
	geom_text(aes(x = coredata(inSkewWeekly), label=round(coredata(inSkewDaily), 2), y = 0.5), color='black', angle=90) +
	geom_text(aes(x = coredata(usSkewWeekly), label=round(coredata(usSkewDaily), 2), y = 0.5), color='black', angle=90) +
	scale_color_manual(values = plotColors) +
	labs(x="skew", y="density", color="", title=sprintf("%s/%s Weekly Return Skew", inInst, usInst), subtitle = sprintf("rolling %d-weeks; %s:%s", 50, startDate, endDate), caption='@StockViz') 

ggsave(sprintf("%s/weekly.skew.density.png", reportPath), width=12, height=6, units="in")			
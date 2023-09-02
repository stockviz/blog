source("d:/stockviz/r/config.r")
source("d:/stockviz/r/plot.common.r")
reportPath<-"."

library('RODBC')
library('RPostgres')
library('highfrequency')
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
pcon <- dbConnect(RPostgres::Postgres(), host=ldbserver2, user= ldbuser2, password=ldbpassword2, dbname=ldbname2)

baseDate <- as.Date("1990-01-01", tz='UTC')
startTime <- hms("09:15:00")
endTime <- hms("15:30:00")

startDate <- as.Date('2015-01-01')
rSmaLbs <- seq(50, 500, by=50)
indices <- c('NIFTY 50', 'NIFTY MIDCAP 50')

statsDf <- data.frame(INAME = "", LB = 0, SR_PRE = 0.0, SR_POST = 0.0)

allRsv <- NULL

for(i in 1:length(indices)){
	indexName <- indices[i]

	niftyDf <- dbGetQuery(pcon, sprintf("select c as cl, tick_stamp from zd_index_bars where symbol='%s' and time_stamp >= '%s'", indexName, startDate))
	niftyDf2 <- niftyDf
	niftyDf2$tick <- as.POSIXct(baseDate + seconds(niftyDf$tick_stamp))
	niftyDf2$tod <- hms(format(niftyDf2$tick, "%H:%M:%S"))

	niftyDf2 <- niftyDf2 %>% filter(tod >= startTime & tod <= endTime) %>% select(cl, tick) 

	pXts <- xts(niftyDf2$cl, niftyDf2$tick)

	rsvXts <- rSVar(pXts, alignBy = 'minutes', alignPeriod = 5, makeReturns = T)
	index(rsvXts) <- as.Date(index(rsvXts))
	
	allRsv <- merge.xts(allRsv, rsvXts)
	
	indexDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name = '%s' and time_stamp >= '%s'", indexName, startDate))
	iXts <- xts(indexDf[,1], indexDf[,2])
	iRetXts <- dailyReturn(iXts)
	
	sraPre <- as.numeric(SharpeRatio.annualized(iRetXts["/2019"]))
	sraPost <- as.numeric(SharpeRatio.annualized(iRetXts["2020-05-01/"]))
		
	statsDf <- rbind(statsDf, c(indexName, 0, sraPre, sraPost))

	for(rSmaLb in rSmaLbs){
		rsvXts$DN_SMA <- SMA(rsvXts$downside, rSmaLb)
		allXts <- na.omit(merge(rsvXts, iRetXts, stats::lag(iRetXts, -1)))
		allXts$SMA_RET <- ifelse(allXts$downside > allXts$DN_SMA, 0, allXts[, ncol(allXts)])
		sraPre <- as.numeric(SharpeRatio.annualized(allXts$SMA_RET["/2019"]))
		sraPost <- as.numeric(SharpeRatio.annualized(allXts$SMA_RET["2020-05-01/"]))
		
		statsDf <- rbind(statsDf, c(indexName, rSmaLb, sraPre, sraPost))
	}
}

names(allRsv) <- do.call(paste0, expand.grid(c("dn_", "up_"), indices))
allRsvTp <- data.frame(allRsv)
allRsvTp <- melt(allRsvTp)

ggplot(allRsvTp, aes(x=value, color=variable)) +
	theme_economist() +
	stat_density(size=1.25, geom="line", position="identity") +
	scale_color_viridis_d() +
	labs(x = "", y="", fill="", color="", title="Realized Semi-Var Density", subtitle=sprintf("%s:%s", first(index(allRsv)), last(index(allRsv)))) +
	annotate("text", x=min(allRsvTp$value), y=0, label = "@StockViz", hjust='left', vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8) 

ggsave(sprintf("%s/realized-semi-var.density.png", reportPath), width=12, height=6, units="in")

statsDf <- statsDf[-1,]

statsDf$LB <- as.integer(statsDf$LB)
statsDf$SR_PRE <- as.numeric(statsDf$SR_PRE)
statsDf$SR_POST <- as.numeric(statsDf$SR_POST)

toPlot <- statsDf
toPlot <- melt(toPlot, id=c('INAME', 'LB'))

toPlot$INAME <- factor(toPlot$INAME, levels = unique(toPlot$INAME))
toPlot$LB <- factor(toPlot$LB, levels = unique(toPlot$LB))

ggplot(toPlot, aes(x=LB, y=value, group=interaction(INAME, variable), color = interaction(INAME, variable))) +
	theme_economist() +
	scale_color_viridis_d() +
	geom_line(linewidth = 2) +
	guides(linewidth='none', color = guide_legend(override.aes = list(linewidth=3))) +
	labs(x="lookback (days)", y="Sharpe Ratio (annualized)", color="", linewidth="", title = "Realized Semi-Var long-only Sharpe Ratios over lookbacks", subtitle=sprintf("%s:%s", first(index(iXts)), last(index(iXts)))) +
	annotate("text", x=1, y=max(as.numeric(toPlot$value)), label = "@StockViz", hjust='left', vjust='top', col="white", cex=5, fontface = "bold", alpha = 0.5)
	
ggsave(sprintf("%s/sharpe-signature.png", reportPath), width=18, height=6, units="in")	

rSmaLbBts <- c(350, 100)

for(i in 1:length(indices)){
	indexName <- indices[i]
	rSmaLbBt <- rSmaLbBts[i]
	
	niftyDf <- dbGetQuery(pcon, sprintf("select c as cl, tick_stamp from zd_index_bars where symbol='%s' and time_stamp >= '%s'", indexName, startDate))
	niftyDf2 <- niftyDf
	niftyDf2$tick <- as.POSIXct(baseDate + seconds(niftyDf$tick_stamp))
	niftyDf2$tod <- hms(format(niftyDf2$tick, "%H:%M:%S"))

	niftyDf2 <- niftyDf2 %>% filter(tod >= startTime & tod <= endTime) %>% select(cl, tick) 

	pXts <- xts(niftyDf2$cl, niftyDf2$tick)

	rsvXts <- rSVar(pXts, alignBy = 'minutes', alignPeriod = 5, makeReturns = T)
	index(rsvXts) <- as.Date(index(rsvXts))

	indexDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name = '%s' and time_stamp >= '%s'", indexName, startDate))
	iXts <- xts(indexDf[,1], indexDf[,2])
	iRetXts <- dailyReturn(iXts)
	
	rsvXts$DN_SMA <- SMA(rsvXts$downside, rSmaLbBt)
	allXts <- na.omit(merge(rsvXts, stats::lag(iRetXts, -1)))
	allXts$SMA_RET <- ifelse(allXts$downside > allXts$DN_SMA, 0, allXts[, ncol(allXts)])

	plotPre <- merge(allXts["/2019", "SMA_RET"], allXts["/2019", ncol(allXts)-1])
	plotPost <- merge(allXts["2020-05-01/", "SMA_RET"], allXts["2020-05-01/", ncol(allXts)-1])
	plotAll <- merge(allXts[, "SMA_RET"], allXts[, ncol(allXts)-1])
	
	Common.PlotCumReturns(plotPre, sprintf("%s Realized Semi-Var Long-Only", indexName), "pre-pandemic", sprintf("%s/%s.pre-pan.png", reportPath, indexName), NULL)
	Common.PlotCumReturns(plotPost, sprintf("%s Realized Semi-Var Long-Only", indexName), "post-pandemic", sprintf("%s/%s.post-pan.png", reportPath, indexName), NULL)
	Common.PlotCumReturns(plotAll, sprintf("%s Realized Semi-Var Long-Only", indexName), "", sprintf("%s/%s.all.png", reportPath, indexName), NULL)
}

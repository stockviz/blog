library('RODBC')
library("RSQLite")
library("DBI") 
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggrepel')
library('gtExtras')
library('ggpmisc')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

getSymbols("FPCPITOTLZGIND", src="FRED") #india annual inflation
getSymbols("DEXINUS", src="FRED") #USDINR daily

usdInrAnnRet <- annualReturn(DEXINUS)
usdInrAnnRet <- usdInrAnnRet[-nrow(usdInrAnnRet)]

fredGoldId <- sqlQuery(lconUs, sprintf("select id from FRED_SERIES where series_id='%s'", 'GOLDPMGBD228NLBM'))[[1]]
fredGoldDf <- sqlQuery(lconUs, sprintf("select val, time_stamp from FRED_OBSERVATION where series_id=%d", fredGoldId))
fredGoldXts <- xts(fredGoldDf$val, fredGoldDf$time_stamp)

wmCon <- dbConnect(RSQLite::SQLite(), "/mnt/siberia/data/westmetall.db", flags = RSQLite::SQLITE_RO)
wmGoldDf <-  dbGetQuery(wmCon, "select TIME_STAMP, PX from PRICE_HISTORY where NAME = 'GOLD_LONDON_FIXING'")
wmGoldEurDf <-  dbGetQuery(wmCon, "select TIME_STAMP, PX from PRICE_HISTORY where NAME = 'GOLD_EUR_KG'")
dbDisconnect(wmCon)

wmGoldXts <- xts(wmGoldDf$PX, as.Date(strptime(wmGoldDf$TIME_STAMP, "%Y%m%d")))
wmGoldEurXts <- xts(wmGoldEurDf$PX, as.Date(strptime(wmGoldEurDf$TIME_STAMP, "%Y%m%d")))

goldXts <- merge(fredGoldXts, wmGoldXts)
goldXts <- xts(rowMeans(goldXts, na.rm=TRUE), index(goldXts))

goldInrXts <- goldXts * DEXINUS
goldInrXts <- na.locf(goldInrXts)
goldInrXts <- merge(goldInrXts, stats::lag(dailyReturn(goldInrXts), -1))
goldInrXts[abs(goldInrXts[, 2]) > 0.95, 1] <- NA #remove outliers

goldAnnRet <- annualReturn(goldInrXts[,1])
goldAnnRet <- goldAnnRet[-nrow(goldAnnRet)]

niftyDf <- sqlQuery(lcon, "select px_close, time_stamp from bhav_index where index_name='NIFTY 50 TR'")
niftyXts <- xts(niftyDf[,1], niftyDf[,2])
niftyAnnRet <- annualReturn(niftyXts)
niftyAnnRet <- niftyAnnRet[-1]
niftyAnnRet <- niftyAnnRet[-nrow(niftyAnnRet)]

#########################

index(FPCPITOTLZGIND)<-as.Date(sprintf("%s-12-15", year(FPCPITOTLZGIND)))
index(goldAnnRet)<-as.Date(sprintf("%s-12-15", year(goldAnnRet)))
index(niftyAnnRet)<-as.Date(sprintf("%s-12-15", year(niftyAnnRet)))
index(usdInrAnnRet)<-as.Date(sprintf("%s-12-15", year(usdInrAnnRet)))

#plot usdinr and inflation
toPlot <- data.frame(FPCPITOTLZGIND)
colnames(toPlot) <- c("INFLATION")
toPlot$Y <- year(index(FPCPITOTLZGIND))

p1 <- ggplot(toPlot, aes(x=Y, y=INFLATION)) +
  theme_economist() +
  geom_bar(stat = 'identity', position=position_dodge(), fill = viridis_pal()(1)) +
  labs(x = '', y='%', subtitle = "Annual")

cumInflation <- cumprod(1 + FPCPITOTLZGIND/100)
toPlot <- data.frame(cumInflation)
colnames(toPlot) <- "INFLATION"
toPlot$Y <- year(index(cumInflation))

p2 <- ggplot(toPlot, aes(x=Y, y=INFLATION, label=round(INFLATION, 2))) +
  theme_economist() +
  geom_line() +
  stat_peaks(colour = "red", geom = "text_repel", size = 2, span=NULL) +
  labs(x = '', y='cumulative', subtitle = "Cumulative")


p1/p2 + 
  plot_layout(axes = 'collect_x') +
  plot_annotation(title = "Annual India Inflation", 
                  subtitle = sprintf("%d:%d", min(year(FPCPITOTLZGIND)), max(year(FPCPITOTLZGIND))),
                  caption = '@StockViz',
                  theme = theme_economist())

ggsave(sprintf("%s/inflation.png", reportPath), units = "in", height=2*6, width=12)

toPlot <- data.frame(DEXINUS)
colnames(toPlot) <- c("USDINR")
toPlot$TS <- index(DEXINUS)

ggplot(toPlot, aes(x=TS, y=USDINR, label=USDINR)) +
  theme_economist() +
  geom_line() +
  stat_peaks(colour = "red", geom = "point", size = 2, span=500) + 
  stat_peaks(colour = "red", geom = "text_repel", size = 2, span=500) +
  stat_peaks(colour = "red", geom = "text_repel", size = 2, span=NULL) +
  labs(x = '', y='USDINR', 
       title = "USDINR",
       subtitle = sprintf("%d:%d", min(year(DEXINUS)), max(year(DEXINUS))),
       caption = '@StockViz')

ggsave(sprintf("%s/usdinr.png", reportPath), units = "in", height=6, width=12)

#################

#inflation vs USDINR
annRets <- na.trim(merge(FPCPITOTLZGIND/100, usdInrAnnRet), side='left')
names(annRets) <- c("INFLATION", "USDINR")

annRetDf <- data.frame(annRets)

ggplot(annRetDf, aes(x=INFLATION*100, y=USDINR*100)) +
  theme_economist() +
  geom_point() +
  labs(x = 'inflation (%)', y='USDINR (%)',
       title = "USDINR vs. Inflation (India)",
       subtitle = sprintf("%d:%d", min(year(annRets)), max(year(annRets))),
       caption = '@StockViz')

ggsave(sprintf("%s/usdinr-inflation.png", reportPath), units = "in", height=6, width=12)

#gold vs USDINR
annRets <- na.trim(merge(goldAnnRet, usdInrAnnRet), side='left')
names(annRets) <- c("GOLD", "USDINR")

annRetDf <- data.frame(annRets)

ggplot(annRetDf, aes(x=GOLD*100, y=USDINR*100)) +
  theme_economist() +
  geom_point() +
  labs(x = 'gold (%)', y='USDINR (%)',
       title = "USDINR vs. Gold (India)",
       subtitle = sprintf("%d:%d", min(year(annRets)), max(year(annRets))),
       caption = '@StockViz')

ggsave(sprintf("%s/usdinr-gold.png", reportPath), units = "in", height=6, width=12)

#inflation vs gold
annRets <- na.trim(merge(FPCPITOTLZGIND/100, goldAnnRet), side='left')
names(annRets) <- c("INFLATION", "GOLD")

annRetDf <- data.frame(annRets)

ggplot(annRetDf, aes(x=INFLATION*100, y=GOLD*100)) +
  theme_economist() +
  geom_point() +
  labs(x = 'inflation (%)', y='gold (%)',
       title = "Gold vs. Inflation (India)",
       subtitle = sprintf("%d:%d", min(year(annRets)), max(year(annRets))),
       caption = '@StockViz')

ggsave(sprintf("%s/gold-inflation.png", reportPath), units = "in", height=6, width=12)

#nifty vs. gold
annRets <- na.trim(merge(niftyAnnRet, goldAnnRet), side='left')
names(annRets) <- c("NIFTY", "GOLD")

annRetDf <- data.frame(annRets)

ggplot(annRetDf, aes(x=NIFTY*100, y=GOLD*100)) +
  theme_economist() +
  geom_point() +
  labs(x = 'nifty (%)', y='gold (%)', 
       title = "Gold vs. NIFTY 50 TR",
       subtitle = sprintf("%d:%d", min(year(annRets)), max(year(annRets))),
       caption = '@StockViz')

ggsave(sprintf("%s/gold-nifty.png", reportPath), units = "in", height=6, width=12)

##################

#rolling returns of inflation, nifty, gold
annRets <- na.trim(merge(FPCPITOTLZGIND/100, niftyAnnRet, goldAnnRet), side='left')
names(annRets) <- c("INFLATION", "NIFTY", "GOLD")

annRets3 <- rollapply(annRets, 3, Return.cumulative, by.column = FALSE)
annRets5 <- rollapply(annRets, 5, Return.cumulative, by.column = FALSE)

names(annRets3) <- names(annRets)
names(annRets5) <- names(annRets)
  
toPlot <- data.frame(annRets)
toPlot$Y <- year(index(annRets))

p1 <- ggplot(toPlot |> pivot_longer(cols=-Y), aes(x=Y, y=value*100, fill=name)) +
  theme_economist() +
  geom_bar(stat = 'identity', position=position_dodge()) +
  scale_fill_viridis_d() +
  labs(x = '', y='returns (%)', fill='', subtitle = "1-year")

toPlot <- data.frame(annRets3)
toPlot$Y <- year(index(annRets3))

p2 <- ggplot(toPlot |> pivot_longer(cols=-Y), aes(x=Y, y=value*100, fill=name)) +
  theme_economist() +
  geom_bar(stat = 'identity', position=position_dodge()) +
  scale_fill_viridis_d() +
  guides(fill='none') +
  labs(x = '', y='returns (%)', fill='', subtitle = "3-year")


toPlot <- data.frame(annRets5)
toPlot$Y <- year(index(annRets5))

p3 <- ggplot(toPlot |> pivot_longer(cols=-Y), aes(x=Y, y=value*100, fill=name)) +
  theme_economist() +
  geom_bar(stat = 'identity', position=position_dodge()) +
  scale_fill_viridis_d() +
  guides(fill='none') +
  labs(x = '', y='returns (%)', fill='', subtitle = "5-year")

p1/p2/p3 + 
  plot_layout(axes = 'collect') +
  plot_annotation(title = "Rolling Inflation, Gold and NIFTY 50 TR", 
                  subtitle = sprintf("%d:%d", min(year(annRets)), max(year(annRets))),
                  caption = '@StockViz',
                  theme = theme_economist())

ggsave(sprintf("%s/rolling.ann.ret.png", reportPath), units = "in", height=3*6, width=12)

##################

#cumulative returns of inflation, nifty and gold

annRets <- na.trim(merge(FPCPITOTLZGIND/100, niftyAnnRet, goldAnnRet), side='left')
names(annRets) <- c("INFLATION", "NIFTY", "GOLD")

goldNiftyXts <- na.trim(merge(niftyXts, goldInrXts[,1]), side='left')
goldNiftyXts <- na.locf(goldNiftyXts)
niftySr <- as.numeric(SharpeRatio.annualized(monthlyReturn(goldNiftyXts[,1])))
goldSr <- as.numeric(SharpeRatio.annualized(monthlyReturn(goldNiftyXts[,2])))

Common.PlotCumReturns(annRets, "Inflation, Gold and NIFTY 50 TR", 
                      sprintf("SR: nifty=%0.2f; gold=%0.2f", niftySr, goldSr),
                      sprintf("%s/cum.ret.png", reportPath))


###################

#gold, silver and equity ETFs
spyUsd <- sqlQuery(lconUs2, "select time_stamp, c from TIINGO_DATA where ticker='SPY'")
spyUsdXts <- xts(spyUsd$c, spyUsd$time_stamp)

gldUsd <- sqlQuery(lconUs2, "select time_stamp, c from TIINGO_DATA where ticker='GLD'")
gldUsdXts <- xts(gldUsd$c, gldUsd$time_stamp)

slvUsd <- sqlQuery(lconUs2, "select time_stamp, c from TIINGO_DATA where ticker='SLV'")
slvUsdXts <- xts(slvUsd$c, slvUsd$time_stamp)

gsFutUsd <- na.trim(merge(spyUsdXts, gldUsdXts, slvUsdXts), sides='left')
gsFutUsd <- na.locf(gsFutUsd)

gsFutUsdRetDaily <- merge(dailyReturn(gsFutUsd[,1]), dailyReturn(gsFutUsd[,2]), dailyReturn(gsFutUsd[,3]))
names(gsFutUsdRetDaily) <- c('SPY', 'GLD', 'SLV')

maxDD <- paste(round(maxDrawdown(gsFutUsdRetDaily["2024-11-01/"])*100, 2), collapse="/")
Common.PlotCumReturns(gsFutUsdRetDaily["2024-11-01/"], 
                      "US ETFs", 
                      sprintf("max dd: %s", maxDD),
                      sprintf("%s/gold-silver-spy.2025.png", reportPath))

maxDD <- paste(round(maxDrawdown(gsFutUsdRetDaily)*100, 2), collapse="/")
Common.PlotCumReturns(gsFutUsdRetDaily, 
                      "US ETFs", 
                      sprintf("max dd: %s", maxDD),
                      sprintf("%s/gold-silver-spy.2008.png", reportPath))


#####################

#gold returns in different currencies

goldTemp <- merge(goldXts, stats::lag(dailyReturn(goldXts), -1))
goldTemp[abs(goldTemp[, 2]) > 0.95, 1] <- NA #remove outliers

goldDailyRetCur <- na.trim(merge(dailyReturn(goldInrXts[,1]), dailyReturn(goldTemp[,1]), dailyReturn(wmGoldEurXts)), sides='left')
names(goldDailyRetCur) <- c('INR', 'USD', 'EUR')

maxDD <- paste(round(maxDrawdown(goldDailyRetCur)*100, 2), collapse="/")
Common.PlotCumReturns(goldDailyRetCur, 
                      "Gold Returns in Currencies", 
                      sprintf("max dd: %s", maxDD),
                      sprintf("%s/gold-cur.png", reportPath))


###################

#gold returns in INR since 1970's

toPlot <- dailyReturn(goldInrXts[,1])
names(toPlot) <- c("GOLD_INR")

maxDD <- paste(round(maxDrawdown(toPlot)*100, 2), collapse="/")
Common.PlotCumReturns(toPlot, 
                      "Gold in INR", 
                      sprintf("max dd: %s", maxDD),
                      sprintf("%s/gold-INR.cum.png", reportPath))


annRet <- annualReturn(goldInrXts[,1])
annRet <- annRet[-1]
annRet <- annRet[-nrow(annRet)]

toPlot <- data.frame(annRet)
colnames(toPlot) <- c("GOLD_INR")
toPlot$TS <- index(annRet)

ggplot(toPlot, aes(x=TS, y=GOLD_INR*100)) +
  theme_economist() +
  geom_bar(stat = 'identity', position=position_dodge(), fill = viridis_pal()(1)) +
  labs(x = '', y='(%)', 
       title = "Gold Returns in INR",
       subtitle = sprintf("%d:%d", min(year(annRet)), max(year(annRet))),
       caption = '@StockViz')

ggsave(sprintf("%s/gold-INR.annual.png", reportPath), units = "in", height=6, width=12)


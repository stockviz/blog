library('tidyverse')
library('quantmod')
library('PerformanceAnalytics')
library('ggthemes')
library('patchwork')
library('viridis')

source("/mnt/hollandC/stockviz/r/plot.common.r")

pdf(NULL)

drag <- 0.1/100

btcDataDf <- read.csv("btcusd_1-min_data.csv") #Timestamp,Open,High,Low,Close,Volume
btcXts <- btcDataDf |> 
  filter(!is.na(Timestamp)) |> 
  mutate(Timestamp = lubridate::as_datetime(Timestamp)) |> 
  as.xts()

btcXts2 <- to.period(btcXts$Close, "days", k = 1, OHLC = TRUE)
names(btcXts2) <- gsub('btcXts.Close.', '', names(btcXts2))

volXts2 <- volatility(btcXts2, n=50, calc='yang.zhang')
volXtsTile <- rollapply(volXts2, 500, \(x) last(ntile(x, 10))) 
names(volXtsTile) <- c('vol')

btcXts2$RET <- btcXts2$Close/stats::lag(btcXts2$Close, 1) - 1
btcXts2$RET_1 <- stats::lag(btcXts2$RET, -1)

#plot sma vs. sharpe till 2019
smaLbs <- seq(5, 200, by=5)
smaXts <- do.call(merge.xts, lapply(smaLbs, \(x) SMA(btcXts2$Close, x)))
allslice <- na.omit(merge(btcXts2$Close, volXtsTile, btcXts2$RET_1, smaXts))
allslice <- allslice["/2019",]

#### sma only

smaRets <- do.call(merge.xts, lapply(1:length(smaLbs), \(x){
  rret <- ifelse(allslice$Close > allslice[, 3 + x], allslice$RET_1, 0)
  rret$T <- ifelse(allslice$Close > allslice[, 3 + x], 1, 0)
  rret$T1 <-  rret$T - stats::lag(rret$T, 1)
  dret <- ifelse(rret$T1 != 0, rret[,1]-drag, rret[,1])
  return(dret)
}))

smaRets <- na.omit(smaRets)
statsDf <- data.frame(LB = smaLbs, SR = as.numeric(t(SharpeRatio.annualized(smaRets))), AR = 100*as.numeric(t(Return.annualized(smaRets))))

smaRets <- do.call(merge.xts, lapply(1:length(smaLbs), \(x){
  rret <- ifelse(allslice$Close > allslice[, 3 + x], ifelse(allslice$vol <= 8, allslice$RET_1, 0.50*allslice$RET_1), 0)
  rret$T <- ifelse(allslice$Close > allslice[, 3 + x], 1, 0)
  rret$T1 <-  rret$T - stats::lag(rret$T, 1)
  dret <- ifelse(rret$T1 != 0, rret[,1]-drag, rret[,1])
  return(dret)
}))

smaRets <- na.omit(smaRets)
statsDf2 <- data.frame(LB = smaLbs, SR = as.numeric(t(SharpeRatio.annualized(smaRets))), AR = 100*as.numeric(t(Return.annualized(smaRets))))

p1 <- statsDf |> inner_join(statsDf2, join_by(LB), suffix=c(".raw", ".vol")) |> select(LB, SR.raw, SR.vol) |> pivot_longer(-LB) |>
  ggplot(aes(x=factor(LB, levels = unique(LB)), y=value, color=name)) +
    theme_economist() +
    geom_point() +
    scale_color_viridis_d() +
    labs(x = '', y = 'annualized sharpe ratio', color='',
         title="Annualized Sharpe Ratio")

p2 <- statsDf |> inner_join(statsDf2, join_by(LB), suffix=c(".raw", ".vol")) |> select(LB, AR.raw, AR.vol) |> pivot_longer(-LB) |>
  ggplot(aes(x=factor(LB, levels = unique(LB)), y=value, color=name)) +
  theme_economist() +
  geom_point() +
  scale_color_viridis_d() +
  labs(x = 'lookback', y = 'annualized returns (%)', color='',
       title="Annualized Returns")

p1 / p2 + plot_annotation(title = "Bitcoin Trend Following", 
                          subtitle = sprintf("%s:%s", first(index(smaRets)), last(index(smaRets))),
                          caption = '@StockViz',
                          theme = theme_economist())
ggsave("btc.sma.sharpe.returns.pre.png", width = 12, height = 12, units = "in")

#################### using stats from above, run if after COVID

smaLbs <- c(15, 20, 25)

smaXts <- do.call(merge.xts, lapply(smaLbs, \(x) SMA(btcXts2$Close, x)))
allslice <- na.omit(merge(btcXts2$Close, volXtsTile, btcXts2$RET_1, smaXts))
allslice <- allslice["2020-05-01/", ]

allResults <- NULL

#### sma only

smaRets <- do.call(merge.xts, lapply(1:length(smaLbs), \(x){
  rret <- ifelse(allslice$Close > allslice[, 3 + x], allslice$RET_1, 0)
  rret$T <- ifelse(allslice$Close > allslice[, 3 + x], 1, 0)
  rret$T1 <-  rret$T - stats::lag(rret$T, 1)
  dret <- ifelse(rret$T1 != 0, rret[,1]-drag, rret[,1])
  return(dret)
}))

sliceSmaRet <- xts(rowMeans(smaRets, na.rm = TRUE), index(smaRets))
sliceSmaRet <- sliceSmaRet[-1]

allResults <- merge.xts(allResults, sliceSmaRet)

print("SMA Only *********************")

sr <- SharpeRatio.annualized(sliceSmaRet)
Common.PlotCumReturns(sliceSmaRet, "Bitcoin SMA", paste('sr:', round(sr,2)), "btc.sma.post.png")
print(paste("Annualized Returns: ", round(100*Return.annualized(sliceSmaRet), 2)))
SharpeRatio.annualized(sliceSmaRet)
print(paste("Annualized Sharpe: ", round(sr, 2)))
print(table.Drawdowns(sliceSmaRet, 10))

#### sma + vol

smaRets <- do.call(merge.xts, lapply(1:length(smaLbs), \(x){
  rret <- ifelse(allslice$Close > allslice[, 3 + x], ifelse(allslice$vol <= 8, allslice$RET_1, 0.50*allslice$RET_1), 0)
  rret$T <- ifelse(allslice$Close > allslice[, 3 + x], 1, 0)
  rret$T1 <-  rret$T - stats::lag(rret$T, 1)
  dret <- ifelse(rret$T1 != 0, rret[,1]-drag, rret[,1])
  return(dret)
}))

sliceSmaRet <- xts(rowMeans(smaRets, na.rm = TRUE), index(smaRets))
sliceSmaRet <- sliceSmaRet[-1]

allResults <- merge.xts(allResults, sliceSmaRet)

print("SMA + vol *********************")

sr <- SharpeRatio.annualized(sliceSmaRet)
Common.PlotCumReturns(sliceSmaRet, "Bitcoin SMA + vol positioning", paste('sr:', round(sr,2)), "btc.sma.vol.post.png")
print(paste("Annualized Returns: ", round(100*Return.annualized(sliceSmaRet), 2)))
print(paste("Annualized Sharpe: ", round(sr, 2)))
print(table.Drawdowns(sliceSmaRet, 10))

# b&H

allResults <- merge.xts(allResults, allslice$RET_1)

print("Buy & Hold *********************")

sr <- SharpeRatio.annualized(allslice$RET_1)
Common.PlotCumReturns(allslice$RET_1, "Bitcoin Buy & Hold", paste('sr:', round(sr,2)), "btc.bh.post.png")

print(paste("Annualized Returns: ", round(100*Return.annualized(allslice$RET_1), 2)))
print(paste("Annualized Sharpe: ", round(sr, 2)))
print(table.Drawdowns(allslice$RET_1, 10))

###########################

names(allResults) <- c("SMA", "SMA+vol", "B&H")
sr <- SharpeRatio.annualized(allResults)
Common.PlotCumReturns(allResults, "Bitcoin Strategies", paste('sr:', paste(round(sr,2), collapse=";")), "btc.post.png")

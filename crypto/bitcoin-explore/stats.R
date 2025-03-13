library('tidyverse')
library('quantmod')
library('PerformanceAnalytics')
library('ggthemes')
library('patchwork')
library('viridis')
library('gtExtras')

pdf(NULL)

btcDataDf <- read.csv("btcusd_1-min_data.csv") #Timestamp,Open,High,Low,Close,Volume
btcXts <- btcDataDf |> 
  filter(!is.na(Timestamp)) |> 
  mutate(Timestamp = lubridate::as_datetime(Timestamp)) |> 
  as.xts()

summXts <- function(allXts, thresh){
  vals <- coredata(allXts$RET)
  vals <- vals[!is.na(vals)]
  avg <- 100*mean(vals)
  med <- 100*median(vals)
  stdDev <- 100*sd(vals)
  qtiles <- data.frame(quantile(vals))
  statsDf <- data.frame(DESC = '', VAL = 0.0)
  statsDf <- rbind(statsDf, c(sprintf("n (> +%d%%)", thresh), length(vals[vals > thresh/100])))
  statsDf <- rbind(statsDf, c(sprintf("n (< -%d%%)",thresh), length(vals[vals < -thresh/100])))
  statsDf <- rbind(statsDf, c("mean", round(avg, 5)))
  statsDf <- rbind(statsDf, c("median", round(med, 5)))
  statsDf <- rbind(statsDf, c("sd", round(stdDev, 2)))
  statsDf <- rbind(statsDf, c("skew", round(skewness(vals), 2)))
  statsDf <- rbind(statsDf, c("kurtosis", round(kurtosis(vals), 2)))
  statsDf <- rbind(statsDf, c("max", round(100*max(vals), 2)))
  statsDf <- rbind(statsDf, c("min", round(100*min(vals), 2)))
  statsDf <- rbind(statsDf, c("25%/75%", paste(round(100*qtiles[c(2, 4), 1], 2), collapse='/')))
  
  statsDf <- statsDf[-1,]

  return(statsDf)
}

btcXts2 <- to.period(btcXts$Close, "minutes", k = 5, OHLC = FALSE)
btcXts2$RET <- btcXts2$Close/stats::lag(btcXts2$Close, 1) - 1
statsDf <- summXts(btcXts2, 10)
statsDfpre <- summXts(btcXts2["/2019"], 10)
statsDfpost <- summXts(btcXts2["2020-05-01/"], 10)

btcXts2 <- to.period(btcXts$Close, "minutes", k = 15, OHLC = FALSE)
btcXts2$RET <- btcXts2$Close/stats::lag(btcXts2$Close, 1) - 1
tempDf <- summXts(btcXts2, 10)
statsDf <- cbind(statsDf, tempDf[,2])
tempDf <- summXts(btcXts2["/2019"], 10)
statsDfpre <- cbind(statsDfpre, tempDf[,2])
tempDf <- summXts(btcXts2["2020-05-01/"], 10)
statsDfpost <- cbind(statsDfpost, tempDf[,2])

btcXts2 <- to.period(btcXts$Close, "minutes", k = 30, OHLC = FALSE)
btcXts2$RET <- btcXts2$Close/stats::lag(btcXts2$Close, 1) - 1
tempDf <- summXts(btcXts2, 10)
statsDf <- cbind(statsDf, tempDf[,2])
tempDf <- summXts(btcXts2["/2019"], 10)
statsDfpre <- cbind(statsDfpre, tempDf[,2])
tempDf <- summXts(btcXts2["2020-05-01/"], 10)
statsDfpost <- cbind(statsDfpost, tempDf[,2])

btcXts2 <- to.period(btcXts$Close, "hours", k = 1, OHLC = FALSE)
btcXts2$RET <- btcXts2$Close/stats::lag(btcXts2$Close, 1) - 1
tempDf <- summXts(btcXts2, 10)
statsDf <- cbind(statsDf, tempDf[,2])
tempDf <- summXts(btcXts2["/2019"], 10)
statsDfpre <- cbind(statsDfpre, tempDf[,2])
tempDf <- summXts(btcXts2["2020-05-01/"], 10)
statsDfpost <- cbind(statsDfpost, tempDf[,2])

btcXts2 <- to.period(btcXts$Close, "days", k = 1, OHLC = FALSE)
btcXts2$RET <- btcXts2$Close/stats::lag(btcXts2$Close, 1) - 1
tempDf <- summXts(btcXts2, 10)
statsDf <- cbind(statsDf, tempDf[,2])
tempDf <- summXts(btcXts2["/2019"], 10)
statsDfpre <- cbind(statsDfpre, tempDf[,2])
tempDf <- summXts(btcXts2["2020-05-01/"], 10)
statsDfpost <- cbind(statsDfpost, tempDf[,2])

colnames(statsDf) <- c("stat", "5m", "15m", "30m", "1h", "1d")
colnames(statsDfpre) <- c("stat", "5m", "15m", "30m", "1h", "1d")
colnames(statsDfpost) <- c("stat", "5m", "15m", "30m", "1h", "1d")

statsDf %>%
  gt() %>%
  tab_header(title = "$BTC Stats", subtitle = sprintf('%s:%s', as.Date(first(index(btcXts2))), as.Date(last(index(btcXts2))))) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  cols_label(stat = '') %>%
  fmt_number(decimals=2) %>%
  sub_missing(missing_text = '') %>%
  opt_stylize(style=5) %>%
  tab_options(table.font.size = '130%') %>%
  gtsave("btc.stats.all.png")

statsDfpre %>%
  gt() %>%
  tab_header(title = "$BTC Stats", subtitle = sprintf('%s:2019', as.Date(first(index(btcXts2))))) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  #tab_options(column_labels.hidden = TRUE) %>%
  cols_label(stat = '') %>%
  fmt_number(decimals=2) %>%
  sub_missing(missing_text = '') %>%
  opt_stylize(style=5) %>%
  tab_options(table.font.size = '130%') %>%
  gtsave("btc.stats.pre.png")

statsDfpost %>%
  gt() %>%
  tab_header(title = "$BTC Stats", subtitle = sprintf('2020-05-01:%s', as.Date(last(index(btcXts2))))) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  #tab_options(column_labels.hidden = TRUE) %>%
  cols_label(stat = '') %>%
  fmt_number(decimals=2) %>%
  sub_missing(missing_text = '') %>%
  opt_stylize(style=5) %>%
  tab_options(table.font.size = '130%') %>%
  gtsave("btc.stats.post.png")
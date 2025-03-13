library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggrepel')
library('gtExtras')
library('webshot2')

options("scipen" = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)

startDate <- as.Date("2016-01-01")

tickers <- dbGetQuery(pgCon, "select ticker, min(date_stamp) st, max(date_stamp) ed from eod_adjusted_nse where date_stamp >= $1 group by ticker", params=list(startDate))
tickers <- tickers |> filter(st <= startDate & ed == max(ed))
tickers$min_daily_return_val <- NA
tickers$min_daily_return_date <- NA

for(i in 1:nrow(tickers)){
  ticker <- tickers$ticker[i]
  pxDf <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse where date_stamp >= $1 and ticker = $2", params=list(startDate, ticker))
  pXts <- xts(pxDf[,2], pxDf[,1])
  dXts <- dailyReturn(pXts)
  tickers$min_daily_return_val[i] <- min(dXts)
  tickers$min_daily_return_date[i] <- as.character(index(dXts[dXts == min(dXts)])[1])
}

tickers$min_daily_return_date <- as.Date(tickers$min_daily_return_date)

tickers2 <- tickers |> filter(min_daily_return_val < -0.19)
tickers2$ret_1 <- NA
tickers2$ret_5 <- NA
tickers2$ret_10 <- NA
tickers2$ret_20 <- NA
tickers2$ret_50 <- NA
tickers2$ret_100 <- NA
tickers2$ret_200 <- NA
horizons <- c(1, 5, 10, 20, 50, 100, 200)

for(i in 1:nrow(tickers2)){
  ticker <- tickers2$ticker[i]
  stDate <- tickers2$min_daily_return_date[i]
  pxDf <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse where date_stamp >= $1 and ticker = $2", params=list(stDate, ticker))
  pXts <- xts(pxDf[,2], pxDf[,1])
  dXts <- dailyReturn(pXts)
  
  if(nrow(dXts) < 4) next
  
  for(j in horizons){
    if(nrow(dXts) > j){
      tickers2[i, paste0('ret_', j)] <- as.numeric(Return.cumulative(dXts[2:(j+1)]))
    }
  }
}

write.csv(tickers2, file=sprintf("%s/returns.-20.csv", reportPath), row.names = FALSE)

allStatsDf <- data.frame(HORIZON = 0, DESC = '', VAL = 0.0)
thresholds <- c(1, 5, 10, 10, 20, 20, 20)
for(i in 1:length(horizons)){
  j <- horizons[i]
  thresh <- thresholds[i]
  vals <- tickers2[, paste0('ret_', j)]
  vals <- vals[!is.na(vals)]
  avg <- 100*mean(vals)
  med <- 100*median(vals)
  stdDev <- 100*sd(vals)
  statsDf <- data.frame(DESC = '', VAL = 0.0)
  statsDf <- rbind(statsDf, c(sprintf("n (> +%d%%)", thresh), length(vals[vals > thresh/100])))
  statsDf <- rbind(statsDf, c(sprintf("n (< -%d%%)",thresh), length(vals[vals < -thresh/100])))
  statsDf <- rbind(statsDf, c("mean", avg))
  statsDf <- rbind(statsDf, c("median", med))
  statsDf <- rbind(statsDf, c("sd", stdDev))
  statsDf <- rbind(statsDf, c("max", 100*max(vals)))
  statsDf <- rbind(statsDf, c("min", 100*min(vals)))
  
  statsDf <- statsDf[-1,]
  statsDf[,2] <- as.numeric(statsDf[,2])
  
  allStatsDf <- rbind(allStatsDf, c(j, 'mean', avg))
  allStatsDf <- rbind(allStatsDf, c(j, 'median', med))
  allStatsDf <- rbind(allStatsDf, c(j, 'sd', stdDev))
  
  statsDf %>%
    gt() %>%
    tab_header(title = sprintf("%d-day returns after a -20%% day", j), subtitle = sprintf('%s onwards', startDate)) %>%
    tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
    #tab_options(column_labels.hidden = TRUE) %>%
    cols_label(everything() ~ '') %>%
    fmt_number(decimals=2) %>%
    sub_missing(missing_text = '') %>%
    opt_stylize(style=5) %>%
    tab_options(table.font.size = '130%') %>%
    gtsave(sprintf("%s/returns.%d-day.stats.png", reportPath, j))
  
  ggplot(tickers2, aes(x=100*.data[[paste0('ret_', j)]])) +
    theme_economist() +
    geom_histogram(binwidth = 2, fill=viridis_pal()(2)[1]) +
    labs(x = 'returns(%)', y = 'count', 
         title=sprintf("%d-day returns after a -20%% day", j), 
         subtitle = sprintf('%s onwards', startDate),
         caption="@StockViz")
  
  ggsave(sprintf("%s/returns.%d-day.hist.png", reportPath, j), width = 12, height = 6, units = "in")
}

allStatsDf <- allStatsDf[-1,]
allStatsDf[,1] <- as.character(allStatsDf[,1])
allStatsDf[,3] <- as.numeric(allStatsDf[,3])

allStatsDf |> pivot_wider(id_cols=HORIZON, names_from=DESC, values_from=VAL) %>%
  gt() %>%
  tab_header(title = "Returns after a -20% day", subtitle = sprintf('%s onwards', startDate)) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  #tab_options(column_labels.hidden = TRUE) %>%
  cols_label(HORIZON ~ 'days') %>%
  fmt_number(decimals=2) %>%
  sub_missing(missing_text = '') %>%
  opt_stylize(style=5) %>%
  tab_options(table.font.size = '130%') %>%
  gtsave(sprintf("%s/returns.stats.png", reportPath))

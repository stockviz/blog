library('duckdb')
library('tidyverse')
library('dplyr')
library('ggthemes')
library('patchwork')

reportPath <- "."

#read stats created from coin_dt.sh
coinBookBuy <- read.csv(file=sprintf("%s/coin_book_dt_buy.csv", reportPath))
coinBookSell <- read.csv(file=sprintf("%s/coin_book_dt_sell.csv", reportPath))

coinBook <- coinBookBuy |> inner_join(coinBookSell, by=c('coin', 'dt'))

coinDtRange <- coinBook |> group_by(coin) |> summarise(BEGIN = min(dt), END = max(dt))
oldCoins <- coinDtRange |> filter(BEGIN <= 20240101 & END == max(END)) |> select(coin)

topBook <- coinBook |> filter(coin %in% oldCoins$coin & avg_buy > 0 & avg_sell > 0 & dt >= 20240101 & dt <= 20240331) |>
  group_by(coin) |>
  summarize(MED_BUY = median(avg_buy), SD_BUY = sd(avg_buy), MED_SELL = median(avg_sell), SD_BUY = sd(avg_sell)) |>
  arrange(desc((MED_BUY + MED_SELL)/2))

write.csv(topBook, file=sprintf("%s/book_median_2024q1.csv", reportPath), row.names = FALSE)

#run get_stats.sh
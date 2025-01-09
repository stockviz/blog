library('duckdb')
library('tidyverse')
library('dplyr')
library('ggthemes')
library('patchwork')

reportPath <- "."

con <- dbConnect(duckdb::duckdb(), 
                 dbdir = "/mnt/data/crypto/hyperliquid/hyper-historical.db", 
                 read_only = TRUE,
                 config = list("memory_limit"="32GB"))

print("getting daily stats... ")
btcSpread <- tbl(con, 'l2') |> 
  filter(coin == 'BTC' & sellPx1 > 0 & buyPx1 > 0) |>
  group_by(dt) |>
  summarise(med_spread = 100*median(sellPx1/buyPx1-1), 
            med_buy_sz = median(wavgBuyPx*totalBuyQty), 
            med_sell_sz = median(wavgSellPx*totalSellQty)) |>
  collect() |>
  arrange(dt)

btcSpread <- btcSpread |> ungroup() |> mutate(T = as.Date(strptime(dt, '%Y%m%d')))

stDt <- first(btcSpread$T)
edDt <- last(btcSpread$T)
p1 <- ggplot(btcSpread, aes(x = T, y=med_spread)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%Y-%m') +
  labs(x = '', y = 'median b/o spread (%)', title = 'Spread')

p2 <- btcSpread |> select(T, med_buy_sz, med_sell_sz) |> pivot_longer(-T) |> 
  mutate(value = value/1000000) |>
  ggplot(aes(x = T, y = value, color = name)) +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_line() +
    scale_x_date(date_breaks = "1 month", date_labels = '%Y-%m') +
    labs(x = '', y = "median order book ('000000)", color='', title='Order book')

p1 / p2 + plot_annotation(title = "BTC on hyperliquid (daily)", 
                          subtitle = sprintf("%s:%s", stDt, edDt),
                          theme = theme_economist())

ggsave(
  sprintf("%s/%s", reportPath, 'btc.daily.png'),
  width = 16,
  height = 16,
  units = "in"
)



###############################3

print("getting hourly stats... ")
btcSpread2 <- tbl(con, 'l2') |> 
  filter(coin == 'BTC' & sellPx1 > 0 & buyPx1 > 0) |>
  group_by(dt, hr) |>
  summarise(med_spread = 100*median(sellPx1/buyPx1-1), 
            med_buy_sz = median(wavgBuyPx*totalBuyQty), 
            med_sell_sz = median(wavgSellPx*totalSellQty)) |>
  collect() |>
  arrange(dt, hr)

btcSpread2 <- btcSpread2 |> ungroup() |> mutate(T = as.Date(strptime(dt, '%Y%m%d')))

p1 <- ggplot(btcSpread2, aes(x = T, color = hr, y = med_spread)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%Y-%m') +
  scale_color_viridis_c() +
  guides(color='none') +
  labs(x = '', y = 'median b/o spread (%)', title='Spread')

p2 <- btcSpread2 |> select(T, hr, med_buy_sz, med_sell_sz) |> pivot_longer(-c(T, hr)) |> 
  mutate(value = value/1000000) |>
  ggplot(aes(x = T, y = value, color = hr, group = name)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = '%Y-%m') +
  scale_color_viridis_c() +
  guides(color='none') +
  labs(x = '', y = "median order book ('000000)", color='', title='Order book')

p1 / p2 + plot_annotation(title = "BTC on hyperliquid (hourly)", 
                          subtitle = sprintf("%s:%s", stDt, edDt),
                          theme = theme_economist())

ggsave(
  sprintf("%s/%s", reportPath, 'btc.hourly.png'),
  width = 16,
  height = 16,
  units = "in"
)


###############################






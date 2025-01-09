library('tidyverse')
library('ggthemes')
library('patchwork')

reportPath <- "."

filenames <- Sys.glob("*_daily_stats.csv") #from running get_stats_daily.sh

for(i in 1:length(filenames)){
  fname = filenames[i]
  coin <- strsplit(fname, '_')[[1]][1]
  
  coinSpread <- read.csv(fname)
  
  coinSpread <- coinSpread |> 
    mutate(across(everything(), ~ as.numeric(.x))) |>
    arrange(dt) |> 
    mutate(T = as.Date(strptime(dt, '%Y%m%d')))
  
  stDt <- first(coinSpread$T)
  edDt <- last(coinSpread$T)
  p1 <- ggplot(coinSpread, aes(x = T, y=med_spread)) +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_line() +
    scale_x_date(date_breaks = "1 month", date_labels = '%Y-%m') +
    labs(x = '', y = 'median b/o spread (%)', title = 'Spread')
  
  p2 <- coinSpread |> select(T, med_buy_sz, med_sell_sz) |> pivot_longer(-T) |> 
    mutate(value = value/1000000) |>
    ggplot(aes(x = T, y = value, color = name)) +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_line() +
    scale_x_date(date_breaks = "1 month", date_labels = '%Y-%m') +
    labs(x = '', y = "median order book ('000000)", color='', title='Order book')
  
  p1 / p2 + plot_annotation(title = sprintf("%s on hyperliquid (daily)", coin), 
                            subtitle = sprintf("%s:%s", stDt, edDt),
                            theme = theme_economist())
  
  ggsave(
    sprintf("%s/%s.daily_stats.png", reportPath, coin),
    width = 16,
    height = 16,
    units = "in"
  )
}
library('tidyverse')
library('ggthemes')
library('patchwork')
library('Hmisc')

reportPath <- "."

filenames <- Sys.glob("*_hourly_stats.csv") #from running get_stats_daily.sh

allStats <- tibble()

for(i in 1:length(filenames)){
  fname = filenames[i]
  coin <- strsplit(fname, '_')[[1]][1]
  
  coinStats <- read.csv(fname)

  coinStats <- coinStats |> 
    mutate(across(everything(), ~ as.numeric(.x))) |>
    arrange(hr)
  
  allStats <- rbind(allStats, coinStats)
  
  stDt <- as.Date(strptime(min(coinStats$dt), '%Y%m%d'))
  edDt <- as.Date(strptime(max(coinStats$dt), '%Y%m%d'))
  
  #plot spreads by hour
  ggplot(coinStats, aes(x = factor(hr, levels = unique(hr)), y=med_spread)) +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_violin() +
    stat_summary(fun.y="median", geom="point") +
    labs(x = 'hour of the day', y = 'b/o spread (%)', 
         title = sprintf("%s spread on hyperliquid (hourly)", coin),
         subtitle = sprintf("%s:%s", stDt, edDt))
  
  ggsave(sprintf("%s/%s.hourly_spreads.png", reportPath, coin), width = 16, height = 8, units = "in")
  
  #plot by day
  
  p1 <- coinStats |> 
    mutate(T = as.Date(strptime(dt, '%Y%m%d'))) |>
    arrange(T) |>
    ggplot(aes(x = T, y=med_spread)) +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      stat_summary(fun.data=mean_sdl, geom="pointrange") +
      scale_x_date(date_breaks = "1 month", date_labels = '%Y-%m') +
      labs(x = '', y = "b/o spread (%)", color='', title='Spread')
  
  p2 <- coinStats |> 
    mutate(T = as.Date(strptime(dt, '%Y%m%d'))) |>
    select(T, med_buy_sz, med_sell_sz) |>
    pivot_longer(-T) |>
    arrange(T) |>
    ggplot(aes(x = T, y=value/1000000, color=name)) +
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    stat_summary(fun.data=mean_sdl, geom="pointrange") +
    scale_x_date(date_breaks = "1 month", date_labels = '%Y-%m') +
    labs(x = '', y = "median order book ('000000)", color='', title='Order book')
    
  p1 / p2 + plot_annotation(title = sprintf("%s on hyperliquid (daily)", coin), 
                            subtitle = sprintf("%s:%s", stDt, edDt),
                            theme = theme_economist())
  
  ggsave(sprintf("%s/%s.daily_range.png", reportPath, coin), width = 16, height = 16, units = "in")
}

stDt <- as.Date(strptime(min(allStats$dt), '%Y%m%d'))
edDt <- as.Date(strptime(max(allStats$dt), '%Y%m%d'))

ggplot(allStats, aes(x = factor(hr, levels = unique(hr)), y=med_spread)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_violin() +
  stat_summary(fun.y="median", geom="point") +
  labs(x = 'hour of the day', y = 'b/o spread (%)', 
       title = "Aggregate spread on hyperliquid (hourly)",
       subtitle = sprintf("%s:%s", stDt, edDt))

ggsave(
  sprintf("%s/aggregate.hourly_spreads.png", reportPath),
  width = 16,
  height = 8,
  units = "in"
)
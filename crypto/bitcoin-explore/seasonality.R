library('tidyverse')
library('quantmod')
library('PerformanceAnalytics')
library('ggthemes')
library('patchwork')
library('viridis')
library('feasts')

pdf(NULL)
reportPath <- "."


btcDataDf <- read.csv("btcusd_1-min_data.csv") #Timestamp,Open,High,Low,Close,Volume
btcXts <- btcDataDf |> 
  filter(!is.na(Timestamp)) |> 
  mutate(Timestamp = lubridate::as_datetime(Timestamp)) |> 
  as.xts()

btcXts2 <- to.period(btcXts$Close, "days", k = 1, OHLC = TRUE)
names(btcXts2) <- gsub('btcXts.Close.', '', names(btcXts2))

################################## volatility #################################

volXts2 <- volatility(btcXts2, n=50, calc='yang.zhang')
names(volXts2) <- c("vol")
iXts <- na.omit(volXts2)
index(iXts) <- as.Date(index(iXts))
indexDf <- as.data.frame(iXts)
indexDf$time_stamp <- index(iXts)

indexDf %>%  arrange(time_stamp) %>% 
  mutate(M = factor(month(time_stamp))) %>% {
    ggplot(., aes(x=M, y=vol, group=M)) +
      theme_economist() +
      geom_violin() +
      geom_boxplot(mapping=aes(color=M), width=0.1, alpha=0.5) +
      scale_color_viridis_d() +
      guides(color='none') +
      labs(x="month", y="volatility", 
         title="Bitcoin volatility", 
         subtitle = sprintf("yang.zhang; %s:%s", min(.$time_stamp), max(.$time_stamp)), 
         caption = "@StockViz")
}

ggsave(sprintf("%s/bitcoin-volatility.monthly.all.png", reportPath), width=12, height=6, units="in")

indexDf %>% arrange(time_stamp) %>% 
  filter(time_stamp <= as.Date("2019-12-01")) %>% 
  mutate(M = factor(month(time_stamp))) %>% {
  ggplot(., aes(x=M, y=vol, group=M)) +
    theme_economist() +
    geom_violin() +
    geom_boxplot(mapping=aes(color=M), width=0.1, alpha=0.5) +
    scale_color_viridis_d() +
    guides(color='none') +
    labs(x="month", y="volatility", 
       title="Bitcoin volatility (pre-COVID)", 
       subtitle = sprintf("yang.zhang; %s:%s", min(.$time_stamp), max(.$time_stamp)), 
       caption = "@StockViz")
}

ggsave(sprintf("%s/bitcoin-volatility.monthly.pre.png", reportPath), width=12, height=6, units="in")

indexDf %>% arrange(time_stamp) %>% 
  filter(time_stamp >= as.Date("2020-05-01")) %>%
  mutate(M = factor(month(time_stamp))) %>% {
  ggplot(., aes(x=M, y=vol, group=M)) +
    theme_economist() +
    geom_violin() +
    geom_boxplot(mapping=aes(color=M), width=0.1, alpha=0.5) +
    scale_color_viridis_d() +
    guides(color='none') +
    labs(x="month", y="volatility", 
         title="Bitcoin volatility (post-COVID)", 
         subtitle = sprintf("yang.zhang; %s:%s", min(.$time_stamp), max(.$time_stamp)), 
         caption = "@StockViz")
}

ggsave(sprintf("%s/bitcoin-volatility.monthly.post.png", reportPath), width=12, height=6, units="in")


### decompose

iTs <- tsbox::ts_tsibble(iXts)
decomp <- iTs %>% model(STL(value)) %>% components()

p <- decomp %>% autoplot() +
  theme_economist() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(x="") +
  scale_x_date(date_breaks='6 months', date_labels='%Y %b %d') 

p + plot_annotation(
  theme = theme_economist(),
  title = "Bitcoin volatility",
  subtitle = sprintf("yang.zhang; %s:%s", first(index(iXts)), last(index(iXts))), 
  caption = '@StockViz'
)

ggsave(sprintf("%s/bitcoin-volatility.decomposition.all.png", reportPath), width=18, height=12, units="in")

decomp %>% mutate(md = as.Date(format(time, "1999-%m-%d")), y = year(time)) %>% arrange(.model, time, md) %>% {
  ggplot(., aes(x=md, y=season_year, color=factor(y, levels=unique(y)))) +
    scale_x_date(date_breaks='1 months', date_labels='%b') +
    scale_color_viridis_d() +
    theme_economist() +
    geom_point() +
    labs(color="", x="", 
         title = "Bitcoin Volatility Seasonality by month", 
         subtitle = sprintf("yang.zhang; %s:%s", first(index(iXts)), last(index(iXts))),
         caption = '@StockViz')
}

ggsave(sprintf("%s/bitcoin-volatility.seasonality.month.all.png", reportPath), width=14, height=8, units="in")

decomp2 <- decomp %>% filter(time <= as.Date("2019-12-01"))

p <- decomp2 %>% autoplot() +
  theme_economist() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(x="") +
  scale_x_date(date_breaks='6 months', date_labels='%Y %b %d') 

p + plot_annotation(
  theme = theme_economist(),
  title = "Bitcoin volatility (pre-COVID)",
  subtitle = sprintf("yang.zhang; %s:%s", min(decomp2$time), max(decomp2$time)), 
  caption = '@StockViz'
)

ggsave(sprintf("%s/bitcoin-volatility.decomposition.pre.png", reportPath), width=18, height=12, units="in")

decomp2 %>% mutate(md = as.Date(format(time, "1999-%m-%d")), y = year(time)) %>% arrange(.model, time, md) %>% {
  ggplot(., aes(x=md, y=season_year, color=factor(y, levels=unique(y)))) +
    scale_x_date(date_breaks='1 months', date_labels='%b') +
    scale_color_viridis_d() +
    theme_economist() +
    geom_point() +
    labs(color="", x="", 
         title = "Bitcoin Volatility Seasonality by month (pre-COVID)", 
         subtitle = sprintf("yang.zhang; %s:%s", min(decomp2$time), max(decomp2$time)),
         caption = '@StockViz')
}

ggsave(sprintf("%s/bitcoin-volatility.seasonality.month.pre.png", reportPath), width=14, height=8, units="in")

decomp2 <- decomp %>% filter(time >= as.Date("2020-05-01"))

p <- decomp2 %>% autoplot() +
  theme_economist() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(x="") +
  scale_x_date(date_breaks='3 months', date_labels='%Y %b %d') 

p + plot_annotation(
  theme = theme_economist(),
  title = "Bitcoin volatility (post-COVID)",
  subtitle = sprintf("yang.zhang; %s:%s", min(decomp2$time), max(decomp2$time)), 
  caption = '@StockViz'
)

ggsave(sprintf("%s/bitcoin-volatility.decomposition.post.png", reportPath), width=18, height=12, units="in")

decomp2 %>% mutate(md = as.Date(format(time, "1999-%m-%d")), y = year(time)) %>% arrange(.model, time, md) %>% {
  ggplot(., aes(x=md, y=season_year, color=factor(y, levels=unique(y)))) +
    scale_x_date(date_breaks='1 months', date_labels='%b') +
    scale_color_viridis_d() +
    theme_economist() +
    geom_point() +
    labs(color="", x="", 
         title = "Bitcoin Volatility Seasonality by month (post-COVID)", 
         subtitle = sprintf("yang.zhang; %s:%s", min(decomp2$time), max(decomp2$time)),
         caption = '@StockViz')
}

ggsave(sprintf("%s/bitcoin-volatility.seasonality.month.post.png", reportPath), width=14, height=8, units="in")



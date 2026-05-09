library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('data.table') #rleid

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

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

coin <- "xyz:CL"
pxDf <- dbGetQuery(pgCon, "select * from hype_crypto_historical_1m where coin=$1", params = list(coin))
pxDf$EDT <- as.POSIXct(pxDf$close_time/1000, tz="EDT")

colors = viridis_pal()(2)
pxDf <- pxDf |> mutate(dow = lubridate::wday(EDT, week_start = 1), 
                       is_weekend = if_else(dow == 6 
                                    | (dow == 5 & hour(EDT) > 16)
                                    | (dow == 7 & hour(EDT) < 18), TRUE, FALSE)) |>
  mutate(dow_color = if_else(is_weekend, colors[1], colors[2]))

ggplot(pxDf, aes(x=EDT, y=px_close)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  geom_line(color=pxDf$dow_color) +
  scale_y_log10() +
  scale_x_datetime(date_breaks="2 days", date_labels = "%b-%d") +
  labs(x='EDT', y='price($)',
       title = sprintf("%s Minute-bars on Hyperliquid", coin),
       subtitle = sprintf("%s:%s", 
                          strftime(min(pxDf$EDT), '%Y-%m-%d %H:%M', tz="EDT"), 
                          strftime(max(pxDf$EDT), '%Y-%m-%d %H:%M', tz="EDT")),
       caption = '@StockViz')

ggsave(sprintf("%s/%s.timeseries.png", reportPath, gsub(":", "_", coin)), width = 12, height = 6, units="in")

pxDf$WK <- rleid(pxDf$is_weekend)

weekend_price_moves <- pxDf |> filter(pxDf$is_weekend) |> group_by(WK) |> 
  summarise(PX_HIGH = max(px_close), PX_LOW = min(px_close)) |> 
  mutate(HL_PCT = 100*(PX_HIGH/PX_LOW - 1)) |>
  select(HL_PCT)

startTs <- pxDf$close_time[1]
pxDf$HOUR_DIFF <- (pxDf$close_time - startTs)/1000/60/60

non_weekend_48hr_price_moves <- pxDf |> 
  mutate(HR48 = rleid(if_else(HOUR_DIFF %% 48 != 0 & !is_weekend, TRUE, FALSE))) |>
  group_by(HR48) |> 
  summarise(CNT = n(), PX_HIGH = max(px_close), PX_LOW = min(px_close)) |> 
  filter(CNT > 1000) |>
  mutate(HL_PCT = 100*(PX_HIGH/PX_LOW - 1)) |>
  select(HL_PCT)

tb1 <- tibble("WEEKDAY48H", non_weekend_48hr_price_moves)
tb2 <- tibble("WEEKEND", weekend_price_moves)
names(tb1) <- c('DAY', 'HL')
names(tb2) <- c('DAY', 'HL')
hlTb <- rbind(tb1, tb2)

ggplot(hlTb, aes(y=HL, x=DAY, color=DAY)) +
  theme_economist() +
  geom_violin() +
  geom_jitter(width = 0.05) +
  scale_color_viridis_d() +
  guides(color='none') +
  labs(x='', y = 'High - Low (%)', 
       title = sprintf("%s Hyperliquid Prices High-Low", coin),
       subtitle = sprintf("%s:%s", 
                          strftime(min(pxDf$EDT), '%Y-%m-%d %H:%M', tz="EDT"), 
                          strftime(max(pxDf$EDT), '%Y-%m-%d %H:%M', tz="EDT")),
       caption = '@StockViz')

ggsave(sprintf("%s/%s.weekend-48H.H-L.png", reportPath, gsub(":", "_", coin)), width = 12, height = 6, units="in")
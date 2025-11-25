library('redux')
library('RPostgres')
library('tidyverse')
library('viridis')
library('ggthemes')
library('jsonlite')
library('patchwork')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

pgCon <- dbConnect(RPostgres::Postgres(), host = 'sweden', user = ldbuser2, password = ldbpassword2, dbname = 'StockVizDyn', sslmode = 'allow')
rStream <- hiredis(url = paste0(redisConn, '/6'))

symbol <- "NIFTY"
tzone <- "Asia/Kolkata"
asof <- as.POSIXct(as.character(Sys.Date()), tz=tzone)
asofStr <- as.character(asof)

zdMax <- dbGetQuery(pgCon, "select max(time_stamp) from zd_master")[[1]]
expiryDt <- dbGetQuery(pgCon, "select min(expiry) from zd_master 
                       where name = $1 
                       and time_stamp = $2
                       and inst_type = 'FUT'",
                       params = list(symbol, zdMax))[[1]]

zdTok <- dbGetQuery(pgCon, "select inst_token from zd_master
                     where name = $1 
                     and time_stamp = $2
                     and expiry = $3
                     and inst_type = 'FUT'",
                    params = list(symbol, zdMax, expiryDt))[[1]]

quoteStream <- rStream$LRANGE(zdTok, 0, -1)

pxTb <- tibble()
spreadTb <- tibble()
for (i in seq_along(quoteStream)) {
  pxQuote <- quoteStream[[i]]
  pxObj <- fromJSON(pxQuote)
  ltp <- pxObj$last_price
  ltt <- pxObj$last_trade_time
  bid1 <- pxObj$depth$buy$price[1]
  ask1 <- pxObj$depth$sell$price[1]
  
  if(bid1 == 0 || ask1 == 0) next
  
  pxTb <- rbind(pxTb, c(ltp, ltt))
  spreadTb <- rbind(spreadTb, c((ask1 - bid1)/ask1, ltt))
}

colnames(pxTb) <- c('px', 'ltt')
colnames(spreadTb) <- c('spread', 'ltt')

pxTb2 <- pxTb |> mutate(px = as.numeric(px), 
                       ltt = with_tz(as.POSIXct(ltt, tz = 'UTC', format = "%Y-%m-%dT%H:%M:%OSZ"), tzone = tzone)) |>
  group_by(ltt) |> summarise(px_avg = mean(px)) |>
  mutate(ret = 1 - px_avg/lag(px_avg, default = first(px_avg))) |>
  arrange(ltt)

spreadTb2 <- spreadTb |> mutate(spread = as.numeric(spread), 
                       ltt = with_tz(as.POSIXct(ltt, tz = 'UTC', format = "%Y-%m-%dT%H:%M:%OSZ"), tzone = tzone)) |>
  group_by(ltt) |> summarise(spread_avg = mean(spread)) |>
  arrange(ltt)

beginTs <- as.POSIXct(paste0(asofStr, "T09:15:00Z"), format = "%Y-%m-%dT%H:%M:%SZ", tzone = tzone)
endTs <- as.POSIXct(paste0(asofStr, "T15:30:00Z"), format = "%Y-%m-%dT%H:%M:%SZ", tzone = tzone)

segBegin <- beginTs
rollTb <- tibble()
while(segBegin < endTs){
  segEnd <- segBegin + 15 * 60
  pxSeg <- pxTb2 |> filter (ltt > segBegin & ltt <= segEnd)
  sprdSeg <- spreadTb2 |> filter (ltt > segBegin & ltt <= segEnd)
  
  if(nrow(pxSeg) == 0 || nrow(sprdSeg) == 0) break
  
  #estimate Roll's bid-ask spread
  cov_lag1 <- acf(pxSeg$ret, lag.max = 1, type = "covariance", plot = FALSE)$acf[2]
  rollspread <- NA
  if (cov_lag1 < 0) {
    rollspread <- 2 * sqrt(-cov_lag1)
    rollFundaVar <- var(pxSeg$ret) - 0.25 * rollspread^2
  }
  actualspread <- median(sprdSeg$spread_avg)
  actualFundaVar <- var(pxSeg$ret) - 0.25 * actualspread^2
  
  
  rollTb <- rbind(rollTb, c(segEnd, rollspread, actualspread, rollFundaVar, actualFundaVar))
  segBegin <- segEnd
}

colnames(rollTb) <- c('segEnd', 'rollSpread', 'actualSpread', 'rollFundaVar', 'actualFundaVar')
rollTb$segEnd <- as.POSIXct(rollTb$segEnd)

p1 <- rollTb |> select(segEnd, rollSpread, actualSpread) |>
  pivot_longer(cols=-segEnd) |> 
  mutate(value=value*1000000) |>
    ggplot(aes(x=segEnd, y=value, color=name)) +
      theme_economist() +
      geom_line() +
      scale_color_viridis_d() +
      scale_x_datetime(date_breaks="15 min", date_labels = "%H:%M", timezone = tzone) +
      labs(x='', y="variance '000000", color='',
           title = "bid/ask Spread")


p2 <- rollTb |> select(segEnd, rollFundaVar, actualFundaVar) |>
  pivot_longer(cols=-segEnd) |> 
  mutate(value=value*1000000) |>
  ggplot(aes(x=segEnd, y=value, color=name)) +
  theme_economist() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_datetime(date_breaks="15 min", date_labels = "%H:%M", timezone = tzone) +
  labs(x='', y="spread '000000", color='', title = "Fundamental Variance")

p1/p2 + plot_layout(axes = 'collect_x') +
  plot_annotation(title = sprintf("%s Roll's vs. Actual", symbol),
                        subtitle = asof,
                        caption = '@StockViz',
                        theme = theme_economist())

ggsave(sprintf("%s/%s.intraday.estimate-vs-actual.png", reportPath, symbol), width = 12, height = 12, units = "in")

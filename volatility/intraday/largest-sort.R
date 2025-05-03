library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')

pdf(NULL)
options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

baseDate <- as.Date("1990-01-01", tz='UTC')
startTime <- hms("09:15:00")
endTime <- hms("15:30:00")

indexName <- 'NIFTY 50'
symbol <- 'NIFTY'

lcon <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    ldbserver,
    "StockViz",
    ldbuser,
    ldbpassword
  ),
  case = "nochange",
  believeNRows = TRUE
)

pcon <- dbConnect(RPostgres::Postgres(), host=ldbserver2, user= ldbuser2, password=ldbpassword2, dbname=ldbname2)

niftyDf <- dbGetQuery(pcon, "select o as low, h as open, l as high, c as close, tick_stamp from zd_index_bars where symbol = $1", params=list(indexName))
vixDf <- dbGetQuery(pcon, "select o as low, h as open, l as high, c as close, tick_stamp from zd_index_bars where symbol = 'INDIA VIX' and time_stamp >= $1", params=list(startDate))

niftyDf2 <- niftyDf
niftyDf2$tick <- as.POSIXct(baseDate + seconds(niftyDf$tick_stamp))
niftyDf2$tod <- hms(format(niftyDf2$tick, "%H:%M:%S"))

niftyDf2 <- niftyDf2 %>% filter(tod >= startTime & tod <= endTime) %>% select(open, high, low, close, tick_stamp, tick) 

pXts <- xts(niftyDf2 %>% select(-tick), niftyDf2$tick, tz='UTC')

vixDf$tick <- as.POSIXct(baseDate + seconds(vixDf$tick_stamp))
vixDf$tod <- hms(format(vixDf$tick, "%H:%M:%S"))

vixDf <- vixDf %>% filter(tod >= startTime & tod <= endTime) %>% select(open, high, low, close, tick) 

vixXts <- xts(vixDf %>% select(-tick), vixDf$tick, tz='UTC')

tradeDates <- unique(as.Date(index(pXts)))

tradeVolatility <- NULL
maxTradedVolatility <- NULL
for(i in 1:length(tradeDates)){
  trdDt <- tradeDates[i]
  trdVol <- volatility(pXts[as.character(trdDt)], n=20, calc="yang.zhang")
  maxTrdVol <- max(trdVol, na.rm=TRUE)
  
  tradeVolatility <- rbind.xts(tradeVolatility, trdVol)
  maxTradedVolatility <- rbind.xts(maxTradedVolatility, xts(maxTrdVol, trdDt))
}
names(tradeVolatility) <- c('YZ_VOL')
names(maxTradedVolatility) <- c('VOL')
mtvDf <- data.frame(maxTradedVolatility)
mtvDf$DATE_STAMP <- as.Date(index(maxTradedVolatility))
mtvDf$TILE <- ntile(mtvDf$VOL, n=10)


ggplot(mtvDf, aes(x = VOL)) +
  geom_histogram(binwidth = 0.0001)

ggplot(mtvDf |> filter(TILE == 10), aes(x = VOL)) +
  geom_histogram(binwidth = 0.0001)

highVolDates <- (mtvDf |> filter(DATE_STAMP <= as.Date("2019-12-31") | DATE_STAMP >= as.Date("2020-05-01")) |>
  slice_max(VOL, n=20) |> select(DATE_STAMP))[,1]

rvXts <- NULL
for(i in 1:length(highVolDates)){
  trdDt <- highVolDates[i]
  pSlice <- pXts[as.character(trdDt)]
  pSlice$ATM_STRIKE <- round(pSlice$close/100,0)*100
  pSlice$ATM_IV <- NA
  
  expiryDt <- sqlQuery(lcon, sprintf("select min(expiry_dt) from bhav_eq_fut where symbol='%s' and time_stamp='%s' and time_stamp <> expiry_dt", symbol, trdDt))[[1]]
  for(j in 1:nrow(pSlice)){
    tokCE <- dbGetQuery(pcon, "select inst_token from zd_master where time_stamp=$1 and name=$2 and strike=$3 and expiry=$4 and inst_type=$5",
               params=list(trdDt, symbol, as.integer(pSlice$ATM_STRIKE[j]), expiryDt, 'CE'))[[1]]
    
    if(length(tokCE) == 0) next
    
    tokPE <- dbGetQuery(pcon, "select inst_token from zd_master where time_stamp=$1 and name=$2 and strike=$3 and expiry=$4 and inst_type=$5",
                        params=list(trdDt, symbol, as.integer(pSlice$ATM_STRIKE[j]), expiryDt, 'PE'))[[1]]
    
    if(length(tokPE) == 0) next
    
    closeCE <- dbGetQuery(pcon, "select c from zd_option_bars where time_stamp=$1 and inst_token=$2 and tick_stamp=$3",
               params=list(trdDt, tokCE, pSlice$tick_stamp[j]))[[1]]
    
    closePE <- dbGetQuery(pcon, "select c from zd_option_bars where time_stamp=$1 and inst_token=$2 and tick_stamp=$3",
                          params=list(trdDt, tokPE, pSlice$tick_stamp[j]))[[1]]
    
    atmPrem <- closeCE + closePE
    if(length(atmPrem) == 0) next
    
    iv <- 1.25 * atmPrem/as.numeric(pSlice$ATM_STRIKE[j]) 
    pSlice$ATM_IV[j] <- iv
  }
  
  if(all(is.na(pSlice$ATM_IV))) next
  rvXts <- rbind.xts(rvXts, merge(pSlice$ATM_IV * sqrt(252) * 100, vixXts[as.character(trdDt), 'close'], tradeVolatility[as.character(trdDt)]*100))
}

rvXts2 <- rvXts
names(rvXts2) <- c("ATM_IV", "VIX", "YZ_VOL")
rvDates <- unique(as.Date(index(rvXts2)))
for(i in 1:length(rvDates)){
  trdDt <- rvDates[i]
  pSlice <- data.frame(pXts[as.character(trdDt), "close"])
  pSlice$TS <- index(pXts[as.character(trdDt)])
  rvSlice <- data.frame(rvXts2[as.character(trdDt)])
  rvSlice$TS <- index(rvXts2[as.character(trdDt)])
  
  p1 <- ggplot(pSlice, aes(x=TS, y=close)) + 
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_line() +
    scale_x_datetime(breaks = date_breaks("15 mins"), date_labels = "%H:%M") +
    labs(x = '', y = '', subtitle='Index')

  p2 <- ggplot(rvSlice, aes(x=TS, y=VIX)) + 
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_line() +
    scale_x_datetime(breaks = date_breaks("15 mins"), date_labels = "%H:%M") +
    labs(x = '', y = '', color='', subtitle='VIX')
  
  p3 <- ggplot(rvSlice, aes(x=TS, y=ATM_IV)) + 
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_line() +
    scale_x_datetime(breaks = date_breaks("15 mins"), date_labels = "%H:%M") +
    labs(x = '', y = '', color='', subtitle='ATM Straddle Implied Volatility')
  
  p4 <- ggplot(rvSlice, aes(x=TS, y=YZ_VOL)) + 
    theme_economist() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_line() +
    scale_x_datetime(breaks = date_breaks("15 mins"), date_labels = "%H:%M") +
    labs(x = '', y = '', color='', subtitle='YZ Volatility')
  
  p1/p2/p3/p4 + plot_annotation(
    theme = theme_economist(),
    title = symbol,
    subtitle = trdDt,
    caption = '@StockViz')
  
  ggsave(
    sprintf("%s/%s.%s.ATM-straddle.vs.VIX.png", reportPath, symbol, trdDt),
    width = 12,
    height = 12*4,
    units = "in"
  )
}
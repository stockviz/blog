library('RODBC')
library('RPostgres')
library('tidyverse')
library('quantmod')
library('PerformanceAnalytics')
library('gtExtras')

reportPath <- "."
pdf(NULL)

source("/mnt/hollandC/StockViz/R/config.r")

lcon <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    ldbserver,
    "crypto",
    ldbuser,
    ldbpassword
  ),
  case = "nochange",
  believeNRows = TRUE
)

pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)

lookbacks <- c(20, 50, 100, 200)
stableCoins <- c("USDT","USDC")

l1CoinDt <- sqlQuery(lcon, "select max(date_stamp) from META_CMC where id = 'LAYER_1'")[[1]]
l1Coins <- sqlQuery(lcon, sprintf("select b.value from META_CMC a cross apply openjson(a.val) as b 
                                  where date_stamp = '%s' 
                                  and id = 'LAYER_1'", l1CoinDt))[,1]

l2CoinDt <- sqlQuery(lcon, "select max(date_stamp) from META_CMC where id = 'LAYER_1'")[[1]]
l2Coins <- sqlQuery(lcon, sprintf("select b.value from META_CMC a cross apply openjson(a.val) as b 
                                  where date_stamp = '%s' 
                                  and id = 'LAYER_2'", l1CoinDt))[,1]

l1l2Coins <- sort(unique(c('BTC', l1Coins, l2Coins)))

pxCache <- list()
for(i in 1:length(l1l2Coins)){
  coin <- l1l2Coins[i]
  pXts <- NULL
  for(sc in stableCoins){
    symbol <- paste0(coin, sc)
    pxDf <- dbGetQuery(pgCon, "select open_time_ts, px_open, px_high, px_low, px_close from binance_crypto_historical_1h where symbol = $1", params=list(symbol))
    if(nrow(pxDf) == 0) next
    
    colnames(pxDf) <- c('time_stamp', 'Open', 'High', 'Low', 'Close')
    pxDf$hr <- hour(pxDf[,1])
    pXts <- xts(pxDf[,-1], pxDf[,1])
    break
  }
  
  if(!is.null(pXts)) pxCache[[coin]] <- pXts
}

cachedCoins <- data.frame(COIN = names(pxCache))
cachedCoins$START_DT <- NA
cachedCoins$END_DT <- NA
for(i in 1:nrow(cachedCoins)){
  coin <- cachedCoins$COIN[i]
  cachedCoins$START_DT <- first(index(pxCache[[coin]]))
  cachedCoins$END_DT <- last(index(pxCache[[coin]]))
}

startDt <- as.Date(max(cachedCoins$START_DT))
endDt <- as.Date(min(cachedCoins$END_DT))

getHighs <- function(coins, asof, lb){
  athCoins <- data.frame(ASOF = "", COIN = "")
  for(i in 1:length(coins)){
    coin <- coins[i]
    allPx <- pxCache[[coin]][paste0('/', asof)]
    if(is.null(allPx) || nrow(allPx) == 0) next
    
    pxClose <- tail(allPx[allPx$hr == 0, 'Close'], lb)
    if(nrow(pxClose) != lb) next
    if(as.numeric(last(pxClose)) != as.numeric(max(pxClose))){
      next
    }
    
    athCoins <- rbind(athCoins, c(as.character(asof), coin))
  }
  athCoins <- athCoins[-1,]
  athCoins[,1] <- as.Date(athCoins[,1])
  return(athCoins)
}

getLows <- function(coins, asof, lb){
  athCoins <- data.frame(ASOF = "", COIN = "")
  for(i in 1:length(coins)){
    coin <- coins[i]
    allPx <- pxCache[[coin]][paste0('/', asof)]
    if(is.null(allPx) || nrow(allPx) == 0) next
    
    pxClose <- tail(allPx[allPx$hr == 0, 'Close'], lb)
    if(nrow(pxClose) != lb) next
    if(as.numeric(last(pxClose)) != as.numeric(min(pxClose))){
      next
    }
    
    athCoins <- rbind(athCoins, c(as.character(asof), coin))
  }
  athCoins <- athCoins[-1,]
  athCoins[,1] <- as.Date(athCoins[,1])
  return(athCoins)
}

getFwdStats <- function(coinSubset){
  retColNames <- c("D1", "D5", "D10", "D20")
  fwdReturns <- tibble()
  for(coin in coinSubset){
    pxClose <- pxCache[[coin]]
    pxClose <- pxClose[pxClose$hr == 0, 'Close']
    rets <- dailyReturn(pxClose)
    rets <- merge(rets, rollapply(rets, 5, \(x) Return.cumulative(x)), 
                  rollapply(rets, 10, \(x) Return.cumulative(x)), 
                  rollapply(rets, 20, \(x) Return.cumulative(x)))
    rets[,1] <- stats::lag(rets[,1], -1)
    rets[,2] <- stats::lag(rets[,2], -5)
    rets[,3] <- stats::lag(rets[,3], -10)
    rets[,4] <- stats::lag(rets[,4], -20)
    
    names(rets) <- retColNames
    retDf <- data.frame(rets)
    retDf$T <- as.Date(index(rets))
    
    fRets <- athCoins |> filter(COIN == coin) |> inner_join(retDf, join_by(ASOF == T))
    fwdReturns <- rbind(fwdReturns, fRets)
  }
  
  retStats <- NULL
  thresh <- 10
  for(colName in retColNames){
    vals <- fwdReturns[, colName]
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
    
    if(is.null(retStats)){
      retStats <- statsDf
    } else {
      retStats <- cbind(retStats, statsDf[,2])
    }
  }
  
  colnames(retStats) <- c('stat', retColNames)
  
  return(retStats)
}

for(lbIndex in 1:length(lookbacks)){
  lb <- lookbacks[lbIndex]
  asof <- startDt 
  athCoins <- tibble()
  while(asof <= endDt){
    athAsof <- getHighs(cachedCoins$COIN, asof, lb)
    athCoins <- rbind(athCoins, athAsof)
    asof <- asof + 1
  }
  
  coinSubset <- (athCoins |> distinct(COIN))[,1]
  retStats <- getFwdStats(coinSubset)
  
  retStats %>%
    gt() %>%
    tab_header(title = sprintf("%d-day Highs Stats", lb), subtitle = sprintf('%s:%s', startDt, endDt)) %>%
    tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
    cols_label(stat = '') %>%
    fmt_number(decimals=2) %>%
    sub_missing(missing_text = '') %>%
    opt_stylize(style=5) %>%
    tab_options(table.font.size = '130%') %>%
    gtsave(sprintf("%d-day.highs.stats.png", lb))
  
  asof <- startDt 
  athCoins <- tibble()
  while(asof <= endDt){
    athAsof <- getLows(cachedCoins$COIN, asof, lb)
    athCoins <- rbind(athCoins, athAsof)
    asof <- asof + 1
  }
  
  coinSubset <- (athCoins |> distinct(COIN))[,1]
  retStats <- getFwdStats(coinSubset)
  
  retStats %>%
    gt() %>%
    tab_header(title = sprintf("%d-day Lows Stats", lb), subtitle = sprintf('%s:%s', startDt, endDt)) %>%
    tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
    cols_label(stat = '') %>%
    fmt_number(decimals=2) %>%
    sub_missing(missing_text = '') %>%
    opt_stylize(style=5) %>%
    tab_options(table.font.size = '130%') %>%
    gtsave(sprintf("%d-day.lows.stats.png", lb))
}
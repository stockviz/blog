library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggrepel')

options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

lconCrypto <- odbcDriverConnect(
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

print("connecting to sweden...")
pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)

getCryptoModelDaily <- function(modelIds){
  rXts <- NULL
  rNames <- c()
  for(i in 1:length(modelIds)){
    modelId <- modelIds[i]
    mName <- sqlQuery(lconCrypto, sprintf("select model_name from MODEL_MASTER where master_id='%s' and master_id = child_id", modelId))[[1]]
    childIdDf <- sqlQuery(lconCrypto, sprintf("select child_id from MODEL_MASTER where master_id='%s' and master_id <> child_id", modelId))
    
    if(nrow(childIdDf) > 0){
      childIds <- childIdDf[,1]
    } else {
      childIds <- c(modelId)
    }
    mtm <- sqlQuery(lconCrypto, sprintf("select * from MODEL_PORTFOLIO_MTM where model_id in ('%s')", 
                                        paste(childIds, collapse="','")))
    
    mtmXts <- mtm |> mutate(T = as_datetime(PX_TS/1000, tz="")) |>
      select(-c(MODEL_ID)) |> 
      group_by(T) |>
      summarise(AVG_MTM = mean(VAL)) |>
      as.xts()
    
    dXts <- to.period(mtmXts, "days")[,4]
    index(dXts) <- as.Date(index(dXts))
    
    rXts <- merge.xts(rXts, dailyReturn(dXts))
    rNames <- c(rNames, mName)
  }
  
  rXts <- rXts[-1,]
  names(rXts) <- rNames
  return(rXts)
}

doCryptoDaily <- function(tickers, startDate){
  rXts <- NULL
  for (i in 1:length(tickers)) {
    iName <- tickers[i]
    pDf <- dbGetQuery(pgCon, "select px_close, time_stamp from tiingo_crypto_usd_5min_ts where curr_code=$1 and time_stamp >= $2 and time_stamp <= $3", 
                      params = list(iName, startDate, Sys.Date() + 1))
    
    pDf <- pDf %>% mutate(T = as.Date(time_stamp)) %>% group_by(T) %>% filter(time_stamp == max(time_stamp)) %>% select(px_close, T)
    pXts <- xts(pDf$px_close, pDf$T)
    dret <- dailyReturn(pXts)
    rXts <- merge.xts(rXts, dret)
  }
  rXts <- rXts[-1, ]
  names(rXts) <- tickers
  
  return(rXts)
}

mXts <- getCryptoModelDaily(c("3DDDC442-044E-4B2B-AAF1-3D5C1891436F",
                              "F7C033DB-9DB0-49FF-9B42-F5F2F7AAAFCE"))

cXts <- doCryptoDaily(c('btcusd', 'ethusd', 'solusd'), first(index(mXts)) - 1)

rXts <- merge(mXts, cXts)

sr <- SharpeRatio.annualized(rXts)
Common.PlotCumReturns(rXts,
                      "L1 & L2 Performance",
                      sprintf("sr: %s", paste(round(sr, 2), collapse="/")), #NULL)
                      sprintf("%s/l1.l2.png", reportPath))


mXts <- getCryptoModelDaily(c("50F5EED6-45CA-46BF-9A1A-188C7F56BEDA",
                              "646D188D-5EC3-48A5-8442-1992D6037510",
                              "DB878B6D-A84A-4541-ABC8-7CBBE10F5C85",
                              "A3593684-7E99-4F71-8C45-931AB88A2AD1",
                              "0A8CBECC-98CA-4970-8713-A701C0C4B6BB",
                              "0F6C11AF-F6E6-4E43-9352-BD26882A76A3",
                              "EDFC5554-FEFB-4AD8-BA94-E47039F251D4"))

cXts <- doCryptoDaily(c('btcusd'), first(index(mXts)) - 1)

rXts <- merge(mXts, cXts)
rXts <- na.trim(rXts, side='left')
rXts <- rXts[-1,]

sr <- SharpeRatio.annualized(rXts)
Common.PlotCumReturns(rXts,
                      "Simple Strategies",
                      sprintf("sr: %s", paste(round(sr, 2), collapse="/")), #NULL)
                      sprintf("%s/simple.png", reportPath))

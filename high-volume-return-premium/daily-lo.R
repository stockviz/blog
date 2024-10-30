library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')

options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

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

indexName <- "NIFTY 100 TR"
lookback <- 50 #days

miscDates <- sqlQuery(lcon, "select distinct time_stamp from EQUITY_MISC_INFO")

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, min(miscDates$time_stamp) - lookback*2, max(miscDates$time_stamp)))
indexXts <- xts(indexPx[,2], indexPx[,1])

rebalDates <- miscDates %>% mutate(YM = year(time_stamp)*100 + month(time_stamp)) %>%
  group_by(YM) %>%
  summarize(ST=min(time_stamp)) %>%
  select(ST) %>%
  ungroup() %>%
  as.data.frame()

getReturns <- function(syms, startDate, endDate, expectedLength){
  returns <- c()
  for(i in 1:length(syms)){
    sym <- syms[i]
    pxDf <- sqlQuery(lcon, sprintf("select time_stamp, daily_return from RETURN_SERIES_ALL 
									where symbol ='%s' and time_stamp >= '%s' and time_stamp <= '%s'", sym, startDate, endDate))
    
    ret <- NA
    if(nrow(pxDf) >= expectedLength-2){
      ret <- as.numeric(Return.cumulative(xts(pxDf[,2], pxDf[,1])))
    } 
    returns <- c(returns, ret)
  }
  
  return(returns)
}

symRets <- data.frame(PERIOD_END="", STRATEGY="", RET=0.0)

for(ii in 2:nrow(rebalDates)){
  startDate <- rebalDates$ST[ii-1]
  endDate <- rebalDates$ST[ii]
  symbols <- sqlQuery(lcon, sprintf("select top 200 symbol, ff_mkt_cap_cr from EQUITY_MISC_INFO where time_stamp='%s' order by ff_mkt_cap_cr desc", startDate))[,1]
  
  formationDate <- first(index(tail(indexXts[paste0("/", startDate)], lookback)))
  volDf <- data.frame(SYMBOL="", VOLUME=0, VOLUME_DECILE=0)
  for(sym in symbols){
    pxDf1 <- sqlQuery(lcon, sprintf("select time_stamp, TOT_TRD_VAL from px_history 
                                    where symbol = '%s' and time_stamp >= '%s' and time_stamp <= '%s' order by time_stamp desc",
                                    sym, formationDate, startDate))

    if(nrow(pxDf1) < lookback) next
    pxDf1$decile <- ntile(pxDf1[,2], n=10)
    
    volDf <- rbind(volDf, c(sym, as.numeric(pxDf1$TOT_TRD_VAL[1]), as.numeric(pxDf1$decile[1])))
  }
  volDf <- volDf[-1,]
  
  volDf <- volDf %>% filter(VOLUME_DECILE == 10)
  volDf$RET <- getReturns(volDf$SYMBOL, formationDate, startDate, lookback)
  
  highVolSyms <- volDf %>% filter(RET > 0) %>% slice_max(VOLUME, n=20) %>% select(SYMBOL)
  if(nrow(highVolSyms) > 0){
    highVolSyms <- highVolSyms[,1]
    
    subLength <- nrow(indexXts[paste0(startDate + 1, "/", endDate)])
    highVolRets <- getReturns(highVolSyms, startDate + 1, endDate, subLength)
    highVolRets <- na.omit(highVolRets)
    highVolRet <- sum(highVolRets) / length(highVolRets)
    
    symRets <- rbind(symRets, c(toString(endDate), "HIGH_VOL", highVolRet))
  } else {
    symRets <- rbind(symRets, c(toString(endDate), "HIGH_VOL", 0.0))
  }
}

symRets <- symRets[-1,]

for(ii in 2:nrow(rebalDates)){
  startDate <- rebalDates$ST[ii-1]
  endDate <- rebalDates$ST[ii]
  benchRet <- as.numeric(Return.cumulative(dailyReturn(indexXts[paste0(startDate, "/", endDate)])))
  symRets <- rbind(symRets, c(toString(endDate), "BENCH", benchRet))
}

symRets$PERIOD_END <- as.Date(symRets$PERIOD_END)
symRets$RET <- as.numeric(symRets$RET)

save(symRets, file=sprintf("%s/symRets.Rdata", reportPath))



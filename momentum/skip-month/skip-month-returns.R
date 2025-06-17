#compare returns of stocks that are common to 
#both zero and 1-month skips

library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics')	
library('ggthemes')
library('viridis')

library('tidyverse')
library('lubridate')

options("scipen"=100)
options(stringsAsFactors = FALSE)
reportPath <- "."

source("/mnt/hollandC/StockViz/R/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host='sweden', user=ldbuser2, password=ldbpassword2, dbname='StockVizDyn', sslmode='allow')

indexName <- "NIFTY 200 TR"
lookback <- 500 #days

miscDates <- sqlQuery(lcon, "select distinct time_stamp from EQUITY_MISC_INFO")

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, min(miscDates$time_stamp) - lookback, max(miscDates$time_stamp)))
indexXts <- xts(indexPx[,2], indexPx[,1])

rebalDates <- miscDates %>% mutate(YM = year(time_stamp)*100 + month(time_stamp)) %>%
  group_by(YM) %>%
  summarize(ST=min(time_stamp)) %>%
  select(ST) %>%
  ungroup() %>%
  as.data.frame()

getReturns <- function(syms, startDate, endDate, expectedTs){
  symCount <- 0
  symRetDf <- data.frame(SYMBOL = "", RET = 0.0)
  for(i in 1:length(syms)){
    sym <- syms[i]
    pxDf <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
									where ticker = $1 and date_stamp >= $2 and date_stamp <= $3 order by date_stamp", params=list(sym, startDate, endDate))
    
    if(nrow(pxDf) < expectedTs-2) next
    
    symCount <- symCount + 1
    ret <- as.numeric(Return.cumulative(dailyReturn(xts(pxDf[,2], pxDf[,1]))))
    
    symRetDf <- rbind(symRetDf, c(sym, ret))
    if (symCount >= 20) break
  }
  
  symRetDf <- symRetDf[-1,]
  return(symRetDf)
}

symRets <- data.frame(PERIOD_END="", 
                      RET_COMMON=0.0, RET_NOSKIP=0.0, RET_SKIP=0.0, 
                      NUM_COMMON=0, NUM_NOSKIP=0, NUM_SKIP=0)

for(ii in 3:nrow(rebalDates)){
  startDate0 <- rebalDates$ST[ii - 1]
  endDate <- rebalDates$ST[ii]

  #no skip  
  symbols0 <- sqlQuery(lcon, sprintf("select top 200 symbol, ff_mkt_cap_cr from EQUITY_MISC_INFO where time_stamp='%s' order by ff_mkt_cap_cr desc", startDate0))[,1]
  
  retDf0 <- data.frame(SYMBOL="", RET=0.0)
  for(sym in symbols0){
    pxDf1 <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
									where ticker = $1 and date_stamp >= $2 and date_stamp < $3 order by date_stamp", params=list(sym, startDate0-365, startDate0))
    
    if(nrow(pxDf1) < 200) next
    
    retDf0 <- rbind(retDf0, c(sym, as.numeric(Return.cumulative(dailyReturn(xts(pxDf1[,2], pxDf1[,1]))))))
  }
  retDf0 <- retDf0[-1,]
  highRet0 <- head(retDf0[order(retDf0$RET, decreasing=T),], 50)
  
  #1-mo skip
  startDate1 <- rebalDates$ST[ii - 2]
  symbols1 <- sqlQuery(lcon, sprintf("select top 200 symbol, ff_mkt_cap_cr from EQUITY_MISC_INFO where time_stamp='%s' order by ff_mkt_cap_cr desc", startDate1))[,1]
  
  retDf1 <- data.frame(SYMBOL="", RET=0.0)
  for(sym in symbols1){
    pxDf1 <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse 
									where ticker = $1 and date_stamp >= $2 and date_stamp < $3 order by date_stamp", params=list(sym, startDate1-365, startDate1))
    
    if(nrow(pxDf1) < 200) next
    
    retDf1 <- rbind(retDf1, c(sym, as.numeric(Return.cumulative(dailyReturn(xts(pxDf1[,2], pxDf1[,1]))))))
  }
  retDf1 <- retDf1[-1,]
  highRet1 <- head(retDf1[order(retDf1$RET, decreasing=T),], 50)
  
  #compare returns of intersecting vs. those that are there only in no-skip
  
  expectedTs <- nrow(indexXts[paste0(startDate0, "/", endDate)])

  ret0Df <- getReturns(highRet0$SYMBOL, startDate0, endDate, expectedTs)
  ret1Df <- getReturns(highRet1$SYMBOL, startDate0, endDate, expectedTs)
  
  noskipSyms <- setdiff(ret0Df$SYMBOL, ret1Df$SYMBOL)
  skipSyms <- setdiff(ret1Df$SYMBOL, ret0Df$SYMBOL)
  commonSyms <- intersect(ret0Df$SYMBOL, ret1Df$SYMBOL)
  
  commonRet <- (ret0Df |> filter(SYMBOL %in% commonSyms) |> summarise(RET_AVG = mean(as.numeric(RET))))[[1]]
  noskipRet <- (ret0Df |> filter(SYMBOL %in% noskipSyms) |> summarise(RET_AVG = mean(as.numeric(RET))))[[1]]
  skipRet <- (ret1Df |> filter(SYMBOL %in% skipSyms) |> summarise(RET_AVG = mean(as.numeric(RET))))[[1]]

  symRets <- rbind(symRets, c(toString(endDate), 
                              commonRet,
                              noskipRet,
                              skipRet,
                             length(commonSyms),
                             length(noskipSyms),
                             length(skipSyms)))
}

symRets <- symRets[-1,]
symRets$PERIOD_END <- as.Date(symRets$PERIOD_END)
symRets$RET_COMMON <- as.numeric(symRets$RET_COMMON) #positions common to both
symRets$RET_NOSKIP <- as.numeric(symRets$RET_NOSKIP) #positions only in 12_0
symRets$RET_SKIP <- as.numeric(symRets$RET_SKIP) #positions only in 12_1

symRets$NUM_COMMON <- as.numeric(symRets$NUM_COMMON)
symRets$NUM_NOSKIP <- as.numeric(symRets$NUM_NOSKIP)
symRets$NUM_SKIP <- as.numeric(symRets$NUM_SKIP)

summary(symRets$RET_COMMON)
summary(symRets$RET_NOSKIP)
summary(symRets$RET_SKIP)

symRets |> select(RET_COMMON, RET_NOSKIP, RET_SKIP) |> pivot_longer(c(RET_COMMON, RET_NOSKIP, RET_SKIP)) |>
  ggplot(aes(x=value*100, color=name)) +
    theme_economist() +
    stat_density(geom = "line", position = "jitter") +
    scale_color_viridis_d() +
    labs(x = 'returns (%)', y = 'density', color='',
         title = 'Distribution of Returns',
         subtitle = sprintf('20 stocks shortlisted for 12_1_1 and 12_0_1 momentum [%s:%s]', min(symRets$PERIOD_END), max(symRets$PERIOD_END)))

ggsave(sprintf("%s/noskip-returns-distribution.png", reportPath), width = 12, height = 6, units = "in")
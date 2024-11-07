#How to Improve Commodity Momentum Using Intra-Market Correlation
#https://ssrn.com/abstract=4964417

library('RODBC')
library('quantmod')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('gmp')

options("scipen" = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/stockviz/blog/common/plot.common.r")

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

rcon <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    dbserver,
    "StockViz",
    dbuser,
    dbpassword
  ),
  case = "nochange",
  believeNRows = TRUE
)

lb1 <- 20
lb2 <- 200

userId <- "30B18A87-D803-4476-8469-858454C2C49A"
modelId <- "D6663194-A965-4262-84F6-BB553B0B8998"
rebalSeqs <- sqlQuery(rcon, sprintf("select seq_id/100000, seq_id%%100000, count(symbol) cnt from advisor_model_portfolio where model_id='%s' and symbol not in ('CASH', 'LIQUIDBEES') group by seq_id order by seq_id", modelId))

rebalSeqs$SEQ_ID <- as.bigq(rebalSeqs[,1])*100000 + as.bigq(rebalSeqs[,2])
startSeq <- rebalSeqs[rebalSeqs$cnt == 20,]$SEQ_ID[1]
rebalSeqs <- rebalSeqs[rebalSeqs$SEQ_ID >= startSeq,]

getAvgCorrelation <- function(symbols, startDate, lb){
  retXts <- NULL
  for(sym in symbols){
    rets <- sqlQuery(lcon, sprintf("select top %d time_stamp, daily_return from return_series_all where symbol='%s' and time_stamp <= '%s' order by time_stamp desc", lb, sym, startDate))
    if(!is.data.frame(rets) || nrow(rets) == 0) next
    tXts <- xts(rets[,2], rets[,1])
    names(tXts) <- c(sym)
    retXts <- merge.xts(retXts, tXts)
  }
  corMat <- cor(retXts, method="kendall")
  meanCor <- mean(corMat[upper.tri(corMat)])
  return(meanCor)
}

avgCorDf <- data.frame(T="", ST_COR=0.0, LT_COR=0.0)
for(i in 2:nrow(rebalSeqs)){
  rseq <- rebalSeqs$SEQ_ID[i-1]
  nextRseq <- rebalSeqs$SEQ_ID[i]
  portfolio <- sqlQuery(rcon, sprintf("select * from advisor_model_portfolio where model_id='%s' and seq_id = %s and symbol not in ('CASH', 'LIQUIDBEES')", modelId, rseq))
  
  curRebalDate <- portfolio$TIME_STAMP[1]
  nextRebalDate <- sqlQuery(rcon, sprintf("select top 1 time_stamp from advisor_model_portfolio where model_id='%s' and seq_id = %s and symbol not in ('CASH', 'LIQUIDBEES')", modelId, nextRseq))[[1]]
  
  curRebalDate <- as.Date(curRebalDate)
  nextRebalDate <- as.Date(nextRebalDate)
  
  if (nextRebalDate == curRebalDate) next
  
  portDates <- sqlQuery(lcon, sprintf("select time_stamp from bhav_index where index_name = 'nifty 50' and time_stamp > '%s' and time_stamp < '%s' order by time_stamp", curRebalDate, nextRebalDate))
  if(!is.data.frame(portDates) || nrow(portDates) == 0) next
  portDates <- portDates[,1]
  
  for(j in 1:length(portDates)){
    stCor <- getAvgCorrelation(portfolio$SYMBOL, portDates[j], lb1)
    ltCor <- getAvgCorrelation(portfolio$SYMBOL, portDates[j], lb2)
    
    avgCorDf <- rbind(avgCorDf, c(toString(portDates[j]), stCor, ltCor))
  }
}

avgCorDf <- avgCorDf[-1,]
avgCorXts <- xts(avgCorDf[,-1], as.Date(avgCorDf[,1]))

portVal <- sqlQuery(rcon, sprintf("select time_stamp, cash_value, equity_value from ADVISOR_PORTFOLIO_VALUE
                                  where model_id = '%s' and user_id='%s'", modelId, userId))

portXts <- xts(portVal[,-1], as.Date(portVal[,1]))
portXts[,1] <- na.fill(portXts[,1], 0)
portXts[,2] <- na.fill(portXts[,2], 0)

allXts <- merge(avgCorXts, portXts[,1]+portXts[,2])
names(allXts) <- c(names(avgCorXts), 'BH')

allXts[,1] <- na.locf(allXts[,1])
allXts[,2] <- na.locf(allXts[,2])

allXts <- na.omit(allXts)

allXts$BH_RET_0 <- dailyReturn(allXts$BH)
allXts$BH_RET <- stats::lag(allXts$BH_RET_0, -1)

allXts$COR_RET <- ifelse(allXts$ST_COR > allXts$LT_COR, allXts$BH_RET, 0)

scenXts <- allXts[, c('COR_RET', 'BH_RET')]

sr <- SharpeRatio.annualized(scenXts["/2019",])
Common.PlotCumReturns(scenXts["/2019",], 
                      "Strategy Returns", 
                      sprintf("sr: %s", paste(round(sr, 2), collapse="/")), 
                      sprintf("%s/momentum.correlation.pre.png", reportPath))

sr <- SharpeRatio.annualized(scenXts["2020-05-01/",])
Common.PlotCumReturns(scenXts["2020-05-01/",], 
                      "Strategy Returns", 
                      sprintf("sr: %s", paste(round(sr, 2), collapse="/")), 
                      sprintf("%s/momentum.correlation.post.png", reportPath))

sr <- SharpeRatio.annualized(scenXts)
Common.PlotCumReturns(scenXts, 
                      "Strategy Returns", 
                      sprintf("sr: %s", paste(round(sr, 2), collapse="/")), 
                      sprintf("%s/momentum.correlation.png", reportPath))

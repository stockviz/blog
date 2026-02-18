library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('PortfolioAnalytics') 
library('tidyverse')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

reportPath <- "."

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

drag <- 0.25/100

sectorIndexNames <- c("NIFTY BANK TR", "NIFTY AUTO TR", "NIFTY COMMODITIES TR", "NIFTY CONSUMER DURABLES TR", 
                      "NIFTY ENERGY TR", "NIFTY FMCG TR", "NIFTY INDIA CONSUMPTION TR", "NIFTY INFRASTRUCTURE TR", "NIFTY IT TR",
                      "NIFTY MEDIA TR", "NIFTY METAL TR", "NIFTY PHARMA TR", "NIFTY PRIVATE BANK TR", "NIFTY PSU BANK TR", "NIFTY REALTY TR", "NIFTY SERVICES SECTOR TR")

xtsColNames <- make.names(sectorIndexNames)

mktCapIndexName <- "NIFTY 100 TR"

minMax <- sqlQuery(lcon, sprintf("select index_name, min(time_stamp) s, max(time_stamp) m from bhav_index where index_name in ('%s') group by index_name", paste(sectorIndexNames, collapse="','")))

startDate <- max(minMax$s)

sectorIndexPrices <- NULL
sectorIndexDailyRets <- NULL
sectorIndexMonthlyRets <- NULL
for(si in sectorIndexNames){
  pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s'", si, startDate))
  pXts <- xts(pDf[,2], pDf[,1])
  sectorIndexPrices <- merge.xts(sectorIndexPrices, pXts)
  sectorIndexDailyRets <- merge.xts(sectorIndexDailyRets, dailyReturn(pXts))
  sectorIndexMonthlyRets <- merge.xts(sectorIndexMonthlyRets, monthlyReturn(pXts))  
}

names(sectorIndexPrices) <- xtsColNames
names(sectorIndexDailyRets) <- xtsColNames
names(sectorIndexMonthlyRets) <- xtsColNames

nextMonthReturns <- stats::lag(sectorIndexMonthlyRets, -1)

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s'", mktCapIndexName, startDate))
mktCapPriceXts <- xts(pDf[,2], pDf[,1])
mktCapDailyRet <- dailyReturn(mktCapPriceXts)
mktCapMonthlyRet <- monthlyReturn(mktCapPriceXts)

names(mktCapPriceXts) <- c(mktCapIndexName)
names(mktCapDailyRet) <- c(mktCapIndexName)
names(mktCapMonthlyRet) <- c(mktCapIndexName)

#########################################################

#equal weight, no tx cost

eqWtGross <- xts(rowMeans(sectorIndexMonthlyRets), index(sectorIndexMonthlyRets))
names(eqWtGross) <- c("EQ_WT_GROSS")

toPlot <- merge(eqWtGross, mktCapMonthlyRet)
sr <- paste(round(SharpeRatio.annualized(toPlot), 2), collapse="/")
Common.PlotCumReturns(toPlot, "Sector Equal Weight", sprintf("sr: %s", sr),
                      sprintf("%s/eq-wt.gross.png", reportPath))



#########################################################

#inverse volatility, no tx cost

pspec <- portfolio.spec(assets = colnames(sectorIndexMonthlyRets))

pspec <- add.constraint(portfolio = pspec, type = "box", min = 0, max = 0.1)
pspec <- add.constraint(portfolio = pspec, type = "weight_sum", min_sum=0.99, max_sum=1.01)
pspec <- add.constraint(portfolio = pspec, type = "long_only")
pspec <- add.constraint(portfolio = pspec, type = "transaction_cost", ptc = drag)

#x <- head(sectorIndexMonthlyRets, 3)
weightsXts <- rollapply(sectorIndexMonthlyRets, 6, \(x){
  wts <- inverse.volatility.weight(R = x, portfolio = pspec)$weights
  asof <- last(index(x))
  ret <- tibble(wts)
  ret$index_name <- names(wts)
  xts(ret |> pivot_wider(names_from = index_name, values_from=wts), asof)
}, by.column = FALSE )


inverseVolRet <- NULL
for(i in 1:length(xtsColNames)){
  inverseVolRet <- cbind.xts(inverseVolRet, weightsXts[, xtsColNames[i]] * nextMonthReturns[, xtsColNames[i]])
}

inverseVolPortRet <- xts(rowSums(inverseVolRet, na.rm = TRUE), index(inverseVolRet))
names(inverseVolPortRet) <- c("INV_VOL_WT_GROSS")

toPlot <- merge(stats::lag(inverseVolPortRet, 1), mktCapMonthlyRet)
sr <- paste(round(SharpeRatio.annualized(toPlot), 2), collapse="/")
Common.PlotCumReturns(toPlot, "Sector Inverse-Volatility Weight", sprintf("sr: %s", sr),
                      sprintf("%s/inv-vol-wt.gross.png", reportPath))

#########################################################

#VAR, no tx cost

# pspec <- portfolio.spec(assets = colnames(sectorIndexMonthlyRets))
# 
# pspec <- add.constraint(portfolio = pspec, type = "box", min = 0, max = 0.1)
# pspec <- add.constraint(portfolio = pspec, type = "weight_sum", min_sum=0.99, max_sum=1.01)
# pspec <- add.constraint(portfolio = pspec, type = "long_only")
# pspec <- add.constraint(portfolio = pspec, type = "transaction_cost", ptc = drag)
# 
# minVar <- add.objective(portfolio=pspec, type="risk", name="ETL")
# 
# weightsXts <- rollapply(sectorIndexMonthlyRets, 6, \(x){
#   wts <- optimize.portfolio(R = x, portfolio = minVar, optimize_method = "random")$weights
#   asof <- last(index(x))
#   ret <- tibble(wts)
#   ret$index_name <- names(wts)
#   xts(ret |> pivot_wider(names_from = index_name, values_from=wts), asof)
# }, by.column = FALSE )
# 
# minVolRet <- NULL
# for(i in 1:length(xtsColNames)){
#   minVolRet <- cbind.xts(minVolRet, weightsXts[, xtsColNames[i]] * nextMonthReturns[, xtsColNames[i]])
# }
# 
# minVolPortRet <- xts(rowSums(minVolRet, na.rm = TRUE), index(minVolRet))
# names(minVolPortRet) <- c("MIN_VOL_WT_GROSS")
# 
# toPlot <- merge(stats::lag(minVolPortRet, 1), mktCapMonthlyRet)
# sr <- paste(round(SharpeRatio.annualized(toPlot), 2), collapse="/")
# Common.PlotCumReturns(toPlot, "Sector Min-Volatility Weight", sprintf("sr: %s", sr),
#                       sprintf("%s/min-vol-wt.gross.png", reportPath))

#########################################################

#momentum, no tx cost

trailing3Rets <- do.call(merge.xts, lapply(sectorIndexMonthlyRets, \(x) rollapply(x, 3, Return.cumulative)))
mom3Ret <- rollapply(trailing3Rets, 1, \(x) nextMonthReturns[index(x), max.col(x)], by.column = FALSE)
names(mom3Ret) <- c('MOM_3_GROSS')

trailing6Rets <- do.call(merge.xts, lapply(sectorIndexMonthlyRets, \(x) rollapply(x, 6, Return.cumulative)))
mom6Ret <- rollapply(trailing6Rets, 1, \(x) nextMonthReturns[index(x), max.col(x)], by.column = FALSE)
names(mom6Ret) <- c('MOM_6_GROSS')

trailing12Rets <- do.call(merge.xts, lapply(sectorIndexMonthlyRets, \(x) rollapply(x, 12, Return.cumulative)))
mom12Ret <- rollapply(trailing12Rets, 1, \(x) nextMonthReturns[index(x), max.col(x)], by.column = FALSE)
names(mom12Ret) <- c('MOM_12_GROSS')

toPlot <- na.omit(merge(stats::lag(mom3Ret, 1), stats::lag(mom6Ret, 1), stats::lag(mom12Ret, 1), mktCapMonthlyRet))
sr <- paste(round(SharpeRatio.annualized(toPlot), 2), collapse="/")
Common.PlotCumReturns(toPlot, "Sector Momentum", sprintf("sr: %s", sr),
                      sprintf("%s/momentum.gross.png", reportPath))


mom6Index <- rollapply(trailing6Rets, 1, \(x) max.col(x, ties.method = 'first'), by.column = FALSE)
mom6IndexChg <- mom6Index - stats::lag(mom6Index, 1)
mom6IndexChg <- ifelse(mom6IndexChg == 0, 0, 1)
mom6RetNet <- mom6Ret - drag * mom6IndexChg
names(mom6RetNet) <- c('MOM_6_NET')

toPlot <- na.omit(merge(stats::lag(mom6RetNet), stats::lag(mom6Ret, 1), mktCapMonthlyRet))
sr <- paste(round(SharpeRatio.annualized(toPlot), 2), collapse="/")
Common.PlotCumReturns(toPlot, "6-month Sector Momentum", sprintf("sr: %s", sr),
                      sprintf("%s/momentum-6mo.png", reportPath))


#########################################################

#SMA, no tx cost

lb <- 20
siPx <- na.locf(sectorIndexPrices)
SMAPx <- do.call(merge.xts, lapply(siPx, SMA, n=lb))
weightsXts <- do.call(merge.xts, lapply(1:ncol(siPx), \(i) ifelse(siPx[,i] > SMAPx[,i], 1, NA)))

smaRet <- NULL
for(i in 1:length(xtsColNames)){
  smaRet <- cbind.xts(smaRet, weightsXts[, xtsColNames[i]] * nextMonthReturns[, xtsColNames[i]])
}

smaPortRet <- xts(rowMeans(smaRet, na.rm = TRUE), index(smaRet))
names(smaPortRet) <- c(sprintf("SMA%d_GROSS", lb))

toPlot <- merge(stats::lag(smaPortRet, 1), mktCapMonthlyRet)
sr <- paste(round(SharpeRatio.annualized(toPlot), 2), collapse="/")
Common.PlotCumReturns(toPlot, sprintf("Sector Equal-Weight %d-day SMA", lb), sprintf("sr: %s", sr),
                      sprintf("%s/sma-%d.gross.png", reportPath, lb))


ivWeightsTb <- tibble()
for(i in 6:nrow(sectorIndexMonthlyRets)){
  x <- sectorIndexMonthlyRets[(i - 6):i]
  asof <- last(index(x))
  colSub <- xtsColNames[weightsXts[asof,] == 1]
  colSub <- colSub[!is.na(colSub)]
  if(length(colSub) == 0){
    next
  }
  if(length(colSub) == 1){
    ivWeightsTb <- rbind(ivWeightsTb, c(1.0, colSub[1], as.character(asof)))
    next
  }
  
  pspec <- portfolio.spec(assets = colSub)
  
  pspec <- add.constraint(portfolio = pspec, type = "box", min = 0, max = 0.1)
  pspec <- add.constraint(portfolio = pspec, type = "weight_sum", min_sum=0.99, max_sum=1.01)
  pspec <- add.constraint(portfolio = pspec, type = "long_only")
  pspec <- add.constraint(portfolio = pspec, type = "transaction_cost", ptc = drag)
  
  wts <- inverse.volatility.weight(R = x, portfolio = pspec)$weights
  
  ret <- tibble(wts)
  ret$index_name <- names(wts)
  ret$asof <- asof
  
  ivWeightsTb <- rbind(ivWeightsTb, ret)
}

ivWeightsXts <- ivWeightsTb |> 
  pivot_wider(id_cols = asof, names_from = index_name, values_from = wts) |>
  as.xts()

inverseVolRet <- NULL
for(i in 1:length(xtsColNames)){
  inverseVolRet <- cbind.xts(inverseVolRet, ivWeightsXts[, xtsColNames[i]] * nextMonthReturns[, xtsColNames[i]])
}

inverseVolPortRet <- xts(rowSums(inverseVolRet, na.rm = TRUE), index(inverseVolRet))
names(inverseVolPortRet) <- c(sprintf("SMA_%d_INV_VOL_WT_GROSS", lb))

toPlot <- na.trim(merge(stats::lag(inverseVolPortRet, 1), mktCapMonthlyRet), sides='left')

sr <- paste(round(SharpeRatio.annualized(toPlot), 2), collapse="/")
Common.PlotCumReturns(toPlot, sprintf("Sector Inverse-Volatility Weight %d-day SMA", lb), sprintf("sr: %s", sr),
                      sprintf("%s/sma-%d.inv-vol-wt.gross.png", reportPath, lb))

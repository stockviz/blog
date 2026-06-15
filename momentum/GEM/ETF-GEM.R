library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggrepel')
library('gtExtras')
library('webshot2')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

print("connecting to norway...")
lconUs2 <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    ldbserver,
    "StockVizUs2",
    ldbuser,
    ldbpassword
  ),
  case = "nochange",
  believeNRows = TRUE
)

startDate <- as.Date("2018-01-01")
fwdDate <- as.Date("2019-02-01")
drag <- 0.05/100

pDf <- sqlQuery(lconUs2, sprintf("select c, time_stamp from TIINGO_DATA where ticker='SPY' and time_stamp >= '%s'",
                                 startDate))

spyXts <- xts(pDf$c, pDf$time_stamp)

pDf <- sqlQuery(lconUs2, sprintf("select c, time_stamp from TIINGO_DATA where ticker='SCHF' and time_stamp >= '%s'",
                                 startDate))

schfXts <- xts(pDf$c, pDf$time_stamp)


pDf <- sqlQuery(lconUs2, sprintf("select c, time_stamp from TIINGO_DATA where ticker='AGG' and time_stamp >= '%s'",
                                 startDate))

aggXts <- xts(pDf$c, pDf$time_stamp)


pDf <- sqlQuery(lconUs2, sprintf("select c, time_stamp from TIINGO_DATA where ticker='SHV' and time_stamp >= '%s'",
                                 startDate))

shvXts <- xts(pDf$c, pDf$time_stamp)

pDf <- sqlQuery(lconUs2, sprintf("select c, time_stamp from TIINGO_DATA where ticker='MTUM' and time_stamp >= '%s'",
                                 startDate))

mtumXts <- xts(pDf$c, pDf$time_stamp)


pDf <- sqlQuery(lconUs2, sprintf("select c, time_stamp from TIINGO_DATA where ticker='IMTM' and time_stamp >= '%s'",
                                 startDate))

imtmXts <- xts(pDf$c, pDf$time_stamp)

mRets <- merge(monthlyReturn(spyXts),
               monthlyReturn(schfXts),
               monthlyReturn(aggXts),
               monthlyReturn(shvXts),
               monthlyReturn(mtumXts),
               monthlyReturn(imtmXts))
names(mRets) <- c("SPY", "SCHF", "AGG", "SHV", "MTUM", "IMTM")

m12Rets <- merge(rollapply(mRets[,"SPY"], 12, Return.cumulative),
                 rollapply(mRets[,"SHV"], 12, Return.cumulative),
                 rollapply(mRets[,"SCHF"], 12, Return.cumulative))
names(m12Rets) <- c("SPY12", "SHV12", "SCHF12")
allRets <- merge(m12Rets, stats::lag(mRets, -1))
allRets <- na.omit(allRets)

#long basic ETFs
grossRet <- ifelse(allRets[,"SPY12"] > allRets [, "SHV12"], 
                   ifelse(allRets[, "SPY12"] > allRets[, "SCHF12"], 
                          allRets[, "SPY"], allRets[, "SCHF"]), 
                   allRets[, "AGG"])


activeEtf <- ifelse(allRets[,"SPY12"] > allRets [, "SHV12"], 
                   ifelse(allRets[, "SPY12"] > allRets[, "SCHF12"], 
                          "SPY", "SCHF"), 
                   "AGG")

trd <- ifelse(activeEtf == stats::lag(activeEtf, 1), 0, 1)

basicRet <- ifelse(activeEtf != 0, grossRet - drag, grossRet)
names(basicRet) <- c("MKT_CAP")

#long mom ETFs
grossRet <- ifelse(allRets[,"SPY12"] > allRets [, "SHV12"], 
                   ifelse(allRets[, "SPY12"] > allRets[, "SCHF12"], 
                          allRets[, "MTUM"], allRets[, "IMTM"]), 
                   allRets[, "AGG"])


activeEtf <- ifelse(allRets[,"SPY12"] > allRets [, "SHV12"], 
                    ifelse(allRets[, "SPY12"] > allRets[, "SCHF12"], 
                           "MTUM", "IMTM"), 
                    "AGG")

trd <- ifelse(activeEtf == stats::lag(activeEtf, 1), 0, 1)

momRet <- ifelse(activeEtf != 0, grossRet - drag, grossRet)
names(momRet) <- c("MOM")

#### data plot
toPlot <- mRets[paste0(fwdDate, "/"), ]
sharpe <- SharpeRatio.annualized(toPlot)
print(sharpe)

Common.PlotCumReturns(toPlot, "ETF Returns", 
                      sprintf("SR: %s", paste(round(sharpe,2), collapse="/")), #NULL)
sprintf("%s/etf-returns.png", reportPath), NULL)

#### gem plot
toPlot <- na.omit(merge(basicRet, momRet, mRets[, !colnames(mRets) %in% c("AGG", "SHV")]))
toPlot <- toPlot[paste0(fwdDate, "/"), ]
sharpe <- SharpeRatio.annualized(toPlot)
print(sharpe)

Common.PlotCumReturns(toPlot, "GEM Mkt.Cap vs. Momentum ETFs", 
                      sprintf("SR: %s", paste(round(sharpe,2), collapse="/")), #NULL)
                      sprintf("%s/etf-GEM-returns.png", reportPath), NULL)

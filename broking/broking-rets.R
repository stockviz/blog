library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('viridis')
library('patchwork')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

print("connecting to norway...")
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

pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)

brokingTickers <- sqlQuery(lcon, "select sc_id from equity_ticker_bse where industry like '%broking%' and sc_group in ('A', 'B')")[,1]

brokingMeta <- dbGetQuery(pgCon, 
                          sprintf("select ticker, min(date_stamp) st, max(date_stamp) et, count(*) cnt 
                              from eod_adjusted_nse 
                              where ticker in ('%s')
                              group by ticker", 
                            paste(brokingTickers, collapse="','")))

medianCnt <- (brokingMeta |> filter(et == max(et)) |> summarise(MCNT = median(cnt)))[[1]]

brokingUni <- brokingMeta |> filter(et == max(et) & cnt >= medianCnt)

startDate <- max(brokingUni$st)

brokingRets <- NULL
for(sym in brokingUni$ticker){
  pxDf <- dbGetQuery(pgCon, "select c, date_stamp from eod_adjusted_nse where ticker=$1 and date_stamp > $2",
             params=list(sym, startDate))
  
  pXts <- xts(pxDf[,1], pxDf[,2])
  
  brokingRets <- merge.xts(brokingRets, dailyReturn(pXts))
}

names(brokingRets) <- brokingUni$ticker
brokingRets <- brokingRets[-1,]

pxDf <- dbGetQuery(pgCon, "select c, date_stamp from eod_adjusted_nse where ticker=$1 and date_stamp > $2",
                   params=list('BSE', startDate))

bseRet <- dailyReturn(xts(pxDf[,1], pxDf[,2]))
names(bseRet) <- c('BSE')
bseRet <- bseRet[-1,]

iName <- 'NIFTY FINANCIAL SERVICES EX-BANK TR'
inDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index 
                               where index_name='%s'
                               and time_stamp > '%s'", iName, startDate))

inRet <- dailyReturn(xts(inDf[,1], inDf[,2]))

names(inRet) <- iName
inRet <- inRet[-1,]

avgRet <- xts(rowMeans(brokingRets), index(brokingRets))
names(avgRet) <- c('BROKING')

toPlot <- merge(avgRet, inRet, bseRet)

Common.PlotCumReturns(toPlot["2024-01-01/",],
                      "Broking firms",
                      "", #NULL)
                      sprintf("%s/broking-all.png", reportPath))

Common.PlotCumReturns(toPlot["2024-01-01/",],
                      "Broking firms",
                      "", #NULL)
                      sprintf("%s/broking-2024.png", reportPath))

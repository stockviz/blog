library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggrepel')
library('gtExtras')
library('webshot2')
library('jsonlite')

options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

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

print("connecting to sweden...")
pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)

startDt <- as.Date("2020-05-01")
endDt <- Sys.Date() -1
indexName <- 'NIFTY NEXT 50'
indexConst <- sqlQuery(lcon, sprintf("select symbol from INDEX_CONST_HISTORY 
                       where index_name='%s' 
                       and time_stamp = (select max(time_stamp) from INDEX_CONST_HISTORY 
                       where index_name='%s')", indexName, indexName))[,1]

resDf <- data.frame(SYMBOL = "", DD_START = "", DD = 0.0, IS_ONGOING = FALSE, PREV_DD = 0.0, RET = 0.0, PE_START = 0.0, PE_LATEST = 0.0, MKT_CAP_START = 0.0, MKT_CAP_LATEST = 0.0)
for(sym in indexConst){
  print(sym)
  pxDf <- dbGetQuery(pgCon, "select date_stamp, c from eod_adjusted_nse where ticker=$1 and date_stamp >= '2014-01-01'", params = list(sym))
  if(nrow(pxDf) == 0){
    print("ignoring")
    next
  }
  pXts <- xts(pxDf[,-1], pxDf[,1])
  dXts <- dailyReturn(pXts)
  allDd <- table.Drawdowns(dXts[paste0(startDt, '/')])
  recentPeakDate <- max(allDd[,1])
  ddFromPeakDate <- allDd[allDd[,1] == recentPeakDate, 4]
  isOngoing <- is.na(allDd[allDd[,1] == recentPeakDate, 3])
  
  ret <- as.numeric(Return.cumulative(dXts[paste0(startDt, '/')]))
  
  preDdFromPeak <- 0.0
  if(year(first(index(dXts))) <= 2016){
    preCovDd <- table.Drawdowns(dXts["/2019"])
    preDdFromPeak <- preCovDd[1,4]
  }
    
  frCurrDf <- sqlQuery(lcon, sprintf("select as_of, ratios from funda_ratios 
                       where src='NSE' 
                       and symbol='%s' 
                       and as_of = (select max(as_of) from funda_ratios where symbol = '%s')",
                       sym, sym))
  
  currentPE <- 0.0
  if(nrow(frCurrDf) > 0 && as.numeric(Sys.Date() - frCurrDf$as_of[1]) < 10){
    currentPE <- fromJSON(frCurrDf$ratios[1])$PE
    if(is.null(currentPE) || !is.finite(currentPE)){
      currentPE <- 0.0
    }
  }
  
  frDdDf <- sqlQuery(lcon, sprintf("select ratios from funda_ratios 
                       where src='NSE' 
                       and symbol='%s' 
                       and as_of = '%s'",
                       sym, recentPeakDate))
  
  if(nrow(frDdDf) == 0){
    frDdDf <- sqlQuery(lcon, sprintf("select top 1 as_of, ratios from funda_ratios 
                       where src='NSE' 
                       and symbol='%s' 
                       and as_of <= '%s'
                       and as_of >= '%s'
                       order by as_of desc",
                       sym, recentPeakDate, recentPeakDate - 20))
  }
  ddPE <- 0.0
  if(nrow(frDdDf) > 0){
    ddPE <- fromJSON(frDdDf$ratios[1])$PE
    if(is.null(ddPE) || !is.finite(ddPE)){
      ddPE <- 0.0
    }
  }
  
  
  miscCurrDf <- sqlQuery(lcon, sprintf("select time_stamp, MKT_CAP_CR from equity_misc_info
                       where symbol='%s' 
                       and time_stamp = (select max(time_stamp) from equity_misc_info where symbol = '%s')",
                                     sym, sym))
  
  currentMktCap <- 0.0
  if(nrow(miscCurrDf) > 0 && as.numeric(Sys.Date() - miscCurrDf$time_stamp[1]) < 10){
    currentMktCap <- miscCurrDf$MKT_CAP_CR[1]
    if(is.null(currentMktCap) || !is.finite(currentMktCap)){
      currentMktCap <- 0.0
    }
  }
  
  miscDdDf <- sqlQuery(lcon, sprintf("select time_stamp, MKT_CAP_CR from equity_misc_info
                       where symbol='%s' 
                       and time_stamp = '%s'
                       and MKT_CAP_CR > 0", sym, recentPeakDate))
  
  if(nrow(miscDdDf) == 0){
    miscDdDf <- sqlQuery(lcon, sprintf("select top 1 time_stamp, MKT_CAP_CR from equity_misc_info 
                       where symbol='%s' 
                       and time_stamp <= '%s'
                       and time_stamp >= '%s'
                       and MKT_CAP_CR > 0
                       order by time_stamp desc",  sym, recentPeakDate, recentPeakDate - 20))
  }
  ddMktCap <- 0.0
  if(nrow(miscDdDf) > 0){
    ddMktCap <- miscDdDf$MKT_CAP_CR[1]
    if(is.null(ddMktCap) || !is.finite(ddMktCap)){
      ddMktCap <- 0.0
    }
  }
  
  resDf <- rbind(resDf, c(sym, as.character(recentPeakDate), ddFromPeakDate, isOngoing, preDdFromPeak, ret, ddPE, currentPE, ddMktCap, currentMktCap))
}

resDf <- resDf[-1,]

resDf$DD_START <- as.Date(resDf$DD_START)
resDf$DD <- as.numeric(resDf$DD)   
resDf$PREV_DD <- as.numeric(resDf$PREV_DD)   
resDf$RET <- as.numeric(resDf$RET)  
resDf$PE_START <- as.numeric(resDf$PE_START)
resDf$PE_LATEST <- as.numeric(resDf$PE_LATEST)
resDf$MKT_CAP_START <- as.numeric(resDf$MKT_CAP_START)
resDf$MKT_CAP_LATEST <- as.numeric(resDf$MKT_CAP_LATEST)

resDf$MKT_CAP_DIFF <- resDf$MKT_CAP_LATEST/resDf$MKT_CAP_START - 1
resDf$MKT_CAP_DIFF <- ifelse(resDf$MKT_CAP_DIFF < -0.80, 100*resDf$MKT_CAP_LATEST/resDf$MKT_CAP_START - 1, resDf$MKT_CAP_DIFF)

resDf$DD_PCT <- -100*resDf$DD
resDf$PREV_DD_PCT <- -100*resDf$PREV_DD
resDf$MKT_CAP_DIFF <- 100*resDf$MKT_CAP_DIFF
resDf$RET_PCT <- 100*resDf$RET

resDf %>% select(SYMBOL, DD_START, DD_PCT, PREV_DD_PCT, RET_PCT, PE_START, PE_LATEST, MKT_CAP_DIFF) %>%
  arrange(desc(DD_PCT)) %>%
  gt() %>%
  tab_header(title = sprintf("%s Drawdown Stats", indexName), subtitle = sprintf('%s:%s', startDt, endDt)) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  cols_label(
    SYMBOL = '',
    DD_START = 'Top',
    DD_PCT = md('Current<br>Drawdown<sub><sup>(%)</sup></sub><br><sub>since 2020-05</sub>'),
    PREV_DD_PCT = md('Previous<br>Drawdown<sub><sup>(%)</sup></sub><br><sub>2014 - 2019</sub>'),
    RET_PCT = md('Returns<sub><sup>(%)</sup></sub><br><sub>since 2020-05</sub>'),
    PE_START = md('Peak<br>P/E'),
    PE_LATEST = md('Current<br>P/E'),
    MKT_CAP_DIFF = md('Total<br>Mkt-cap<br>Chg<sub><sup>(%)</sup></sub>')) %>%
  fmt(
    columns = c(PREV_DD_PCT, PE_START, PE_LATEST, MKT_CAP_DIFF, RET_PCT),
    fns = function(x) ifelse(x == 0 | !is.finite(x), "—", format(round(x, 2), nsmall=2))
  ) %>%
  sub_missing(missing_text = '') %>%
  opt_stylize(style=5) %>%
  cols_align(align='center') %>%
  cols_width(DD_START ~ pct(15)) %>%
  opt_horizontal_padding(scale = 1) %>%
  opt_vertical_padding(scale = 0) %>%
  gtsave(sprintf("%s/%s.dd-stats.html", reportPath, indexName))

webshot2::webshot(
  sprintf("%s/%s.dd-stats.html", reportPath, indexName),
  sprintf("%s/%s.dd-stats.png", reportPath, indexName) 
)

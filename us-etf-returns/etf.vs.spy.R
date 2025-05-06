library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('tidyverse')
library('gtExtras')
library('webshot2')

pdf(NULL)
options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), 
  case = "nochange", believeNRows = TRUE
)

lconUs2 <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), 
  case = "nochange", believeNRows = TRUE
)

minDt <- sqlQuery(lconUs2, "select min(time_stamp) from bhav_eq_td where symbol='SPY'")[[1]]
maxDt <- sqlQuery(lconUs2, "select max(time_stamp) from bhav_eq_td where symbol='SPY'")[[1]]
etfSymbols <- sqlQuery(lcon, sprintf("select symbol, fund from etf_meta where INCEPTION_DATE <= '%s'", minDt))
etfSymbols <- rbind(etfSymbols, c('BRK/B', 'Berkshire Hathaway Inc Class B'))

etfs <- sqlQuery(lconUs2, sprintf("select symbol, min(time_stamp) minTs, max(time_stamp) maxTs from bhav_eq_td 
                               where symbol in ('%s') group by symbol", paste(etfSymbols$symbol, collapse="','")))

etfs <- etfs |> filter(minTs == minDt & maxTs == maxDt)

dXts <- NULL
for(i in 1:nrow(etfs)){
  pxDf <- sqlQuery(lconUs2, sprintf("select c, time_stamp from bhav_eq_td 
                                    where symbol = '%s'
                                    and time_stamp >= '%s'
                                    and time_stamp <= '%s'", etfs$symbol[i], minDt, maxDt))
  
  pXts <- xts(pxDf[,1], pxDf[,2])
  dXts <- merge.xts(dXts, dailyReturn(pXts))
}

names(dXts) <- etfs$symbol

spyRet <- as.numeric(Return.annualized(dXts[,'SPY']))*100
spySharpe <- as.numeric(SharpeRatio.annualized(dXts[,'SPY']))

annRetDf <- data.frame(t(data.frame(Return.annualized(dXts)*100)))
colnames(annRetDf) <- c('ANN_RET')
annRetDf$SYMBOL <- row.names(annRetDf)

annSharpeDf <- data.frame(t(data.frame(SharpeRatio.annualized(dXts))))
colnames(annSharpeDf) <- c('ANN_SHARPE')
annSharpeDf$SYMBOL <- row.names(annSharpeDf)

annDf <- annRetDf |> 
  inner_join(annSharpeDf, join_by(SYMBOL)) |> 
  inner_join(etfSymbols |> mutate(symbol = gsub('/', '.', symbol)), join_by(SYMBOL == symbol)) |>
  relocate(c(SYMBOL, fund), 1) |>
  arrange(desc(ANN_RET))

annDf %>%
  gt() %>%
  tab_header(title = "Annual Returns", subtitle = sprintf('%s:%s', minDt, maxDt)) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  fmt_number(decimals=2) %>%
  opt_stylize(style=5) %>%
  data_color(columns = ANN_RET,
             fn = \(arr) ifelse(arr < spyRet, 'red', ifelse(arr > spyRet, 'darkgreen', 'black')), 
             apply_to = 'text') %>%
  data_color(columns = ANN_SHARPE,
             fn = \(arr) ifelse(arr < spySharpe, 'red', ifelse(arr > spySharpe, 'darkgreen', 'black')), 
             apply_to = 'text') %>%
  tab_options(table.font.size = '130%') %>%
  cols_label('SYMBOL' = '',
             'fund' = '') %>%
  gtsave(sprintf("%s/etf.annual.returns.html", reportPath))

webshot2::webshot(
  sprintf("%s/etf.annual.returns.html", reportPath),
  sprintf("%s/etf.annual.returns.png", reportPath)
)


###############################

startDateStr <- "2009-01-01/"

spyRet <- as.numeric(Return.annualized(dXts[startDateStr,'SPY']))*100
spySharpe <- as.numeric(SharpeRatio.annualized(dXts[startDateStr,'SPY']))

annRetDf <- data.frame(t(data.frame(Return.annualized(dXts[startDateStr,])*100)))
colnames(annRetDf) <- c('ANN_RET')
annRetDf$SYMBOL <- row.names(annRetDf)

annSharpeDf <- data.frame(t(data.frame(SharpeRatio.annualized(dXts[startDateStr,]))))
colnames(annSharpeDf) <- c('ANN_SHARPE')
annSharpeDf$SYMBOL <- row.names(annSharpeDf)

annDf <- annRetDf |> 
  inner_join(annSharpeDf, join_by(SYMBOL)) |> 
  inner_join(etfSymbols |> mutate(symbol = gsub('/', '.', symbol)), join_by(SYMBOL == symbol)) |>
  relocate(c(SYMBOL, fund), 1) |>
  arrange(desc(ANN_RET))

annDf %>%
  gt() %>%
  tab_header(title = "Annual Returns (post-GFC)", subtitle = sprintf('%s%s', startDateStr, maxDt)) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  fmt_number(decimals=2) %>%
  opt_stylize(style=5) %>%
  data_color(columns = ANN_RET,
             fn = \(arr) ifelse(arr < spyRet, 'red', ifelse(arr > spyRet, 'darkgreen', 'black')), 
             apply_to = 'text') %>%
  data_color(columns = ANN_SHARPE,
             fn = \(arr) ifelse(arr < spySharpe, 'red', ifelse(arr > spySharpe, 'darkgreen', 'black')), 
             apply_to = 'text') %>%
  tab_options(table.font.size = '130%') %>%
  cols_label('SYMBOL' = '',
             'fund' = '') %>%
  gtsave(sprintf("%s/etf.annual.returns.post-GFC.html", reportPath))

webshot2::webshot(
  sprintf("%s/etf.annual.returns.post-GFC.html", reportPath),
  sprintf("%s/etf.annual.returns.post-GFC.png", reportPath)
)

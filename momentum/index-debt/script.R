library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('gtExtras')
library('webshot2')

pdf(NULL)
options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

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

overNightFundCode <- 120785
momIndex <- 'NIFTY MIDCAP150 MOMENTUM 50 TR'
indexName <- "NIFTY MIDCAP 150 TR"
startDt <- as.Date('2018-01-01')
maxDt <- as.Date("2025-04-30")
funds <- sqlQuery(lcon, sprintf("select scheme_code, scheme_name from mf_nav_history 
                                 where as_of='%s' 
                                 and scheme_name like '%%midcap%%' 
                                 and scheme_name like '%%growth%%' 
                                 and scheme_name like '%%direct%%' 
                                 and scheme_name not like '%%index%%' 
                                 and scheme_name not like '%%large%%' 
                                 and scheme_name not like '%%tax%%'", maxDt))

fundStDate <- sqlQuery(lcon, sprintf("select scheme_code, min(as_of) stDt from mf_nav_history where scheme_code in (%s) group by scheme_code",
                       paste(funds$scheme_code, collapse=',')))

funds2018 <- fundStDate |> filter(stDt < startDt)

funds$clean_name <- str_trim(gsub('-|midcap|direct|plan|growth|fund|option|\\(|\\)', '', funds$scheme_name, ignore.case=TRUE))
funds$clean_name <- str_trim(gsub('  ', ' ', funds$clean_name))

funds <- rbind(funds, c(50, momIndex, momIndex))
funds <- rbind(funds, c(150, indexName, indexName))
funds$scheme_code <- as.integer(funds$scheme_code)

fXts <- NULL
for(i in 1:nrow(funds2018)){
  scode <- funds2018$scheme_code[i]
  fPdf <- sqlQuery(lcon, sprintf("select as_of, nav from mf_nav_history where scheme_code = %d and as_of >= '%s' and as_of <= '%s'",
                         scode, startDt, maxDt))
  
  fXts <- merge.xts(fXts, xts(fPdf[,2], fPdf[,1]))
}
names(fXts) <- funds2018$scheme_code

iPxDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", 
                                indexName, startDt, maxDt))
iXts <- xts(iPxDf[,2], iPxDf[,1])
annRetIndex <- annualReturn(iXts)

iPxDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", 
                                momIndex, startDt, maxDt))
momXts <- xts(iPxDf[,2], iPxDf[,1])
annRetMomIndex <- annualReturn(momXts)

annRetFund <- do.call(merge.xts, lapply(1:ncol(fXts), \(x) annualReturn(fXts[,x])))
names(annRetFund) <- funds2018$scheme_code

annRetFund <- merge(annRetFund, annRetMomIndex)

annRetDiff <- do.call(merge.xts, lapply(1:ncol(annRetFund), \(x) annRetFund[,x] - annRetIndex))
names(annRetDiff) <- c(funds2018$scheme_code, '50')

annRetDiffDf <- data.frame(t(data.frame(annRetDiff*100)))
colnames(annRetDiffDf) <- year(strptime(gsub('X', '', colnames(annRetDiffDf)), '%Y.%m.%d'))
annRetDiffDf$SC <- as.integer(gsub('X', '', rownames(annRetDiffDf)))

retDf <- annRetDiffDf %>% inner_join(funds, join_by(SC == scheme_code)) |> 
  select(-c(SC, scheme_name)) |>
  relocate(clean_name, 1)

retDf %>%
  gt() %>%
  tab_header(title = "Excess Annual Returns", subtitle = sprintf('%s:%s', startDt, maxDt)) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  fmt_number(decimals=2) %>%
  opt_stylize(style=5) %>%
  data_color(columns = -clean_name,
             fn = \(arr) ifelse(arr < -5, 'red', ifelse(arr > 5, 'darkgreen', 'black')), 
             apply_to = 'text') %>%
  tab_options(table.font.size = '130%') %>%
  cols_label('clean_name' = '') %>%
  gtsave(sprintf("%s/excess.annual.returns.html", reportPath))

webshot2::webshot(
  sprintf("%s/excess.annual.returns.html", reportPath),
  sprintf("%s/excess.annual.returns.png", reportPath)
)

##############################

ep <- endpoints(fXts, 'years')
annSharpeFund <- do.call(merge.xts, 
                         lapply(1:ncol(fXts), 
                                \(x) period.apply(fXts[,x], ep, 
                                                  \(y) SharpeRatio(dailyReturn(y), FUN = "StdDev"))))

annSharpeMomIndex <- period.apply(momXts, endpoints(momXts, 'years'), \(y) SharpeRatio(dailyReturn(y), FUN = "StdDev"))
annSharpeIndex <- period.apply(iXts, endpoints(iXts, 'years'), \(y) SharpeRatio(dailyReturn(y), FUN = "StdDev"))

annSharpeFund <- merge(annSharpeFund, annSharpeMomIndex, annSharpeIndex)
names(annSharpeFund) <- c(funds2018$scheme_code, '50', '150')

annSharpeFundDf <- data.frame(t(data.frame(annSharpeFund * 100)))
colnames(annSharpeFundDf) <- year(strptime(gsub('X', '', colnames(annSharpeFundDf)), '%Y.%m.%d'))
annSharpeFundDf$SC <- as.integer(gsub('X', '', rownames(annSharpeFundDf)))

retDf <- annSharpeFundDf %>% inner_join(funds, join_by(SC == scheme_code)) |> 
  select(-c(SC, scheme_name)) |>
  relocate(clean_name, 1)

retDf %>%
  gt() %>%
  tab_header(title = "Yearly Sharpe Ratios", subtitle = sprintf('x100; %s:%s', startDt, maxDt)) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  fmt_number(decimals=2) %>%
  opt_stylize(style=5) %>%
  data_color(columns = -clean_name,
             direction = 'column',
             fn = \(arr) ifelse(arr == min(arr), 'red', ifelse(arr == max(arr), 'darkgreen', 'black')), 
             apply_to = 'text') %>%
  tab_options(table.font.size = '130%') %>%
  cols_label('clean_name' = '') %>%
  gtsave(sprintf("%s/sharpe-ratio.html", reportPath))

webshot2::webshot(
  sprintf("%s/sharpe-ratio.html", reportPath),
  sprintf("%s/sharpe-ratio.png", reportPath)
)


######################

fPdf <- sqlQuery(lcon, sprintf("select as_of, nav from mf_nav_history where scheme_code = %d and as_of >= '%s' and as_of <= '%s'",
                               119833, startDt, maxDt))

onXts <- xts(fPdf[,2], fPdf[,1])

dRets <- merge(dailyReturn(momXts), dailyReturn(onXts), dailyReturn(iXts))
dRets <- na.fill(dRets, 0)

eqRatio <- c(0.85, 0.75, 0.65)
mixedRets <- do.call(merge.xts, lapply(eqRatio, \(eq) eq * dRets[,1] + (1-eq) * dRets[,2]))
eqRatioNames <- sapply(eqRatio, \(x) paste0('MOM', x))

toPlot <- merge(dRets[,1], mixedRets, dRets[,3])
names(toPlot) <- c(momIndex, eqRatioNames, indexName)

sharpe <- SharpeRatio.annualized(toPlot)
Common.PlotCumReturns(toPlot, "Midcap Momentum Scenarios", sprintf("SR: %s", paste(round(sharpe,2), collapse="/")),
                      sprintf("%s/midcap-momentum.scenarios.png", reportPath), NULL)

############################

dailyRetFund <- do.call(merge.xts, lapply(1:ncol(fXts), \(x) dailyReturn(fXts[,x])))
fundNames <- unlist(lapply(names(fXts), \(x) funds[funds$scheme_code == as.integer(x),]$clean_name[1]))
allDailyRets <- merge(dailyRetFund, dRets[,1], mixedRets, dRets[,3])
allDailyRets <- na.fill(allDailyRets, 0)
names(allDailyRets) <- c(fundNames, momIndex, eqRatioNames, indexName)

retDf <- data.frame(FUND = names(allDailyRets))
retDf$Returns <- unlist(lapply(1:ncol(allDailyRets), \(x) Return.annualized(allDailyRets[,x])))*100
retDf$SharpeRatio <- unlist(lapply(1:ncol(allDailyRets), \(x) SharpeRatio.annualized(allDailyRets[,x])))
retDf$MaxDrawdown <- unlist(lapply(1:ncol(allDailyRets), \(x) maxDrawdown(allDailyRets[,x])))*100
retDf$MaxDrawdownPostCovid <- unlist(lapply(1:ncol(allDailyRets), \(x) maxDrawdown(allDailyRets["2020-05-01/",x])))*100

retDf %>%
  gt() %>%
  tab_header(title = "Combined Midcap Funds and Index Stats", subtitle = sprintf('%s:%s', startDt, maxDt)) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  fmt_number(decimals=2) %>%
  opt_stylize(style=5) %>%
  data_color(columns = -c(FUND, MaxDrawdown, MaxDrawdownPostCovid),
             direction = 'column',
             fn = \(arr) ifelse(arr == min(arr), 'red', ifelse(arr == max(arr), 'darkgreen', 'black')), 
             apply_to = 'text') %>%
  data_color(columns = c(MaxDrawdown, MaxDrawdownPostCovid),
             direction = 'column',
             fn = \(arr) ifelse(arr == max(arr), 'red', ifelse(arr == min(arr), 'darkgreen', 'black')), 
             apply_to = 'text') %>%
  tab_options(table.font.size = '130%') %>%
  cols_width(FUND ~ pct(30), starts_with('M') ~ pct(15)) %>%
  cols_label('FUND' = '',
             'Returns' = md('Returns\n<sup>(Annualized %)</sup>'),
             'SharpeRatio' = md('SharpeRatio\n<sup>(Annualized)</sup>'),
             'MaxDrawdown' = md('MaxDradown\n<sup>(All)</sup>'),
             'MaxDrawdownPostCovid' = md('MaxDradown\n<sup>(Post Covid)</sup>')) %>%
  gtsave(sprintf("%s/combined.html", reportPath))

webshot2::webshot(
  sprintf("%s/combined.html", reportPath),
  sprintf("%s/combined.png", reportPath)
)

library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('viridis')
library('gtExtras')
library('webshot2')

options("scipen" = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

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

pcon <- dbConnect(RPostgres::Postgres(), host=ldbserver2, user= ldbuser2, password=ldbpassword2, dbname=ldbname2)

startDate <- as.Date("2024-01-01") 
indices <- c("NIFTY 100 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR", "NIFTY MICROCAP 250 TR")

getIndexRet <- function(iName, fromDate){
  pDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index 
                                where index_name='%s' 
                                and time_stamp >= '%s'", 
                                iName, fromDate))
  
  dXts <- dailyReturn(xts(pDf[, 1], pDf[, 2]))
  return(dXts)
}

constiRetsTb <- tibble()
indexDDTb <- tibble()
for(iName in indices){
  dXts <- getIndexRet(iName, startDate)
  ddTable <- table.Drawdowns(dXts)
  ddMax <- ddTable[1,]
  if(nrow(ddMax) == 0) next
  ddFrom <- ddMax$From[[1]]
  ddDepth <- ddMax$Depth[[1]]
  
  retFromStart <- as.numeric(Return.cumulative(dXts))
  retFromDD <- as.numeric(Return.cumulative(dXts[paste0(ddFrom, "/")]))
  
  indexDDTb <- rbind(indexDDTb, c(iName, as.character(ddFrom), ddDepth, retFromDD, retFromStart))
  
  constiDt <- sqlQuery(lcon, sprintf("select max(time_stamp) from index_const_history 
                                     where index_name='%s' 
                                     and time_stamp <= '%s'", 
                                     gsub(" TR", "", iName), 
                                     ddFrom))[[1]]
  constits <- sqlQuery(lcon, sprintf("select SYMBOL from index_const_history 
                                    where index_name='%s' 
                                    and time_stamp = '%s'",
                                    gsub(" TR", "", iName),
                                    constiDt))[,1]
  
  
  for(sym in constits){
    pDf <- dbGetQuery(pcon, "select c, date_stamp from eod_adjusted_nse
                      where ticker = $1
                      and date_stamp >= $2",
                      params = list(sym, ddFrom))
    if(nrow(pDf) == 0) next
    cumRet <- as.numeric(Return.cumulative(dailyReturn(xts(pDf[,1], pDf[,2]))))
    constiRetsTb <- rbind(constiRetsTb, c(iName, sym, cumRet))
  }
}

names(constiRetsTb) <- c("INDEX", "SYMBOL", "RET")
constiRetsTb$RET <- as.numeric(constiRetsTb$RET) * 100

names(indexDDTb) <- c("INDEX", "FROM", "DD", "RET", "RET_START")
indexDDTb$FROM <- as.Date(indexDDTb$FROM)
indexDDTb$DD <- as.numeric(indexDDTb$DD) * 100
indexDDTb$RET <- as.numeric(indexDDTb$RET) * 100
indexDDTb$RET_START <- as.numeric(indexDDTb$RET_START) * 100

ggplot(constiRetsTb, aes(x=INDEX, y = RET, group = INDEX, color = INDEX)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_violin(trim = TRUE) +
  geom_jitter(position = position_jitter(0.2), size=0.5) +
  geom_point(data = indexDDTb, size=5) +
  scale_y_continuous(trans = 'pseudo_log') +
  scale_color_viridis_d() +
  labs(x='', y='log returns (%)', color='', 
       title = 'Returns since latest drawdown',
       subtitle = sprintf("%s:%s", startDate, Sys.Date()),
       caption = '@StockViz')

ggsave(
  sprintf("%s/index-constituent.returns.png", reportPath),
  width = 16,
  height = 16,
  units = "in"
)

constiBeats <- constiRetsTb |> 
  inner_join(indexDDTb, join_by(INDEX)) |>
  group_by(INDEX) |>
  summarise(BEAT_PCT = 100 * sum(if_else(RET.x > RET.y, 1, 0))/n())

constiAgg <- constiRetsTb |> 
  group_by(INDEX) |> 
  summarise(MED_RET = median(RET),
            MAX_RET = max(RET),
            MIN_RET = min(RET)) |>
  inner_join(constiBeats, join_by(INDEX)) |>
  inner_join(indexDDTb, join_by(INDEX))
            
constiAgg |> select(INDEX, FROM, MED_RET, RET, BEAT_PCT) |>
  gt() %>%
  tab_header(title = "Constituent performance", subtitle = Sys.Date()) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(MED_RET, RET, BEAT_PCT))
  ) %>%
  cols_label(INDEX = '',
             FROM = 'Top',
             MED_RET = md('Median<br>Returns (%)'),
             RET = md('Index<br>Returns (%)'),
             BEAT_PCT = md('Constituents Beating<br>Index Returns (%)')) %>%
  fmt_number(decimals=2) %>%
  sub_missing(missing_text = '') %>%
  opt_stylize(style=5) %>%
  gtsave(sprintf("%s/consti-outperformance.from-dd.html", reportPath))

webshot2::webshot(
  sprintf("%s/consti-outperformance.from-dd.html", reportPath),
  sprintf("%s/consti-outperformance.from-dd.png", reportPath)
)


aggIndexTb <- constiRetsTb |> 
  group_by(INDEX) |> 
  summarise(MED_RET = median(RET),
            MAX_RET = max(RET),
            MIN_RET = min(RET),
            SD_RET = sd(RET)) |>
  inner_join(indexDDTb, join_by(INDEX))

getMfRet <- function(schemeType, navStartDate){
  mfSchemes <- sqlQuery(lcon, sprintf("select n1.scheme_code, n1.scheme_name, min(n1.as_of) st, max(n1.as_of) et
                            from MF_SCHEME_CLASS m1, MF_NAV_HISTORY n1
                            where m1.class_1 = 'equity'
                            and m1.class_2 = '%s'
                            and m1.as_of = (select max(as_of) from MF_SCHEME_CLASS m2
                                            where m1.scheme_code = m2.scheme_code)
                            and m1.scheme_code = n1.scheme_code
                            and n1.scheme_name like '%%growth%%'
                            and n1.scheme_name not like '%%dividend%%'
                            and n1.scheme_name not like '%%index%%'
                            group by n1.scheme_code, n1.scheme_name",
                          schemeType))

  mfSchemes <- mfSchemes |> filter(st <= navStartDate & et == max(et))

  mfRetTb <- tibble()
  for(i in 1:nrow(mfSchemes)){
    scode <- mfSchemes$scheme_code[i]
    pDf <- sqlQuery(lcon, sprintf("select nav, as_of from MF_NAV_HISTORY
                                  where scheme_code = %d
                                  and as_of >= '%s'",
                                  scode,
                                  navStartDate))
    ret <- as.numeric(Return.cumulative(dailyReturn(xts(pDf[,1], pDf[,2]))))
    mfRetTb <- rbind(mfRetTb, c(scode, ret))
  }
  names(mfRetTb) <- c("CODE", "RET")
  mfRetTb$RET <- 100 * as.numeric(mfRetTb$RET)
  return(mfRetTb)
}

mfRets <- getMfRet('large cap fund', (aggIndexTb |> filter(INDEX == "NIFTY 100 TR") |> select(FROM))[[1]])
mfRets$INDEX <- "NIFTY 100 TR"

mfRetsTemp <- getMfRet('mid cap fund', (aggIndexTb |> filter(INDEX == "NIFTY MIDCAP 150 TR") |> select(FROM))[[1]])
mfRetsTemp$INDEX <- "NIFTY MIDCAP 150 TR"
mfRets <- rbind(mfRets, mfRetsTemp)

mfRetsTemp <- getMfRet('small cap fund', (aggIndexTb |> filter(INDEX == "NIFTY SMALLCAP 250 TR") |> select(FROM))[[1]])
mfRetsTemp$INDEX <- "NIFTY SMALLCAP 250 TR"
mfRets <- rbind(mfRets, mfRetsTemp)


mfBeats <- mfRets |> inner_join(aggIndexTb, join_by(INDEX)) |>
  group_by(INDEX) |> 
  summarise(NUM_BEAT = sum(if_else(RET.x > RET.y, 1, 0)),
            NUM_TOTAL = n())

mfAggTb <- mfRets |> group_by(INDEX) |> summarise(MED_RET = median(RET),
                                       MIN_RET = min(RET),
                                       MAX_RET = max(RET)) |>
  inner_join(mfBeats, join_by(INDEX))


allAggTb <- mfAggTb |> right_join(aggIndexTb, join_by(INDEX), suffix=c(".mf", ".in"))


allAggTb |> mutate(BEAT_PCT = 100*NUM_BEAT/NUM_TOTAL) |>
  select(INDEX, FROM, RET, MED_RET.mf, BEAT_PCT) |>
    gt() %>%
    tab_header(title = "Index Drawdowns and Fund Performance", subtitle = Sys.Date()) %>%
    tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = c(RET, BEAT_PCT))
    ) %>%
    cols_label(INDEX = '',
               RET = md('Index Returns<br>(since drawdown)'),
               MED_RET.mf = md('Median MF Returns<br>(since drawdown)'),
               BEAT_PCT = md('Funds Beating<br>Index Returns (%)')) %>%
    fmt_number(decimals=2) %>%
    sub_missing(missing_text = '') %>%
    opt_stylize(style=5) %>%
    gtsave(sprintf("%s/mf-outperformance.from-dd.html", reportPath))

webshot2::webshot(
  sprintf("%s/mf-outperformance.from-dd.html", reportPath),
  sprintf("%s/mf-outperformance.from-dd.png", reportPath)
)

############################

mfRets <- getMfRet('large cap fund', startDate)
mfRets$INDEX <- "NIFTY 100 TR"

mfRetsTemp <- getMfRet('mid cap fund', startDate)
mfRetsTemp$INDEX <- "NIFTY MIDCAP 150 TR"
mfRets <- rbind(mfRets, mfRetsTemp)

mfRetsTemp <- getMfRet('small cap fund', startDate)
mfRetsTemp$INDEX <- "NIFTY SMALLCAP 250 TR"
mfRets <- rbind(mfRets, mfRetsTemp)

mfBeats <- mfRets |> inner_join(aggIndexTb, join_by(INDEX)) |>
  group_by(INDEX) |> 
  summarise(NUM_BEAT = sum(if_else(RET.x > RET_START, 1, 0)),
            NUM_TOTAL = n())

mfAggTb <- mfRets |> group_by(INDEX) |> summarise(MED_RET = median(RET),
                                                  MIN_RET = min(RET),
                                                  MAX_RET = max(RET)) |>
  inner_join(mfBeats, join_by(INDEX))


allAggTb <- mfAggTb |> right_join(aggIndexTb, join_by(INDEX), suffix=c(".mf", ".in"))


allAggTb |> mutate(BEAT_PCT = 100*NUM_BEAT/NUM_TOTAL) |>
  select(INDEX, BEAT_PCT) |>
  gt() %>%
  tab_header(title = "Fund Performance", subtitle = sprintf("%s:%s", startDate, Sys.Date())) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(BEAT_PCT))
  ) %>%
  cols_label(INDEX = '',
             BEAT_PCT = md('Funds Beating<br>Index Returns (%)')) %>%
  fmt_number(decimals=2) %>%
  sub_missing(missing_text = '') %>%
  opt_stylize(style=5) %>%
  gtsave(sprintf("%s/mf-outperformance.from-%s.html", reportPath, startDate))

webshot2::webshot(
  sprintf("%s/mf-outperformance.from-%s.html", reportPath, startDate),
  sprintf("%s/mf-outperformance.from-%s.png", reportPath, startDate)
)

############################

covidDate <- as.Date("2020-05-01")

indexRetTb <- tibble()
for(iName in indices){
  dXts <- getIndexRet(iName, covidDate)
  ret <- as.numeric(Return.cumulative(dXts))
  indexRetTb <- rbind(indexRetTb, c(iName, ret))
}
names(indexRetTb) <- c("INDEX", "RET")
indexRetTb$RET <- 100*as.numeric(indexRetTb$RET)

mfRets <- getMfRet('large cap fund', covidDate)
mfRets$INDEX <- "NIFTY 100 TR"

mfRetsTemp <- getMfRet('mid cap fund', covidDate)
mfRetsTemp$INDEX <- "NIFTY MIDCAP 150 TR"
mfRets <- rbind(mfRets, mfRetsTemp)

mfRetsTemp <- getMfRet('small cap fund', covidDate)
mfRetsTemp$INDEX <- "NIFTY SMALLCAP 250 TR"
mfRets <- rbind(mfRets, mfRetsTemp)

mfBeats <- mfRets |> inner_join(indexRetTb, join_by(INDEX)) |>
  group_by(INDEX) |> 
  summarise(BEAT_PCT = 100*sum(if_else(RET.x > RET.y, 1, 0))/n())


mfBeats |> 
  select(INDEX, BEAT_PCT) |>
  gt() %>%
  tab_header(title = "Fund Performance", subtitle = sprintf("%s:%s", covidDate, Sys.Date())) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(BEAT_PCT))
  ) %>%
  cols_label(INDEX = '',
             BEAT_PCT = md('Funds Beating<br>Index Returns (%)')) %>%
  fmt_number(decimals=2) %>%
  sub_missing(missing_text = '') %>%
  opt_stylize(style=5) %>%
  gtsave(sprintf("%s/mf-outperformance.from-%s.html", reportPath, covidDate))

webshot2::webshot(
  sprintf("%s/mf-outperformance.from-%s.html", reportPath, covidDate),
  sprintf("%s/mf-outperformance.from-%s.png", reportPath, covidDate)
)

source("../common/baseline_analysis.R")

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

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")
source("/mnt/data/blog/common/theme.returns.common.r")

drag <- 0.2/100

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

univDt <- as.Date("2019-01-01")
tsStartDt <- as.Date("2020-05-01")
trainEndDt <- as.Date("2023-12-01")
testRange <- "2024-01-01/2024-12-31"

#define the universe
n500Dt <- sqlQuery(lcon, sprintf("select min(time_stamp) from index_const_history 
                   where index_name='NIFTY 500'
                   and time_stamp > '%s'", univDt))[[1]]

symbols <- sqlQuery(lcon, sprintf("select symbol from index_const_history
                                  where index_name='NIFTY 500'
                                  and time_stamp = '%s'", n500Dt))[,1]

#fetch the prices
fileName <- "../common/prices.Rdata"
pXts <- NULL
syms <- NULL
if(file.exists(fileName)){
  print("loading prices from cache...")
  load(fileName)
  syms <- names(pXts)
} else {
  print("loading prices from database...")
  syms <- c()
  for(sym in symbols){
    pDf <- dbGetQuery(pgCon, "select c, date_stamp from eod_adjusted_nse
                      where ticker = $1
                      and date_stamp >= $2",
                      params = list(sym, tsStartDt))
    
    if(nrow(pDf) == 0) next
    pXts <- merge.xts(pXts, xts(pDf$c, pDf$date_stamp))
    syms <- c(syms, sym)
  }
  names(pXts) <- syms
  
  save(pXts, file = fileName)
}

#ignore symbols that do not have prices for the whole range
loa <- lapply(syms, \(x) c(x, 
                           as.character(index(xts::first(na.omit(pXts[,x])))), 
                           as.character(index(xts::last(na.omit(pXts[,x]))))
                           ))

symMinMaxTs <- tibble(map_dfr(loa, ~ as.data.frame(t(.x))))
names(symMinMaxTs) <- c("SYMBOL", "START_TS", "END_TS")

symsFull <- symMinMaxTs |> 
  filter(START_TS == min(START_TS) & END_TS == max(END_TS)) 


#calcuate daily returns
dSymXts <- do.call(merge.xts, lapply(symsFull$SYMBOL, \(x) dailyReturn(pXts[,x])))
names(dSymXts) <- symsFull$SYMBOL

statsFile <- "../common/cp-stats.Rdata"
if(file.exists(statsFile)){
  print("loading stats from cache...")
  load(statsFile)
} else {
  print("calculating stats...")
  statsTb <- tibble()
  for(i in 1:ncol(dSymXts)){
    sym <- symsFull$SYMBOL[i]
    abRet <- analyze_baseline(dSymXts[paste0("/", trainEndDt), i])
    statsTb <- rbind(statsTb, c(sym, t(abRet$summary)[2,]))
  }
  
  names(statsTb) <- c('SYMBOL', t(abRet$summary)[1,])
  
  statsTb <- statsTb |> 
    mutate(across(-SYMBOL, as.numeric)) |>
    mutate(total_cp = rowSums(across(where(is.numeric)), na.rm = TRUE)) |>
    mutate(cp_rank = ntile(total_cp, 5)) 
  
  save(statsTb, file = statsFile)
}

statsTile <- statsTb |> dplyr::select(SYMBOL, cp_rank)

#calculat 50-day sd and take the sd of it

sdsdLst <- unlist(lapply(symsFull$SYMBOL, \(x) sd(rollapply(dSymXts[paste0("/", trainEndDt), x], 50, sd), na.rm=TRUE)))
sdTb <- tibble(SYMBOL = symsFull$SYMBOL, SDSD = sdsdLst)
sdTb <- sdTb |> mutate(sd_rank = ntile(SDSD, 5))

cpsd1 <- statsTile |> inner_join(sdTb, join_by(SYMBOL)) |> filter(cp_rank == 1 & sd_rank == 1)

overlapPct <- nrow(cpsd1)/nrow(statsTile |> filter(cp_rank == 1))

#forward returns by tile
sdStats <- tibble()
for(nt in 1:5){
  sdSyms <- (sdTb |> filter(sd_rank == nt) |> dplyr::select(SYMBOL))[,1]
  sdRets <- unlist(lapply(sdSyms, \(x) Return.cumulative(dSymXts[testRange, x])))
  sdStats <- rbind(sdStats, c(nt, mean(sdRets), sd(sdRets), min(sdRets), max(sdRets)))
}

names(sdStats) <- c("TILE", "AVG_RET", "SD_RET", "MIN_RET", "MAX_RET")

ntStats <- tibble()
for(nt in 1:5){
  ntSyms <- (statsTile |> filter(cp_rank == nt) |> dplyr::select(SYMBOL))[,1]
  ntRets <- unlist(lapply(ntSyms, \(x) Return.cumulative(dSymXts[testRange, x])))
  ntStats <- rbind(ntStats, c(nt, mean(ntRets), sd(ntRets), min(ntRets), max(ntRets)))
}

names(ntStats) <- c("TILE", "AVG_RET", "SD_RET", "MIN_RET", "MAX_RET")

toPlot <- sdStats |> 
  pivot_longer(cols = -TILE) |> mutate(STRAT = "sd of sd") |> 
  rbind(ntStats |> pivot_longer(cols = -TILE) |> mutate(STRAT = "change-point")) |>
  mutate(value = format(round(100*value, 2), nsmall=2)) |>
  group_by(TILE, name) |> 
  relocate(STRAT, name, value) |> 
  arrange(name) 

toPlot |>
  gt() |>
  tab_header(title = "Forward Returns", subtitle = testRange) |>
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) |>
  tab_options(
    row_group.as_column = TRUE,
  ) |>
  tab_options(column_labels.hidden = TRUE) |>
  opt_stylize(style=5) |>
  gtsave(sprintf("%s/quintile-volatility.html", reportPath))

webshot2::webshot(
  sprintf("%s/quintile-volatility.html", reportPath),
  sprintf("%s/quintile-volatility.png", reportPath), 
  selector = "table.gt_table", 
  expand = c(10, 10, 10, 10)
)

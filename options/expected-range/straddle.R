library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')

options("scipen" = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

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

startDate <- as.Date("2015-01-01")
indexName <- "NIFTY 50"
symbol <- "NIFTY"

expiries <- sqlQuery(lcon, sprintf("select distinct expiry_dt from bhav_eq_fut 
                                   where symbol = '%s' 
                                   and time_stamp = expiry_dt 
                                   and time_stamp >= '%s'", 
                                   symbol, startDate))[,1]

tradingDays <- sqlQuery(lcon, sprintf("select distinct time_stamp from bhav_eq_fut 
                                      where symbol = '%s' 
                                      and time_stamp >= '%s'", 
                                      symbol, startDate))[,1]

tradingDays <- sort(tradingDays)

statsDataDf <- data.frame(TIME_STAMP = "", EXPIRY = "", D2E = 0, UPPER_DIFF = 0.0, LOWER_DIFF = 0.0, SHORT_STRADDLE_PL = 0.0)
for(i in 2:length(expiries)){
  stDate <- expiries[i-1]
  edDate <- expiries[i]
  edSpot <- sqlQuery(lcon, sprintf("select px_close from bhav_index where index_name='%s' and time_stamp='%s'", indexName, edDate))[[1]]
  
  tdays <- tradingDays[tradingDays >= stDate & tradingDays <= edDate]
  numDays <- length(tdays)
  for(j in 1:numDays){
    tday <- tdays[j]
    spot <- sqlQuery(lcon, sprintf("select px_close from bhav_index where index_name='%s' and time_stamp='%s'", indexName, tday))[[1]]
    strikeDf <- sqlQuery(lcon, sprintf("select top 1 strike_pr, abs(strike_pr-%f) d 
                                     from bhav_eq_opt where symbol='%s' and OPTION_TYP='CE' 
                                     and time_stamp='%s' and expiry_dt='%s' 
                                     and (convert(integer, strike_pr) %% 100) = 0
                                     order by d", spot, symbol, tday, edDate))
    
    if(nrow(strikeDf) == 0) next
    
    strike <- strikeDf$strike_pr[1]
    stCE <- sqlQuery(lcon, sprintf("select px_close
                                     from bhav_eq_opt where symbol='%s' and OPTION_TYP='CE' 
                                     and time_stamp='%s' and expiry_dt='%s' 
                                     and strike_pr=%f", symbol, tday, edDate, strike))$px_close[1]
    
    stPE <- sqlQuery(lcon, sprintf("select px_close
                                     from bhav_eq_opt where symbol='%s' and OPTION_TYP='PE' 
                                     and time_stamp='%s' and expiry_dt='%s' 
                                     and strike_pr=%f", symbol, tday, edDate, strike))$px_close[1]
    
    edCE <- sqlQuery(lcon, sprintf("select px_close
                                     from bhav_eq_opt where symbol='%s' and OPTION_TYP='CE' 
                                     and time_stamp='%s' and expiry_dt='%s' 
                                     and strike_pr=%f", symbol, edDate, edDate, strike))$px_close[1]
    
    edPE <- sqlQuery(lcon, sprintf("select px_close
                                     from bhav_eq_opt where symbol='%s' and OPTION_TYP='PE' 
                                     and time_stamp='%s' and expiry_dt='%s' 
                                     and strike_pr=%f", symbol, edDate, edDate, strike))$px_close[1]

    stUpper <- spot + 1.25*(stCE + stPE)
    stLower <- spot - 1.25*(stCE + stPE)
    
    statsDataDf <- rbind(statsDataDf, c(as.character(tday), 
                                as.character(edDate), 
                                numDays - j, 
                                as.numeric(stUpper/edSpot - 1), 
                                as.numeric(edSpot/stLower - 1),
                                (stCE + stPE) - (edCE + edPE)))
  }
}

statsDf <- statsDataDf[-1,]

statsDf$TIME_STAMP <- as.Date(statsDf$TIME_STAMP)
statsDf$EXPIRY <- as.Date(statsDf$EXPIRY)
statsDf$D2E <- as.numeric(statsDf$D2E)
statsDf$UPPER_DIFF <- as.numeric(statsDf$UPPER_DIFF)
statsDf$LOWER_DIFF <- as.numeric(statsDf$LOWER_DIFF)
statsDf$SHORT_STRADDLE_PL <- as.numeric(statsDf$SHORT_STRADDLE_PL)

statsDf$UPPER_DIFF <- 100*statsDf$UPPER_DIFF
statsDf$LOWER_DIFF <- 100*statsDf$LOWER_DIFF

summary(statsDf$UPPER_DIFF)
summary(statsDf$LOWER_DIFF)
summary(statsDf$SHORT_STRADDLE_PL)

validD2e <- (statsDf |> filter(D2E != 0) |> 
               group_by(D2E) |> 
               summarise(CNT = n()) |>
               filter(CNT > 1) |> 
               select(-CNT))$D2E

rangeStats <- statsDf |> filter(D2E %in% validD2e) |> group_by(D2E) |> 
  summarise(RNG_CNT = 100*sum(if_else(UPPER_DIFF > 0 & LOWER_DIFF > 0, 1, 0))/n())

inRangePct <- 100*sum(ifelse(statsDf$UPPER_DIFF > 0 & statsDf$LOWER_DIFF > 0, 1, 0))/nrow(statsDf)

ggplot(rangeStats, aes(x=D2E, y=RNG_CNT)) + 
  theme_economist() +
  geom_point(size=2) +
  labs(x = 'straddle days to expiry', y = '% inside straddle range at expiry',
       title = sprintf("%s spot expiring inside ATM straddle implied range", symbol),
       subtitle = sprintf("%s:%s", min(statsDf$TIME_STAMP), max(statsDf$TIME_STAMP)),
       caption = '@StockViz') +
  annotate("text", x = 0, y=90, label = sprintf("in range across all d2e: %.2f%%", inRangePct), hjust=0, vjust=0)

ggsave(sprintf("%s/%s.spot-expiry.png", reportPath, symbol), width = 12, height = 6, units = "in")

ggplot(statsDf |> filter(D2E %in% validD2e), aes(x=D2E, y = SHORT_STRADDLE_PL, group=D2E)) +
  theme_economist() +
  geom_violin() +
  labs(x = 'straddle days to expiry', y = 'straddle p&l at expiry',
       title = sprintf("%s short ATM straddle P&L at expiry", symbol),
       subtitle = sprintf("%s:%s", min(statsDf$TIME_STAMP), max(statsDf$TIME_STAMP)),
       caption = '@StockViz') +
  annotate("text", x = 0, y=-2000, label = sprintf("mean: %.2f\nmedian: %.2f", mean(statsDf$SHORT_STRADDLE_PL), median(statsDf$SHORT_STRADDLE_PL)), hjust=0, vjust=0)

ggsave(sprintf("%s/%s.short-ATM.straddle.png", reportPath, symbol), width = 12, height = 6, units = "in")
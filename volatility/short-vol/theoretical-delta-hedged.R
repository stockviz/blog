library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('greeks')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggpubr')

pdf(NULL)
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

symbol <- "NIFTY"
startDt <- as.Date("2015-01-01")
volLb <- 30 #days
numDaysYr <- 242 #number of days in a year

expiryDates <- sqlQuery(lcon, sprintf("select distinct time_stamp from bhav_eq_fut where symbol = '%s' and time_stamp = expiry_dt and time_stamp >= '%s'", symbol, startDt))[,1]
expiryDates <- sort(expiryDates)

pFutXts <- NULL
for(i in 2:length(expiryDates)){
  stDt <- expiryDates[i-1]
  edDt <- expiryDates[i]
  
  pxDf <- sqlQuery(lcon, sprintf("select time_stamp, px_high [High], px_low [Low], px_open [Open], px_close [Close] from bhav_eq_fut 
                         where symbol = '%s' and time_stamp > '%s' and time_stamp <= '%s'
                         and expiry_dt = '%s'", symbol, stDt, edDt, edDt))
  
  pFutXts <- rbind.xts(pFutXts, xts(pxDf[,-1], pxDf[, 1]))
}

tradeDates <- index(pFutXts)

#daysYear <- data.frame(DT = tradeDates, Y = year(tradeDates))
#daysYear |> filter(Y < 2025) |> group_by(Y) |> summarise(CNT = n()) |> summarise(AVG = mean(CNT))

volCC <- volatility(pFutXts$Close, n=volLb, calc='close') 
volYZ <- volatility(pFutXts, n=volLb, calc='yang.zhang') 

volXts <- na.omit(merge(volCC, volYZ))
names(volXts) <- c("VOL_CC", "VOL_YZ")
volDf <- data.frame(volXts)
volDf$T <-index(volXts)

startTradeDateIndex <- which(tradeDates == first(index(volXts)))

ivRelDf <- data.frame(TIME_STAMP = "", STRIKE_DIST_CE = 0.0, STRIKE_DIST_PE = 0.0, BS_IV = 0.0, D2E = 0)
greeksCeDf <- tibble()
greeksPeDf <- tibble()
for(i in startTradeDateIndex:length(tradeDates)){
  trdDt <- tradeDates[i]
  
  optExps <- sqlQuery(lcon, sprintf("select distinct expiry_dt from bhav_eq_opt
                                    where symbol = '%s' and time_stamp = '%s'
                                    and option_typ in ('CE', 'PE')
                                    order by expiry_dt", symbol, trdDt))[,1]

  spot <- as.numeric(pFutXts[trdDt, 'Close'])
  #atmStrike <- as.integer(100*round(spot/100, 0))
  strikeCE <- 100*ceiling(spot/100)
  strikePE <- 100*floor(spot/100)
  volSpotCC <- as.numeric(volXts[trdDt, 'VOL_CC'])
  
  for(j in 1:length(optExps)){
    optExp <- optExps[j]
    
    if(trdDt >= optExp) next
    
    premiumCE <- sqlQuery(lcon, sprintf("select px_close from bhav_eq_opt
                                  where symbol = '%s'
                                  and strike_pr = %d
                                  and expiry_dt = '%s'
                                  and option_typ = 'CE'
                                  and time_stamp = '%s'", symbol, strikeCE, optExp, trdDt))[[1]]
    
    if(length(premiumCE) == 0) next
    
    premiumPE <- sqlQuery(lcon, sprintf("select px_close from bhav_eq_opt
                                  where symbol = '%s'
                                  and strike_pr = %d
                                  and expiry_dt = '%s'
                                  and option_typ = 'PE'
                                  and time_stamp = '%s'", symbol, strikePE, optExp, trdDt))[[1]]
    
    if(length(premiumPE) == 0) next
    
    d2eDays <- as.numeric(difftime(optExp, trdDt, units='days'))
    d2e <- d2eDays/numDaysYr
    
    ivCE <- tryCatch({
      BS_Implied_Volatility(
        option_price = premiumCE, 
        initial_price = spot, 
        exercise_price = strikeCE, 
        r = 0, 
        time_to_maturity = d2e,
        payoff = "call")
    }, error = \(e) {
      print(paste(i, j, e))
      return(NA)
      })
    
    greeksCE <- tryCatch({
      BS_European_Greeks(
        initial_price = spot, 
        exercise_price = strikeCE, 
        r = 0, 
        time_to_maturity = d2e,
        volatility = volSpotCC,
        payoff = "call")
    }, error = \(e) {
      print(paste(i, j, e))
      return(NA)
    })
    
    ivPE <- tryCatch({
      BS_Implied_Volatility(
        option_price = premiumPE, 
        initial_price = spot, 
        exercise_price = strikePE, 
        r = 0, 
        time_to_maturity = d2e,
        payoff = "put")
    }, error = \(e) {
      print(paste(i, j, e))
      return(NA)
      })
    
    greeksPE <- tryCatch({
      BS_European_Greeks(
        initial_price = spot, 
        exercise_price = strikePE, 
        r = 0, 
        time_to_maturity = d2e,
        volatility = volSpotCC,
        payoff = "put")
    }, error = \(e) {
      print(paste(i, j, e))
      return(NA)
    })
    
    if(is.na(ivCE) || is.na(ivPE) || all(is.na(greeksCE)) || all(is.na(greeksPE))) next
    
    gCE <- tibble(data.frame(t(data.frame(greeksCE))))
    gCE$time_stamp <- trdDt
    gCE$d2eDays <- d2eDays
    gCE$mkt_px <- premiumCE
    gCE$volatility <- volSpotCC
    gCE$iv <- ivCE
    gCE$expiry <- optExp
    
    gPE <- tibble(data.frame(t(data.frame(greeksPE))))
    gPE$time_stamp <- trdDt
    gPE$d2eDays <- d2eDays
    gPE$mkt_px <- premiumPE
    gPE$volatility <- volSpotCC
    gPE$iv <- ivPE
    gPE$expiry <- optExp
    
    greeksCeDf <- rbind(greeksCeDf, gCE)
    greeksPeDf <- rbind(greeksPeDf, gPE)
    
    avgBsIv <- (ivCE + ivPE)/2

    strikeDistCE <- strikeCE/spot - 1
    strikeDistPE <- strikePE/spot - 1
    ivRelDf <- rbind(ivRelDf, c(as.character(trdDt), 100 * strikeDistCE, 100 * strikeDistPE, avgBsIv, d2eDays))
  }
}

ivRelDf2 <- ivRelDf[-1,]
ivRelDf2 <- ivRelDf2 |> 
  mutate(across(!TIME_STAMP, as.numeric)) |> 
  mutate(TIME_STAMP = as.Date(TIME_STAMP))

meanIV <- ivRelDf2 |> 
  group_by(D2E) |>
  summarise(N = n(), BS_IV_MED = median(100*BS_IV), BS_IV_MEAN = mean(100*BS_IV)) 

#meanIV |> arrange(desc(N)) |> print(n = 100)

meanIV |>
  filter(D2E < 50 & N > 100) |> 
  select(D2E, BS_IV_MED, BS_IV_MEAN) |>
  pivot_longer(-D2E) |>
    ggplot(aes(x=D2E, y=value, color=name)) +
      theme_economist() +
      geom_point() +
      scale_color_viridis_d() +
      labs(x = 'days to expiry', y='implied volatility (%)', color='',
       title = sprintf('%s ATM Implied Volatility', symbol),
       subtitle = sprintf("%s:%s", min(ivRelDf2$TIME_STAMP), max(ivRelDf2$TIME_STAMP)),
       caption = '@StockViz')

ggsave(sprintf("%s/%s.atm.iv.png", reportPath, symbol), width = 16, height = 8, units = "in")

# ivRelDf2 |> filter(D2E == 30) |> inner_join(volDf, join_by(TIME_STAMP == T)) |>
#   select(TIME_STAMP, BS_IV, VOL_CC, VOL_YZ) |>
#   filter(TIME_STAMP >= as.Date("2022-01-01")) |>
#   pivot_longer(-TIME_STAMP) |>
#     ggplot(aes(x=TIME_STAMP, y=value, color=name)) +
#       geom_line()
# 
# 
# ivRelDf2 |> filter(D2E == 30) |> inner_join(volDf, join_by(TIME_STAMP == T)) |>
#   select(BS_IV, VOL_CC, VOL_YZ) |>
#   filter(BS_IV < 0.15) |>
#   pivot_longer(-BS_IV) |>
#     ggplot(aes(x=BS_IV, y=value, color=name)) +
#       geom_point()

tradeStartD2e <- 30
tradeEndD2e <- 7
startCE <- greeksCeDf |> filter(d2eDays == tradeStartD2e) |> select(expiry, vega, iv)
endCE <- greeksCeDf |> filter(d2eDays == tradeEndD2e) |> select(expiry, iv)

shortVolCePL <- startCE |> inner_join(endCE, join_by(expiry)) |>
  mutate(PL = -vega * (iv.y - iv.x))

#shortVolCePL |> arrange(PL)

ggplot(shortVolCePL, aes(x = PL)) +
  theme_economist() +
  geom_histogram(binwidth = 20) +
  annotate('text', x = min(shortVolCePL$PL), y = 10, label = sprintf("total: %.2f", sum(shortVolCePL$PL)), hjust=0) +
  labs(x = 'p&l', y='count', color='',
     title = sprintf('%s Short Volatilty Theoretical Maximum P&L (%d, %d)', symbol, tradeStartD2e, tradeEndD2e),
     subtitle = sprintf("delta-hedged short ATM call [%s:%s]", min(ivRelDf2$TIME_STAMP), max(ivRelDf2$TIME_STAMP)),
     caption = '@StockViz')

ggsave(sprintf("%s/%s.short-vol.call.theoretical.%d-%d.png", reportPath, symbol, tradeStartD2e, tradeEndD2e), width = 16, height = 8, units = "in")


tradeStartD2e <- 30
tradeEndD2e <- 2
startCE <- greeksCeDf |> filter(d2eDays == tradeStartD2e) |> select(expiry, vega, iv)
endCE <- greeksCeDf |> filter(d2eDays == tradeEndD2e) |> select(expiry, iv)

shortVolCePL <- startCE |> inner_join(endCE, join_by(expiry)) |>
  mutate(PL = -vega * (iv.y - iv.x))

#shortVolCePL |> arrange(PL)

ggplot(shortVolCePL, aes(x = PL)) +
  theme_economist() +
  geom_histogram(binwidth = 20) +
  annotate('text', x = min(shortVolCePL$PL), y = 10, label = sprintf("total: %.2f", sum(shortVolCePL$PL)), hjust=0) +
  labs(x = 'p&l', y='count', color='',
       title = sprintf('%s Short Volatilty Theoretical Maximum P&L (%d, %d)', symbol, tradeStartD2e, tradeEndD2e),
       subtitle = sprintf("delta-hedged short ATM call [%s:%s]", min(ivRelDf2$TIME_STAMP), max(ivRelDf2$TIME_STAMP)),
       caption = '@StockViz')

ggsave(sprintf("%s/%s.short-vol.call.theoretical.%d-%d.png", reportPath, symbol, tradeStartD2e, tradeEndD2e), width = 16, height = 8, units = "in")

tradeStartD2e <- 30
tradeEndD2e <- 7
startPE <- greeksPeDf |> filter(d2eDays == tradeStartD2e) |> select(expiry, vega, iv)
endPE <- greeksPeDf |> filter(d2eDays == tradeEndD2e) |> select(expiry, iv)

shortVolPePL <- startPE |> inner_join(endPE, join_by(expiry)) |>
  mutate(PL = -vega * (iv.y - iv.x))

#shortVolCePL |> arrange(PL)

ggplot(shortVolPePL, aes(x = PL)) +
  theme_economist() +
  geom_histogram(binwidth = 20) +
  annotate('text', x = min(shortVolPePL$PL), y = 10, label = sprintf("total: %.2f", sum(shortVolPePL$PL)), hjust=0) +
  labs(x = 'p&l', y='count', color='',
       title = sprintf('%s Short Volatilty Theoretical Maximum P&L (%d, %d)', symbol, tradeStartD2e, tradeEndD2e),
       subtitle = sprintf("delta-hedged short ATM put [%s:%s]", min(ivRelDf2$TIME_STAMP), max(ivRelDf2$TIME_STAMP)),
       caption = '@StockViz')

ggsave(sprintf("%s/%s.short-vol.put.theoretical.%d-%d.png", reportPath, symbol, tradeStartD2e, tradeEndD2e), width = 16, height = 8, units = "in")

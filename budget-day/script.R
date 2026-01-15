library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggrepel')
library('gtExtras')
library('derivmkts')

options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

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

indices <- c("NIFTY 50", "NIFTY MIDCAP 150", "NIFTY SMALLCAP 250", "NIFTY MICROCAP 250")
lookback <- 50 #days

budgetDays <- read.csv(sprintf("%s/days.csv", reportPath), header = TRUE)

budgetDays <- budgetDays |> filter(toupper(type) != "INTERIM") |> mutate(date = as.Date(str_trim(date)))

symbol <- "NIFTY"
iName <- "NIFTY 50"

shortStranglePLTb <- tibble()
for(bdi in 1:nrow(budgetDays)){
  d1 <- budgetDays[bdi, 1]
  
  dayBefore <- sqlQuery(lcon, sprintf("select max(time_stamp) from bhav_eq_opt 
                                      where symbol = '%s'
                                      and time_stamp < '%s'
                                      and option_typ = 'CE'", symbol, d1))[[1]]
  
  if(is.na(dayBefore)) next
  
  if((d1 - dayBefore) > 5) next
  
  dayAfter <- sqlQuery(lcon, sprintf("select min(time_stamp) from bhav_eq_opt 
                                      where symbol = '%s'
                                      and time_stamp >= '%s'
                                      and option_typ = 'CE'", symbol, d1))[[1]]
  
  if(is.na(dayAfter)) next
  
  spots <- sqlQuery(lcon, sprintf("select * from bhav_eq_fut
                                  where symbol = '%s'
                                  and TIME_STAMP = '%s'
                                  order by EXPIRY_DT", symbol, dayBefore))
  
  if(nrow(spots) == 0) next
  
  spots2 <- sqlQuery(lcon, sprintf("select * from bhav_eq_fut
                                  where symbol = '%s'
                                  and TIME_STAMP = '%s'
                                  order by EXPIRY_DT", symbol, dayAfter))
  
  if(nrow(spots2) == 0) next
  
  vTs <- sqlQuery(lcon, sprintf("select max(time_stamp) from technical_index
                                where symbol = '%s'
                                and time_stamp < '%s'", iName, d1))[[1]]
  iVol <- sqlQuery(lcon, sprintf("select VOLATILITY_YZ_30 from technical_index
                                 where symbol = '%s'
                                 and time_stamp = '%s'", iName, vTs))[[1]]
  
  vTs2 <- sqlQuery(lcon, sprintf("select max(time_stamp) from technical_index
                                where symbol = '%s'
                                and time_stamp <= '%s'", iName, dayAfter))[[1]]
  iVol2 <- sqlQuery(lcon, sprintf("select VOLATILITY_YZ_30 from technical_index
                                 where symbol = '%s'
                                 and time_stamp = '%s'", iName, vTs2))[[1]]
  
  ####
  
  strikeCE <- sqlQuery(lcon, sprintf("select min(strike_pr) from bhav_eq_opt
                                     where symbol = '%s'
                                     and option_typ = 'CE'
                                     and time_stamp = '%s'
                                     and strike_pr > %f
                                     and (convert(int, strike_pr) %% 100) = 0", symbol, dayBefore, spots$PX_CLOSE[1]))[[1]]
  
  strikePE <- sqlQuery(lcon, sprintf("select max(strike_pr) from bhav_eq_opt
                                     where symbol = '%s'
                                     and option_typ = 'CE'
                                     and time_stamp = '%s'
                                     and strike_pr < %f
                                     and (convert(int, strike_pr) %% 100) = 0", symbol, dayBefore, spots$PX_CLOSE[1]))[[1]]
  
  if(is.na(strikeCE) || is.na(strikePE)) next
  
  premCEs <- sqlQuery(lcon, sprintf("select top 2 * from bhav_eq_opt
                                where symbol = '%s'
                                and OPTION_TYP = 'CE'
                                and STRIKE_PR = %f
                                and TIME_STAMP = '%s'
                                and expiry_dt > '%s'
                                and expiry_dt in ('%s')
                                order by EXPIRY_DT", symbol, strikeCE, dayBefore, dayAfter, paste(spots2$EXPIRY_DT, collapse="','")))
  
  premPEs <- sqlQuery(lcon, sprintf("select top 2 * from bhav_eq_opt
                                where symbol = '%s'
                                and OPTION_TYP = 'PE'
                                and STRIKE_PR = %f
                                and TIME_STAMP = '%s'
                                and expiry_dt > '%s'
                                and expiry_dt in ('%s')
                                order by EXPIRY_DT", symbol, strikePE, dayBefore, dayAfter, paste(spots2$EXPIRY_DT, collapse="','")))
  
  premCEs$IV <- NA
  premPEs$IV <- NA
  
  premCEs$DELTA <- NA
  premPEs$DELTA <- NA
  
  for(k in 1:nrow(premCEs)){
    t2e <- as.numeric(premCEs$EXPIRY_DT[k] - dayBefore)/365
    premCEs$IV[k] <- bscallimpvol(price = premCEs$PX_SETTLE[k], 
                                   s = spots$PX_CLOSE[k], 
                                   k = strikeCE, 
                                   tt = t2e, 
                                   r = 0, 
                                   d = 0)
    
    premPEs$IV[k] <- bsputimpvol(price = premPEs$PX_SETTLE[k], 
                                  s = spots$PX_CLOSE[k], 
                                  k = strikePE, 
                                  tt = t2e, 
                                  r = 0, 
                                  d = 0)
    
    premCEs$DELTA[k] <- greeks2(bscall, list(s=spots$PX_CLOSE[k], k=strikeCE, v=iVol, r=0, tt=t2e, d=0))[c('Delta'),]
    premPEs$DELTA[k] <- greeks2(bsput, list(s=spots$PX_CLOSE[k], k=strikePE, v=iVol, r=0, tt=t2e, d=0))[c('Delta'),]
  }
  
  
  ####
  
  premCEs2 <- sqlQuery(lcon, sprintf("select top 2 * from bhav_eq_opt
                                where symbol = '%s'
                                and OPTION_TYP = 'CE'
                                and STRIKE_PR = %f
                                and TIME_STAMP = '%s'
                                and expiry_dt in ('%s')
                                order by EXPIRY_DT", symbol, strikeCE, dayAfter, paste(premCEs$EXPIRY_DT, collapse="','")))
  
  premPEs2 <- sqlQuery(lcon, sprintf("select top 2 * from bhav_eq_opt
                                where symbol = '%s'
                                and OPTION_TYP = 'PE'
                                and STRIKE_PR = %f
                                and TIME_STAMP = '%s'
                                and expiry_dt in ('%s')
                                order by EXPIRY_DT", symbol, strikePE, dayAfter, paste(premPEs$EXPIRY_DT, collapse="','")))
  
  premCEs2$IV <- NA
  premPEs2$IV <- NA
  
  premCEs2$DELTA <- NA
  premPEs2$DELTA <- NA
  
  for(k in 1:nrow(premCEs2)){
    t2e <- as.numeric(premCEs2$EXPIRY_DT[k] - dayAfter)/365
    premCEs2$IV[k] <- bscallimpvol(price = premCEs2$PX_SETTLE[k], 
                                  s = spots2$PX_CLOSE[k], 
                                  k = strikeCE, 
                                  tt = t2e, 
                                  r = 0, 
                                  d = 0)
    
    premPEs2$IV[k] <- bsputimpvol(price = premPEs2$PX_SETTLE[k], 
                                 s = spots2$PX_CLOSE[k], 
                                 k = strikePE, 
                                 tt = t2e, 
                                 r = 0, 
                                 d = 0)
    
    premCEs2$DELTA[k] <- greeks2(bscall, list(s=spots2$PX_CLOSE[k], k=strikeCE, v=iVol2, r=0, tt=t2e, d=0))[c('Delta'),]
    premPEs2$DELTA[k] <- greeks2(bsput, list(s=spots2$PX_CLOSE[k], k=strikePE, v=iVol2, r=0, tt=t2e, d=0))[c('Delta'),]
  }
  
  shortStranglePL <- premCEs$PX_SETTLE[1] - premCEs2$PX_SETTLE[1] + 
    premPEs$PX_SETTLE[1] - premPEs2$PX_SETTLE[1] + 
    (premCEs$DELTA[1] + premPEs$DELTA[1])*(spots$PX_CLOSE[1] - spots2$PX_CLOSE[1])
  
  shortStranglePLTb <- rbind(shortStranglePLTb, c(as.character(d1), 
                                                  (as.numeric(premCEs$IV[1]) + as.numeric(premPEs$IV[1]))/2,
                                                  (as.numeric(premCEs2$IV[1]) + as.numeric(premPEs2$IV[1]))/2,
                                                  (as.numeric(premCEs$DELTA[1]) + as.numeric(premPEs$DELTA[1])),
                                                  shortStranglePL))
}  

names(shortStranglePLTb) <- c("BUDGET_DAY", "IV_OPEN", "IV_CLOSE", "NET_DELTA_OPEN", "DHSS")

shortStranglePLTb <- shortStranglePLTb |> 
  mutate(across(-BUDGET_DAY, as.numeric)) 

shortStranglePLTb |>
  mutate(dhssOut = DHSS > 0) |> 
  select(BUDGET_DAY, DHSS, dhssOut) |>
gt() %>%
  tab_header(title = sprintf('%s Delta-Hedged Short Strangle Budget-day Returns', symbol)) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  fmt_number(decimals=2) %>%
  sub_missing(missing_text = '') %>%
  opt_stylize(style=5) %>%
  data_color(columns = dhssOut, target_columns = DHSS,
             fn = scales::col_factor(palette = c("darkred", "darkgreen"), domain = c(FALSE, TRUE)),
             apply_to = 'text') %>%
  cols_hide(c(dhssOut)) %>%
  cols_label(BUDGET_DAY = 'Budget Day',
             DHSS = 'Returns (Rs. per contract)') %>%
  tab_options(table.font.size = '130%') %>%
  gtsave(sprintf("%s/budget-day.%s.dh-strangle.table.html", reportPath, symbol))

webshot2::webshot(
  sprintf(sprintf("%s/budget-day.%s.dh-strangle.table.html", reportPath, symbol)),
  sprintf(sprintf("%s/budget-day.%s.dh-strangle.table.png", reportPath, symbol))
)

for(i in 1:length(indices)){
  eqIndex <- indices[i]
  bdDf <- tibble()
  for(bdi in 1:nrow(budgetDays)){
    d1 <- budgetDays[bdi, 1]
    pDf <- sqlQuery(lcon, sprintf("select top %d * from bhav_index 
                                  where index_name = '%s' 
                                  and time_stamp <= '%s' 
                                  order by time_stamp desc", 
                                  lookback + 1, eqIndex, d1))  
    
    if(nrow(pDf) == 0) next
    pXts <- xts(pDf |> select(-TIME_STAMP) |> mutate(across(all_of(everything()), as.numeric)), pDf$TIME_STAMP)
    pXts$PREV_CLOSE <- stats::lag(pXts$PX_CLOSE, 1)
    pXts$RET <- 100*dailyReturn(pXts$PX_CLOSE)
    pXts$HL <- 100*ifelse(!is.na(pXts$PREV_CLOSE), (pXts$PX_HIGH - pXts$PX_LOW)/pXts$PREV_CLOSE, NA)
    
    pDf2 <- data.frame(pXts)
    pDf2$T <- index(pXts)
    pDf2 <- pDf2[-1,]
    pDf2$BD <- d1
    
    bdDf <- rbind(bdDf, pDf2 |> select(T, RET, HL, BD))
  }
  
  bdDf %>% pivot_longer(cols=-c(T, BD)) %>% {
    ggplot(., aes(x=as.factor(BD), y=value, group=interaction(BD, name))) +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_violin(aes(color=name)) +
      geom_point(data = .|>filter(BD == T), mapping = aes(color=name)) +
      scale_color_viridis_d() +
      labs(x = '', y='returns (%)', color='',
           title = sprintf('%s Budget-day Returns vs. Previous %d-day Daily Returns', eqIndex, lookback),
           subtitle = sprintf("%s:%s", min(.$T), max(.$T)),
           caption = '@StockViz')
  }
  ggsave(sprintf("%s/budget-day.%s.%d.png", reportPath, eqIndex, lookback), width = 16, height = 8, units = "in")
  
  bdDf |> 
    filter(BD == T) |>  
    select(-BD) |> 
    mutate(retOut = RET > 0) |>
  gt() %>%
    tab_header(title = sprintf('%s Budget-day Returns', eqIndex, lookback)) %>%
    tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
    fmt_number(decimals=2) %>%
    sub_missing(missing_text = '') %>%
    opt_stylize(style=5) %>%
    data_color(columns = retOut, target_columns = RET,
               fn = scales::col_factor(palette = c("darkred", "darkgreen"), domain = c(FALSE, TRUE)),
               apply_to = 'text') %>%
    cols_hide(c(retOut)) %>%
    cols_label(T = 'Budget Day',
               RET = 'Returns (%)',
               HL = 'High-Low Range (%)') %>%
    tab_options(table.font.size = '130%') %>%
    gtsave(sprintf("%s/budget-day.%s.%d.table.html", reportPath, eqIndex, lookback))
  
  webshot2::webshot(
    sprintf(sprintf("%s/budget-day.%s.%d.table.html", reportPath, eqIndex, lookback)),
    sprintf(sprintf("%s/budget-day.%s.%d.table.png", reportPath, eqIndex, lookback))
  )
}


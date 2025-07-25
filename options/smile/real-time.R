library('RPostgres')
library('tidyverse')
library('viridis')
library('ggthemes')
library('ggpmisc')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)

symbol <- "NIFTY"
tzone <- "Asia/Kolkata"
asof <- as.POSIXct("2025-07-25", tz=tzone)

dbTs <- c()
for(h in 10:15){
  tradingTs <- as.numeric(as.POSIXct(asof + h*60*60, tz=tzone))
  tickStamp <- dbGetQuery(pgCon, "select min(tick_stamp) from zd_option_greeks where tick_stamp >= $1",
                          params = list(tradingTs))[[1]]
  dbTs <- c(dbTs, tickStamp)
}

expiries <- dbGetQuery(pgCon, "select distinct expiry e from zd_master
                       where name = $1
                       and time_stamp = $2
                       and inst_type <> 'FUT'
                       and exch = 'NFO'
                       order by e",
                       params = list(symbol, asof))[,1]

expiries <- expiries[expiries > (asof + day(2))]

for(eIndex in 1:length(expiries)){
expiry <- expiries[eIndex]
    for(dbTsIndex in 1:length(dbTs)){
      sampleTs <- dbTs[dbTsIndex]
      
      spotPx <- dbGetQuery(pgCon, "select (g.calcs_stddev ->> 'midFut')::real spot
                                      from zd_master m, zd_option_greeks g
                                      where m.inst_token = g.inst_tok
                                      and m.name = $1
                                      and m.time_stamp = $2
                                      and m.expiry = $3
                                      and m.inst_type = 'CE'
                                      and g.tick_stamp = $4
                                    limit 1",
                           params = list(symbol, asof, expiry, sampleTs))[[1]]
      
      ceGreeks <- dbGetQuery(pgCon, "select m.strike, 100*((g.calcs_stddev ->> 'askIv')::real + (g.calcs_stddev ->> 'bidIv')::real)/2 iv
                                      from zd_master m, zd_option_greeks g
                                      where m.inst_token = g.inst_tok
                                      and m.name = $1
                                      and m.time_stamp = $2
                                      and m.expiry = $3
                                      and m.inst_type = 'CE'
                                      and g.tick_stamp = $4",
                             params = list(symbol, asof, expiry, sampleTs))
      
      peGreeks <- dbGetQuery(pgCon, "select m.strike, 100*((g.calcs_stddev ->> 'askIv')::real + (g.calcs_stddev ->> 'bidIv')::real)/2 iv
                                      from zd_master m, zd_option_greeks g
                                      where m.inst_token = g.inst_tok
                                      and m.name = $1
                                      and m.time_stamp = $2
                                      and m.expiry = $3
                                      and m.inst_type = 'PE'
                                      and g.tick_stamp = $4",
                             params = list(symbol, asof, expiry, sampleTs))
      
      greeks <- ceGreeks |> inner_join(peGreeks, join_by(strike), suffix = c(".ce", ".pe")) |>
        mutate(strike_pct = 100*(strike - spotPx)/spotPx) |>
        select(strike_pct, iv.ce, iv.pe)
      
      ceModel <- lm(iv.ce ~ strike_pct + I(strike_pct^2), data = greeks)
      peModel <- lm(iv.pe ~ strike_pct + I(strike_pct^2), data = greeks)
      
      predDf <- data.frame(ceModel$coefficients)
      predDf <- cbind(predDf, peModel$coefficients)
      predDf <- cbind(rownames(predDf), predDf)
      predDf <- rbind(predDf, c('r^2', summary(ceModel)$r.squared, summary(peModel)$r.squared))
      colnames(predDf) <- c("", "ce", "pe")
      predDf[,2] <- round(as.numeric(predDf[,2]), 5)
      predDf[,3] <- round(as.numeric(predDf[,3]), 5)
      
      interpolationPoints <- tibble(strike_pct = seq(-5, 5, by=0.5))
      
      cePred <- tibble(iv = predict(ceModel, interpolationPoints), strike_pct = interpolationPoints$strike_pct)
      pePred <- tibble(iv = predict(peModel, interpolationPoints), strike_pct = interpolationPoints$strike_pct)
      
      preds <- cePred |> inner_join(pePred, join_by(strike_pct), suffix=c(".ce", ".pe"))
      
      toPlot <- greeks |> full_join(preds, join_by(strike_pct), suffix=c(".obv", ".pred")) |> 
        filter(abs(strike_pct) <= 5) |>
        pivot_longer(-strike_pct) |> 
        arrange(strike_pct) |> 
        mutate(sz = factor(if_else(endsWith(name, "obv"), 1, 2))) |>
        drop_na() 
      
      
      ggplot(toPlot, aes(x = strike_pct, y = value, color = name)) +
        theme_economist() +
        geom_line(aes(linewidth = sz, linetype = sz)) +
        scale_color_viridis_d() +
        scale_linewidth_manual(values = c(1, 1.5)) + 
        scale_linetype_manual(values = c('solid', 'dotdash')) + 
        guides(linewidth='none', linetype='none') +
        annotate(geom = "table", x = -1, y = max(toPlot$value), vjust = 1, hjust = 0,
                 label = list(predDf)) +
        labs(x = 'strike (%)', y = 'iv', color = '', 
             title = sprintf('%s %s Option IV', symbol, expiry),
             subtitle = as.POSIXct(sampleTs))
      
      ggsave(sprintf("%s/%s.%s.%d.png", reportPath, symbol, expiry, sampleTs), width = 12, height = 6, units = "in")
    }
}

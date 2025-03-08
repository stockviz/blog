library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('ggrepel')
library('viridis')

options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

print("connecting to us2...")
lconUS2 <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    ldbserver,
    "StockVizUs2",
    ldbuser,
    ldbpassword
  ),
  case = "nochange",
  believeNRows = TRUE
)

stDate1 <- as.Date("2016-11-04")
stDate2 <- as.Date("2024-11-04")

edDate1 <- as.Date("2017-03-07")
edDate2 <- as.Date("2025-03-07")

tickers <- c("SPY", "QQQ")
coins <- c("BTC-USD")

for(sym in tickers){
  pxDf1 <- sqlQuery(lconUS2, sprintf("select time_stamp, c from bhav_eq_td where symbol='%s' and time_stamp >='%s' and time_stamp <='%s' order by time_stamp", sym, stDate1, edDate1))
  pxDf2 <- sqlQuery(lconUS2, sprintf("select time_stamp, c from bhav_eq_td where symbol='%s' and time_stamp >='%s' and time_stamp <='%s' order by time_stamp", sym, stDate2, edDate2))
  
  pXts1 <- xts(pxDf1[,2], pxDf1[,1])
  dret1 <- dailyReturn(pXts1)
  
  pXts2 <- xts(pxDf2[,2], pxDf2[,1])
  dret2 <- dailyReturn(pXts2)
  
  index(dret1) <- seq.Date(as.Date("1970-01-01"), length.out = nrow(dret1), by=1)
  index(dret2) <- seq.Date(as.Date("1970-01-01"), length.out = nrow(dret2), by=1)
  
  cret <- merge(cumprod(dret1 + 1), cumprod(dret2 + 1))
  names(cret) <- c("TRUMP_1", "TRUMP_2")
  cret$d <- seq(1, nrow(cret), by=1)
  
  toPlot <- data.frame(cret) |> pivot_longer(-d)
  toPlot <- toPlot |> mutate(label = if_else(d == max(d), paste0(round(100*(value - 1), 2), '%'), NA))
  ggplot(toPlot, aes(x = d, y=value, color=name)) +
    theme_economist() +
    geom_line(linewidth=1) +
    geom_text_repel(aes(label = label), show.legend = FALSE, color='black') +
    scale_color_viridis_d() +
    labs(x = 'Days from November 4th', y='growth of $1', color='', 
         title = sprintf('%s under Trump', sym), 
         subtitle = sprintf("%s:%s/%s:%s", stDate1, edDate1, stDate2, edDate2),
         caption = '@StockViz')
  
  ggsave(sprintf("%s/%s.returns.png", reportPath, sym), width = 12, height = 6, units = "in" )
}

for(coin in coins){
  pxDf <- read.csv(sprintf("%s/yahoo.%s.csv", reportPath, coin), header = FALSE)
  pxDf <- pxDf |> distinct(V1, V2, .keep_all=TRUE) |> rename('time_stamp' = V1, 'px' = V2) |> mutate(time_stamp = as.Date(time_stamp), px = as.numeric(px))
  
  pXts1 <- pxDf |> filter(time_stamp >= stDate1 & time_stamp <= edDate1) |> as.xts()
  pXts2 <- pxDf |> filter(time_stamp >= stDate2 & time_stamp <= edDate2) |> as.xts()
 
  dret1 <- dailyReturn(pXts1)
  dret2 <- dailyReturn(pXts2)
  
  index(dret1) <- seq.Date(as.Date("1970-01-01"), length.out = nrow(dret1), by=1)
  index(dret2) <- seq.Date(as.Date("1970-01-01"), length.out = nrow(dret2), by=1)
  
  cret <- merge(cumprod(dret1 + 1), cumprod(dret2 + 1))
  names(cret) <- c("TRUMP_1", "TRUMP_2")
  cret$d <- seq(1, nrow(cret), by=1)
  
  toPlot <- data.frame(cret) |> pivot_longer(-d)
  toPlot <- toPlot |> mutate(label = if_else(d == max(d), paste0(round(100*(value - 1), 2), '%'), NA))
  ggplot(toPlot, aes(x = d, y=value, color=name)) +
    theme_economist() +
    geom_line(linewidth=1) +
    geom_text_repel(aes(label = label), show.legend = FALSE, color='black') +
    scale_color_viridis_d() +
    labs(x = 'Days from November 4th', y='growth of $1', color='', 
         title = sprintf('%s under Trump', coin), 
         subtitle = sprintf("%s:%s/%s:%s", stDate1, edDate1, stDate2, edDate2),
         caption = '@StockViz')
  
  ggsave(sprintf("%s/%s.returns.png", reportPath, coin), width = 12, height = 6, units = "in" )
}







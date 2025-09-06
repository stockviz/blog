library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('tidyverse')
library('gtExtras')
library('webshot2')
library("treemapify")
library('ggthemes')

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

maxDt <- sqlQuery(lconUs2, "select max(time_stamp) from bhav_eq_td where symbol='SPY'")[[1]]

etfSymbols <- sqlQuery(lcon, "select symbol, fund, aum from etf_meta where CATEGORY='Absolute Returns'")

etfs <- sqlQuery(lconUs2, sprintf("select symbol, min(time_stamp) minTs, max(time_stamp) maxTs from bhav_eq_td 
                                  where symbol in ('%s') 
                                  group by symbol", 
                                  paste(etfSymbols$symbol, collapse="','")))

etfs <- etfs |> filter(maxTs >= (maxDt - 365) & maxTs == maxDt)
startDate <- min(etfs$minTs)

spyDf <- sqlQuery(lconUs2, sprintf("select c, time_stamp from bhav_eq_td where symbol = 'SPY' and time_stamp >= '%s'", startDate - 10))
spyXts <- xts(spyDf[,1], spyDf[,2])
spyDret <- dailyReturn(spyXts)

retDf <- data.frame(SYMBOL = "", START_DT = "", RET = 0.0, SHARPE = 0.0, SPY_RET = 0.0, SPY_SHARPE = 0.0)
for(i in 1:nrow(etfs)){
  pxDf <- sqlQuery(lconUs2, sprintf("select c, time_stamp from bhav_eq_td 
                                    where symbol = '%s'", etfs$symbol[i]))
  
  pXts <- xts(pxDf[,1], pxDf[,2])
  
  pXts[pXts < 0.01] <- NA
  pXts <- na.trim(pXts, sides='left')
  pXts <- na.fill(pXts, 0)
  dXts <- dailyReturn(pXts)
  dXts <- dXts[-1]
  
  annRet <- Return.annualized(dXts)
  annRetSpy <- Return.annualized(spyDret[paste0(index(dXts)[1],'/')])
  
  sr <- SharpeRatio.annualized(dXts)
  srSpy <- SharpeRatio.annualized(spyDret[paste0(index(dXts)[1],'/')])
  
  retDf <- rbind(retDf, c(etfs$symbol[i], toString(index(dXts)[1]), 
                          as.numeric(annRet), as.numeric(sr),
                          as.numeric(annRetSpy), as.numeric(srSpy)))
}

retDf <- retDf[-1,]
retDf <- retDf |> mutate(START_DT = as.Date(START_DT)) |> 
  mutate(across(-c(SYMBOL, START_DT), ~ as.numeric(.x))) |>
  mutate(across(contains('RET'), ~ . *100))

annDf <- retDf |> 
  inner_join(etfSymbols, join_by(SYMBOL == symbol)) |>
  relocate(c(SYMBOL, fund), 1) |>
  arrange(desc(RET))

print(sum(annDf$aum)/1000000)

ggplot(annDf, aes(area=aum, fill = RET, label = sprintf("%s\n%.2f", SYMBOL, aum/1000000))) +
  theme_economist() +
  scale_fill_continuous(type = "viridis") +
  geom_treemap() +
  geom_treemap_text() +
  guides(fill = 'none') +
  labs(title = 'Alternative ETFs', 
       subtitle = sprintf("AUM ($million); %s", maxDt),
       caption = '@StockViz')

ggsave(sprintf("%s/alternative-etf.aum.png", reportPath), height=10, width=10, units="in")

annDf |> 
  mutate(retOut = RET > SPY_RET, sharpeOut = SHARPE > SPY_SHARPE) |> 
  gt() %>%
  tab_header(title = "Absolute Returns ETFs Performance", subtitle = sprintf('%s:%s', startDate, maxDt)) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  fmt_number(decimals=2) %>%
  opt_stylize(style=5) %>%
  data_color(columns = retOut, target_columns = RET,
             fn = scales::col_factor(palette = c("darkred", "darkgreen"), domain = c(FALSE, TRUE)),
             apply_to = 'text') %>%
  data_color(columns = sharpeOut, target_columns = SHARPE,
             fn = scales::col_factor(palette = c("darkred", "darkgreen"), domain = c(FALSE, TRUE)),
             apply_to = 'text') %>%
  tab_style(locations = cells_body(columns = c(fund, START_DT)), style = cell_text(size='small')) %>%
  text_transform(locations = cells_body(columns = START_DT), 
                 fn = \(x) paste0("<div style='white-space: nowrap;'>", x, "</div>")) %>%
  cols_hide(c(retOut, sharpeOut, aum)) %>%
  cols_label('SYMBOL' = '',
             'fund' = '',
             'START_DT' = 'Since',
             'RET' = 'Returns',
             'SHARPE' = 'Sharpe',
             'SPY_RET' = 'SPY Returns',
             'SPY_SHARPE' = 'SPY Sharpe') %>%
  gtsave(sprintf("%s/alternative-etf.annual.returns.html", reportPath))

webshot2::webshot(
  sprintf("%s/alternative-etf.annual.returns.html", reportPath),
  sprintf("%s/alternative-etf.annual.returns.png", reportPath)
)


###############################



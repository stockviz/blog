library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggrepel')
library('gtExtras')

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

budgetDays <- read.csv(sprintf("%s/days.csv", reportPath), header = FALSE)
budgetDays[,1] <- as.Date(budgetDays[,1])

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
           title = sprintf('%s Budget-day Returns vs. Previous %d-day Returns', eqIndex, lookback),
           subtitle = sprintf("%s:%s", min(.$T), max(.$T)),
           caption = '@StockViz')
  }
  ggsave(sprintf("%s/budget-day.%s.%d.png", reportPath, eqIndex, lookback), width = 16, height = 8, units = "in")
  
  bdDf %>% 
    filter(BD == T) %>% 
    select(-BD) %>% 
    rename('Budget Day' = T, 'Returns (%)' = RET, 'High-Low Range (%)' = HL) %>%
  gt() %>%
    tab_header(title = sprintf('%s Budget-day Returns', eqIndex, lookback)) %>%
    tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
    fmt_number(decimals=2) %>%
    sub_missing(missing_text = '') %>%
    opt_stylize(style=5) %>%
    tab_options(table.font.size = '130%') %>%
    gtsave(sprintf("%s/budget-day.%s.%d.table.html", reportPath, eqIndex, lookback))
  
  webshot2::webshot(
    sprintf(sprintf("%s/budget-day.%s.%d.table.html", reportPath, eqIndex, lookback)),
    sprintf(sprintf("%s/budget-day.%s.%d.table.png", reportPath, eqIndex, lookback))
  )
}


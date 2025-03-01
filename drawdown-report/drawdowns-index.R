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

print("connecting to sweden...")
pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)

startDt <- as.Date("2021-01-01")
endDt <- as.Date("2025-03-01")

refDts <- sqlQuery(lcon, sprintf("select distinct(time_stamp) from bhav_index where time_stamp >= '%s' and time_stamp <= '%s' and index_name = 'nifty 50'", startDt, endDt))[,1]

indices <- c("NIFTY 50", "NIFTY MIDCAP 150", "NIFTY SMALLCAP 250", "NIFTY MICROCAP 250")

retStats <- data.frame(INDEX = "", SYMBOL = "", TOTAL_CUM_RET = 0.0, PEAK_DT = "", PEAK_CUM_RET = 0.0, DD_FROM_PEAK = 0.0, ROUND_TRIP = FALSE) 

for(i in 1:length(indices)){
  iName <- indices[i]
  
  startSymDt <- sqlQuery(lcon, sprintf("select min(time_stamp) from index_const_history where index_name = '%s' and time_stamp >= '%s'", iName, startDt))[[1]]
  startSyms <- sqlQuery(lcon, sprintf("select symbol from index_const_history where index_name = '%s' and time_stamp = '%s'", iName, startSymDt))[,1]
  
  for(j in 1:length(startSyms)){
    sym <- startSyms[j]
    pxDf <- dbGetQuery(pgCon, sprintf("select date_stamp, c from eod_adjusted_nse where ticker = '%s' and date_stamp >= '%s' and date_stamp <= '%s'", sym, startDt, endDt))
    if(nrow(pxDf) < 0.95*length(refDts)){
      print(paste("skipping", sym))
      next
    }
    
    pXts <- xts(pxDf[,2], pxDf[,1])
    
    roundTrip <- FALSE
    if(coredata(first(pXts)) >= coredata(last(pXts))){
      roundTrip <- TRUE
    }
    dXts <- dailyReturn(pXts)
    cumRet <- Return.cumulative(dXts)
    tdds <- table.Drawdowns(dXts)
    if(any(is.na(tdds[, 3]))){
      peakDt <- tdds[is.na(tdds[, 3]), 1]
      peakRet <- Return.cumulative(dXts[paste0("/", peakDt)])
      ddepth <- tdds[is.na(tdds[, 3]), 4]
      retStats <- rbind(retStats, c(iName, sym, as.numeric(cumRet), as.character(peakDt), as.numeric(peakRet), as.numeric(ddepth), roundTrip))
    } else {
      retStats <- rbind(retStats, c(iName, sym, as.numeric(cumRet), NA, NA, NA, roundTrip))
    }
  }
}

retStats <- retStats[-1,]
retStats$PEAK_DT <- as.Date(retStats$PEAK_DT)
retStats$TOTAL_CUM_RET <- as.numeric(retStats$TOTAL_CUM_RET)
retStats$PEAK_CUM_RET <- as.numeric(retStats$PEAK_CUM_RET)
retStats$DD_FROM_PEAK <- as.numeric(retStats$DD_FROM_PEAK)
retStats$ROUND_TRIP <- as.logical(retStats$ROUND_TRIP)

toPlotMin <- retStats |> 
  select(-DD_FROM_PEAK) |> 
  filter(PEAK_DT >= as.Date("2024-01-01") & PEAK_CUM_RET < 5) |> 
  select(-c(SYMBOL, PEAK_DT, ROUND_TRIP)) |> 
  pivot_longer(-INDEX)

toPlotMax <- retStats |> 
  select(-DD_FROM_PEAK) |> 
  filter(PEAK_DT >= as.Date("2024-01-01") & PEAK_CUM_RET >= 5) |> 
  select(-c(SYMBOL, PEAK_DT, ROUND_TRIP)) |> 
  pivot_longer(-INDEX)

toPlotMinDD <- retStats |> 
  filter(PEAK_DT >= as.Date("2024-01-01") & PEAK_CUM_RET < 5) |> 
  mutate(DD_FROM_PEAK = abs(DD_FROM_PEAK)) |>
  select(INDEX, DD_FROM_PEAK)

toPlotMaxDD <- retStats |> 
  filter(PEAK_DT >= as.Date("2024-01-01") & PEAK_CUM_RET >= 5) |> 
  mutate(DD_FROM_PEAK = abs(DD_FROM_PEAK)) |>
  select(INDEX, DD_FROM_PEAK)

summMin <- retStats |> filter(PEAK_DT >= as.Date("2024-01-01") & PEAK_CUM_RET < 5) |> 
  group_by(INDEX) |> 
  summarise(MED_TOTAL = 100*median(TOTAL_CUM_RET), MED_PEAK = 100*median(PEAK_CUM_RET), MED_DD = -100*median(DD_FROM_PEAK))

summMax <- retStats |> filter(PEAK_DT >= as.Date("2024-01-01") & PEAK_CUM_RET >= 5) |> 
  group_by(INDEX) |> 
  summarise(MED_TOTAL = 100*median(TOTAL_CUM_RET), MED_PEAK = 100*median(PEAK_CUM_RET), MED_DD = -100*median(DD_FROM_PEAK))

summMin |> inner_join(summMax, join_by(INDEX)) |>
  gt() %>%
  tab_header(title = "Index Constituent Median Peak vs. Total Cumulative Returns", subtitle = sprintf('%s:%s', startDt, endDt)) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  cols_label(MED_TOTAL.x ~ 'Total',
             MED_PEAK.x ~ 'Peak',
             MED_DD.x ~ 'DrawDown',
             MED_TOTAL.y ~ 'Total',
             MED_PEAK.y ~ 'Peak',
             MED_DD.y ~ 'DrawDown',
             INDEX ~ '') %>%
  tab_spanner(label = md('**Less than 500%**'), columns = 2:4) %>%
  tab_spanner(label = md('**Over 500%**'), columns = 5:7) %>%
  fmt_number(decimals=2) %>%
  sub_missing(missing_text = '') %>%
  opt_stylize(style=5) %>%
  cols_width(INDEX ~ pct(30)) %>%
  gtsave(sprintf("%s/index-constituent.cum-returns.html", reportPath))

webshot2::webshot(
  sprintf("%s/index-constituent.cum-returns.html", reportPath),
  sprintf("%s/index-constituent.cum-returns.png", reportPath)
)

#######################################################

retStats |> group_by(INDEX) |> summarise(RT = sum(ROUND_TRIP)) |>
  gt() %>%
  tab_header(title = "Index Constituent Round-trips", subtitle = sprintf('%s:%s', startDt, endDt)) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  cols_label(everything() ~ '') %>%
  opt_stylize(style=5) %>%
  gtsave(sprintf("%s/index-constituent.roundtrip.html", reportPath))

webshot2::webshot(
  sprintf("%s/index-constituent.roundtrip.html", reportPath),
  sprintf("%s/index-constituent.roundtrip.png", reportPath)
)

#######################################################


p1 <- ggplot(toPlotMin, aes(x=INDEX, y=value*100, group=interaction(INDEX, name), color=name)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_violin(position = position_dodge(0.9)) +
  stat_summary(fun = median, geom = "point", position = position_dodge(0.9)) +
  scale_color_viridis_d() +
  labs(x = '', y='%', color='', title = 'Cumulative Returns')

p2 <- ggplot(toPlotMinDD, aes(x=INDEX, y=DD_FROM_PEAK*100)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_violin(position = position_dodge(0.9)) +
  stat_summary(fun = median, geom = "point", position = position_dodge(0.9)) +
  scale_color_viridis_d() +
  labs(x = '', y='%', color='', title = 'Drawdowns')

p1/p2 + plot_annotation(title = "Index Constituents",
                        subtitle = sprintf("%s:%s", startDt, endDt),
                        theme = theme_economist(),
                        caption = '@StockViz')

ggsave(sprintf("%s/index-constituent.non-parabolic.png", reportPath), width = 10, height = 12, units = "in")

###############################################

p1 <- ggplot(toPlotMax, aes(x=INDEX, y=value*100, group=interaction(INDEX, name), color=name)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_violin(position = position_dodge(0.9)) +
  stat_summary(fun = median, geom = "point", position = position_dodge(0.9)) +
  scale_color_viridis_d() +
  labs(x = '', y='%', color='', title = 'Cumulative Returns')

p2 <- ggplot(toPlotMaxDD, aes(x=INDEX, y=DD_FROM_PEAK*100)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_violin(position = position_dodge(0.9)) +
  stat_summary(fun = median, geom = "point", position = position_dodge(0.9)) +
  scale_color_viridis_d() +
  labs(x = '', y='%', color='', title = 'Drawdowns')

p1/p2 + plot_annotation(title = "Index Constituents",
                        subtitle = sprintf("%s:%s", startDt, endDt),
                        theme = theme_economist(),
                        caption = '@StockViz')

ggsave(sprintf("%s/index-constituent.parabolic.png", reportPath), width = 10, height = 12, units = "in")
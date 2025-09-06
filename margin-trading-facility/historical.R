library('RODBC')
library('quantmod')
library('lubridate')
library('tidyverse')
library("treemapify")
library('ggthemes')
library('gtExtras')
library('webshot2')
library('ggrepel')

pdf(NULL)
options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), 
  case = "nochange", believeNRows = TRUE
)

mtfDates <- sqlQuery(lcon, "select distinct TIME_STAMP from MTF_REPORT")

mtfMe <- mtfDates |> 
  mutate(YM = 100*year(TIME_STAMP) + month(TIME_STAMP)) |>
  group_by(YM) |> 
  summarise(ME = max(TIME_STAMP))

stDt <- min(mtfMe$ME)
edDt <- max(mtfMe$ME)

miscDates <- sqlQuery(lcon, sprintf("select distinct TIME_STAMP from EQUITY_MISC_INFO where time_stamp >= '%s' and time_stamp <= '%s'", stDt, edDt))

miscMe <- miscDates |> mutate(YM = 100*year(TIME_STAMP) + month(TIME_STAMP)) |>
  group_by(YM) |> 
  summarise(ME = max(TIME_STAMP))


sampleDates <- mtfMe |> left_join(miscMe, join_by(YM)) |> 
  group_by(YM) |>
  summarise(ME = min(ME.x, ME.y)) 

mtfInfo <- tibble()
for(i in 1:nrow(sampleDates)){
  dataDf <- sqlQuery(lcon, sprintf("select a.TIME_STAMP, a.SYMBOL, TOT_FINANCED_LAKHS, FF_MKT_CAP_CR 
                         from MTF_REPORT a, EQUITY_MISC_INFO b
                         where a.time_stamp = b.time_stamp
                         and a.symbol = b.symbol
                         and a.time_stamp = '%s'", sampleDates$ME[i]))
  
  mtfInfo <- rbind(mtfInfo, dataDf)
}

mtfPct <- mtfInfo |> mutate(FF = if_else(TIME_STAMP >= '2024-03-01', FF_MKT_CAP_CR * 100, FF_MKT_CAP_CR), FF_PCT = 100*TOT_FINANCED_LAKHS/(100*FF))


mtfHighest <- mtfPct |> filter(!is.infinite(FF_PCT)) |> 
  group_by(TIME_STAMP) |> 
  mutate(FF_RANK = rank(desc(FF_PCT))) |> 
  ungroup() |>
  filter(FF_RANK == 1)

mtfHighest |> select(TIME_STAMP, SYMBOL, FF_PCT) |> mutate(retOut = FF_PCT < 10) |>
  gt() %>%
  tab_header(title = "Max Financed vs. Free Float (%)", subtitle = sprintf('end of month; %s:%s', stDt, edDt)) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  fmt_number(decimals=2) %>%
  fmt_date(columns = TIME_STAMP, date_style = 'yMMM') %>%
  opt_stylize(style=5) %>%
  data_color(columns = retOut, target_columns = FF_PCT,
             fn = scales::col_factor(palette = c("darkred", "darkgreen"), domain = c(FALSE, TRUE)),
             apply_to = 'text') %>%
  cols_hide(c(retOut)) %>%
  cols_label(everything() ~ '') %>%
  gtsave(sprintf("%s/mtf.fin-vs-ff.html", reportPath))

webshot2::webshot(
  sprintf("%s/mtf.fin-vs-ff.html", reportPath),
  sprintf("%s/mtf.fin-vs-ff.png", reportPath)
)

#######################


mtfGrowth <- mtfInfo |> group_by(TIME_STAMP) |> 
  summarise(TOT = sum(TOT_FINANCED_LAKHS)/100) |>
  ungroup() |>
  mutate(G = 100*(TOT - lag(TOT))/lag(TOT))


ggplot(mtfGrowth, aes(x=TIME_STAMP, y = TOT)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line() +
  geom_text_repel(aes(label = sprintf('%+.2f', G)), color = ifelse(mtfGrowth$G < 0, 'darkred', 'darkgreen')) +
  scale_x_date(date_breaks = '6 months', date_labels = '%Y-%b') +
  labs(x='', y = 'Total Financed (Rs. Crores)',
       title = 'Total Financed under MTF',
       subtitle = sprintf('end of month; %s:%s', stDt, edDt),
       caption = '@StockViz')

ggsave(sprintf("%s/mtf.total-fin.png", reportPath), width = 12, height = 6, units = "in")

#######################

mtfPct |> filter(TIME_STAMP == max(TIME_STAMP) & FF_PCT > 5) |> arrange(desc(FF_PCT)) |>
  select(SYMBOL, FF_PCT) |>
  gt() %>%
  tab_header(title = "Percentage of Freefloat Margined", subtitle = sprintf('%s', edDt)) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  fmt_number(decimals=2) %>%
  opt_stylize(style=5) %>%
  cols_label(everything() ~ '') %>%
  gtsave(sprintf("%s/mtf.ff-margined.html", reportPath))

webshot2::webshot(
  sprintf("%s/mtf.ff-margined.html", reportPath),
  sprintf("%s/mtf.ff-margined.png", reportPath)
)
  
  
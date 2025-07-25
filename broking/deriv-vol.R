library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('viridis')
library('patchwork')

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

startDate <- as.Date("2018-01-01")
endDate <- as.Date("2025-07-18")

valLakhBreakDate <- as.Date("2024-05-03")

bnDf <- sqlQuery(lcon, sprintf("select time_stamp, sum(CONTRACTS) CONT, sum(VAL_IN_LAKH) VAL 
                               from bhav_eq_opt 
                               where symbol in ('BANKNIFTY', 'NIFTY') 
                               and time_stamp >= '%s'
                               and time_stamp <= '%s'
                               group by time_stamp", startDate, endDate))

bnDf2 <- sqlQuery(lcon, sprintf("select time_stamp, sum(CONTRACTS) CONT, sum(VAL_IN_LAKH) VAL 
                               from bhav_eq_fut 
                               where symbol in ('BANKNIFTY', 'NIFTY') 
                               and time_stamp >= '%s'
                               and time_stamp <= '%s'
                               group by time_stamp", startDate, endDate))


bnWkly <- bnDf |> 
  mutate(VAL = if_else(time_stamp >= valLakhBreakDate, VAL/100000, VAL)) |>
  mutate(WY = year(time_stamp)*100 + month(time_stamp)) |>
  group_by(WY) |> 
  summarize(TC = sum(CONT)/1000000, TV = sum(VAL)/1000000000) |>
  arrange(WY) |>
  mutate(FILL = as.factor(as.integer(WY/100)), WY = as.factor(WY))

bnWkly2 <- bnDf2 |> 
  mutate(VAL = if_else(time_stamp >= valLakhBreakDate, VAL/100000, VAL)) |>
  mutate(WY = year(time_stamp)*100 + month(time_stamp)) |>
  group_by(WY) |> 
  summarize(TC = sum(CONT)/1000000, TV = sum(VAL)/1000000000) |>
  arrange(WY) |>
  mutate(FILL = as.factor(as.integer(WY/100)), WY = as.factor(WY))

p1 <- ggplot(bnWkly, aes(x=WY, y=TC, fill = FILL)) +
  theme_economist() +
  theme(axis.text.x=element_blank()) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  labs(x = "", y = "count (millions)", fill='year', title='Options', subtitle = "number of contracts traded")

p2 <- ggplot(bnWkly, aes(x=WY, y=TV, fill = FILL)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  guides(fill='none') +
  labs(x = "week", y = "billion lakhs", fill='year', title='Options', subtitle = "total value traded")

p3 <- ggplot(bnWkly2, aes(x=WY, y=TC, fill = FILL)) +
  theme_economist() +
  theme(axis.text.x=element_blank()) +
  geom_bar(stat = "identity") +
  guides(fill='none') +
  scale_fill_viridis_d() +
  labs(x = "", y = "count (millions)", fill='year', title='Futures', subtitle = "number of contracts traded")

p4 <- ggplot(bnWkly2, aes(x=WY, y=TV, fill = FILL)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  guides(fill='none') +
  labs(x = "week", y = "billion lakhs", fill='year', title='Futures', subtitle = "total value traded")

(p1 / p2 / p3 / p4) + 
  plot_layout(widths = c(4, 1)) + 
  plot_annotation(title = "NIFTY & BANKNIFTY", 
                  subtitle = sprintf("%s:%s", startDate, endDate), 
                  caption = '@StockViz',
                  theme = theme_economist())

ggsave(sprintf("%s/deriv.vol.png", reportPath), width=12, height=6*4, units="in")


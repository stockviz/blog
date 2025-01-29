library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')

source("/mnt/hollandC/StockViz/R/config.r")

reportPath <- "."

options("scipen" = 100)
options(stringsAsFactors = FALSE)

lcon <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    ldbserver,
    "crypto",
    ldbuser,
    ldbpassword
  ),
  case = "nochange",
  believeNRows = TRUE
)

pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)

tiTicker <- 'ethusd'
hypeTicker <- 'eth'

pDfHype <- sqlQuery(lcon, sprintf("select DATE_STAMP, HR, PX_CLOSE from PX_HISTORY_HYPE_HIST 
                                    where coin = '%s'", hypeTicker))

startDate <- min(pDfHype$DATE_STAMP)
endDate <- max(pDfHype$DATE_STAMP)

pDfTi <- dbGetQuery(pgCon, "select time_stamp, px_close from tiingo_crypto_usd_5min_ts 
             where curr_code = $1 and time_stamp >= $2 and time_stamp <= $3",
                  params = list(tiTicker, startDate, endDate))

pXtsTi <- xts(pDfTi[,-1], pDfTi[,1])
pXtsTi <- to.period(pXtsTi, 'hours')[,4]
names(pXtsTi) <- c('TIINGO')

pDfTi2 <- data.frame(pXtsTi)
pDfTi2$T <- index(pXtsTi)

pDf <- pDfTi2 |> mutate(DATE_STAMP = as.Date(T), HR = hour(T)) |> 
  select(-T) |> 
  inner_join(pDfHype, by=join_by(DATE_STAMP, HR)) |> 
  rename('HYPE' = 'PX_CLOSE') |>
  mutate(DIFF = 100*(HYPE/TIINGO - 1))

summary(pDf$DIFF)

ggplot(pDf, aes(x=DATE_STAMP, y=DIFF, group=DATE_STAMP)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  stat_summary(fun = mean, fun.min = min, fun.max = max, shape = 3) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%b") +
  labs(x = '', y='diff (%)', title = sprintf('$%s Hyperliquid vs. TIINGO data', toupper(hypeTicker)),
       subtitle = sprintf("%s:%s", min(pDf$DATE_STAMP), max(pDf$DATE_STAMP)), 
       caption = '@StockViz')

ggsave(sprintf("%s/%s.diff.png", reportPath, hypeTicker), width = 16, height = 8, units = "in")

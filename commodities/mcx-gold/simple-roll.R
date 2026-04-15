library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", 
                                     ldbserver, 'StockViz', ldbuser, ldbpassword), 
                             case = "nochange", 
                             believeNRows = TRUE)

etfTicker <- "GOLDBEES"
mcxContract <- "GOLDM"

gldDf <- dbGetQuery(pgCon, "select c, date_stamp from eod_adjusted_nse where ticker=$1", params=list(etfTicker))
gldXts <- xts(gldDf[,1], gldDf[,2])
gldRet <- dailyReturn(gldXts)
names(gldRet) <- c(etfTicker)

mcxDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp, expiry from bhav_com_mcx 
                               where contract='%s' 
                               and OTYPE in ('FUTCOM', 'XX')", 
                               mcxContract))

mcxDfRanked <- mcxDf |> group_by(time_stamp) |> mutate(exp_series = rank(expiry)) |> ungroup() |> arrange(time_stamp)
expiries <- unique(mcxDf$expiry)

mcxPx0 <- mcxDfRanked |> filter(exp_series == 1)
mcxPx1Exp <- mcxDfRanked |> filter(exp_series == 2) |> filter(time_stamp %in% expiries)

mcxRet <- mcxPx0 |> left_join(mcxPx1Exp, join_by(time_stamp)) |> 
  mutate(ret = px_close.x/lag(px_close.x) - 1, 
         ret_adj = if_else(!is.na(lag(px_close.y)), px_close.x/lag(px_close.y) - 1, px_close.x/lag(px_close.x) - 1)) |>
  select(time_stamp, ret, ret_adj)

mcxRollover <- mcxDfRanked |> filter(exp_series == 1 | exp_series == 2) |>
  filter(time_stamp %in% expiries) |>
  pivot_wider(id_cols = time_stamp, names_from = exp_series, values_from = px_close, names_prefix = "exp") |>
  mutate(roll_cost = exp2/exp1 - 1) |>
  drop_na() |>
  select(time_stamp, roll_cost)

ggplot(mcxRollover, aes(x=time_stamp, y = roll_cost*100)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line() +
    scale_x_date(date_breaks = "12 months", date_labels = "%Y-%b") +
    labs(x = '', y='roll cost(%)', 
         title = sprintf('%s Rollover Cost', mcxContract),
         subtitle = sprintf("%s:%s", min(mcxRollover$time_stamp), max(mcxRollover$time_stamp)),
         caption = '@StockViz')

ggsave(
  sprintf("%s/%s.roll_cost.png", reportPath, mcxContract),
  width = 12,
  height = 6,
  units = "in"
)

mcxRetXts <- xts(mcxRet |> select(-time_stamp), mcxRet$time_stamp)
names(mcxRetXts) <- c("FRONT_MTH_FUT", "FRONT_MTH_FUT_ADJ")

toPlot <- mcxRetXts
toPlot <- na.omit(toPlot)

Common.PlotCumReturns(toPlot, "Futures Buy & Hold Gold Returns", mcxContract, #NULL)
                      sprintf("%s/gold.fut.cum.png", reportPath))

toPlot <- merge(gldRet, mcxRetXts$FRONT_MTH_FUT_ADJ)
toPlot <- na.omit(toPlot)

Common.PlotCumReturns(toPlot, "ETF vs. Futures Buy & Hold Gold Returns", "", #NULL)
                      sprintf("%s/gold.etf-fut.cum.png", reportPath))






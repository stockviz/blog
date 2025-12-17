library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggrepel')

options("scipen" = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

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
lconUS <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    ldbserver,
    "StockVizUs",
    ldbuser,
    ldbpassword
  ),
  case = "nochange",
  believeNRows = TRUE
)

oecdDf <- sqlQuery(lconUS, "select * from oecd_cli")
oecdMaxDts <- oecdDf |> 
  group_by(AREA, YR, MTH) |> 
  summarise(UT = max(UPDATE_DT))

oecdLatest <- oecdDf |> inner_join(oecdMaxDts, join_by(AREA, YR, MTH, UPDATE_DT == UT))
oecdLatest <- oecdLatest |> mutate(YM = as.Date(sprintf("%d-%d-%d", YR, MTH, days_in_month(as.Date(sprintf("%d-%d-01", YR, MTH))))))

p1 <- oecdLatest |> 
  filter(AREA %in% c('G7', 'IND')) |> 
  select(AREA, VAL, YM) |>
  ggplot(aes(x=YM, y = VAL, color = AREA)) +
    theme_economist() +
    geom_line(linewidth = 1) +
    scale_color_viridis_d() +
    labs(x='', y='OECD CLI', subtitle = "w/ outliers")

p2 <- oecdLatest |> 
  filter(AREA %in% c('G7', 'IND') & VAL > 90) |> 
  select(AREA, VAL, YM) |>
  ggplot(aes(x=YM, y = VAL, color = AREA)) +
  theme_economist() +
  geom_line(linewidth = 1) +
  scale_color_viridis_d() +
  guides(color='none') +
  labs(x='', y='OECD CLI', subtitle = "w/o outliers")

p1/p2 + plot_layout(axes = 'collect') +
  plot_annotation(title = "OECD Composite Leading Indicator",
                  subtitle = sprintf("%s:%s", min(oecdLatest$YM), max(oecdLatest$YM)),
                  caption = '@StockViz',
                  theme = theme_economist())

ggsave(
  sprintf("%s/india-g7.oecd-cli.png", reportPath),
  width = 16,
  height = 16,
  units = "in"
)

indCLI <- oecdLatest |> 
  filter(AREA %in% c('G7', 'IND')) |> 
  select(AREA, VAL, YM) |>
  pivot_wider(id_cols=YM, names_from=AREA, values_from = VAL) |>
  filter((!is.na(G7) | cumsum(!is.na(G7)) > 0) & (!is.na(IND) | cumsum(!is.na(IND)) > 0))

startDate <- min(indCLI$YM) - 30

niftyDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='NIFTY 50' and time_stamp >= '%s'", startDate))

pXts <- xts(niftyDf[,2], niftyDf[,1])
mRet <- monthlyReturn(pXts)
index(mRet) <- as.Date(sprintf("%d-%d-%d", year(index(mRet)), month(index(mRet)), days_in_month(index(mRet))))
names(mRet) <- c('RET')

indCLIXts <- xts(indCLI |> select(-YM), indCLI$YM)

allXts <- merge(indCLIXts, mRet)
allXts$RET_1 <- stats::lag(allXts$RET, -1)

allXts$IND_DIFF <- allXts$IND - 100
allXts$IND_ACCEL <- allXts$IND - stats::lag(allXts$IND, 1)

p1 <- as_tibble(allXts) |> select(IND_DIFF, RET_1) |>
  mutate(RET_1 = RET_1*100) |>
  ggplot(aes(x = RET_1, y = IND_DIFF)) +
    theme_economist() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 'dashed', color='darkgrey') +
    geom_vline(xintercept = 0, linetype = 'dashed', color='darkgrey') +
    labs(x='NIFTY 50 Returns (%)', y = 'CLI - 100', subtitle = "w/ outliers") 

p2 <- as_tibble(allXts) |> select(IND_DIFF, RET_1) |>
  filter(IND_DIFF > -5) |>
  mutate(RET_1 = RET_1*100) |>
  ggplot(aes(x = RET_1, y = IND_DIFF)) +
  theme_economist() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 'dashed', color='darkgrey') +
  geom_vline(xintercept = 0, linetype = 'dashed', color='darkgrey') +
  labs(x='NIFTY 50 Returns (%)', y = 'CLI - 100', subtitle = "w/o outliers") 

p1/p2 + plot_layout(axes = 'collect') +
        plot_annotation(title = "India OECD CLI vs Next Month NIFTY 50 Returns",
                  subtitle = sprintf("%s:%s", first(index(allXts)), last(index(allXts))),
                  caption = '@StockViz',
                  theme = theme_economist())

ggsave(
  sprintf("%s/india-cli.vs.nifty50.png", reportPath),
  width = 16,
  height = 16,
  units = "in"
)

p1 <- as_tibble(allXts) |> select(IND_ACCEL, RET_1) |>
  mutate(RET_1 = RET_1*100) |>
  ggplot(aes(x = RET_1, y = IND_ACCEL)) +
  theme_economist() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 'dashed', color='darkgrey') +
  geom_vline(xintercept = 0, linetype = 'dashed', color='darkgrey') +
  labs(x='NIFTY 50 Returns (%)', y = 'CLI MoM diff', subtitle = "w/ outliers") 

p2 <- as_tibble(allXts) |> select(IND_ACCEL, RET_1) |>
  filter(abs(IND_ACCEL) < 1) |>
  mutate(RET_1 = RET_1*100) |>
  ggplot(aes(x = RET_1, y = IND_ACCEL)) +
  theme_economist() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 'dashed', color='darkgrey') +
  geom_vline(xintercept = 0, linetype = 'dashed', color='darkgrey') +
  labs(x='NIFTY 50 Returns (%)', y = 'CLI MoM diff', subtitle = "w/o outliers") 

p1/p2 + plot_layout(axes = 'collect') +
  plot_annotation(title = "India OECD CLI MoM vs Next Month NIFTY 50 Returns",
                  subtitle = sprintf("%s:%s", first(index(allXts)), last(index(allXts))),
                  caption = '@StockViz',
                  theme = theme_economist())

ggsave(
  sprintf("%s/india-cli-diff.vs.nifty50.png", reportPath),
  width = 16,
  height = 16,
  units = "in"
)

scen1 <- ifelse(allXts$IND >= allXts$G7, allXts$RET_1, 0)
toPlot <- na.omit(merge(scen1, allXts$RET_1))
names(toPlot) <- c('IND > G7', 'BH')
Common.PlotCumReturns(toPlot, 
                      "When India grows more than the G7", 
                      "NIFTY 50", #NULL)
                      sprintf("%s/india.g7.png", reportPath))

scen2 <- ifelse(allXts$IND >= 100, allXts$RET_1, 0)
scen3 <- ifelse(allXts$IND >= 80, allXts$RET_1, 0)
toPlot <- na.omit(merge(scen2, scen3, allXts$RET_1))
names(toPlot) <- c('CLI_100', 'CLI_800', 'BH')
Common.PlotCumReturns(toPlot, 
                      "India CLI", 
                      "NIFTY 50", #NULL)
                      sprintf("%s/india.80.100.png", reportPath))

scen4 <- ifelse(allXts$IND_ACCEL > 0, allXts$RET_1, 0)
toPlot <- na.omit(merge(scen4, allXts$RET_1))
names(toPlot) <- c('IND_ACCEL', 'BH')
Common.PlotCumReturns(toPlot, 
                      "When India CLI is Accelerating", 
                      "NIFTY 50", #NULL)
                      sprintf("%s/india.accel.png", reportPath))

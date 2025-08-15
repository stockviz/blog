library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('viridis')
library('patchwork')

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

startDate <- as.Date("2024-01-01")

iDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from vix_history where time_stamp >= '%s'", startDate))

vDf <- sqlQuery(lconUs2, sprintf("select time_stamp, expiry, px_settle val
                                 from bhav_cboe_fut 
                                 where contract = 'VX' 
                                 and duration = 'M' 
                                 and time_stamp >= '%s' 
                                 order by time_stamp, expiry", startDate))

vDf <- vDf |> group_by(time_stamp) |> mutate(color = rank(expiry) + 1)

sDf <- sqlQuery(lconUs2, sprintf("select time_stamp, px_close val
                                 from cboe_vix
                                 where time_stamp >= '%s'", startDate))

sDf$expiry <- NA
sDf$color <- 1

toPlot <- rbind(vDf, sDf)

p1 <- ggplot(toPlot, aes(x=time_stamp, y=val, color= factor(color))) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(aes(linewidth = color)) +
  scale_x_date(date_labels='%Y-%m', date_breaks = '1 month') +
  scale_linewidth(range=c(1.1, 0.25), transform='log') +
  scale_color_viridis_d() +
  guides(color='none', linewidth='none') +
  labs(x='', y='spot/futures', title='CBOE VIX + futures')
  

p2 <- ggplot(iDf, aes(x=time_stamp, y=px_close)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line() +
  scale_x_date(date_labels='%Y-%m', date_breaks = '1 month') +
  labs(x='', y='spot', title='India VIX', caption = '@StockViz')

p1/p2

ggsave(
  sprintf("%s/us-india-vix.png", reportPath),
  width = 12,
  height = 12,
  units = "in"
)
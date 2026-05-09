library('RODBC')
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

lconUS2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", 
                                  ldbserver, 'StockVizUS2', ldbuser, ldbpassword), 
                          case = "nochange", 
                          believeNRows = TRUE)


symbols <- c("INDA", "FXI")

drets <- NULL
arets <- NULL
for(symbol in symbols){
  pDf <- sqlQuery(lconUS2, sprintf("select C, TIME_STAMP from TIINGO_DATA where TICKER = '%s'", symbol))
  pXts <- xts(pDf[,1], pDf[,2])
  drets <- merge.xts(drets, dailyReturn(pXts))
  arets <- merge.xts(arets, annualReturn(pXts))
}
names(drets) <- symbols
names(arets) <- symbols

toPlot <- na.omit(drets)
yr <- year(first(index(toPlot)))
toPlot <- toPlot[paste0(yr+1, "/"),]
toPlot[1,] <- 0.0

sr <- paste(round(SharpeRatio.annualized(toPlot), 2), collapse = "/")

Common.PlotCumReturns(toPlot, sprintf("%s ($)", paste(symbols, collapse="/")), sprintf("sharpe: %s", sr), #NULL)
                      sprintf("%s/%s.cum.png", reportPath, paste(symbols, collapse="-")))


toPlot <- arets[paste0(yr+1, "/"),]
tpDf <- data.frame(toPlot)
tpDf$Y <- year(index(toPlot))

ggplot(tpDf |> pivot_longer(cols=-Y), aes(x=Y, y=value*100, fill=name)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  geom_bar(stat = "identity", position = "dodge2") +
  scale_x_continuous(breaks = unique(c(seq(first(tpDf$Y), last(tpDf$Y), by = 2), last(tpDf$Y)))) +
  scale_fill_viridis_d() +
  labs(x='', y='(%)', fill='',
       title = sprintf("%s Annual Returns ($)", paste(symbols, collapse="/")),
       subtitle = sprintf("%d:%d", first(tpDf$Y), last(tpDf$Y)))

ggsave(sprintf("%s/%s.ann.png", reportPath, paste(symbols, collapse="-")), width = 12, height = 6, units="in")
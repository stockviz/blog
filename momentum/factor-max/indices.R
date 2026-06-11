source("common.R")

factorPx <- NULL
for(iName in factorIndices){
  pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s'",
                                iName, factorStDt))
  
  factorPx <- merge.xts(factorPx, xts(pDf[,2], pDf[,1]))
}
names(factorPx) <- factorIndices

factorDailyRet <- do.call(merge.xts, lapply(1:ncol(factorPx), \(x) dailyReturn(factorPx[,x])))
names(factorDailyRet) <- factorIndices

factorMonthlyRet <- do.call(merge.xts, lapply(1:ncol(factorPx), \(x) monthlyReturn(factorPx[,x])))
names(factorMonthlyRet) <- factorIndices

factorDates <- index(factorMonthlyRet)

#max daily return (factor MAX)

activeFactorMaxDt <- tibble()
factorMax <- NULL
for(i in 2:(length(factorDates)-1)){
  sDt <- factorDates[i-1]
  eDt <- factorDates[i] - 1 #exclude the last date

  fdrSubset <- factorDailyRet[paste0(sDt, '/', eDt), ]
  factorIndex <- arrayInd(which.max(fdrSubset), dim(fdrSubset))[2]
  maxFactorName <- factorIndices[factorIndex]
  
  factorMax <- rbind(factorMax, xts(matrix(c(as.numeric(factorMonthlyRet[i+1, maxFactorName]), factorIndex), nrow=1), factorDates[i+1]))
  activeFactorMaxDt <- rbind(activeFactorMaxDt, c(as.character(factorDates[i+1]), maxFactorName))
}

names(activeFactorMaxDt) <- c("MONTH", "ACTIVE_FACTOR")
activeFactorMaxDt$MONTH <- as.Date(activeFactorMaxDt$MONTH)
names(factorMax) <- c("FACTOR_MAX", "FACTOR_INDEX")

factorMax$T <- ifelse(factorMax$FACTOR_INDEX == stats::lag(factorMax$FACTOR_INDEX, 1), 0, 1)
factorMax$FACTOR_RET <- ifelse(factorMax$T != 0, factorMax$FACTOR_MAX-drag, factorMax$FACTOR_MAX)

toPlot <- na.omit(merge.xts(factorMax$FACTOR_RET, benchMonthyRet))
names(toPlot) <- c("FACTOR_MAX", indexBench)
sharpe <- SharpeRatio.annualized(toPlot)
print(sharpe)

Common.PlotCumReturns(toPlot, "NIFTY 500 Factor MAX", 
                      sprintf("SR: %s", paste(round(sharpe,2), collapse="/")), #NULL)
                      sprintf("%s/factor-MAX.%s.png", reportPath, indexBench), NULL)


#max monthly return - (factor momentum)

activeFactorMonthlyDt <- tibble()
factorMonthly <- NULL
for(i in 2:length(factorDates)){
  sDt <- factorDates[i-1]
  eDt <- factorDates[i]
  
  factorIndex <- which.max(factorMonthlyRet[sDt, ])
  maxFactorName <- factorIndices[factorIndex]
  
  factorMonthly <- rbind(factorMonthly, xts(matrix(c(as.numeric(factorMonthlyRet[eDt, maxFactorName]), factorIndex), nrow=1), eDt))
  activeFactorMonthlyDt <- rbind(activeFactorMonthlyDt, c(as.character(eDt), maxFactorName))
}

names(activeFactorMonthlyDt) <- c("MONTH", "ACTIVE_FACTOR")
activeFactorMonthlyDt$MONTH <- as.Date(activeFactorMonthlyDt$MONTH)
names(factorMonthly) <- c("FACTOR_MONTHLY", "FACTOR_INDEX")

factorMonthly$T <- ifelse(factorMonthly$FACTOR_INDEX == stats::lag(factorMonthly$FACTOR_INDEX, 1), 0, 1)
factorMonthly$FACTOR_RET <- ifelse(factorMonthly$T != 0, factorMonthly$FACTOR_MONTHLY-drag, factorMonthly$FACTOR_MONTHLY)

toPlot <- na.omit(merge.xts(factorMonthly$FACTOR_RET, benchMonthyRet))
names(toPlot) <- c("FACTOR_MONTHLY", indexBench)
sharpe <- SharpeRatio.annualized(toPlot)
print(sharpe)

Common.PlotCumReturns(toPlot, "NIFTY 500 Factor Monthly Momentum", 
                      sprintf("SR: %s", paste(round(sharpe,2), collapse="/")), #NULL)
                      sprintf("%s/factor-monthly.%s.png", reportPath, indexBench), NULL)


#combined 

toPlot <- na.omit(merge.xts(factorMax$FACTOR_RET, factorMonthly$FACTOR_RET, benchMonthyRet))
names(toPlot) <- c("FACTOR_MAX", "FACTOR_MONTHLY", indexBench)
sharpe <- SharpeRatio.annualized(toPlot)
print(sharpe)

Common.PlotCumReturns(toPlot, "NIFTY 500 Factor MAX vs. Monthly Momentum", 
                      sprintf("SR: %s", paste(round(sharpe,2), collapse="/")), #NULL)
                      sprintf("%s/factor-monthly-max.%s.png", reportPath, indexBench), NULL)

#active factor

activeFactor <- activeFactorMaxDt |> inner_join(activeFactorMonthlyDt, join_by(MONTH)) |>
  pivot_longer(cols = -MONTH) |>
  mutate(name = if_else(name == "ACTIVE_FACTOR.x", "FACTOR MAX", "FACTOR MOMENTUM"), value = str_remove(value, "NIFTY500 "))

ggplot(activeFactor, aes(x=MONTH, y = value, color = name)) + 
  theme_economist() +
  geom_point(position = position_dodge(width = 0.5)) +
  scale_color_viridis_d() +
  labs(x='', y = '', color = '',
       title = "NIFTY500 Factor MAX vs. Factor Momentum",
       subtitle = sprintf("%s:%s", min(activeFactorMaxDt$MONTH), max(activeFactorMaxDt$MONTH)),
       caption = "@StockViz")

ggsave(sprintf("%s/factor-active.%s.png", reportPath, indexBench), 
       width = 10,
       height = 5,
       units = "in")

htmlFileName = sprintf("%s/factor-active.table.%s.html", reportPath, indexBench)
pngFileName = sprintf("%s/factor-active.table.%s.png", reportPath, indexBench)

activeFactor |> group_by(name, value) |> 
  summarise(CNT = format(round(100*n()/nrow(activeFactorMaxDt), 2), nsmall = 2)) |>
  gt(groupname_col = "name") %>%
  tab_header(title = "NIFTY500 Active Factors", subtitle = sprintf('%s:%s', min(activeFactorMaxDt$MONTH), max(activeFactorMaxDt$MONTH))) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  tab_style(
    style = list(cell_fill(color = "lightblue"), cell_text(size = "large")),
    locations = cells_row_groups()
  ) %>%
  cols_label(value = '', CNT = 'active(%)') %>%
  opt_stylize(style=5) %>%
  gtsave(htmlFileName)

webshot2::webshot(htmlFileName, pngFileName, selector = "table.gt_table", expand = c(10, 10, 10, 10))
  
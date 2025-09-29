source("/mnt/hollandC/StockViz/R/config.r")

reportPath<-"."

library('RODBC')
library('quantmod')
library('tidyverse')
library('ggthemes')
library('ggpmisc')
library('patchwork')
library('viridis')

pdf(NULL)
options(stringsAsFactors = FALSE)
options("scipen"=100)

predLb <- 500 
statLb <- 20
predLf <- 20

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexDf <- sqlQuery(lcon, "select px_close, time_stamp from VIX_HISTORY order by time_stamp")
indexXts <- xts(indexDf[,1], indexDf[,2])

indexXts <- merge(indexXts, rollapply(indexXts, statLb, mean), rollapply(indexXts, statLb, sd))
indexXts <- na.omit(indexXts)

names(indexXts) <- c('INDEX', 'MEAN', 'STD_DEV')

laggedIndex <- do.call(merge, lapply(1:predLf, \(x) stats::lag(indexXts$INDEX, -x)))
laggedMean <- do.call(merge, lapply(1:predLf, \(x) stats::lag(indexXts$MEAN, -x)))
laggedStdDev <- do.call(merge, lapply(1:predLf, \(x) stats::lag(indexXts$STD_DEV, -x)))
locfXts <- do.call(merge, lapply(1:predLf, \(x) indexXts$INDEX))

names(laggedMean) <- unlist(sapply(1:predLf, \(x) paste0('MEAN_', x)))
names(laggedStdDev) <- unlist(sapply(1:predLf, \(x) paste0('SD_', x)))

meanXts <- na.omit(merge(indexXts$MEAN, laggedMean))
stdDevXts <- na.omit(merge(indexXts$STD_DEV, laggedStdDev))

meanPredXts <- rollapply(meanXts, predLb, by.column = FALSE, FUN = \(x){
  predXts <- NULL
  for(l in 1:predLf){
    fit <- lm(formula(paste0('MEAN_', l, ' ~ MEAN')), data = head(x, predLb-1))
    predVal <- xts(predict(fit, xts::last(x)), index(xts::last(x)))
    predXts <- merge.xts(predXts, predVal)
  }
  predXts
})
names(meanPredXts) <- unlist(sapply(1:predLf, \(x) paste0('MEAN_PRED_', x)))

stdDevPredXts <- rollapply(stdDevXts, predLb, by.column = FALSE, FUN = \(x){
  predXts <- NULL
  for(l in 1:predLf){
    fit <- lm(formula(paste0('SD_', l, ' ~ STD_DEV')), data = head(x, predLb-1))
    predVal <- xts(predict(fit, xts::last(x)), index(xts::last(x)))
    predXts <- merge.xts(predXts, predVal)
  }
  predXts
})
names(stdDevPredXts) <- unlist(sapply(1:predLf, \(x) paste0('SD_PRED_', x)))

vixPredXts <- do.call(merge.xts, lapply(1:predLf, \(x) {
  sqrt(meanPredXts[, paste0('MEAN_PRED_', x)]^2 + stdDevPredXts[, paste0('SD_PRED_', x)]^2)
}))

meanRmseXts <- xts(sqrt(rowMeans(laggedMean - meanPredXts)^2), index(meanPredXts))
stdDevRmseXts <- xts(sqrt(rowMeans(laggedStdDev - stdDevPredXts)^2), index(stdDevPredXts))
vixPredRmseXts <- xts(sqrt(rowMeans(laggedIndex - vixPredXts)^2), index(vixPredXts))
vixLocfRmseXts <- xts(sqrt(rowMeans(laggedIndex - locfXts)^2), index(locfXts))


vixPredAllXts <- na.omit(merge(vixPredRmseXts, vixLocfRmseXts))
errorDt <- tibble(data.frame(vixPredAllXts))
names(errorDt) <- c('rmsePred', 'rmseLast')
errorDt$dt <- index(vixPredAllXts)

diffStat1 <- summary(errorDt$rmsePred)
diffStat1 <- data.frame(stat=names(diffStat1), value=matrix(diffStat1))
diffStat1[,2] <- round(diffStat1[,2], 2)
colnames(diffStat1) <- c("", "vvix")

diffStat2 <- summary(errorDt$rmseLast)
diffStat2 <- data.frame(stat=names(diffStat2), value=matrix(diffStat2))
diffStat2[,2] <- round(diffStat2[,2], 2)
colnames(diffStat2) <- c("", "last")

diffStat <- cbind(diffStat1, diffStat2[,2])
colnames(diffStat) <- c("", "vvix", "last")

errorDt |> filter(rmsePred < (median(rmsePred) + 3*sd(rmsePred)) 
                  & rmseLast < median((rmseLast) + 3*sd(rmseLast))) |> 
  pivot_longer(-dt) |>
    ggplot(aes(x = value, color = name)) +
    theme_economist() +
    scale_color_viridis_d() +
    stat_density(linewidth = 1.25, geom="line", position="identity") +
    labs(x="Forecast Error", y="", color='',
         title=sprintf("INDIA VIX %d-day Forecast RMSE", predLf), 
         subtitle = sprintf("%s:%s", min(errorDt$dt), max(errorDt$dt))) +
    annotate("table", x=10, y=0.4, vjust='top', hjust='left', label=list(diffStat), cex=4)

ggsave(sprintf("%s/vix-forecast.error.png", reportPath), width=12, height=6, units="in")

p1 <- indexDf |> filter(time_stamp >= as.Date('2012-01-01')) |>
  ggplot(aes(x = time_stamp, y = px_close)) + 
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line() +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(x = '', y = 'VIX')

p2 <- errorDt |> 
  filter(dt >= as.Date('2012-01-01')) |> 
  pivot_longer(-dt) |>
  ggplot(aes(x = dt, y = value, color = name)) + 
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_viridis_d() +
  geom_point(size = 0.5, alpha=0.5) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(x = '', y = 'RMSE', color='')

p1 / p2 + plot_layout(axes = "collect") + 
  plot_annotation(title = 'India VIX', 
                  theme = theme_economist(),
                  caption = '@StockViz')

ggsave(sprintf("%s/vix-vs-error.png", reportPath), width=12, height=12, units="in")

for(i in 1:predLf){
  toPlotXts <- na.omit(merge(laggedIndex[,i], vixPredXts[, i]))
  toPlot <- data.frame(toPlotXts)
  colnames(toPlot) <- c("INDEX", "PRED")
  toPlot$T <- index(toPlotXts)
  
  toPlot |> pivot_longer(-T) |>
    ggplot(aes(x=T, y=value, color=name)) +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_color_viridis_d() +
      geom_line() +
      scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
      labs(x = '', y = '', color='',
           title = sprintf('India VIX vs. %d-day Prediction', i),
           subtitle = sprintf("%s:%s", min(toPlot$T), max(toPlot$T)),
           caption = '@StockViz')
  
  ggsave(sprintf("%s/vix.%d-day.pred.png", reportPath, i), width=12, height=6, units="in")
}
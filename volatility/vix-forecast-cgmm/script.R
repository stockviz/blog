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

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexDf <- sqlQuery(lcon, "select px_close, time_stamp from VIX_HISTORY order by time_stamp")

predsDf <- read.csv(sprintf("%s/cgmm-vix-prediction.csv", reportPath), header = FALSE)
numCols <- ncol(predsDf)
numPreds <- numCols - 2

errorDt <- tibble()
for(i in 1:nrow(predsDf)){
  dt <- as.Date(predsDf[i,1])
  predVec <- unlist(predsDf[i, 3:numCols], use.names = FALSE)
  
  lastVal <- (indexDf |> filter(time_stamp == dt) |> select(px_close))[[1]]
  
  actuals <- indexDf |> filter(time_stamp > dt)
  if(nrow(actuals) < numPreds) next
  
  actualVec <- (actuals |> 
    slice_min(time_stamp, n = numPreds) |>
    select(px_close))[,1]
  
  rmsePred <- sqrt(mean((actualVec - predVec)^2))
  rmseLast <- sqrt(mean((actualVec - rep(lastVal, numPreds))^2))
  errorDt <- rbind(errorDt, tibble(dt, rmsePred, rmseLast))
}

diffStat1 <- summary(errorDt$rmsePred)
diffStat1 <- data.frame(stat=names(diffStat1), value=matrix(diffStat1))
diffStat1[,2] <- round(diffStat1[,2], 2)
colnames(diffStat1) <- c("", "cgmm")

diffStat2 <- summary(errorDt$rmseLast)
diffStat2 <- data.frame(stat=names(diffStat2), value=matrix(diffStat2))
diffStat2[,2] <- round(diffStat2[,2], 2)
colnames(diffStat2) <- c("", "last")

diffStat <- cbind(diffStat1, diffStat2[,2])
colnames(diffStat) <- c("", "cgmm", "last")

errorDt |> filter(rmsePred < (median(rmsePred) + 3*sd(rmsePred)) 
                  & rmseLast < median((rmseLast) + 3*sd(rmseLast))) |> 
  pivot_longer(-dt) |>
    ggplot(aes(x = value, color = name)) +
    theme_economist() +
    scale_color_viridis_d() +
    stat_density(linewidth = 1.25, geom="line", position="identity") +
    labs(x="Forecast Error", y="", color='',
         title="INDIA VIX 20-day Forecast RMSE", 
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
            geom_point() +
            scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
            labs(x = '', y = 'RMSE', color='')

p1 / p2 + plot_layout(axes = "collect") + 
          plot_annotation(title = 'India VIX', 
                          theme = theme_economist(),
                          caption = '@StockViz')

ggsave(sprintf("%s/vix-vs-error.png", reportPath), width=12, height=12, units="in")

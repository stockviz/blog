library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('gtExtras')
library('webshot2')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
#reportPath <- "D:/StockViz/public/blog/gaps"
source("/mnt/hollandC/StockViz/R/config.r")


lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("2000-01-01")
endDate<-as.Date("2020-01-01")
preOpen<-as.Date("2010-10-18")

niftyDf<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_OPEN, PX_CLOSE from BHAV_INDEX where index_name='NIFTY 50' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
niftyXts<-xts(niftyDf[,-1], as.Date(niftyDf[,1]))

niftyXts<-merge(niftyXts, stats::lag(niftyXts$PX_CLOSE, 1))

names(niftyXts)<-c('O', 'C', 'PC')

niftyXts$C2O<-niftyXts$O-niftyXts$PC
niftyXts$C2O_PCT<-100*niftyXts$C2O/niftyXts$PC

niftyXts<-na.omit(niftyXts)

preSumm <- data.frame(summary(niftyXts[paste0("/", preOpen - 1)]$C2O_PCT)[,2])
summNames <- gsub(":", "", str_extract(preSumm[,1], ".*:"))
preSumm <- trimws(str_remove(preSumm[,1], ".*:"))

postSumm <- data.frame(summary(niftyXts[paste0(preOpen + 1, "/")]$C2O_PCT)[,2])
postSumm <- trimws(str_remove(postSumm[,1], ".*:"))

gapSumm <- data.frame(cbind(summNames, preSumm, postSumm))
colnames(gapSumm) <- c("stat", "Pre", "Post")
gapSumm[,2] <- round(as.numeric(gapSumm[,2]), 2)
gapSumm[,3] <- round(as.numeric(gapSumm[,3]), 2)

niftyTb <- tibble(data.frame(niftyXts))
niftyTb$T <- index(niftyXts)

ggplot(niftyTb, aes(x=T, y=C2O_PCT)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat='identity') +
  geom_vline(xintercept = preOpen, color = 'darkred', linetype = "dashed") +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y-%m') +
  labs(x = '', y='close-open gap (%)', title = 'NIFTY 50 previous close to open gap (%)', 
       subtitle = sprintf("%s:%s", min(niftyTb$T), max(niftyTb$T)),
       caption = '@StockViz')
  
ggsave(sprintf("%s/nifty.pre-vs-post-open.png", reportPath), width = 12, height = 6, units = "in")

gapSumm %>%
  gt() %>%
  tab_header(title = "NIFTY 50 previous close to open gap (%)", subtitle = sprintf('%s:%s', min(niftyTb$T), max(niftyTb$T))) %>%
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>")) %>%
  fmt_number(decimals=2) %>%
  opt_stylize(style=5) %>%
  tab_options(table.font.size = '130%') %>%
  gtsave(sprintf("%s/nifty.pre-vs-post-open.stats.html", reportPath))

webshot2::webshot(
  sprintf("%s/nifty.pre-vs-post-open.stats.html", reportPath),
  sprintf("%s/nifty.pre-vs-post-open.stats.png", reportPath)
)
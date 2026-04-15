library('RODBC')
library("RSQLite")
library("DBI") 
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggrepel')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

contractRoots <- c('CL', 'GC')

sampleDates <- c(as.Date("2026-01-02"), as.Date("2026-02-02"), as.Date("2026-03-02"), as.Date("2026-03-27"))

for(cRoot in contractRoots){
  wmCon <- dbConnect(RSQLite::SQLite(), "/mnt/tb4/stockviz/yahoo_nymex_futures.db", flags = RSQLite::SQLITE_RO)
  
  futCurve <- NULL
  for(i in 1:length(sampleDates)){
    dt <- as.Date(sampleDates[i])
    nymexDf <- dbGetQuery(wmCon, "select expiry_date, close 
                             from futures_eod 
                             where root=$1 
                             and date=$2
                             order by expiry_date", 
                             params = list(cRoot, as.character(dt)))
  
    if(is.null(futCurve)){
      futCurve <- nymexDf |> filter(!is.na(expiry_date)) |> mutate(EXPIRY_SERIES = rank(expiry_date)) |> select(-expiry_date)
    } else {
      futCurve <- futCurve |> 
        inner_join(nymexDf |> filter(!is.na(expiry_date)) |> mutate(EXPIRY_SERIES = rank(expiry_date)) |> select(-expiry_date), 
                   join_by(EXPIRY_SERIES))
    }
  }
  
  futCurve <- futCurve |> rename_with(.cols = starts_with('close'), .fn = ~sapply(sampleDates, \(x) strftime(x, "%Y%m%d")))
  dbDisconnect(wmCon)
  
  futCurve |> pivot_longer(cols=-EXPIRY_SERIES) |>
    ggplot(aes(x=EXPIRY_SERIES, y = value, color = name)) +
      theme_economist() +
      geom_line(linewidth = 1) +
      scale_color_viridis_d() +
      labs(x = 'expiry series', y = '$', 
           color = '',
           title = sprintf('NYMEX %s Futures', cRoot), 
           caption = '@StockViz')
  
  ggsave(
    sprintf("%s/nymex-%s.png", reportPath, cRoot),
    width = 12,
    height = 6,
    units = "in"
  )
}
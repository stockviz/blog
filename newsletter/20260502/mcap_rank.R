library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

options("scipen"=100)
options(stringsAsFactors = FALSE)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

minInDt <- sqlQuery(lcon, "select min(time_stamp) from DECILE_CONSTITUENTS")[[1]]
minInMcap1 <- sqlQuery(lcon, sprintf("select symbol from DECILE_CONSTITUENTS where decile=0 and time_stamp = '%s'", minInDt))[,1]

maxUsDt <- sqlQuery(lconUs2, "select max(time_stamp) from SEC_SINGLE_SECURITY_METRICS")[[1]]
maxUsMcapRanks <- sqlQuery(lconUs2, sprintf("select ticker, avg(mcap_rank) mcap from SEC_SINGLE_SECURITY_METRICS 
                                       where time_stamp >= '%4d-%2d-01'
                                       and security = 'STOCK'
                                       group by ticker", year(maxUsDt), month(maxUsDt)))

maxUsMcap1 <- (maxUsMcapRanks |> mutate(mcap = round(mcap, 0)) |> filter(mcap == 10) |> select(ticker))[,1]


maxInDt <- sqlQuery(lcon, sprintf("select max(time_stamp) from DECILE_CONSTITUENTS where time_stamp <= '%s'", maxUsDt))[[1]]
maxInMcap1 <- sqlQuery(lcon, sprintf("select symbol from DECILE_CONSTITUENTS where decile=0 and time_stamp = '%s'", maxInDt))[,1]

minUsDt <- sqlQuery(lconUs2, sprintf("select max(time_stamp) from SEC_SINGLE_SECURITY_METRICS where time_stamp <= '%s'", minInDt))[[1]]
minUsMcapRanks <- sqlQuery(lconUs2, sprintf("select ticker, avg(mcap_rank) mcap from SEC_SINGLE_SECURITY_METRICS 
                                       where time_stamp >= '%4d-%2d-01'
                                       and time_stamp < '%4d-%2d-01'
                                       and security = 'STOCK'
                                       group by ticker", 
                                            year(minUsDt), month(minUsDt),
                                            year(minUsDt + 25), month(minUsDt + 25)))

minUsMcap1 <- (minUsMcapRanks |> mutate(mcap = round(mcap, 0)) |> filter(mcap == 10) |> select(ticker))[,1]

inOut <- setdiff(minInMcap1, maxInMcap1)
usOut <- setdiff(minUsMcap1, maxUsMcap1)

inOutPct <- 100*length(inOut)/length(minInMcap1)
usOutPct <- 100*length(usOut)/length(minUsMcap1)

maxAllInSyms <- sqlQuery(lcon, sprintf("select distinct symbol from px_history where series in ('eq', 'be') and time_stamp = '%s'", maxInDt))[,1]
maxAllUsSyms <- sqlQuery(lconUs2, sprintf("select ticker from SEC_SINGLE_SECURITY_METRICS where security = 'STOCK' and time_stamp = '%s'", maxUsDt))[,1]

inMovedPct <- 100*length(intersect(inOut, maxAllInSyms))/length(inOut)
usMovedPct <- 100*length(intersect(usOut, maxAllUsSyms))/length(usOut)



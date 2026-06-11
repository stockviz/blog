library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggrepel')
library('gtExtras')
library('webshot2')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")
source("/mnt/data/blog/common/theme.returns.common.r")

drag <- 0.2/100

print("connecting to azure...")
rcon <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    dbserver,
    "StockViz",
    dbuser,
    dbpassword
  ),
  case = "nochange",
  believeNRows = TRUE
)

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

indexBench <- "NIFTY 500 TR"
factorIndices <- c("NIFTY500 LOW VOLATILITY 50 TR", "NIFTY500 MOMENTUM 50 TR", "NIFTY500 QUALITY 50 TR", "NIFTY500 VALUE 50 TR")
modelIds <- c("2df79fd7-64f4-47f2-9f53-6b1ace8012a3", 
              "90BEDCEA-FF82-43E7-A20C-020CCB29E328", 
              "4E4C54CB-FA44-4A73-B4E4-94E4AB646660",
              "9EABD2F0-392A-4D89-A665-EF6576A7D471",
              "04EA5378-8A3E-478E-A6AE-02BF173A14C8",
              "0AA77403-63BA-4EDF-9F31-6E19D8F83B83",
              "D6663194-A965-4262-84F6-BB553B0B8998",
              "6CAECA7A-A2C9-4B9D-BD0B-397FCE9597D2")

factorDts <- sqlQuery(lcon, sprintf("select index_name, min(time_stamp) sdt, max(time_stamp) edt from bhav_index
                                    where index_name in ('%s')
                                    group by index_name",
                                    paste(factorIndices, collapse="','")))

factorStDt <- max(factorDts$sdt)

if(!all(factorDts$edt == factorDts$edt[1])) {
  error("factor indices don't have the same end date")
  q()
}

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s'",
                              indexBench, factorStDt))

pXts <- xts(pDf[,2], pDf[,1])
if(index(xts::last(pXts)) != factorDts$edt[1]){
  error("benchmark index doesn't have the same end date")
  q()
}
benchMonthyRet <- monthlyReturn(pXts)

factorStDt <- max(index(pXts)[1], factorStDt)


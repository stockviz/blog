library('RODBC')
options("scipen" = 100)
options(stringsAsFactors = FALSE)
source("/mnt/hollandC/StockViz/R/config.r")

lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

# 1. What CONTRACT values look like our candidates?
cand <- c("GOLD", "SILVER", "CRUDEOIL", "NATURALGAS", "COPPER")
q <- sprintf("select distinct CONTRACT, OTYPE, EXPIRY_SERIES, count(*) as n,
              min(TIME_STAMP) as mn, max(TIME_STAMP) as mx
              from BHAV_COM_MCX
              where CONTRACT in ('%s')
              group by CONTRACT, OTYPE, EXPIRY_SERIES
              order by CONTRACT, OTYPE, EXPIRY_SERIES", paste(cand, collapse = "','"))
res <- sqlQuery(lcon, q)
cat("== distinct CONTRACT/OTYPE/EXPIRY_SERIES for candidates ==\n")
print(res, row.names = FALSE)

# 2. All distinct CONTRACT values containing candidate substrings
q2 <- "select distinct CONTRACT from BHAV_COM_MCX where CONTRACT like '%GOLD%' or CONTRACT like '%SILVER%' or CONTRACT like '%CRUDEOIL%' or CONTRACT like '%NATURALGAS%' or CONTRACT like '%COPPER%' order by CONTRACT"
res2 <- sqlQuery(lcon, q2)
cat("\n== distinct CONTRACT matching substrings ==\n")
print(res2, row.names = FALSE)

# 3. All distinct OTYPE values in the table (sample)
q3 <- "select distinct OTYPE from BHAV_COM_MCX"
res3 <- sqlQuery(lcon, q3)
cat("\n== distinct OTYPE ==\n")
print(res3, row.names = FALSE)

odbcClose(lcon)

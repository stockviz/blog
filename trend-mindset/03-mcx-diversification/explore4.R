library('RODBC')
options("scipen" = 100)
options(stringsAsFactors = FALSE)
source("/mnt/hollandC/StockViz/R/config.r")
lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)
r <- sqlQuery(lcon, "select index_name, min(time_stamp) mn, max(time_stamp) mx, count(*) n
  from bhav_index where index_name in ('NIFTY 50 TR','NIFTY MIDCAP SELECT TR','NIFTY 50','NIFTY MIDCAP SELECT')
  group by index_name order by index_name")
print(r, row.names = FALSE)
odbcClose(lcon)

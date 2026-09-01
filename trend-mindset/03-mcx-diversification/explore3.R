library('RODBC')
options("scipen" = 100)
options(stringsAsFactors = FALSE)
source("/mnt/hollandC/StockViz/R/config.r")

lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

df <- sqlQuery(lcon, "select expiry, expiry_series, time_stamp, px_close from BHAV_COM_MCX
     where contract='CRUDEOIL' and OTYPE in ('FUTCOM','XX') order by time_stamp")
df$time_stamp <- as.Date(df$time_stamp)
fr <- df[df$expiry_series == 0, ]
fr <- fr[order(fr$time_stamp), ]
dr <- fr$px_close / c(NA, head(fr$px_close, -1)) - 1
big <- which(abs(dr) > 0.15)
cat("CRUDEOIL front |ret|>15% days:\n")
for (i in big) {
  j <- (i-2):(i+2); j <- j[j >= 1 & j <= nrow(fr)]
  cat("---- around", format(fr$time_stamp[i]), "ret=", sprintf("%.3f", dr[i]), "----\n")
  print(data.frame(date=fr$time_stamp[j], px=fr$px_close[j], expiry=fr$expiry[j], series=fr$expiry_series[j]))
}
odbcClose(lcon)

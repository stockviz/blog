library('RODBC')
library('tidyverse')
options("scipen" = 100)
options(stringsAsFactors = FALSE)
source("/mnt/hollandC/StockViz/R/config.r")

lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

cands <- c("GOLD", "SILVER", "CRUDEOIL", "NATURALGAS", "COPPER")
preEnd <- as.Date("2019-12-31")
postStart <- as.Date("2020-05-01")

for (cm in cands) {
  cat("\n================ ", cm, " ================\n")
  df <- sqlQuery(lcon, sprintf(
    "select expiry, expiry_series, time_stamp, px_close from BHAV_COM_MCX
     where contract='%s' and OTYPE in ('FUTCOM','XX') order by time_stamp", cm))
  df$time_stamp <- as.Date(df$time_stamp)
  df$expiry <- as.Date(df$expiry)

  cat("rows:", nrow(df), "  px<=0:", sum(df$px_close <= 0, na.rm=TRUE),
      "  NA px:", sum(is.na(df$px_close)), "\n")

  # FUTCOM vs XX split
  ft <- df %>% filter(expiry_series == 0)
  cat("front (series 0) rows:", nrow(ft), "\n")

  # price level at transition: last 3 FUTCOM-era vs first 3 XX-era (series 0)
  futcom <- ft %>% filter(time_stamp <= as.Date("2017-10-13")) %>% tail(3)
  xx     <- ft %>% filter(time_stamp >= as.Date("2017-10-16")) %>% head(3)
  cat("-- FUTCOM tail (series 0) --\n"); print(as.data.frame(futcom[,c("time_stamp","px_close","expiry")]))
  cat("-- XX head (series 0) --\n"); print(as.data.frame(xx[,c("time_stamp","px_close","expiry")]))

  # window observation counts (front series 0, XX era and full)
  xxser0 <- df %>% filter(expiry_series == 0)
  cnt_pre  <- sum(xxser0$time_stamp <= preEnd)
  cnt_post <- sum(xxser0$time_stamp >= postStart)
  cat("series0 obs pre(<=2019-12-31):", cnt_pre, "  post(>=2020-05-01):", cnt_post, "\n")

  # stale-price runs on front series (consecutive equal closes)
  fr <- xxser0 %>% arrange(time_stamp)
  r <- diff(fr$px_close)
  zero <- r == 0 | is.na(r)
  runs <- rle(zero)
  maxrun <- max(runs$lengths[runs$values], 0)
  nzero <- sum(zero, na.rm = TRUE)
  cat("zero-return days:", nzero, "/", length(r), "  max stale run:", maxrun, "\n")

  # daily return magnitude sanity
  dr <- fr$px_close / lag(fr$px_close) - 1
  dr <- dr[is.finite(dr)]
  big <- sum(abs(dr) > 0.15, na.rm = TRUE)
  cat("|ret|>15% days:", big, "  max|ret|:", sprintf("%.3f", max(abs(dr), na.rm=TRUE)), "\n")

  # verify expiry_series=0 == min(expiry) per day (sample first 200 days of XX era)
  xxera <- df %>% filter(time_stamp >= as.Date("2017-10-16"))
  chk <- xxera %>% group_by(time_stamp) %>%
    summarise(s0_exp = expiry[expiry_series==0][1], min_exp = min(expiry), n_series0 = sum(expiry_series==0)) %>%
    ungroup()
  mismatch <- sum(chk$s0_exp != chk$min_exp, na.rm=TRUE)
  multi <- sum(chk$n_series0 > 1, na.rm=TRUE)
  cat("XX-era days:", nrow(chk), "  series0!=min(expiry) days:", mismatch, "  days with >1 series0:", multi, "\n")
}

odbcClose(lcon)

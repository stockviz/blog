#!/usr/bin/env Rscript
# EMA timing, drawdown decomposition, and exposure sizing.
# Uses validated daily series from the completed turbulence data audit.
options(stringsAsFactors = FALSE, scipen = 100)
OUT <- dirname(normalizePath(commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1], mustWork = FALSE))
if (!nzchar(OUT) || OUT == ".") OUT <- getwd()
DRAG <- 0.0025
PRE_END <- as.Date("2019-12-31"); POST_START <- as.Date("2020-05-01")
read_series <- function(path, price_col) {
  x <- read.csv(path, stringsAsFactors = FALSE)
  d <- as.Date(x$date); p <- as.numeric(x[[price_col]])
  ok <- is.finite(p) & p > 0
  d <- d[ok]; p <- p[ok]
  r <- c(NA_real_, p[-1] / p[-length(p)] - 1)
  data.frame(date=d, price=p, ret=r)
}
metrics <- function(r) {
  r <- r[is.finite(r)]; n <- length(r)
  if (n < 2) return(c(N=n, CAGR=NA, Vol=NA, Sharpe=NA, MaxDD=NA, Turnover=NA))
  eq <- cumprod(1+r); years <- n/252
  dd <- eq/cummax(eq)-1
  c(N=n, CAGR=eq[n]^(1/years)-1, Vol=sd(r)*sqrt(252), Sharpe=mean(r)/sd(r)*sqrt(252), MaxDD=-min(dd), Turnover=NA)
}
run_one <- function(s, ema_n, sizing) {
  n <- nrow(s); ema <- rep(NA_real_, n)
  for (i in seq_len(n)) if (is.finite(s$price[i])) ema[i] <- if (i < ema_n) NA else mean(s$price[(i-ema_n+1):i])
  desired <- ifelse(is.finite(ema), as.numeric(s$price > ema), 0)
  pos <- c(0, desired[-n])
  if (sizing == "vol") {
    rv <- rep(NA_real_, n)
    for (i in seq_len(n)) if (i >= 21) rv[i] <- sd(s$ret[(i-20):i], na.rm=TRUE)*sqrt(252)
    scale <- pmin(1, 0.15/pmax(rv, 1e-8)); scale[!is.finite(scale)] <- 1
    pos <- pos * c(1, scale[-n])
  }
  turnover <- abs(pos-c(0,pos[-n])); net <- pos*s$ret - DRAG*turnover
  net[!is.finite(net)] <- 0
  data.frame(date=s$date, price=s$price, ret=s$ret, ema=ema, position=pos, turnover=turnover, net=net, sizing=sizing)
}
all_metrics <- list(); all_daily <- list()
for (nm in c("NIFTY","SELECT")) {
  f <- if (nm == "NIFTY") "/mnt/data/blog/turbulence/simple/daily_NIFTY.csv" else "/mnt/data/blog/turbulence/simple/daily_SELECT.csv"
  col <- if (nm == "NIFTY") "price" else "price"
  s <- read_series(f, col)
  # daily files contain strategy-ready price; duplicate timestamps are not expected
  for (lb in c(20L, 50L, 100L)) for (sz in c("unit","vol")) {
    z <- run_one(s, lb, sz); z$instrument <- nm; z$ema_lb <- lb
    all_daily[[length(all_daily)+1L]] <- z
    for (w in c("pre","post","full")) {
      keep <- if (w=="pre") z$date <= PRE_END else if (w=="post") z$date >= POST_START else rep(TRUE,nrow(z))
      m <- metrics(z$net[keep]); m["Turnover"] <- mean(z$turnover[keep], na.rm=TRUE)
      all_metrics[[length(all_metrics)+1L]] <- data.frame(Window=w,Instrument=nm,EMA=lb,Sizing=sz,t(m),row.names=NULL)
    }
  }
}
md <- do.call(rbind, all_metrics); dy <- do.call(rbind, all_daily)
write.csv(md, file.path(OUT,"metrics.csv"), row.names=FALSE)
write.csv(dy, file.path(OUT,"daily_outputs.csv"), row.names=FALSE)
cat(sprintf("Wrote %d metric rows and %d daily rows; drag=%.4f\n", nrow(md), nrow(dy), DRAG))

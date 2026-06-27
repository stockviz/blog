library('RODBC')
library('RPostgres')
library('tidyverse')
library('quantmod')
library('PerformanceAnalytics')
library('xts')

source("/mnt/hollandC/StockViz/R/config.r")
source("strategy_nine.R")
source("/mnt/data/blog/common/plot.common.r")

options("scipen" = 100)

reportPath <- "."
drag <- 0.2 / 100

# ── parameters ────────────────────────────────────────────────────────────────
# min_daily_bars: minimum daily bars required for a coin to be considered.
#   - Must be >= 256 (the Strategy Nine warm-up period for EWMAC64).
#   - Higher values (500–1000) ensure the EWMA filters are well-converged and
#     the Sharpe estimate is stable, but reduce the available coin universe and
#     delay the backtest start.
#   - Lower values (100–250) include more coins sooner but risk selecting on
#     noise due to insufficient history.
#   - Default 500 balances universe size with statistical reliability.
args <- commandArgs(trailingOnly = TRUE)
min_daily_bars <- if (length(args) >= 1) as.integer(args[1]) else 500
if (is.na(min_daily_bars) || min_daily_bars < 256) {
  stop("min_daily_bars must be >= 256 (Strategy Nine warm-up)")
}
# Sharpe lookback: use the same window as min_daily_bars for consistency.
# Require at least half the window to have valid returns before computing Sharpe.
sharpe_lookback <- min_daily_bars
min_sharpe_rows  <- floor(min_daily_bars / 2)

# min_coins: minimum coins that must pass selection before trading begins.
#   - 5 is a reasonable floor to avoid single-coin concentration risk while
#     still starting early enough to capture meaningful history.
min_coins <- 5

cat(sprintf("  lookback = %d days, Sharpe window = %d, min Sharpe rows = %d, min coins = %d\n",
            min_daily_bars, sharpe_lookback, min_sharpe_rows, min_coins))

# ── connect ───────────────────────────────────────────────────────────────────
pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden', user = ldbuser2, password = ldbpassword2,
  dbname = 'StockVizDyn', sslmode = 'allow'
)

daily_prices <- list()
daily_rets   <- list()

# ── walk-forward backtest ─────────────────────────────────────────────────────
print("running walk-forward...")
selection_log <- tibble()
portfolio_rets <- xts()
started <- FALSE

# generate month-ends from earliest available data to today
earliest_ts <- dbGetQuery(pgCon,
  "select min(close_time) from binance_crypto_historical_1h")[[1]]
earliest_date <- as.Date(as.POSIXct(earliest_ts / 1000, origin = "1970-01-01", tz = "UTC"))
first_month_start <- as.Date(format(earliest_date + 32, "%Y-%m-01"))  # at least 1 full month
last_date <- Sys.Date()
month_starts <- seq.Date(from = first_month_start,
                          to = as.Date(format(last_date, "%Y-%m-01")), by = "month")
all_month_ends <- month_starts - 1

for (i in seq_len(length(all_month_ends) - 1)) {
  me <- all_month_ends[i]
  next_me <- all_month_ends[i+1]
  cat(sprintf("  %s ...\n", me))

  # re-discover coins available up to this month-end
  me_uts <- as.integer(as.POSIXct(me + 1)) * 1000
  current_coins <- dbGetQuery(pgCon,
    "select distinct symbol from binance_crypto_historical_1h where close_time < $1",
    params = list(me_uts))
  current_coins <- (current_coins |> filter(str_ends(symbol, "USDT")))[,1]

  # load any new coins on demand
  for (sym in current_coins) {
    if (sym %in% names(daily_prices)) next
    pxDt <- dbGetQuery(pgCon,
      "select close_time, px_close from binance_crypto_historical_1h
       where symbol = $1 and close_time >= $2 order by close_time",
      params = list(sym, earliest_ts))
    if (nrow(pxDt) < min_daily_bars) next
    hXts <- xts(pxDt$px_close, as.POSIXct(pxDt$close_time / 1000, origin = "1970-01-01", tz = "UTC"))
    daily <- to.period(na.omit(hXts), period = "days")
    close_col <- daily[, 4]
    index(close_col) <- as.Date(index(close_col))
    if (nrow(close_col) < min_daily_bars) next
    daily_prices[[sym]] <- close_col
    daily_rets[[sym]]   <- dailyReturn(close_col)
    cat(sprintf("    new coin: %s (%d daily bars)\n", sym, nrow(close_col)))
  }

  selected <- c()

  for (sym in current_coins) {
    prices <- daily_prices[[sym]]
    if (is.null(prices)) next
    rets   <- daily_rets[[sym]]

    # data available up to and including this month-end
    hist_prices <- prices[paste0("/", me)]
    if (nrow(hist_prices) < min_daily_bars) next

    # run strategy nine on historical data (NO look-forward)
    sig <- tryCatch({
      strategy_nine_signal(hist_prices, use_cost_screen = FALSE)
    }, error = function(e) NULL)
    if (is.null(sig)) next

    forecast_xts <- attr(sig, "forecast")
    if (is.null(forecast_xts)) next

    retL1 <- stats::lag(rets[paste0("/", me)], -1)
    common_dates <- intersect(intersect(index(na.omit(sig)), index(retL1)),
                              index(na.omit(forecast_xts)))
    if (length(common_dates) < 50) next

    sa <- sig[common_dates]
    ra <- retL1[common_dates]

    # binary long-short on historical data (most recent min_daily_bars days only)
    trd <- coredata(sa); trd <- trd - c(NA, trd[-length(trd)])
    bls <- ifelse(coredata(sa) == 1, coredata(ra), -coredata(ra))
    bls <- ifelse(trd != 0 & !is.na(trd), bls - drag, bls)
    bls_xts <- xts(bls, order.by = common_dates)

    # evaluate Sharpe on last sharpe_lookback days only
    recent_bls <- tail(bls_xts, sharpe_lookback)
    if (nrow(recent_bls) < min_sharpe_rows) next
    sr <- tryCatch({
      if (sd(coredata(recent_bls), na.rm = TRUE) == 0) NA
      else SharpeRatio.annualized(recent_bls)[1,1]
    }, error = function(e) NA)
    if (is.na(sr) || sr <= 0) next
    #cat(sprintf("         %s: %.2f  \n", sym, sr))
    selected <- c(selected, sym)
  }

  cat(sprintf("    %s selected %d coins ***\n", me, length(selected)))

  # log selection
  if (length(selected) > 0) {
    selection_log <- rbind(selection_log, tibble(
      MonthEnd = me, Coins = paste(sort(selected), collapse = ","), Count = length(selected)
    ))
  }

  if (!started && length(selected) >= min_coins) {
    started <- TRUE
    cat(sprintf("    *** BACKTEST STARTED at %s with %d coins ***\n", me, length(selected)))
  }

  if (!started) {
    cat(sprintf("    %d coins selected (need %d), waiting...\n", length(selected), min_coins))
    next
  }

  if (length(selected) < 2) { cat(sprintf("    %d coins selected, skipping month\n", length(selected))); next }

  cat(sprintf("    *** Calculating strategy 9 for %s with %d coins ***\n", me, length(selected)))
  # next month's trading days (weekdays only)
  next_dates <- seq.Date(from = me + 1, to = next_me, by = "day")
  next_dates <- next_dates[!weekdays(next_dates) %in% c("Saturday", "Sunday")]
  if (length(next_dates) == 0) next

  daily_port <- NULL
  for (d_raw in next_dates) {
    d <- as.Date(d_raw, origin = "1970-01-01")
    d_ret <- 0
    n_valid <- 0
    for (sym in selected) {
      rets <- daily_rets[[sym]]
      ret_row <- rets[as.character(d)]
      if (nrow(ret_row) == 0 || is.na(coredata(ret_row)[1])) next

      # need the signal direction for THIS day
      prices <- daily_prices[[sym]]
      hist_for_sig <- prices[paste0("/", d - 1)]
      if (nrow(hist_for_sig) < min_daily_bars) next

      sig <- tryCatch({
        strategy_nine_signal(hist_for_sig, use_cost_screen = FALSE)
      }, error = function(e) NULL)
      if (is.null(sig)) next

      sig_val <- as.numeric(last(na.omit(sig)))
      if (is.na(sig_val)) next

      ret_val <- as.numeric(coredata(ret_row))[1]
      if (is.na(ret_val) || length(ret_val) != 1) next
      if (sig_val == 1) {
        d_ret <- d_ret + ret_val
      } else {
        d_ret <- d_ret - ret_val
      }
      n_valid <- n_valid + 1
    }
    if (n_valid > 0) {
      d_port <- as.numeric(d_ret / n_valid)
      if (is.na(d_port) || abs(d_port) > 1) next
      daily_port <- rbind(daily_port, xts(d_port, order.by = d))
    }
  }

  if (!is.null(daily_port)) {
    index(daily_port) <- as.Date(index(daily_port))
    portfolio_rets <- rbind(portfolio_rets, daily_port)
  }
}

# force Date index
if (nrow(portfolio_rets) > 0) index(portfolio_rets) <- as.Date(index(portfolio_rets))

# ── save selection log ────────────────────────────────────────────────────────
write.csv(selection_log, sprintf("%s/dynamic-universe-selection-bls-ew.csv", reportPath), row.names = FALSE)
cat(sprintf("\nSelection log saved: %d months\n", nrow(selection_log)))

# ── portfolio metrics ─────────────────────────────────────────────────────────
if (nrow(portfolio_rets) > 0) {
  names(portfolio_rets) <- "Dynamic_BLS_EW"
  cat("\n=== Portfolio Metrics ===\n")
  sr  <- SharpeRatio.annualized(portfolio_rets)[1,1]
  ret <- as.numeric(Return.annualized(portfolio_rets))
  dd  <- as.numeric(maxDrawdown(portfolio_rets))
  cat(sprintf("  Dynamic BLS EW: SR=%.2f  Ret=%.2f%%  DD=%.2f%%\n", sr, ret*100, dd*100))
  cat(sprintf("  Period: %s → %s (%d days)\n",
              first(index(portfolio_rets)), last(index(portfolio_rets)), nrow(portfolio_rets)))

  write.csv(data.frame(Date = index(portfolio_rets), Return = coredata(portfolio_rets)),
            sprintf("%s/dynamic-universe-returns-bls-ew.csv", reportPath), row.names = FALSE)
  cat("  returns saved to dynamic-universe-returns-bls-ew.csv\n")

  # ── cumulative chart vs BTCUSDT B&H ──────────────────────────────────────
  if (!is.null(daily_prices[["BTCUSDT"]])) {
    btc_bh <- daily_rets[["BTCUSDT"]]
    cd <- intersect(index(portfolio_rets), index(btc_bh))
    to_plot <- na.omit(merge(portfolio_rets[cd], btc_bh[cd]))
    names(to_plot) <- c("Dynamic BLS EW", "BTCUSDT B&H")

    dyn_sr <- tryCatch(SharpeRatio.annualized(to_plot[, "Dynamic BLS EW"])[1,1],
                        error = function(e) NA)

    Common.PlotCumReturns(to_plot, "Dynamic Universe — Binary Long-Short EW",
      sprintf("Monthly re-selection (≥%d bars, SR>0); SR = %.2f", min_daily_bars, dyn_sr),
      sprintf("%s/dynamic-universe-bls-ew.cumret.png", reportPath), NULL)
  }
}

dbDisconnect(pgCon)

print("Done.")

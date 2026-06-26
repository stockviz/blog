library('RODBC')
library('RPostgres')
library('tidyverse')
library('quantmod')
library('PerformanceAnalytics')
library('gt')
library('webshot2')
library('xts')

source("/mnt/hollandC/StockViz/R/config.r")
source("strategy_nine.R")
source("/mnt/data/blog/common/plot.common.r")

pdf(NULL)
options("scipen" = 100)

reportPath <- "."
drag <- 0.2 / 100

# ── connect ───────────────────────────────────────────────────────────────────
pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)

lconCrypto <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    ldbserver,
    "crypto",
    ldbuser,
    ldbpassword
  ),
  case = "nochange",
  believeNRows = TRUE
)

# ── discover coins ────────────────────────────────────────────────────────────
startDate <- as.Date("2019-01-01")
startDateUTS <- as.integer(as.POSIXct(startDate))*1000

legacyCoins <- dbGetQuery(pgCon, "select distinct symbol from binance_crypto_historical_1h where close_time < $1",
                    params = list(startDateUTS))

coins <- (legacyCoins |> filter(str_ends(symbol, "USDT")))[,1]

cat(sprintf("  found %d USDT coins with data before %s\n", length(coins), startDate))

# ── load hourly prices, convert to daily ──────────────────────────────────────
print("loading prices...")
daily_prices <- list()
daily_rets   <- list()

for (sym in coins) {
  pxDt <- dbGetQuery(pgCon,
    "select close_time, px_close from binance_crypto_historical_1h
     where symbol = $1 and close_time >= $2 order by close_time",
    params = list(sym, startDateUTS))

  if (nrow(pxDt) < 500) { cat(sprintf("  %s: too few rows (%d), skipping\n", sym, nrow(pxDt))); next }

  hXts <- xts(pxDt$px_close, as.POSIXct(pxDt$close_time / 1000, origin = "1970-01-01", tz = "UTC"))
  daily <- to.period(na.omit(hXts), period = "days")
  close_col <- daily[, 4]
  index(close_col) <- as.Date(index(close_col))

  if (nrow(close_col) < 260) { cat(sprintf("  %s: too few daily bars (%d), skipping\n", sym, nrow(close_col))); next }

  daily_prices[[sym]] <- close_col
  daily_rets[[sym]]   <- dailyReturn(close_col)
  cat(sprintf("  %s: %d daily bars\n", sym, nrow(close_col)))
}

valid_coins <- names(daily_prices)
cat(sprintf("  %d coins loaded\n", length(valid_coins)))

# ── strategy nine signals: 4 variants per coin ────────────────────────────────
print("computing signals...")
scaled_lo <- list()
scaled_ls <- list()
binary_lo <- list()
binary_ls <- list()
bh_rets   <- list()

for (sym in valid_coins) {
  prices <- daily_prices[[sym]]
  rets   <- daily_rets[[sym]]
  cat(sprintf("  %s ...\n", sym))

  sig <- tryCatch({
    strategy_nine_signal(prices, use_cost_screen = FALSE)
  }, error = function(e) { cat(sprintf("    ERROR: %s\n", e$message)); NULL })
  if (is.null(sig)) next

  forecast_xts <- attr(sig, "forecast")
  if (is.null(forecast_xts)) { cat("    no forecast\n"); next }

  retL1 <- stats::lag(rets, -1)
  common_dates <- intersect(intersect(index(na.omit(sig)), index(retL1)),
                            index(na.omit(forecast_xts)))
  if (length(common_dates) == 0) { cat("    no common dates\n"); next }

  sa <- sig[common_dates]
  fc <- forecast_xts[common_dates] / 20
  ra <- retL1[common_dates]

  # trade indicator (binary signal change) for drag
  trd <- coredata(sa); trd <- trd - c(NA, trd[-length(trd)])

  # scaled long-only (weight ∈ [0, 1])
  w_lo <- pmax(pmin(coredata(fc), 1), 0)
  sclo <- w_lo * coredata(ra)
  sclo <- ifelse(trd != 0 & !is.na(trd), sclo - drag, sclo)
  scaled_lo[[sym]] <- xts(sclo, order.by = common_dates)

  # scaled long-short (weight ∈ [-1, 1])
  w_ls <- pmax(pmin(coredata(fc), 1), -1)
  scls <- w_ls * coredata(ra)
  scls <- ifelse(trd != 0 & !is.na(trd), scls - drag, scls)
  scaled_ls[[sym]] <- xts(scls, order.by = common_dates)

  # binary long-only (1 → long, 0 → flat)
  blo <- ifelse(coredata(sa) == 1, coredata(ra), 0)
  blo <- ifelse(trd != 0 & !is.na(trd), blo - drag, blo)
  binary_lo[[sym]] <- xts(blo, order.by = common_dates)

  # binary long-short (1 → long, 0 → short)
  bls <- ifelse(coredata(sa) == 1, coredata(ra), -coredata(ra))
  bls <- ifelse(trd != 0 & !is.na(trd), bls - drag, bls)
  binary_ls[[sym]] <- xts(bls, order.by = common_dates)

  bh_rets[[sym]] <- ra
}

# ═════════════════════════════════════════════════════════════════════════════
# Build portfolios: 4 strategies × 2 weighting schemes = 8 series
# ═════════════════════════════════════════════════════════════════════════════
print("\n=== Building Portfolios ===")

strat_sets <- list(
  "Scaled LO"  = scaled_lo,
  "Scaled LS"  = scaled_ls,
  "Binary LO"  = binary_lo,
  "Binary LS"  = binary_ls
)

# inverse-vol portfolio helper
invvol_portfolio <- function(ret_xts, lookback = 60) {
  dates <- index(ret_xts)
  n <- nrow(ret_xts)
  k <- ncol(ret_xts)
  port <- numeric(n)
  month_starts <- unique(as.Date(format(dates, "%Y-%m-01")))
  month_starts <- month_starts[month_starts >= dates[1]]

  for (i in seq_len(n)) {
    d <- dates[i]
    ms <- max(month_starts[month_starts <= d])
    win_end <- ms - 1
    win_start <- win_end - lookback
    win <- ret_xts[paste0(win_start, "/", win_end)]
    if (nrow(win) < 20) {
      w <- rep(1/k, k)
    } else {
      vols <- apply(coredata(win), 2, sd, na.rm = TRUE)
      vols[is.na(vols) | vols == 0] <- Inf
      inv_vol <- 1/vols
      w <- inv_vol / sum(inv_vol)
    }
    port[i] <- sum(w * as.numeric(coredata(ret_xts[i, ])), na.rm = TRUE)
  }
  xts(port, order.by = dates)
}

# merge all assets into one xts per strategy
valid_assets <- names(bh_rets)
merged <- list()
for (nm in names(strat_sets)) {
  merged[[nm]] <- do.call(merge.xts, lapply(valid_assets, \(a) strat_sets[[nm]][[a]]))
  names(merged[[nm]]) <- valid_assets
}

# find common start
common_start <- first(index(na.omit(merged[["Scaled LO"]])))
first_valid <- sapply(valid_assets, \(a) first(index(na.omit(scaled_lo[[a]]))))
from_max_start <- max(as.Date(unlist(first_valid)))
portfolio_start <- max(common_start, from_max_start)
cat(sprintf("  portfolio start: %s\n", portfolio_start))

# build all 8 portfolio series
portfolios <- list()
port_names <- c()

for (nm in names(strat_sets)) {
  rets <- merged[[nm]][paste0(portfolio_start, "/"), ]
  n_assets <- ncol(rets)

  # equal-weight
  ew_name <- paste0(nm, " EW")
  portfolios[[ew_name]] <- xts(rowMeans(rets, na.rm = TRUE), order.by = index(rets))
  port_names <- c(port_names, ew_name)

  # inverse-vol
  iv_name <- paste0(nm, " InvVol")
  portfolios[[iv_name]] <- invvol_portfolio(rets)
  port_names <- c(port_names, iv_name)
}

# ── consolidated summary ─────────────────────────────────────────────────────
cat("\n=== Consolidated Summary ===\n")
summary_tbl <- tibble()
for (nm in port_names) {
  px <- portfolios[[nm]]
  sr  <- SharpeRatio.annualized(px)[1,1]
  ret <- as.numeric(Return.annualized(px))
  dd  <- as.numeric(maxDrawdown(px))
  cat(sprintf("  %-25s  SR=%.2f  Ret=%.2f%%  DD=%.2f%%\n", nm, sr, ret*100, dd*100))
  summary_tbl <- rbind(summary_tbl, tibble(
    Strategy = nm, SR = round(sr, 3), AnnRet = round(ret, 4), MaxDD = round(dd, 4)
  ))
}

# add BTCUSDT B&H
if ("BTCUSDT" %in% valid_assets) {
  btc_bh <- bh_rets[["BTCUSDT"]][paste0(portfolio_start, "/")]
  btc_sr  <- SharpeRatio.annualized(btc_bh)[1,1]
  btc_ret <- as.numeric(Return.annualized(btc_bh))
  btc_dd  <- as.numeric(maxDrawdown(btc_bh))
  cat(sprintf("  %-25s  SR=%.2f  Ret=%.2f%%  DD=%.2f%%\n", "BTCUSDT B&H", btc_sr, btc_ret*100, btc_dd*100))
  summary_tbl <- rbind(summary_tbl, tibble(
    Strategy = "BTCUSDT B&H", SR = round(btc_sr, 3), AnnRet = round(btc_ret, 4), MaxDD = round(btc_dd, 4)
  ))
}

# ── gt summary table ─────────────────────────────────────────────────────────
summary_tbl |>
  gt() |>
  tab_header(
    title = sprintf("Crypto Strategy Nine — Portfolio Comparison (%d assets)", length(valid_assets)),
    subtitle = sprintf("Equal-weight + Inverse-vol; %s → %s; drag = %.1f%%",
                       portfolio_start, format(Sys.Date(), "%Y-%m-%d"), drag * 100)
  ) |>
  fmt_percent(columns = c(AnnRet, MaxDD), decimals = 2) |>
  fmt_number(columns = SR, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  cols_label(SR = "Sharpe", AnnRet = "Ann.Return", MaxDD = "Max Drawdown") |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes()) |>
  gtsave(sprintf("%s/crypto-portfolio-summary.html", reportPath))

webshot2::webshot(
  sprintf("%s/crypto-portfolio-summary.html", reportPath),
  sprintf("%s/crypto-portfolio-summary.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── cumulative chart: all portfolios + BTCUSDT B&H ────────────────────────────
to_plot <- do.call(merge.xts, lapply(port_names, \(nm) portfolios[[nm]]))
names(to_plot) <- port_names
if ("BTCUSDT" %in% valid_assets) {
  to_plot <- na.omit(merge(to_plot, btc_bh))
  names(to_plot)[ncol(to_plot)] <- "BTCUSDT B&H"
}

Common.PlotCumReturns(to_plot, "Crypto Strategy Nine — All Portfolios",
  sprintf("%d assets; Equal-weight + Inverse-vol; %s → %s",
          length(valid_assets), portfolio_start, format(Sys.Date(), "%Y-%m-%d")),
  sprintf("%s/crypto-all-portfolios.cumret.png", reportPath), NULL)

odbcClose(lconCrypto)
dbDisconnect(pgCon)

print("Done.")

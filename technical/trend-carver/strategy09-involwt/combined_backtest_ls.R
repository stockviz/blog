# Combined Strategy Nine Equal-Weight Backtest:
#   NIFTY indices  +  Crypto (INR-adjusted)  +  MCX commodity indices
#
# Data sources:
#   NIFTY  → bhav_index (SQL Server / Norway)
#   Crypto → binance_crypto_historical_1h (PostgreSQL / Sweden), USD → INR via av_fx_usd_daily_ts
#   MCX    → bhav_index_mcx (SQL Server / StockViz)
#
# Equal-weight EWMAC filters, no cost screen.  Drag = 0.2% per trade.

library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('gt')
library('webshot2')
library('viridis')
library('ggthemes')
library('RODBC')
library('RPostgres')
library('xts')

source("/mnt/data/blog/technical/trend-carver/strategy09-involwt/strategy_nine.R")
source("/mnt/data/blog/common/plot.common.r")
source("/mnt/hollandC/StockViz/R/config.r")

pdf(NULL)
options("scipen" = 100)

reportPath <- "."
drag <- 0.2 / 100

# ── asset lists ──────────────────────────────────────────────────────────────
nifty_indices <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR", "NIFTY BANK TR")
crypto_syms   <- c("BTCUSDT", "ETHUSDT", "SOLUSDT")
mcx_indices   <- c("MCXCOMPDEX", "MCXMETLDEX", "MCXCRUDEX", "MCXGOLDEX", "MCXSILVDEX", "MCXCOPRDEX")
us_etfs       <- c("SPY", "QQQ")

# ── connect ───────────────────────────────────────────────────────────────────
print("connecting to Norway (SQL Server)...")
lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE
)

print("connecting to StockViz (SQL Server) for MCX...")
lconMcx <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockViz", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE
)

print("connecting to Sweden (PostgreSQL)...")
pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden', user = ldbuser2, password = ldbpassword2,
  dbname = 'StockVizDyn', sslmode = 'allow'
)

print("connecting to US2 (SQL Server)...")
lconUS2 <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockVizUs2", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE
)

# ── helper: convert hourly to daily close ────────────────────────────────────
hourly_to_daily <- function(hXts) {
  daily <- to.period(na.omit(hXts), period = "days")
  close_col <- daily[, 4]
  index(close_col) <- as.Date(index(close_col))
  close_col
}

# ═════════════════════════════════════════════════════════════════════════════
# 1. NIFTY indices
# ═════════════════════════════════════════════════════════════════════════════
print("=== NIFTY indices ===")
nifty_prices <- list()
for (iName in nifty_indices) {
  pDf <- sqlQuery(lcon, sprintf(
    "select px_close, time_stamp from bhav_index
     where index_name = '%s' and time_stamp >= '2005-04-01'
     order by time_stamp", iName))
  if (nrow(pDf) == 0) { cat(sprintf("  WARNING: no data for %s\n", iName)); next }
  nifty_prices[[iName]] <- xts(pDf$px_close, as.Date(pDf$time_stamp))
  cat(sprintf("  %s: %d rows\n", iName, nrow(nifty_prices[[iName]])))
}

# ═════════════════════════════════════════════════════════════════════════════
# 2. Crypto (USD → INR adjusted)
# ═════════════════════════════════════════════════════════════════════════════
print("=== Crypto (INR-adjusted) ===")

# fetch USDINR
fxDf <- dbGetQuery(pgCon, "select time_stamp, px_close from av_fx_usd_daily_ts where curr_code=$1",
                   params = list("INR"))
usdinr <- xts(fxDf$px_close, as.Date(fxDf$time_stamp))
cat(sprintf("  USDINR: %d rows, %s → %s\n", nrow(usdinr),
            as.character(first(index(usdinr))), as.character(last(index(usdinr)))))

# get max open_time for crypto date range
maxOt <- dbGetQuery(pgCon, "select max(open_time) from binance_crypto_historical_1h where symbol='BTCUSDT'")[[1]]
startOt <- as.numeric(as.POSIXct("2018-01-01", tz = "UTC")) * 1000

crypto_prices <- list()
for (sym in crypto_syms) {
  pxDt <- dbGetQuery(pgCon,
    "select close_time, px_close from binance_crypto_historical_1h
     where symbol = $1 and close_time >= $2 order by close_time",
    params = list(sym, startOt))

  if (nrow(pxDt) == 0) { cat(sprintf("  WARNING: no data for %s\n", sym)); next }

  # hourly USD prices → daily
  hXts <- xts(pxDt$px_close, as.POSIXct(pxDt$close_time / 1000, origin = "1970-01-01", tz = "UTC"))
  daily_usd <- hourly_to_daily(hXts)

  # align with USDINR and convert to INR
  cd <- intersect(index(daily_usd), index(usdinr))
  daily_inr <- daily_usd[cd] * usdinr[cd]
  names(daily_inr) <- sym
  crypto_prices[[sym]] <- daily_inr
  cat(sprintf("  %s: %d daily rows (INR), %s → %s\n", sym, nrow(daily_inr),
              as.character(first(index(daily_inr))), as.character(last(index(daily_inr)))))
}

# ═════════════════════════════════════════════════════════════════════════════
# 3. MCX commodity indices
# ═════════════════════════════════════════════════════════════════════════════
print("=== MCX commodity indices ===")

mcxStart <- as.Date("2010-01-01")
mcxEnd   <- Sys.Date()

mcx_prices <- list()
mcx_names   <- c()
for (idx in mcx_indices) {
  pDf <- sqlQuery(lconMcx, sprintf(
    "select c, time_stamp from bhav_index_mcx
     where INDEX_CODE = '%s' and time_stamp >= '%s' and time_stamp <= '%s'
     order by time_stamp", idx, mcxStart, mcxEnd))

  if (nrow(pDf) == 0) { cat(sprintf("  WARNING: no data for %s\n", idx)); next }

  mcxName <- sqlQuery(lconMcx, sprintf(
    "select top 1 INDEX_NAME from BHAV_INDEX_MCX_MAP where INDEX_CODE='%s' order by time_stamp desc", idx))[[1]]
  display_name <- gsub("MCX iCOMDEX ", "", mcxName)

  mcx_prices[[display_name]] <- xts(pDf$c, as.Date(pDf$time_stamp))
  mcx_names <- c(mcx_names, display_name)
  cat(sprintf("  %s: %d rows (%s → %s)\n", display_name, nrow(mcx_prices[[display_name]]),
              as.character(first(index(mcx_prices[[display_name]]))),
              as.character(last(index(mcx_prices[[display_name]])))))
}

# ═════════════════════════════════════════════════════════════════════════════
# 4. US ETFs (USD → INR adjusted)
# ═════════════════════════════════════════════════════════════════════════════
print("=== US ETFs (INR-adjusted) ===")

us_prices <- list()
us_names  <- c()
for (etf in us_etfs) {
  pDft <- sqlQuery(lconUS2, sprintf(
    "select c, time_stamp from TIINGO_DATA where ticker='%s' and time_stamp >= '2010-01-01' order by time_stamp", etf))
  pDfs <- sqlQuery(lconUS2, sprintf(
    "select c, time_stamp from BHAV_EQ_TD where symbol='%s' and time_stamp >= '2010-01-01' order by time_stamp", etf))

  if (nrow(pDfs) > nrow(pDft)) {
    pDf <- pDfs
  } else {
    pDf <- pDft
  }

  if (nrow(pDf) == 0) { cat(sprintf("  WARNING: no data for %s\n", etf)); next }

  usd_xts <- xts(pDf[, 1], as.Date(pDf[, 2]))
  cd <- intersect(index(usd_xts), index(usdinr))
  inr_xts <- usd_xts[cd] * usdinr[cd]
  names(inr_xts) <- etf
  us_prices[[etf]] <- inr_xts
  us_names <- c(us_names, etf)
  cat(sprintf("  %s: %d daily rows (INR), %s → %s\n", etf, nrow(inr_xts),
              as.character(first(index(inr_xts))), as.character(last(index(inr_xts)))))
}

# ═════════════════════════════════════════════════════════════════════════════
# 5. Run Strategy Nine on all assets (equal weight, no cost screen)
# ═════════════════════════════════════════════════════════════════════════════
print("\n=== Strategy Nine Signal Computation ===")

all_assets <- c(nifty_indices, crypto_syms, mcx_names, us_names)
all_prices <- c(nifty_prices, crypto_prices, mcx_prices, us_prices)

results <- tibble()
binary_ls  <- list()
scaled_ls  <- list()
bh_rets    <- list()

for (asset in all_assets) {
  prices <- all_prices[[asset]]
  if (is.null(prices)) next
  cat(sprintf("  %s (%d rows) ...\n", asset, nrow(prices)))

  sig <- tryCatch({
    strategy_nine_signal(prices, use_cost_screen = FALSE)
  }, error = function(e) { cat(sprintf("    ERROR: %s\n", e$message)); NULL })
  if (is.null(sig)) next

  forecast_xts <- attr(sig, "forecast")
  if (is.null(forecast_xts)) { cat("    no forecast\n"); next }

  rets <- dailyReturn(prices)
  retL1 <- stats::lag(rets, -1)

  common_dates <- intersect(intersect(index(na.omit(sig)), index(retL1)),
                            index(na.omit(forecast_xts)))
  if (length(common_dates) == 0) { cat("    no common dates\n"); next }

  sa <- sig[common_dates]
  fc <- forecast_xts[common_dates] / 20
  ra <- retL1[common_dates]

  # binary long-only
  strat_bin <- ifelse(coredata(sa) == 1, coredata(ra), -coredata(ra))
  trd <- coredata(sa); trd <- trd - c(NA, trd[-length(trd)])
  strat_bin <- ifelse(trd != 0 & !is.na(trd), strat_bin - drag, strat_bin)
  bin_xts <- xts(strat_bin, order.by = common_dates)
  binary_ls[[asset]] <- bin_xts

  # scaled long-only
  w_ls <- pmax(pmin(coredata(fc), 1), -1)
  strat_scls <- w_ls * coredata(ra)
  strat_scls <- ifelse(trd != 0 & !is.na(trd), strat_scls - drag, strat_scls)
  scaled_ls[[asset]] <- xts(strat_scls, order.by = common_dates)

  # B&H
  bh_xts <- ra
  bh_rets[[asset]] <- bh_xts

  # metrics
  bin_sr  <- SharpeRatio.annualized(bin_xts)[1,1]
  bin_ret <- as.numeric(Return.annualized(bin_xts))
  bin_dd  <- as.numeric(maxDrawdown(bin_xts))
  sclo_sr  <- SharpeRatio.annualized(scaled_ls[[asset]])[1,1]
  sclo_ret <- as.numeric(Return.annualized(scaled_ls[[asset]]))
  sclo_dd  <- as.numeric(maxDrawdown(scaled_ls[[asset]]))
  bh_sr  <- SharpeRatio.annualized(bh_xts)[1,1]
  bh_ret <- as.numeric(Return.annualized(bh_xts))
  bh_dd  <- as.numeric(maxDrawdown(bh_xts))

  results <- rbind(results, tibble(
    Asset      = asset,
    Bin_SR     = round(bin_sr, 3),
    Bin_Ret    = round(bin_ret, 4),
    Bin_DD     = round(bin_dd, 4),
    ScLO_SR    = round(sclo_sr, 3),
    ScLO_Ret   = round(sclo_ret, 4),
    ScLO_DD    = round(sclo_dd, 4),
    BH_SR      = round(bh_sr, 3),
    BH_Ret     = round(bh_ret, 4),
    BH_DD      = round(bh_dd, 4)
  ))
}

# ── console summary ──────────────────────────────────────────────────────────
cat("\n=== Summary ===\n")
print(results, n = Inf)

# ── gt summary table ─────────────────────────────────────────────────────────
if (nrow(results) > 0) {
  results |>
    gt() |>
    tab_header(
      title = "Strategy Nine Equal-Weight — All Asset Classes",
      subtitle = "NIFTY + Crypto (INR) + MCX Commodities + US ETFs (INR); drag = 0.2%"
    ) |>
    tab_spanner(label = "Binary LS",  columns = starts_with("Bin_")) |>
    tab_spanner(label = "Scaled LS",  columns = starts_with("ScLO_")) |>
    tab_spanner(label = "Buy & Hold", columns = starts_with("BH_")) |>
    fmt_percent(columns = ends_with("_Ret") | ends_with("_DD"), decimals = 2) |>
    fmt_number(columns = ends_with("_SR"), decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    cols_label(
      Bin_SR = "Sharpe", Bin_Ret = "Ret", Bin_DD = "MaxDD",
      ScLO_SR = "Sharpe", ScLO_Ret = "Ret", ScLO_DD = "MaxDD",
      BH_SR = "Sharpe", BH_Ret = "Ret", BH_DD = "MaxDD"
    ) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes()) |>
    gtsave(sprintf("%s/combined-summary-involwt-ls.html", reportPath))

  webshot2::webshot(
    sprintf("%s/combined-summary-involwt-ls.html", reportPath),
    sprintf("%s/combined-summary-involwt-ls.png", reportPath),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))
}

# ── cumulative return charts per asset ───────────────────────────────────────
cat("\n=== Cumulative Charts ===\n")
for (asset in names(scaled_ls)) {
  combined <- na.omit(merge(scaled_ls[[asset]], binary_ls[[asset]], bh_rets[[asset]]))
  names(combined) <- c("Scaled LS", "Binary LS", "B&H")

  safe_name <- gsub("[ /]", "_", asset)
  Common.PlotCumReturns(combined, asset,
    "Strategy Nine: Scaled vs Binary Long-Short (equal weight)",
    sprintf("%s/%s-involwt-ls.cumret.png", reportPath, safe_name), NULL)
}

# ── combined charts: all binary LO, all scaled LS ───────────────────────────
valid_bin <- names(binary_ls)
if (length(valid_bin) > 1) {
  bin_all <- do.call(merge.xts, lapply(valid_bin, \(a) binary_ls[[a]]))
  names(bin_all) <- valid_bin
  Common.PlotCumReturns(bin_all, "All Assets",
    "Strategy Nine Binary Long-Short — Equal Weight",
    sprintf("%s/combined-all-involwt.binary-ls.cumret.png", reportPath), NULL)

  sclo_all <- do.call(merge.xts, lapply(valid_bin, \(a) scaled_ls[[a]]))
  names(sclo_all) <- valid_bin
  Common.PlotCumReturns(sclo_all, "All Assets",
    "Strategy Nine Scaled Long-Short — Equal Weight",
    sprintf("%s/combined-all-involwt.scaled-ls.cumret.png", reportPath), NULL)
}

# ── grouped charts: NIFTY, Crypto, MCX ───────────────────────────────────────
for (group_name in c("NIFTY", "Crypto", "MCX", "US")) {
  if (group_name == "NIFTY") {
    group_assets <- intersect(nifty_indices, valid_bin)
  } else if (group_name == "Crypto") {
    group_assets <- intersect(crypto_syms, valid_bin)
  } else if (group_name == "US") {
    group_assets <- intersect(us_names, valid_bin)
  } else {
    group_assets <- intersect(mcx_names, valid_bin)
  }
  if (length(group_assets) < 2) next

  # scaled LO
  g_sclo <- do.call(merge.xts, lapply(group_assets, \(a) scaled_ls[[a]]))
  names(g_sclo) <- group_assets
  Common.PlotCumReturns(g_sclo, group_name,
    sprintf("Strategy Nine Scaled LS — %s (equal weight)", group_name),
    sprintf("%s/group-involwt-%s.scaled-ls.cumret.png", reportPath, tolower(group_name)), NULL)

  # binary LO
  g_bin <- do.call(merge.xts, lapply(group_assets, \(a) binary_ls[[a]]))
  names(g_bin) <- group_assets
  Common.PlotCumReturns(g_bin, group_name,
    sprintf("Strategy Nine Binary LS — %s (equal weight)", group_name),
    sprintf("%s/group-involwt-%s.binary-ls.cumret.png", reportPath, tolower(group_name)), NULL)
}

# ── Inverse-Volatility Portfolio: monthly rebalance ────────────────────────────
cat("\n=== Inverse-Volatility Portfolio (monthly rebalance) ===\n")
if (length(valid_bin) >= 2) {
  vol_lookback <- 60  # trailing days for vol estimation

  # merge all scaled LS strategy returns
  ew_scaled <- do.call(merge.xts, lapply(valid_bin, \(a) scaled_ls[[a]]))
  ew_bin    <- do.call(merge.xts, lapply(valid_bin, \(a) binary_ls[[a]]))
  ew_bh     <- do.call(merge.xts, lapply(valid_bin, \(a) bh_rets[[a]]))

  # find the first date where EVERY asset has non-NA returns
  common_start <- first(index(na.omit(ew_scaled)))
  first_valid <- sapply(valid_bin, \(a) first(index(na.omit(scaled_ls[[a]]))))
  from_max_start <- max(as.Date(unlist(first_valid)))
  portfolio_start <- max(common_start, from_max_start)
  cat(sprintf("  portfolio start date: %s\n", portfolio_start))

  n_assets <- length(valid_bin)

  # slice from common start
  s_ret <- ew_scaled[paste0(portfolio_start, "/"), ]
  b_ret <- ew_bin[paste0(portfolio_start, "/"), ]
  h_ret <- ew_bh[paste0(portfolio_start, "/"), ]

  # ── inverse-vol weighting function ──
  invvol_portfolio <- function(ret_xts, lookback) {
    dates <- index(ret_xts)
    n <- nrow(ret_xts)
    k <- ncol(ret_xts)
    port <- numeric(n)

    # month-start dates for rebalancing
    month_starts <- unique(as.Date(format(dates, "%Y-%m-01")))
    month_starts <- month_starts[month_starts >= dates[1]]

    for (i in seq_len(n)) {
      d <- dates[i]
      # find the most recent rebalance date <= d
      ms <- max(month_starts[month_starts <= d])
      # compute vol using trailing window ending the day before rebalance
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

  port_scaled <- invvol_portfolio(s_ret, vol_lookback)
  port_bin    <- invvol_portfolio(b_ret, vol_lookback)
  port_bh     <- invvol_portfolio(h_ret, vol_lookback)

  # ── portfolio metrics ──
  port_sr    <- SharpeRatio.annualized(port_scaled)[1,1]
  port_ret   <- as.numeric(Return.annualized(port_scaled))
  port_dd    <- as.numeric(maxDrawdown(port_scaled))
  port_bin_sr  <- SharpeRatio.annualized(port_bin)[1,1]
  port_bin_ret <- as.numeric(Return.annualized(port_bin))
  port_bin_dd  <- as.numeric(maxDrawdown(port_bin))
  port_bh_sr   <- SharpeRatio.annualized(port_bh)[1,1]
  port_bh_ret  <- as.numeric(Return.annualized(port_bh))
  port_bh_dd   <- as.numeric(maxDrawdown(port_bh))

  cat(sprintf("  Scaled LS  InvVol: SR=%.2f  Ret=%.2f%%  DD=%.2f%%\n",
              port_sr, port_ret*100, port_dd*100))
  cat(sprintf("  Binary LS  InvVol: SR=%.2f  Ret=%.2f%%  DD=%.2f%%\n",
              port_bin_sr, port_bin_ret*100, port_bin_dd*100))
  cat(sprintf("  B&H         InvVol: SR=%.2f  Ret=%.2f%%  DD=%.2f%%\n",
              port_bh_sr, port_bh_ret*100, port_bh_dd*100))

  # save portfolio returns time series for later use
  port_all <- na.omit(merge(port_scaled, port_bin, port_bh))
  names(port_all) <- c("Scaled_LS", "Binary_LS", "BH")
  port_df <- data.frame(Date = index(port_all), coredata(port_all))
  write.csv(port_df, sprintf("%s/portfolio-returns-involwt-ls.csv", reportPath), row.names = FALSE)
  cat(sprintf("  saved portfolio returns: portfolio-returns-involwt-ls.csv (%d rows)\n", nrow(port_df)))

  # ── gt portfolio table ──
  port_results <- tibble(
    Strategy   = c("Scaled LS (InvVol)", "Binary LS (InvVol)", "B&H (InvVol)"),
    SR         = c(round(port_sr, 3), round(port_bin_sr, 3), round(port_bh_sr, 3)),
    AnnRet     = c(round(port_ret, 4), round(port_bin_ret, 4), round(port_bh_ret, 4)),
    MaxDD      = c(round(port_dd, 4), round(port_bin_dd, 4), round(port_bh_dd, 4))
  )

  port_results |>
    gt() |>
    tab_header(
      title = sprintf("Inverse-Vol Portfolio (%d assets, %dd lookback)", n_assets, vol_lookback),
      subtitle = sprintf("From %s → %s; monthly rebalance; drag = 0.2%%", portfolio_start,
                         format(last(index(port_scaled)), "%Y-%m-%d"))
    ) |>
    fmt_percent(columns = c(AnnRet, MaxDD), decimals = 2) |>
    fmt_number(columns = SR, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    cols_label(SR = "Sharpe", AnnRet = "Ann.Return", MaxDD = "Max Drawdown") |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes()) |>
    gtsave(sprintf("%s/portfolio-metrics-involwt-ls.html", reportPath))

  webshot2::webshot(
    sprintf("%s/portfolio-metrics-involwt-ls.html", reportPath),
    sprintf("%s/portfolio-metrics-involwt-ls.png", reportPath),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # ── cumulative chart: Scaled LS vs Binary LS vs B&H ──
  combined <- na.omit(merge(port_scaled, port_bin, port_bh))
  names(combined) <- c("Scaled LS (InvVol)", "Binary LS (InvVol)", "B&H (InvVol)")
  Common.PlotCumReturns(combined, sprintf("Inverse-Vol Portfolio (%d assets)", n_assets),
    sprintf("Strategy Nine — All Assets InvVol from %s", portfolio_start),
    sprintf("%s/portfolio-ew-involwt-ls.cumret.png", reportPath), NULL)

  # ── annual metrics for the portfolio ──
  annual_ret <- apply.yearly(combined, Return.cumulative)
  ar_tbl <- fortify(annual_ret) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))

  tbl <- ar_tbl |>
    gt() |>
    tab_header(
      title = sprintf("Inverse-Vol Portfolio (%d assets) — Annual Returns", n_assets),
      subtitle = sprintf("Scaled LS vs Binary LS vs B&H from %s", portfolio_start)
    ) |>
    fmt_percent(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

  for (c in names(ar_tbl)[-1]) {
    neg <- which(ar_tbl[[c]] < 0)
    if (length(neg) > 0) tbl <- tbl |>
      tab_style(style = cell_text(color = "#8B0000"),
                locations = cells_body(columns = all_of(c), rows = neg))
  }

  tbl |> gtsave(sprintf("%s/portfolio-annual-involwt.returns-ls.html", reportPath))
  webshot2::webshot(
    sprintf("%s/portfolio-annual-involwt.returns-ls.html", reportPath),
    sprintf("%s/portfolio-annual-involwt.returns-ls.table.png", reportPath),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # ── annual max drawdown ──
  annual_dd <- apply.yearly(combined, maxDrawdown)
  dd_tbl <- fortify(annual_dd) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))

  tbl <- dd_tbl |>
    gt() |>
    tab_header(
      title = sprintf("Inverse-Vol Portfolio (%d assets) — Max Drawdown", n_assets),
      subtitle = sprintf("Scaled LS vs Binary LS vs B&H from %s", portfolio_start)
    ) |>
    fmt_percent(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

  for (c in names(dd_tbl)[-1]) {
    severe <- which(dd_tbl[[c]] > 0.20)
    mild   <- which(dd_tbl[[c]] < 0.10 & dd_tbl[[c]] > 0)
    if (length(severe) > 0) tbl <- tbl |>
      tab_style(style = cell_text(color = "#8B0000", weight = "bold"),
                locations = cells_body(columns = all_of(c), rows = severe))
    if (length(mild) > 0) tbl <- tbl |>
      tab_style(style = cell_text(color = "#006400"),
                locations = cells_body(columns = all_of(c), rows = mild))
  }

  tbl |> gtsave(sprintf("%s/portfolio-annual-involwt.drawdowns-ls.html", reportPath))
  webshot2::webshot(
    sprintf("%s/portfolio-annual-involwt.drawdowns-ls.html", reportPath),
    sprintf("%s/portfolio-annual-involwt.drawdowns-ls.table.png", reportPath),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # ── annual Sharpe ──
  annual_sr <- apply.yearly(combined, SharpeRatio.annualized)
  sr_tbl <- fortify(annual_sr) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))

  tbl <- sr_tbl |>
    gt() |>
    tab_header(
      title = sprintf("Inverse-Vol Portfolio (%d assets) — Sharpe Ratio", n_assets),
      subtitle = sprintf("Scaled LS vs Binary LS vs B&H from %s", portfolio_start)
    ) |>
    fmt_number(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

  for (c in names(sr_tbl)[-1]) {
    neg <- which(sr_tbl[[c]] < 0)
    if (length(neg) > 0) tbl <- tbl |>
      tab_style(style = cell_text(color = "#8B0000"),
                locations = cells_body(columns = all_of(c), rows = neg))
  }

  tbl |> gtsave(sprintf("%s/portfolio-annual-involwt.sharpe-ls.html", reportPath))
  webshot2::webshot(
    sprintf("%s/portfolio-annual-involwt.sharpe-ls.html", reportPath),
    sprintf("%s/portfolio-annual-involwt.sharpe-ls.table.png", reportPath),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))
}

odbcClose(lcon)
odbcClose(lconMcx)
odbcClose(lconUS2)
dbDisconnect(pgCon)

print("Done.")

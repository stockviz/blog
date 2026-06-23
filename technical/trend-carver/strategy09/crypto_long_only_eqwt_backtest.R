# Backtest: Strategy Nine Long-Only Equal Weight on Crypto — Binary + Scaled
# Compares the binary (1/0) and scaled forecast (weight ∈ [0,1]) long-only
# variants, both built from equal-weighted EWMAC filters (no cost screen).
# Drag = 0.2% per trade (entry + exit).
#
# Data: binance_crypto_historical_1h (PostgreSQL / sweden)
# Symbols: BTCUSDT, ETHUSDT, SOLUSDT
#
# Outputs:
#   - summary gt table with Ann.Ret, Sharpe, MaxDD vs B&H
#   - per-symbol cumulative return charts (scaled vs binary vs B&H)
#   - all-symbols combined cumulative charts
#   - annual returns / drawdowns / Sharpe gt tables per symbol

library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('gt')
library('webshot2')
library('viridis')
library('ggthemes')
library('RPostgres')
library('xts')

source("strategy_nine.R")
source("/mnt/data/blog/common/plot.common.r")
source("/mnt/hollandC/StockViz/R/config.r")

pdf(NULL)
options("scipen" = 100)

reportPath <- "."
drag <- 0.2 / 100

symbols <- c("BTCUSDT", "ETHUSDT", "SOLUSDT")

# ── connect to PostgreSQL (sweden) ────────────────────────────────────────────
print("connecting to sweden (PostgreSQL)...")
pgCon <- dbConnect(
  RPostgres::Postgres(),
  host   = 'sweden',
  user   = ldbuser2,
  password = ldbpassword2,
  dbname   = 'StockVizDyn',
  sslmode  = 'allow'
)

# ── load hourly crypto prices ─────────────────────────────────────────────────
print("loading crypto prices from binance_crypto_historical_1h...")

# get max open_time to determine data range
maxOt <- dbGetQuery(pgCon,
  "select max(open_time) from binance_crypto_historical_1h where symbol = 'BTCUSDT'"
)[[1]]
# start from 2018-01-01 UTC (millisecond epoch, matching DB convention)
startOt <- as.numeric(as.POSIXct("2018-01-01", tz = "UTC")) * 1000
endDate <- as.POSIXct(maxOt / 1000, tz = "UTC")

cat(sprintf("  data range: 2018-01-01 → %s\n", format(endDate, "%Y-%m-%d")))

pXts <- NULL
loaded_symbols <- c()
for (sym in symbols) {
  pxDt <- dbGetQuery(pgCon,
    "select (close_time / 1000.0)::float8 as close_time_sec, px_close
     from binance_crypto_historical_1h
     where symbol = $1 and close_time >= $2
     order by close_time",
    params = list(sym, startOt))

  if (nrow(pxDt) == 0) {
    cat(sprintf("  WARNING: no data for %s\n", sym))
    next
  }
  cat(sprintf("  %s: %d rows\n", sym, nrow(pxDt)))
  pXts <- merge.xts(pXts, xts(pxDt$px_close, order.by = as.POSIXct(pxDt$close_time_sec, origin = "1970-01-01", tz = "UTC")))
  loaded_symbols <- c(loaded_symbols, sym)
}
names(pXts) <- loaded_symbols
symbols <- loaded_symbols

if (length(symbols) == 0) {
  stop("No crypto data loaded — check PostgreSQL connection and binance_crypto_historical_1h table.")
}

cat(sprintf("  loaded %d symbols\n", length(symbols)))
  cat(sprintf("  pXts rows: %d, date range: %s → %s\n",
            nrow(pXts),
            as.character(as.Date(first(index(pXts)))),
            as.character(as.Date(last(index(pXts))))))

# convert hourly to daily prices — per-symbol, no outer join (avoids NA propagation in EWMA)
cat("  converting hourly to daily prices...\n")
daily_prices <- list()
daily_rets   <- list()
for (sym in symbols) {
  # extract this symbol's hourly prices (no NAs from other symbols)
  sym_xts <- na.omit(pXts[, sym])
  daily <- to.period(sym_xts, period = "days")
  close_col <- daily[, 4]
  index(close_col) <- as.Date(index(close_col))
  daily_prices[[sym]] <- close_col
  daily_rets[[sym]] <- dailyReturn(close_col)
  cat(sprintf("  %s: %d daily bars, %s → %s, anyNA=%s\n",
              sym, nrow(close_col),
              as.character(first(index(close_col))),
              as.character(last(index(close_col))),
              any(is.na(close_col))))
}

# ── run backtests: binary LO + scaled LO, equal weight only ──────────────────
results <- tibble()
binary_lo <- list()
scaled_lo <- list()
bh_strats  <- list()

cat("\n=== CRYPTO LONG-ONLY EQUAL WEIGHT ===\n")

for (sym in symbols) {
  prices_sym <- daily_prices[[sym]]
  rets_sym   <- daily_rets[[sym]]
  cat(sprintf("  %s (daily rows: %d) ...\n", sym, nrow(prices_sym)))

  sig <- tryCatch({
    strategy_nine_signal(prices_sym, use_cost_screen = FALSE)
  }, error = function(e) {
    cat(sprintf("    ERROR: %s\n", e$message))
    NULL
  })

  if (is.null(sig)) {
    cat(sprintf("    sig is NULL\n"))
    next
  }

  na_count <- sum(is.na(sig))
  valid_count <- sum(!is.na(sig))
  cat(sprintf("    sig: %d NA, %d valid\n", na_count, valid_count))

  if (all(is.na(sig))) {
    fc <- attr(sig, "forecast")
    cat(sprintf("    forecast: %d NA, %d NaN, %d valid\n",
                sum(is.na(coredata(fc))), sum(is.nan(coredata(fc))),
                sum(!is.na(coredata(fc)) & !is.nan(coredata(fc)))))
    next
  }

  # extract the FDM-adjusted forecast and scale to [-1, +1]
  forecast_xts <- attr(sig, "forecast")
  if (is.null(forecast_xts)) {
    cat(sprintf("    forecast attr is NULL\n"))
    next
  }
  scaled_fc <- forecast_xts / 20

  retL1 <- stats::lag(rets_sym, -1)
  common_dates <- intersect(intersect(index(na.omit(sig)), index(retL1)),
                            index(na.omit(scaled_fc)))
  sig_aligned    <- sig[common_dates]
  fc_aligned     <- scaled_fc[common_dates]
  retL1_aligned  <- retL1[common_dates]

  # ── binary long-only ──
  strat_bin <- ifelse(coredata(sig_aligned) == 1,
                      coredata(retL1_aligned), 0)
  trd <- coredata(sig_aligned)
  trd <- trd - c(NA, trd[-length(trd)])
  strat_bin <- ifelse(trd != 0 & !is.na(trd), strat_bin - drag, strat_bin)
  bin_xts <- xts(strat_bin, order.by = common_dates)
  binary_lo[[sym]] <- bin_xts

  # ── scaled long-only (weight ∈ [0, 1]) ──
  w_lo <- pmax(pmin(coredata(fc_aligned), 1), 0)
  strat_sclo <- w_lo * coredata(retL1_aligned)
  strat_sclo <- ifelse(trd != 0 & !is.na(trd), strat_sclo - drag, strat_sclo)
  sclo_xts <- xts(strat_sclo, order.by = common_dates)
  scaled_lo[[sym]] <- sclo_xts

  # B&H on same dates
  bh_xts <- retL1_aligned
  bh_strats[[sym]] <- bh_xts

  # ── metrics ──
  bin_sr   <- SharpeRatio.annualized(bin_xts)[1, 1]
  bin_ret  <- as.numeric(Return.annualized(bin_xts))
  bin_dd   <- as.numeric(maxDrawdown(bin_xts))

  sclo_sr  <- SharpeRatio.annualized(sclo_xts)[1, 1]
  sclo_ret <- as.numeric(Return.annualized(sclo_xts))
  sclo_dd  <- as.numeric(maxDrawdown(sclo_xts))

  bh_sr   <- SharpeRatio.annualized(bh_xts)[1, 1]
  bh_ret  <- as.numeric(Return.annualized(bh_xts))
  bh_dd   <- as.numeric(maxDrawdown(bh_xts))

  results <- rbind(results, tibble(
    Symbol      = sym,
    Type        = "binary long-only",
    Ret         = round(bin_ret, 4),
    Sharpe      = round(bin_sr, 3),
    MaxDD       = round(bin_dd, 4),
    BH_Ret      = round(bh_ret, 4),
    BH_Sharpe   = round(bh_sr, 3),
    BH_MaxDD    = round(bh_dd, 4)
  ))
  results <- rbind(results, tibble(
    Symbol      = sym,
    Type        = "scaled long-only",
    Ret         = round(sclo_ret, 4),
    Sharpe      = round(sclo_sr, 3),
    MaxDD       = round(sclo_dd, 4),
    BH_Ret      = round(bh_ret, 4),
    BH_Sharpe   = round(bh_sr, 3),
    BH_MaxDD    = round(bh_dd, 4)
  ))
}

# ── console summary ──────────────────────────────────────────────────────────
cat("\n=== Summary Metrics ===\n")
print(results, n = Inf)

if (nrow(results) == 0) {
  stop("No valid backtest results — check debug output above for per-symbol status.")
}

# ── gt summary table ─────────────────────────────────────────────────────────
results |>
  gt(groupname_col = "Symbol") |>
  tab_header(
    title = "Crypto — Strategy Nine Long-Only Equal Weight",
    subtitle = sprintf("2018-01-01 → %s; drag = %.1f%%",
                       format(endDate, "%Y-%m-%d"), drag * 100)
  ) |>
  tab_spanner(label = "Strategy Nine", columns = c(Ret, Sharpe, MaxDD)) |>
  tab_spanner(label = "Buy & Hold", columns = c(BH_Ret, BH_Sharpe, BH_MaxDD)) |>
  fmt_percent(columns = c(Ret, BH_Ret, MaxDD, BH_MaxDD), decimals = 2) |>
  fmt_number(columns = c(Sharpe, BH_Sharpe), decimals = 2) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = list(cell_fill(color = "#f0fff0"), cell_text(weight = "bold")),
    locations = cells_body(rows = Type == "scaled long-only")
  ) |>
  cols_label(
    Ret     = "Ann.Ret",  Sharpe  = "Sharpe",  MaxDD   = "MaxDD",
    BH_Ret  = "Ann.Ret",  BH_Sharpe = "Sharpe", BH_MaxDD = "MaxDD"
  ) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_source_notes()
  ) |>
  gtsave(sprintf("%s/crypto-lo-eqwt-summary.html", reportPath))

webshot2::webshot(
  sprintf("%s/crypto-lo-eqwt-summary.html", reportPath),
  sprintf("%s/crypto-lo-eqwt-summary.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10)
)

# ── per-symbol cumulative return charts ──────────────────────────────────────
cat("\n=== Cumulative Return Charts ===\n")
for (sym in symbols) {
  if (!sym %in% names(binary_lo)) next

  combined <- na.omit(merge(scaled_lo[[sym]],
                            binary_lo[[sym]],
                            bh_strats[[sym]]))
  names(combined) <- c("Scaled LO", "Binary LO", "B&H")

  Common.PlotCumReturns(combined, sym,
    "Crypto Strategy Nine: Scaled vs Binary Long-Only (equal weight)",
    sprintf("%s/crypto-%s.lo-eqwt.cumret.png", reportPath, sym), NULL)
}

# ── combined multi-symbol charts ─────────────────────────────────────────────
# Binary LO — all valid symbols together
cat("\n=== Combined Binary Long-Only ===\n")
valid_bin <- names(binary_lo)
if (length(valid_bin) > 0) {
  bin_combined <- do.call(merge.xts, lapply(valid_bin, \(sym) binary_lo[[sym]]))
  names(bin_combined) <- valid_bin
  Common.PlotCumReturns(bin_combined, "Crypto — All Symbols",
    "Crypto Strategy Nine Binary Long-Only — Equal Weight (all symbols)",
    sprintf("%s/crypto-all.lo-eqwt.binary.cumret.png", reportPath), NULL)
}

# Scaled LO — all valid symbols together
cat("=== Combined Scaled Long-Only ===\n")
valid_sclo <- names(scaled_lo)
if (length(valid_sclo) > 0) {
  sclo_combined <- do.call(merge.xts, lapply(valid_sclo, \(sym) scaled_lo[[sym]]))
  names(sclo_combined) <- valid_sclo
  Common.PlotCumReturns(sclo_combined, "Crypto — All Symbols",
    "Crypto Strategy Nine Scaled Long-Only — Equal Weight (all symbols)",
    sprintf("%s/crypto-all.lo-eqwt.scaled.cumret.png", reportPath), NULL)
}

# ── per-symbol annual tables ─────────────────────────────────────────────────
cat("\n=== Annual Tables ===\n")
for (sym in symbols) {
  if (!sym %in% names(scaled_lo)) next

  combined <- na.omit(merge(scaled_lo[[sym]],
                            binary_lo[[sym]],
                            bh_strats[[sym]]))
  names(combined) <- c("Scaled LO", "Binary LO", "B&H")
  cols <- names(combined)

  # ─ annual returns bar chart ─
  annual_ret <- apply.yearly(combined, Return.cumulative)
  annual_df <- fortify(annual_ret, melt = TRUE)
  names(annual_df) <- c("Year", "Scenario", "Return")
  annual_df$Year <- as.numeric(format(annual_df$Year, "%Y"))

  p <- ggplot(annual_df, aes(x = factor(Year), y = Return, fill = Scenario)) +
    geom_col(position = "dodge", width = 0.8) +
    scale_fill_viridis_d(end = 0.9) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = sprintf("Crypto %s", sym),
         subtitle = "Annual Returns — Scaled vs Binary Long-Only",
         caption = "@StockViz",
         x = NULL, y = NULL) +
    theme_economist() +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 1),
          axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(sprintf("%s/crypto-%s.lo-eqwt.annual.returns.png", reportPath, sym),
         plot = p, width = 12, height = 6)

  # ─ annual returns gt table ─
  ar_tbl <- fortify(annual_ret) |>
    rename(Year = Index) |>
    mutate(Year = format(Year, "%Y"))
  scenario_cols <- setdiff(cols, "B&H")

  tbl <- ar_tbl |>
    gt() |>
    tab_header(
      title = sprintf("Crypto %s — Annual Returns", sym),
      subtitle = "Scaled LO vs Binary LO vs B&H (□ = beats B&H by >2%)"
    ) |>
    fmt_percent(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"),
              locations = cells_source_notes())

  for (c in cols) {
    neg_rows <- which(ar_tbl[[c]] < 0)
    if (length(neg_rows) > 0)
      tbl <- tbl |>
        tab_style(style = cell_text(color = "#8B0000"),
                  locations = cells_body(columns = all_of(c), rows = neg_rows))
  }

  for (c in scenario_cols) {
    beat_rows <- which(ar_tbl[[c]] - ar_tbl[["B&H"]] > 0.02)
    if (length(beat_rows) > 0)
      tbl <- tbl |>
        tab_style(
          style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
          locations = cells_body(columns = all_of(c), rows = beat_rows))
  }

  tbl |>
    gtsave(sprintf("%s/crypto-%s.lo-eqwt.annual.returns.html", reportPath, sym))
  webshot2::webshot(
    sprintf("%s/crypto-%s.lo-eqwt.annual.returns.html", reportPath, sym),
    sprintf("%s/crypto-%s.lo-eqwt.annual.returns.table.png", reportPath, sym),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # ─ annual drawdowns gt table ─
  annual_dd <- apply.yearly(combined, maxDrawdown)
  dd_tbl <- fortify(annual_dd) |>
    rename(Year = Index) |>
    mutate(Year = format(Year, "%Y"))

  tbl <- dd_tbl |>
    gt() |>
    tab_header(
      title = sprintf("Crypto %s — Max Drawdown", sym),
      subtitle = "Scaled LO vs Binary LO vs B&H (□ = less DD than B&H by >2%)"
    ) |>
    fmt_percent(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"),
              locations = cells_source_notes())

  for (c in cols) {
    severe <- which(dd_tbl[[c]] > 0.20)
    mild   <- which(dd_tbl[[c]] < 0.10 & dd_tbl[[c]] > 0)
    if (length(severe) > 0)
      tbl <- tbl |>
        tab_style(
          style = cell_text(color = "#8B0000", weight = "bold"),
          locations = cells_body(columns = all_of(c), rows = severe))
    if (length(mild) > 0)
      tbl <- tbl |>
        tab_style(
          style = cell_text(color = "#006400"),
          locations = cells_body(columns = all_of(c), rows = mild))
  }

  for (c in scenario_cols) {
    beat_rows <- which(dd_tbl[["B&H"]] - dd_tbl[[c]] > 0.02)
    if (length(beat_rows) > 0)
      tbl <- tbl |>
        tab_style(
          style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
          locations = cells_body(columns = all_of(c), rows = beat_rows))
  }

  tbl |>
    gtsave(sprintf("%s/crypto-%s.lo-eqwt.annual.drawdowns.html", reportPath, sym))
  webshot2::webshot(
    sprintf("%s/crypto-%s.lo-eqwt.annual.drawdowns.html", reportPath, sym),
    sprintf("%s/crypto-%s.lo-eqwt.annual.drawdowns.table.png", reportPath, sym),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # ─ annual Sharpe gt table ─
  annual_sr <- apply.yearly(combined, SharpeRatio.annualized)
  sr_tbl <- fortify(annual_sr) |>
    rename(Year = Index) |>
    mutate(Year = format(Year, "%Y"))

  tbl <- sr_tbl |>
    gt() |>
    tab_header(
      title = sprintf("Crypto %s — Sharpe Ratio", sym),
      subtitle = "Scaled LO vs Binary LO vs B&H (□ = beats B&H by >2)"
    ) |>
    fmt_number(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"),
              locations = cells_source_notes())

  for (c in cols) {
    neg_rows <- which(sr_tbl[[c]] < 0)
    if (length(neg_rows) > 0)
      tbl <- tbl |>
        tab_style(style = cell_text(color = "#8B0000"),
                  locations = cells_body(columns = all_of(c), rows = neg_rows))
  }

  for (c in scenario_cols) {
    beat_rows <- which(sr_tbl[[c]] - sr_tbl[["B&H"]] > 2)
    if (length(beat_rows) > 0)
      tbl <- tbl |>
        tab_style(
          style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
          locations = cells_body(columns = all_of(c), rows = beat_rows))
  }

  tbl |>
    gtsave(sprintf("%s/crypto-%s.lo-eqwt.annual.sharpe.html", reportPath, sym))
  webshot2::webshot(
    sprintf("%s/crypto-%s.lo-eqwt.annual.sharpe.html", reportPath, sym),
    sprintf("%s/crypto-%s.lo-eqwt.annual.sharpe.table.png", reportPath, sym),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  cat(sprintf("  %s done.\n", sym))
}

print("Done.")

#!/usr/bin/env Rscript
# Bézier-Curve Market Turbulence on NIFTY and synthetic SELECT futures
# =====================================================================
# Exploratory implementation of Zheng & Dong (Mathematics 2024, 12, 1416).
#
# The paper leaves the curve fit, pivot detector, threshold, sizing, exits,
# and costs unspecified. This script makes those choices explicit and keeps
# them causal:
#   - rolling quadratic Bézier channels over prior CLOSE observations;
#   - three equal historical segments supply upper maxima/lower minima;
#   - turbulence = abs(MA20 - MA50) / close >= threshold;
#   - boundary/turbulence observations are lagged one trading day;
#   - reversal position is held until the opposite boundary signal;
#   - long/flat and long/short variants, 10 bps per unit traded;
#   - threshold and channel length are selected on train (through 2019-12-31).
#
# NIFTY uses the canonical BHAV_EQ_FUT monthly roll calendar. SELECT uses the
# validated synthetic MIDCAP SELECT front-month series constructed in
# midcpnifty-synth-futures. BHAV_EQ_FUT availability in the repository is
# close-only, so CLOSE is used for both channel construction and the boundary
# test; this is reported in the output README and console summary.
#
# Usage:
#   Rscript turbulence_futures.R [--outdir PATH]
###############################################################################

suppressPackageStartupMessages({
  library("RODBC")
  library("xts")
  library("zoo")
  library("PerformanceAnalytics")
  library("ggplot2")
  library("tidyverse")
  library("ggthemes")
  library("viridis")
  library("patchwork")
  library("scales")
  library("gt")
  library("webshot2")
})

options("scipen" = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

source("/mnt/ssd1/stockviz/R2/backtests/common/runtime.R")
source_common("returns")
source_common("futures")
source_common("charts")
CONFIG_FILE <- Sys.getenv("STOCKVIZ_CONFIG", "/mnt/hollandC/StockViz/R/config.r")
if (!file.exists(CONFIG_FILE)) {
  stop(sprintf("StockViz config not found: %s (set STOCKVIZ_CONFIG)", CONFIG_FILE))
}
source(CONFIG_FILE)

# ── Parameters ───────────────────────────────────────────────────────────────
TRAIN_END   <- as.Date("2019-12-31")
POST_START  <- as.Date("2020-05-01")
SYNTH_FILE  <- file.path(BACKTEST_ROOT, "midcpnifty-synth-futures",
                         "synthetic_midcpnifty_front.csv")
CHANNEL_GRID <- c(42L, 63L, 84L, 126L)
THRESH_GRID  <- c(0.005, 0.010, 0.020, 0.030, 0.050)
DRAG         <- 0.0025
ROLL_OFFSET  <- 5L
CURVE_T      <- 0.75

parse_outdir <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  file_arg <- grep("^--file=", commandArgs(), value = TRUE)
  script_dir <- if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1L]), mustWork = TRUE))
  } else getwd()
  out <- script_dir
  i <- 1L
  while (i <= length(args)) {
    if (grepl("^--outdir=", args[i])) out <- sub("^--outdir=", "", args[i])
    if (args[i] %in% c("--outdir", "-o") && i < length(args)) out <- args[i + 1L]
    i <- i + 1L
  }
  out
}
OUT_DIR <- parse_outdir()
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ── Data loading ─────────────────────────────────────────────────────────────
load_nifty <- function() {
  con <- odbcDriverConnect(
    sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            ldbserver, "StockViz", ldbuser, ldbpassword),
    case = "nochange", believeNRows = TRUE)
  on.exit(try(odbcClose(con), silent = TRUE), add = TRUE)
  df <- sqlQuery(con, paste(
    "SELECT SYMBOL, TIME_STAMP, EXPIRY_DT, PX_CLOSE",
    "FROM BHAV_EQ_FUT",
    "WHERE SYMBOL = 'NIFTY' AND STRIKE_PR = 0",
    "ORDER BY TIME_STAMP, EXPIRY_DT"))
  if (!is.data.frame(df) || !nrow(df)) stop("NIFTY futures query failed or returned no rows")
  df$TIME_STAMP <- as.Date(df$TIME_STAMP)
  df$EXPIRY_DT <- as.Date(df$EXPIRY_DT)
  df <- df[is.finite(df$PX_CLOSE) & df$PX_CLOSE > 0, ]
  cal <- build_monthly_futures_calendar(df, min(df$TIME_STAMP), ROLL_OFFSET)
  held_price <- function(fut, calendar) {
    out <- rep(NA_real_, length(calendar$trading_dates))
    for (i in seq_along(out)) {
      exp <- calendar$held_after[i]
      if (is.na(exp)) next
      rows <- fut$EXPIRY_DT == exp & fut$TIME_STAMP == calendar$trading_dates[i]
      if (any(rows)) out[i] <- fut$PX_CLOSE[which(rows)[1L]]
    }
    xts(out, calendar$trading_dates)
  }
  px <- held_price(df, cal)
  px <- px[is.finite(as.numeric(px)) & as.numeric(px) > 0]
  colnames(px) <- "NIFTY"
  px
}

load_select <- function() {
  if (!file.exists(SYNTH_FILE)) stop(sprintf("Synthetic SELECT file not found: %s", SYNTH_FILE))
  df <- read.csv(SYNTH_FILE, stringsAsFactors = FALSE)
  assert_columns(df, c("date", "synth_fut_close"), "synthetic SELECT CSV")
  df$date <- as.Date(df$date)
  df <- df[is.finite(df$synth_fut_close) & df$synth_fut_close > 0, ]
  px <- xts(df$synth_fut_close, order.by = df$date)
  px <- px[!duplicated(index(px))]
  colnames(px) <- "SELECT"
  px
}

cat("Loading NIFTY futures and synthetic SELECT futures...\n")
prices <- list(NIFTY = load_nifty(), SELECT = load_select())
for (nm in names(prices)) {
  cat(sprintf("  %s: %d observations (%s -> %s)\n", nm, NROW(prices[[nm]]),
              format(start(prices[[nm]])), format(end(prices[[nm]]))))
}

# ── Causal Bézier channel and strategy ───────────────────────────────────────
bezier_value <- function(p0, p1, p2, t) (1 - t)^2 * p0 + 2 * (1 - t) * t * p1 + t^2 * p2

build_channel <- function(px, channel_len) {
  v <- as.numeric(px)
  n <- length(v)
  upper <- lower <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (i <= channel_len) next
    hist <- v[(i - channel_len):(i - 1L)]
    if (any(!is.finite(hist))) next
    cuts <- floor(seq(1, channel_len + 1, length.out = 4))
    segs <- lapply(seq_len(3L), function(k) hist[cuts[k]:(cuts[k + 1L] - 1L)])
    upper[i] <- bezier_value(max(segs[[1L]]), max(segs[[2L]]),
                              max(segs[[3L]]), CURVE_T)
    lower[i] <- bezier_value(min(segs[[1L]]), min(segs[[2L]]),
                              min(segs[[3L]]), CURVE_T)
  }
  # The boundary at date t is formed only from data through t-1.
  xts(cbind(upper, lower), order.by = index(px))
}

run_system <- function(px, channel_len, threshold) {
  v <- as.numeric(px)
  n <- length(v)
  ma20 <- zoo::rollmean(v, 20L, fill = NA, align = "right")
  ma50 <- zoo::rollmean(v, 50L, fill = NA, align = "right")
  turbulent <- is.finite(ma20) & is.finite(ma50) & is.finite(v) &
    abs(ma20 - ma50) / v >= threshold
  ch <- build_channel(px, channel_len)
  upper <- as.numeric(ch[, "upper"])
  lower <- as.numeric(ch[, "lower"])
  # Signal generated at t-1, applied to the return at t.
  pos <- rep(0, n)
  for (i in 2:n) {
    prior <- i - 1L
    pos[i] <- pos[i - 1L]
    if (isTRUE(turbulent[prior]) && is.finite(v[prior])) {
      if (is.finite(lower[prior]) && v[prior] <= lower[prior]) pos[i] <- 1
      if (is.finite(upper[prior]) && v[prior] >= upper[prior]) pos[i] <- -1
    }
  }
  ret <- c(NA_real_, v[-1L] / v[-n] - 1)
  turnover <- abs(c(NA_real_, diff(pos)))
  strat_ls <- pos * ret - DRAG * turnover
  strat_lo <- pmax(pos, 0) * ret - DRAG * abs(c(NA_real_, diff(pmax(pos, 0))))
  bh <- ret
  data.frame(date = as.Date(index(px)), price = v, upper = upper, lower = lower,
             turbulent = turbulent, position = pos, turnover = turnover,
             ls = strat_ls, long_flat = strat_lo, bh = bh)
}

metric_row <- function(rets, instrument, system, window, selected_len, selected_thr) {
  x <- xts(rets, order.by = window$date)
  x <- x[is.finite(as.numeric(x))]
  m <- strategy_metrics(x)
  data.frame(Instrument = instrument, System = system, N = as.numeric(m[["N"]]),
             CAGR = as.numeric(m[["CAGR"]]), Vol = as.numeric(m[["Vol"]]),
             Sharpe = as.numeric(m[["Sharpe"]]), MaxDD = as.numeric(m[["MaxDD"]]),
             AvgExposure = mean(abs(window$position), na.rm = TRUE),
             Turnover = mean(window$turnover, na.rm = TRUE),
             SelectedChannel = selected_len, SelectedThreshold = selected_thr,
             stringsAsFactors = FALSE)
}

select_params <- function(px) {
  rows <- list()
  k <- 0L
  for (len in CHANNEL_GRID) for (thr in THRESH_GRID) {
    bt <- run_system(px, len, thr)
    train <- bt$ls[bt$date <= TRAIN_END]
    train <- train[is.finite(train)]
    sr <- if (length(train) > 2L && stats::sd(train) > 0)
      mean(train) / stats::sd(train) * sqrt(252) else NA_real_
    k <- k + 1L
    rows[[k]] <- data.frame(Channel = len, Threshold = thr, TrainSharpe = sr,
                            TrainCAGR = if (length(train)) prod(1 + train)^(252 / length(train)) - 1 else NA_real_)
  }
  sweep <- do.call(rbind, rows)
  sweep <- sweep[order(-sweep$TrainSharpe, sweep$Channel, sweep$Threshold), ]
  list(best = sweep[1L, c("Channel", "Threshold")], sweep = sweep)
}

all_metrics <- list()
all_bt <- list()
for (instrument in names(prices)) {
  px <- prices[[instrument]]
  selection <- select_params(px)
  picked <- selection$best
  len <- as.integer(picked$Channel)
  thr <- as.numeric(picked$Threshold)
  bt <- run_system(px, len, thr)
  all_bt[[instrument]] <- bt
  write.csv(selection$sweep, file.path(OUT_DIR, sprintf("parameter_sweep_train_%s.csv", instrument)), row.names = FALSE)
  cat(sprintf("  %s selected channel=%d threshold=%.1f%% on train\n", instrument, len, 100 * thr))
  for (window_name in c("pre", "post", "full")) {
    w <- switch(window_name,
                pre = bt[bt$date <= TRAIN_END, ],
                post = bt[bt$date >= POST_START, ],
                full = bt)
    for (system in c("B&H", "Turbulence Long/Flat", "Turbulence Long/Short")) {
      col <- switch(system, `B&H` = "bh", `Turbulence Long/Flat` = "long_flat", `Turbulence Long/Short` = "ls")
      all_metrics[[length(all_metrics) + 1L]] <-
        cbind(Window = window_name, metric_row(w[[col]], instrument, system, w, len, thr))
    }
  }
  daily <- xts(bt[, c("long_flat", "ls", "bh")], order.by = bt$date)
  colnames(daily) <- c("Turbulence Long/Flat", "Turbulence Long/Short", "B&H")
  write.csv(bt, file.path(OUT_DIR, sprintf("daily_%s.csv", instrument)), row.names = FALSE)
  for (window_name in c("pre", "post", "full")) {
    range <- switch(window_name, pre = paste0("/", TRAIN_END), post = paste0(POST_START, "/"), full = NULL)
    plotCumDrawdown(list(`Turbulence Long/Flat` = daily[, 1L],
                         `Turbulence Long/Short` = daily[, 2L],
                         `B&H` = daily[, 3L]), dateRange = range,
                    title = sprintf("Market Turbulence — %s futures — %s", instrument, window_name),
                    subtitle = sprintf("Causal rolling Bézier channel; MA20/MA50 turbulence; channel=%d, threshold=%.1f%%", len, 100 * thr),
                    outPath = file.path(OUT_DIR, sprintf("cumulative_%s_%s.png", instrument, window_name)),
                    linetypeBySeries = TRUE)
  }
}

metrics <- do.call(rbind, all_metrics)
for (window_name in c("pre", "post", "full")) {
  out <- metrics[metrics$Window == window_name, ]
  write.csv(out, file.path(OUT_DIR, sprintf("metrics_%s.csv", window_name)), row.names = FALSE)
  metric_table <- out |>
    gt() |>
    tab_header(
      title = "Market Turbulence Futures Metrics",
      subtitle = sprintf("%s window | train selection through %s; post begins %s",
                         window_name, TRAIN_END, POST_START)
    ) |>
    fmt_number(columns = c(N, SelectedChannel), decimals = 0) |>
    fmt_percent(columns = c(CAGR, Vol, MaxDD, AvgExposure, Turnover, SelectedThreshold), decimals = 1) |>
    fmt_number(columns = Sharpe, decimals = 2) |>
    cols_label(N = "Days", CAGR = "CAGR", Vol = "Volatility", MaxDD = "Max DD",
               AvgExposure = "Avg Exposure", Turnover = "Turnover",
               SelectedChannel = "Channel", SelectedThreshold = "Threshold") |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes())
  gtsave(metric_table, filename = file.path(OUT_DIR, sprintf("metrics_%s.png", window_name)))
}
writeLines(c(
  "Bézier-Curve Market Turbulence — exploratory futures test",
  "",
  "The paper does not fully specify the implementation. This run uses rolling quadratic",
  "channels from prior closes, MA20/MA50 normalized spread turbulence, one-day signal lag,",
  "unit exposure, reversal positions held until the opposite boundary, and 25 bps per unit traded.",
  "BHAV_EQ_FUT is close-only in this data path; closes are used instead of OHLC pivots.",
  "NIFTY uses the canonical monthly futures calendar; SELECT uses the validated synthetic",
  "MIDCAP SELECT front-month series from midcpnifty-synth-futures.",
  "Train selection: through 2019-12-31. Post evaluation: from 2020-05-01.",
  "Metrics and cumulative+drawdown charts are emitted for pre, post, and full windows.",
  "",
  "This is an exploratory research implementation, not a validated trading system."
), file.path(OUT_DIR, "README.txt"))
cat(sprintf("Completed. Outputs written to %s\n", normalizePath(OUT_DIR, mustWork = FALSE)))

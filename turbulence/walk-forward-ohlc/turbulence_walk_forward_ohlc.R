#!/usr/bin/env Rscript
# Combined walk-forward re-selection with causal OHLC turning points
# ====================================================================
# NIFTY futures use observed BHAV_EQ_FUT OHLC. SELECT uses the validated
# SELECT close is the validated synthetic front-month close, with OHLC
# reconstructed by scaling MIDCAP SELECT PR OHLC where available and using
# long-history NIFTY 50 OHLC as an explicit range proxy before that date.

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
if (!file.exists(CONFIG_FILE)) stop(sprintf("StockViz config not found: %s", CONFIG_FILE))
source(CONFIG_FILE)

TRAIN_YEARS <- 5L
TEST_YEARS <- 1L
TRAIN_END <- as.Date("2019-12-31")
POST_START <- as.Date("2020-05-01")
DRAG <- 0.0025
ROLL_OFFSET <- 5L
PIVOT_SPAN <- 2L
CHANNEL_GRID <- c(42L, 63L, 84L, 126L)
THRESH_GRID <- c(0.005, 0.010, 0.020, 0.030, 0.050)
SYNTH_FILE <- file.path(BACKTEST_ROOT, "midcpnifty-synth-futures", "synthetic_midcpnifty_front.csv")

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
SCRIPT_DIR <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[1L]))) else getwd()
OUT_DIR <- SCRIPT_DIR
args <- commandArgs(trailingOnly = TRUE)
for (i in seq_along(args)) {
  if (grepl("^--outdir=", args[i])) OUT_DIR <- sub("^--outdir=", "", args[i])
  if (args[i] %in% c("--outdir", "-o") && i < length(args)) OUT_DIR <- args[i + 1L]
}
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ── Data loading ─────────────────────────────────────────────────────────────
held_ohlc <- function(fut, calendar) {
  out <- matrix(NA_real_, nrow = length(calendar$trading_dates), ncol = 4L,
                dimnames = list(NULL, c("open", "high", "low", "close")))
  for (i in seq_along(calendar$trading_dates)) {
    exp <- calendar$held_after[i]
    if (is.na(exp)) next
    ok <- fut$EXPIRY_DT == exp & fut$TIME_STAMP == calendar$trading_dates[i]
    if (any(ok)) out[i, ] <- as.numeric(fut[which(ok)[1L], c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_CLOSE")])
  }
  data.frame(date = calendar$trading_dates, out, stringsAsFactors = FALSE)
}

load_data <- function() {
  con <- odbcDriverConnect(
    sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            ldbserver, "StockViz", ldbuser, ldbpassword),
    case = "nochange", believeNRows = TRUE)
  on.exit(try(odbcClose(con), silent = TRUE), add = TRUE)
  fut <- sqlQuery(con, paste(
    "SELECT SYMBOL, TIME_STAMP, EXPIRY_DT, PX_OPEN, PX_HIGH, PX_LOW, PX_CLOSE",
    "FROM BHAV_EQ_FUT WHERE SYMBOL = 'NIFTY' AND STRIKE_PR = 0",
    "ORDER BY TIME_STAMP, EXPIRY_DT"))
  if (!is.data.frame(fut) || !nrow(fut)) stop("NIFTY futures OHLC query failed")
  fut$TIME_STAMP <- as.Date(fut$TIME_STAMP); fut$EXPIRY_DT <- as.Date(fut$EXPIRY_DT)
  fut <- fut[is.finite(fut$PX_CLOSE) & fut$PX_CLOSE > 0 &
             fut$PX_HIGH >= fut$PX_LOW & fut$PX_HIGH > 0, ]
  cal <- build_monthly_futures_calendar(fut, min(fut$TIME_STAMP), ROLL_OFFSET)
  nifty <- held_ohlc(fut, cal)
  nifty <- nifty[is.finite(nifty$close) & nifty$close > 0, ]
  nifty$instrument <- "NIFTY"

  idx <- sqlQuery(con, paste(
    "SELECT INDEX_NAME, TIME_STAMP, PX_OPEN, PX_HIGH, PX_LOW, PX_CLOSE",
    "FROM BHAV_INDEX WHERE INDEX_NAME IN ('NIFTY MIDCAP SELECT', 'NIFTY 50')",
    "ORDER BY INDEX_NAME, TIME_STAMP"))
  if (!is.data.frame(idx) || !nrow(idx)) stop("index OHLC query failed")
  idx$date <- as.Date(idx$TIME_STAMP)
  idx <- idx[is.finite(idx$PX_CLOSE) & idx$PX_CLOSE > 0 &
             is.finite(idx$PX_HIGH) & is.finite(idx$PX_LOW) &
             idx$PX_HIGH >= idx$PX_LOW, ]
  syn <- read.csv(SYNTH_FILE, stringsAsFactors = FALSE)
  assert_columns(syn, c("date", "synth_fut_close"), "synthetic SELECT CSV")
  syn$date <- as.Date(syn$date)
  # MIDCAP SELECT OHLC starts in 2022. Before that, use the long-history
  # NIFTY 50 OHLC range as a transparent synthetic range proxy, scaled so
  # the close remains exactly the validated synthetic SELECT close.
  select_idx <- idx[idx$INDEX_NAME == "NIFTY MIDCAP SELECT", ]
  fallback_idx <- idx[idx$INDEX_NAME == "NIFTY 50", ]
  sel <- merge(syn[, c("date", "synth_fut_close")],
               select_idx[, c("date", "PX_OPEN", "PX_HIGH", "PX_LOW", "PX_CLOSE")],
               by = "date", all.x = TRUE)
  fb <- fallback_idx[match(sel$date, fallback_idx$date), ]
  use_fallback <- !is.finite(sel$PX_CLOSE) | sel$PX_CLOSE <= 0
  for (nm in c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_CLOSE")) sel[[nm]][use_fallback] <- fb[[nm]][use_fallback]
  scale <- sel$synth_fut_close / sel$PX_CLOSE
  select <- data.frame(date = sel$date,
                       open = sel$PX_OPEN * scale, high = sel$PX_HIGH * scale,
                       low = sel$PX_LOW * scale, close = sel$synth_fut_close,
                       instrument = "SELECT", stringsAsFactors = FALSE)
  select <- select[is.finite(select$close) & select$close > 0 &
                   is.finite(select$high) & is.finite(select$low) &
                   select$high >= select$low, ]
  list(NIFTY = nifty[, c("date", "open", "high", "low", "close", "instrument")],
       SELECT = select)
}

cat("Loading observed NIFTY OHLC and synthetic SELECT OHLC...\n")
data <- load_data()
for (nm in names(data)) cat(sprintf("  %s: %d rows (%s -> %s)\n", nm, nrow(data[[nm]]), min(data[[nm]]$date), max(data[[nm]]$date)))

# ── Causal OHLC turning-point channels ───────────────────────────────────────
bezier_value <- function(p0, p1, p2, t) (1 - t)^2 * p0 + 2 * (1 - t) * t * p1 + t^2 * p2

turning_points <- function(d, span = PIVOT_SPAN) {
  n <- nrow(d); hi <- lo <- rep(FALSE, n); conf_hi <- conf_lo <- rep(NA_integer_, n)
  for (i in (span + 1L):(n - span)) {
    left <- (i - span):(i - 1L); right <- (i + 1L):(i + span)
    hi[i] <- is.finite(d$high[i]) && d$high[i] > max(d$high[c(left, right)])
    lo[i] <- is.finite(d$low[i]) && d$low[i] < min(d$low[c(left, right)])
  }
  conf_hi <- ifelse(hi, seq_len(n) + span, NA_integer_)
  conf_lo <- ifelse(lo, seq_len(n) + span, NA_integer_)
  list(high = hi, low = lo, high_confirmation = conf_hi, low_confirmation = conf_lo)
}

build_channel <- function(d, channel_len, tp = NULL) {
  n <- nrow(d); if (is.null(tp)) tp <- turning_points(d)
  upper <- lower <- rep(NA_real_, n); all_idx <- seq_len(n)
  for (i in seq_len(n)) {
    hist_start <- max(1L, i - channel_len); hist_end <- i - 1L
    if (hist_end <= hist_start) next
    hi_idx <- which(tp$high & tp$high_confirmation <= hist_end & all_idx >= hist_start & all_idx <= hist_end)
    lo_idx <- which(tp$low & tp$low_confirmation <= hist_end & all_idx >= hist_start & all_idx <= hist_end)
    if (length(hi_idx) >= 3L) {
      hi_idx <- tail(hi_idx, 3L)
      upper[i] <- bezier_value(d$high[hi_idx[1L]], d$high[hi_idx[2L]], d$high[hi_idx[3L]],
                               min(max((i - hist_start) / channel_len, 0), 1))
    }
    if (length(lo_idx) >= 3L) {
      lo_idx <- tail(lo_idx, 3L)
      lower[i] <- bezier_value(d$low[lo_idx[1L]], d$low[lo_idx[2L]], d$low[lo_idx[3L]],
                               min(max((i - hist_start) / channel_len, 0), 1))
    }
  }
  data.frame(upper = upper, lower = lower)
}

run_variant <- function(d, channel_len, threshold, mode = "reversal", trend_filter = FALSE,
                        feature = NULL) {
  n <- nrow(d); close <- d$close
  if (is.null(feature)) {
    ma20 <- zoo::rollmean(close, 20L, fill = NA, align = "right")
    ma50 <- zoo::rollmean(close, 50L, fill = NA, align = "right")
    feature <- list(ma20 = ma20, ma50 = ma50, slope = c(NA_real_, diff(ma50)),
                    ret = c(NA_real_, close[-1L] / close[-n]),
                    channel = build_channel(d, channel_len))
    feature$ret <- c(NA_real_, close[-1L] / close[-n] - 1)
  }
  ma20 <- feature$ma20; ma50 <- feature$ma50; slope <- feature$slope; ret <- feature$ret
  turbulent <- is.finite(ma20) & is.finite(ma50) & is.finite(close) & abs(ma20 - ma50) / close >= threshold
  ch <- feature$channel; pos <- rep(0, n)
  for (i in 2:n) {
    j <- i - 1L; pos[i] <- pos[i - 1L]
    if (!isTRUE(turbulent[j])) next
    at_hi <- is.finite(ch$upper[j]) && close[j] >= ch$upper[j]
    at_lo <- is.finite(ch$lower[j]) && close[j] <= ch$lower[j]
    if (mode == "reversal" && at_lo) pos[i] <- 1
    if (mode == "reversal" && at_hi && (!trend_filter || !is.finite(slope[j]) || slope[j] <= 0)) pos[i] <- -1
    if (mode == "breakout" && at_hi) pos[i] <- 1
    if (mode == "breakout" && at_lo) pos[i] <- -1
  }
  turn <- abs(c(NA_real_, diff(pos)))
  data.frame(date = d$date, bh = ret,
             lo = pmax(pos, 0) * ret - DRAG * abs(c(NA_real_, diff(pmax(pos, 0)))),
             ls = pos * ret - DRAG * turn, position = pos, turnover = turn,
             turbulent = turbulent, upper = ch$upper, lower = ch$lower)
}

SYSTEMS <- c("Reversal Long/Flat", "Reversal Long/Short", "Breakout Long/Flat",
             "Breakout Long/Short", "Trend-Filtered Reversal Long/Flat",
             "Trend-Filtered Reversal Long/Short")
variant_spec <- function(system) list(
  mode = if (grepl("Breakout", system)) "breakout" else "reversal",
  trend = grepl("Trend-Filtered", system), arm = if (grepl("Long/Short", system)) "ls" else "lo")

sharpe <- function(x) if (sum(is.finite(x)) > 2L && sd(x, na.rm = TRUE) > 0) mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE) * sqrt(252) else NA_real_

# ── Combined walk-forward selection ──────────────────────────────────────────
walk_forward <- function(d) {
  years <- sort(unique(as.integer(format(d$date, "%Y"))))
  first_test <- min(years) + TRAIN_YEARS
  last_test <- max(years)
  close <- d$close
  ma20 <- zoo::rollmean(close, 20L, fill = NA, align = "right")
  ma50 <- zoo::rollmean(close, 50L, fill = NA, align = "right")
  tp <- turning_points(d)
  features <- lapply(CHANNEL_GRID, function(len) list(
    ma20 = ma20, ma50 = ma50, slope = c(NA_real_, diff(ma50)),
    ret = c(NA_real_, close[-1L] / close[-nrow(d)] - 1),
    channel = build_channel(d, len, tp = tp)))
  names(features) <- as.character(CHANNEL_GRID)
  oos <- list(); picks <- list(); k <- 0L
  for (test_year in seq(first_test, last_test, by = TEST_YEARS)) {
    test_start <- as.Date(sprintf("%d-01-01", test_year)); test_end <- as.Date(sprintf("%d-12-31", test_year))
    train_start <- as.Date(sprintf("%d-01-01", test_year - TRAIN_YEARS)); train_end <- test_start - 1L
    train_idx <- d$date >= train_start & d$date <= train_end
    test_idx <- d$date >= test_start & d$date <= test_end
    if (sum(train_idx) < 500L || sum(test_idx) < 20L) next
    candidates <- list(); q <- 0L
    for (system in SYSTEMS) for (len in CHANNEL_GRID) for (thr in THRESH_GRID) {
      spec <- variant_spec(system); bt <- run_variant(d, len, thr, spec$mode, spec$trend,
                                             feature = features[[as.character(len)]])
      x <- bt[[spec$arm]][train_idx]; q <- q + 1L
      candidates[[q]] <- data.frame(System = system, Channel = len, Threshold = thr,
                                    TrainSharpe = sharpe(x), TrainCAGR = if (sum(is.finite(x))) prod(1 + x[is.finite(x)])^(252 / sum(is.finite(x))) - 1 else NA_real_)
    }
    cand <- do.call(rbind, candidates); cand <- cand[order(-cand$TrainSharpe, cand$Channel, cand$Threshold), ]
    pick <- cand[1L, ]; spec <- variant_spec(pick$System)
    chosen <- run_variant(d, as.integer(pick$Channel), as.numeric(pick$Threshold), spec$mode, spec$trend,
                          feature = features[[as.character(pick$Channel)]])
    k <- k + 1L; picks[[k]] <- cbind(Instrument = unique(d$instrument), TestYear = test_year,
                                      TrainStart = train_start, TrainEnd = train_end,
                                      TestStart = min(d$date[test_idx]), TestEnd = max(d$date[test_idx]), pick)
    oos[[k]] <- data.frame(date = d$date[test_idx], system = pick$System,
                           ret = chosen[[spec$arm]][test_idx], bh = chosen$bh[test_idx],
                           position = chosen$position[test_idx], turnover = chosen$turnover[test_idx])
  }
  list(oos = do.call(rbind, oos), picks = do.call(rbind, picks))
}

all_oos <- list(); all_picks <- list(); kk <- 0L
for (nm in names(data)) { wf <- walk_forward(data[[nm]]); kk <- kk + 1L; all_oos[[nm]] <- wf$oos; all_picks[[nm]] <- wf$picks }
picks <- do.call(rbind, all_picks); oos <- do.call(rbind, all_oos)
write.csv(picks, file.path(OUT_DIR, "walk_forward_selection.csv"), row.names = FALSE)
write.csv(oos, file.path(OUT_DIR, "walk_forward_daily.csv"), row.names = FALSE)

# ── Metrics, charts, and tables ──────────────────────────────────────────────
metric_row <- function(x, instrument, system, window) {
  x <- x[is.finite(x$ret) & is.finite(x$date), ]
  rx <- xts(x$ret, x$date); m <- strategy_metrics(rx)
  data.frame(Window = window, Instrument = instrument, System = system, N = as.numeric(m[["N"]]),
             CAGR = as.numeric(m[["CAGR"]]), Vol = as.numeric(m[["Vol"]]), Sharpe = as.numeric(m[["Sharpe"]]),
             MaxDD = as.numeric(m[["MaxDD"]]), AvgExposure = mean(abs(x$position), na.rm = TRUE),
             Turnover = mean(x$turnover, na.rm = TRUE), stringsAsFactors = FALSE)
}
metrics <- list(); daily_chart <- list()
for (nm in names(all_oos)) {
  x <- all_oos[[nm]]; x$Instrument <- nm
  for (w in c("pre", "post", "full")) {
    z <- if (w == "pre") x[x$date <= TRAIN_END, ] else if (w == "post") x[x$date >= POST_START, ] else x
    metrics[[length(metrics) + 1L]] <- metric_row(z, nm, "WF Selected", w)
    metrics[[length(metrics) + 1L]] <- metric_row(transform(z, ret = bh, position = 1, turnover = 0), nm, "B&H", w)
  }
  daily_chart[[nm]] <- xts(cbind(WF_Selected = x$ret, BH = x$bh), x$date)
}
metrics <- do.call(rbind, metrics)
for (w in c("pre", "post", "full")) {
  z <- metrics[metrics$Window == w, ]
  write.csv(z, file.path(OUT_DIR, sprintf("metrics_%s.csv", w)), row.names = FALSE)
  gt(z) |>
    tab_header(title = "Combined Walk-Forward OHLC Metrics", subtitle = sprintf("%s window | 5-year train / 1-year test | 25 bps drag", w)) |>
    fmt_number(columns = N, decimals = 0) |>
    fmt_percent(columns = c(CAGR, Vol, MaxDD, AvgExposure, Turnover), decimals = 1) |>
    fmt_number(columns = Sharpe, decimals = 2) |>
    tab_source_note(source_note = "@StockViz") |>
    gtsave(filename = file.path(OUT_DIR, sprintf("metrics_%s.png", w)))
}
for (nm in names(daily_chart)) for (w in c("pre", "post", "full")) {
  rng <- if (w == "pre") paste0("/", TRAIN_END) else if (w == "post") paste0(POST_START, "/") else NULL
  plotCumDrawdown(list(`WF Selected` = daily_chart[[nm]][, 1L], `B&H` = daily_chart[[nm]][, 2L]),
                  dateRange = rng, title = sprintf("Walk-Forward OHLC Turbulence — %s — %s", nm, w),
                  subtitle = "System and parameters re-selected on each preceding five-year training window",
                  outPath = file.path(OUT_DIR, sprintf("cumulative_%s_%s.png", nm, w)))
}

writeLines(c(
  "Combined walk-forward OHLC turbulence study",
  "",
  "Each one-year test period selects both the strategy interpretation and its channel/threshold",
  "from the preceding five years. The selected system is then applied unchanged to the next year.",
  "Systems: reversal long/flat, reversal long/short, breakout long/flat, breakout long/short,",
  "and reversal with a rising-MA50 short filter in long/flat and long/short forms.",
  "Turning points use observed OHLC with a two-bar confirmation delay; all signals are lagged one day.",
  "NIFTY uses observed futures OHLC. SELECT close is the validated synthetic front-month close;",
  "SELECT OHLC is scaled from MIDCAP SELECT PR where available and uses NIFTY 50 OHLC as a range",
  "proxy before that, so pre-real-futures SELECT OHLC is synthetic.",
  "Train/test: five years train, one year test; named report windows use pre <= 2019-12-31 and",
  "post >= 2020-05-01. Trading drag is 25 bps per unit traded.",
  "This remains an exploratory implementation and is not a validated trading system."
), file.path(OUT_DIR, "README.txt"))
cat(sprintf("Walk-forward OHLC study complete. Outputs written to %s\n", normalizePath(OUT_DIR, mustWork = FALSE)))

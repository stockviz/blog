#!/usr/bin/env Rscript
# Trend-filtered reversal long/flat on MIDCAP 150 TR and SMALLCAP 250 TR
# =======================================================================
# Uses the same exploratory Bézier/turbulence implementation as the futures
# studies, applied to each index's TR close series:
#   - prior-close rolling quadratic Bézier channel;
#   - turbulence = abs(MA20 - MA50) / close;
#   - lower boundary -> long; upper boundary -> short signal;
#   - rising lagged MA50 suppresses new shorts;
#   - one-day signal lag and 25 bps per unit traded.
#
# Parameters are selected independently for each index on data through
# 2019-12-31. This is a long/flat strategy: suppressed/short signals mean flat.
# The TR database series is used for P&L; the channel is close-derived, matching
# the existing regime-aware turbulence experiment.
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
source_common("charts")
CONFIG_FILE <- Sys.getenv("STOCKVIZ_CONFIG", "/mnt/hollandC/StockViz/R/config.r")
if (!file.exists(CONFIG_FILE)) stop(sprintf("StockViz config not found: %s", CONFIG_FILE))
source(CONFIG_FILE)

TRAIN_END <- as.Date("2019-12-31")
POST_START <- as.Date("2020-05-01")
CHANNEL_GRID <- c(42L, 63L, 84L, 126L)
THRESH_GRID <- c(0.005, 0.010, 0.020, 0.030, 0.050)
DRAG <- 0.0025
INDEXES <- c("NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR")

parse_outdir <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  file_arg <- grep("^--file=", commandArgs(), value = TRUE)
  out <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1L]), mustWork = TRUE)) else getwd()
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

load_indices <- function() {
  con <- odbcDriverConnect(
    sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            ldbserver, "StockViz", ldbuser, ldbpassword),
    case = "nochange", believeNRows = TRUE)
  on.exit(try(odbcClose(con), silent = TRUE), add = TRUE)
  sql <- sprintf(
    "SELECT index_name, time_stamp, px_close FROM bhav_index WHERE index_name IN (%s) ORDER BY index_name, time_stamp",
    paste(shQuote(INDEXES), collapse = ","))
  raw <- sqlQuery(con, sql)
  if (!is.data.frame(raw) || !nrow(raw)) stop("TR index query failed or returned no rows")
  out <- list()
  for (nm in INDEXES) {
    x <- raw[raw$index_name == nm, ]
    x$date <- as.Date(x$time_stamp)
    x <- x[is.finite(x$px_close) & x$px_close > 0 & !duplicated(x$date), ]
    if (nrow(x) < 100L) stop(sprintf("Insufficient data for %s", nm))
    out[[nm]] <- data.frame(date = x$date, close = x$px_close,
                            instrument = nm, stringsAsFactors = FALSE)
  }
  out
}

bezier_value <- function(p0, p1, p2, t) (1 - t)^2 * p0 + 2 * (1 - t) * t * p1 + t^2 * p2

build_channel <- function(v, channel_len) {
  n <- length(v); upper <- lower <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (i <= channel_len) next
    hist <- v[(i - channel_len):(i - 1L)]
    if (any(!is.finite(hist))) next
    cuts <- floor(seq(1, channel_len + 1, length.out = 4))
    segs <- lapply(seq_len(3L), function(k) hist[cuts[k]:(cuts[k + 1L] - 1L)])
    upper[i] <- bezier_value(max(segs[[1L]]), max(segs[[2L]]), max(segs[[3L]]), 0.75)
    lower[i] <- bezier_value(min(segs[[1L]]), min(segs[[2L]]), min(segs[[3L]]), 0.75)
  }
  list(upper = upper, lower = lower)
}

run_system <- function(d, channel_len, threshold) {
  v <- d$close; n <- length(v)
  ma20 <- zoo::rollmean(v, 20L, fill = NA, align = "right")
  ma50 <- zoo::rollmean(v, 50L, fill = NA, align = "right")
  slope <- c(NA_real_, diff(ma50))
  turbulent <- is.finite(ma20) & is.finite(ma50) & is.finite(v) & abs(ma20 - ma50) / v >= threshold
  ch <- build_channel(v, channel_len)
  pos <- rep(0, n)
  for (i in 2:n) {
    j <- i - 1L; pos[i] <- pos[i - 1L]
    if (!isTRUE(turbulent[j])) next
    at_upper <- is.finite(ch$upper[j]) && v[j] >= ch$upper[j]
    at_lower <- is.finite(ch$lower[j]) && v[j] <= ch$lower[j]
    if (at_lower) pos[i] <- 1L
    if (at_upper && (!is.finite(slope[j]) || slope[j] <= 0)) pos[i] <- 0L
  }
  ret <- c(NA_real_, v[-1L] / v[-n] - 1)
  turnover <- abs(c(NA_real_, diff(pos)))
  data.frame(date = d$date, close = v, upper = ch$upper, lower = ch$lower,
             turbulent = turbulent, ma50_slope = slope, position = pos,
             turnover = turnover, strategy = pos * ret - DRAG * turnover,
             bh = ret, stringsAsFactors = FALSE)
}

sharpe <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) > 2L && stats::sd(x) > 0) mean(x) / stats::sd(x) * sqrt(252) else NA_real_
}

select_params <- function(d) {
  rows <- list(); k <- 0L
  for (len in CHANNEL_GRID) for (thr in THRESH_GRID) {
    bt <- run_system(d, len, thr)
    x <- bt$strategy[bt$date <= TRAIN_END]
    x <- x[is.finite(x)]
    k <- k + 1L
    rows[[k]] <- data.frame(Channel = len, Threshold = thr, TrainSharpe = sharpe(x),
                            TrainCAGR = if (length(x)) prod(1 + x)^(252 / length(x)) - 1 else NA_real_)
  }
  sweep <- do.call(rbind, rows)
  sweep <- sweep[order(-sweep$TrainSharpe, sweep$Channel, sweep$Threshold), ]
  list(best = sweep[1L, ], sweep = sweep)
}

metric_row <- function(w, instrument, system, window_name) {
  ok <- is.finite(w$strategy) & is.finite(w$date)
  w <- w[ok, ]
  m <- strategy_metrics(xts(w$strategy, w$date))
  data.frame(Window = window_name, Instrument = instrument, System = system,
             N = as.numeric(m[["N"]]), CAGR = as.numeric(m[["CAGR"]]),
             Vol = as.numeric(m[["Vol"]]), Sharpe = as.numeric(m[["Sharpe"]]),
             MaxDD = as.numeric(m[["MaxDD"]]), AvgExposure = mean(w$position, na.rm = TRUE),
             Turnover = mean(w$turnover, na.rm = TRUE), stringsAsFactors = FALSE)
}

cat("Loading TR indices...\n")
data <- load_indices()
all_metrics <- list(); chart_data <- list()
for (nm in names(data)) {
  d <- data[[nm]]; sel <- select_params(d); p <- sel$best
  len <- as.integer(p$Channel); thr <- as.numeric(p$Threshold)
  bt <- run_system(d, len, thr)
  key <- gsub(" ", "_", nm)
  write.csv(sel$sweep, file.path(OUT_DIR, sprintf("parameter_sweep_train_%s.csv", key)), row.names = FALSE)
  write.csv(bt, file.path(OUT_DIR, sprintf("daily_%s.csv", key)), row.names = FALSE)
  cat(sprintf("  %s: %d observations (%s -> %s), selected channel=%d threshold=%.1f%%\n",
              nm, nrow(bt), min(bt$date), max(bt$date), len, 100 * thr))
  for (wname in c("pre", "post", "full")) {
    w <- if (wname == "pre") bt[bt$date <= TRAIN_END, ] else if (wname == "post") bt[bt$date >= POST_START, ] else bt
    if (nrow(w) < 40L) next
    all_metrics[[length(all_metrics) + 1L]] <- metric_row(w, nm, "Trend-Filtered Reversal Long/Flat", wname)
    all_metrics[[length(all_metrics) + 1L]] <- metric_row(transform(w, strategy = w$bh, position = 1, turnover = 0), nm, "B&H", wname)
  }
  chart_data[[nm]] <- xts(cbind(Strategy = bt$strategy, `B&H` = bt$bh), bt$date)
}

metrics <- do.call(rbind, all_metrics)
for (wname in c("pre", "post", "full")) {
  out <- metrics[metrics$Window == wname, ]
  write.csv(out, file.path(OUT_DIR, sprintf("metrics_%s.csv", wname)), row.names = FALSE)
  tbl <- out |>
    gt() |>
    tab_header(title = "Trend-Filtered Reversal Long/Flat on TR Indices",
               subtitle = sprintf("%s window | train selection through %s | 25 bps drag", wname, TRAIN_END)) |>
    fmt_number(columns = N, decimals = 0) |>
    fmt_percent(columns = c(CAGR, Vol, MaxDD, AvgExposure, Turnover), decimals = 1) |>
    fmt_number(columns = Sharpe, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes())
  gtsave(tbl, filename = file.path(OUT_DIR, sprintf("metrics_%s.png", wname)))
}

for (nm in names(chart_data)) for (wname in c("pre", "post", "full")) {
  range <- if (wname == "pre") paste0("/", TRAIN_END) else if (wname == "post") paste0(POST_START, "/") else NULL
  plotCumDrawdown(as.list(chart_data[[nm]]), dateRange = range,
                  title = sprintf("Trend-Filtered Reversal Long/Flat — %s — %s", nm, wname),
                  subtitle = "TR close series; parameters selected on the pre-period training sample; 25 bps drag",
                  outPath = file.path(OUT_DIR, sprintf("cumulative_%s_%s.png", gsub(" ", "_", nm), wname)),
                  linetypeBySeries = TRUE)
}

writeLines(c(
  "Trend-filtered reversal long/flat on MIDCAP 150 TR and SMALLCAP 250 TR",
  "",
  "The strategy goes long after a turbulent lower-boundary event. An upper-boundary",
  "event exits to flat only when the lagged MA50 is not rising; rising-MA50 upper",
  "events are ignored to avoid shorting persistent advances.",
  "",
  "Each index selects its channel length and turbulence threshold independently using",
  "only data through 2019-12-31. Signals are lagged one day and drag is 25 bps per",
  "unit traded. Metrics and charts use pre <= 2019-12-31, post >= 2020-05-01, and full",
  "windows. The intervening 2020-01-01 to 2020-04-30 period is excluded from named",
  "pre/post comparisons.",
  "",
  "The TR series is used for P&L and the channel is close-derived, matching the",
  "existing regime-aware turbulence experiment. This is exploratory research, not",
  "a validated trading system."), file.path(OUT_DIR, "findings.md"))
cat(sprintf("Study complete. Outputs written to %s\n", normalizePath(OUT_DIR, mustWork = FALSE)))

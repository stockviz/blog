#!/usr/bin/env Rscript
#
# Diagnostic event-reaction backtest for the SEBI surveillance event extract.
#
# The source file contains event-day returns, not prices after the event. The
# script therefore measures the opportunity/risk in the observed event-day
# reaction and deliberately does not present it as a causal tradable strategy.
# A causal implementation would require next-session prices and execution data.

suppressPackageStartupMessages({
  library(ggplot2)
  library(gt)
  library(webshot2)
})

options(stringsAsFactors = FALSE)
options(scipen = 100)
set.seed(20260907)
TRANSACTION_DRAG <- 0.005

#' Resolve the repository root from --root or the script location.
parse_root <- function() {
  argv <- commandArgs(trailingOnly = FALSE)
  root_arg <- grep("^--root=", argv, value = TRUE)
  if (length(root_arg) > 0L) return(normalizePath(sub("^--root=", "", root_arg[1])))
  file_arg <- grep("^--file=", argv, value = TRUE)
  if (length(file_arg) > 0L) return(normalizePath(file.path(dirname(sub("^--file=", "", file_arg[1])), "..")))
  normalizePath("..")
}

ROOT <- parse_root()
OUT_DIR <- file.path(ROOT, "backtest")
EVENTS_PATH <- file.path(ROOT, "surveillance-events.csv")

#' Load and validate the event extract used by every downstream calculation.
load_events <- function(path) {
  if (!file.exists(path)) stop("Missing event extract: ", path)
  x <- read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  required <- c("symbol", "event_date", "event_type", "measure", "return")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) stop("Missing columns: ", paste(missing, collapse = ", "))
  x$event_date <- as.Date(x$event_date)
  x$return <- as.numeric(x$return)
  x <- x[is.finite(x$return) & !is.na(x$event_date), ]
  if (nrow(x) == 0L) stop("No usable event returns")
  x
}

#' Aggregate event-day reactions into an equal-weight daily diagnostic series.
#'
#' Equal weighting is across events on each date. Apply the stated 50 bps
#' one-way drag to the equal-weight event portfolio once per observed event
#' date; this is still not deployable because the event-day return precedes
#' event knowledge.
daily_reaction <- function(events, name, direction = 1, event_types = NULL, measures = NULL) {
  keep <- rep(TRUE, nrow(events))
  if (!is.null(event_types)) keep <- keep & events$event_type %in% event_types
  if (!is.null(measures)) keep <- keep & events$measure %in% measures
  d <- events[keep, , drop = FALSE]
  if (nrow(d) == 0L) return(data.frame())
  split_days <- split(seq_len(nrow(d)), as.character(d$event_date))
  rows <- lapply(names(split_days), function(day) {
    values <- d$return[split_days[[day]]] * direction
    n_events <- length(values)
    transaction_drag <- TRANSACTION_DRAG
    data.frame(event_date = as.Date(day), gross_return = mean(values),
               transaction_drag = transaction_drag,
               daily_return = mean(values) - transaction_drag,
               n_events = n_events)
  })
  out <- do.call(rbind, rows)
  out$strategy <- name
  out[order(out$event_date), c("event_date", "strategy", "gross_return",
                               "transaction_drag", "daily_return", "n_events")]
}

#' Calculate risk and opportunity metrics for one diagnostic return series.
metrics <- function(d, window) {
  if (nrow(d) == 0L) return(data.frame())
  r <- d$daily_return
  wealth <- cumprod(1 + r)
  drawdown <- wealth / cummax(wealth) - 1
  years <- max(as.numeric(d$event_date) - min(as.numeric(d$event_date)) + 1, 1) / 365.25
  cagr <- if (years > 0 && tail(wealth, 1) > 0) tail(wealth, 1)^(1 / years) - 1 else NA_real_
  sd_r <- sd(r)
  q05 <- as.numeric(quantile(r, 0.05, names = FALSE, type = 7))
  tail5 <- r[r <= q05]
  data.frame(
    window = window,
    strategy = d$strategy[1],
    start = min(d$event_date), end = max(d$event_date),
    n_days = nrow(d), total_events = sum(d$n_events),
    total_transaction_drag = sum(d$transaction_drag),
    avg_events_per_day = mean(d$n_events), max_events_per_day = max(d$n_events),
    mean_daily_return = mean(r), median_daily_return = median(r),
    positive_day_rate = mean(r > 0),
    cagr = cagr,
    sharpe = if (is.finite(sd_r) && sd_r > 0) mean(r) / sd_r * sqrt(252) else NA_real_,
    max_drawdown = min(drawdown),
    var_05 = q05,
    cvar_05 = mean(tail5),
    worst_day = min(r), best_day = max(r),
    stringsAsFactors = FALSE
  )
}

#' Bootstrap the event-level mean to show uncertainty around the reaction size.
bootstrap_mean <- function(events, event_types = NULL, measures = NULL, n_boot = 5000L) {
  keep <- is.finite(events$return)
  if (!is.null(event_types)) keep <- keep & events$event_type %in% event_types
  if (!is.null(measures)) keep <- keep & events$measure %in% measures
  x <- events$return[keep]
  if (length(x) == 0L) return(data.frame())
  draws <- replicate(n_boot, mean(sample(x, length(x), replace = TRUE)))
  data.frame(
    n_events = length(x), observed_mean = mean(x),
    boot_ci_025 = unname(quantile(draws, 0.025)),
    boot_ci_975 = unname(quantile(draws, 0.975)),
    stringsAsFactors = FALSE
  )
}

#' Save a cumulative/drawdown diagnostic chart for the event-day series.
save_chart <- function(d, path, title) {
  if (nrow(d) == 0L) return(invisible(NULL))
  date_range <- sprintf("%s to %s", min(d$event_date), max(d$event_date))
  d$wealth <- cumprod(1 + d$daily_return)
  d$drawdown <- d$wealth / cummax(d$wealth) - 1
  cumulative <- transform(d[, c("event_date", "strategy", "wealth")],
                          panel = "Cumulative", value = wealth)
  drawdowns <- transform(d[, c("event_date", "strategy", "drawdown")],
                         panel = "Drawdown", value = drawdown)
  plot_df <- rbind(cumulative[, c("event_date", "strategy", "panel", "value")],
                   drawdowns[, c("event_date", "strategy", "panel", "value")])
  ends <- do.call(rbind, lapply(split(plot_df, list(plot_df$strategy, plot_df$panel)), function(x) {
    x[nrow(x), , drop = FALSE]
  }))
  ends$label <- sub(":.*$", "", ends$strategy)
  p <- ggplot(plot_df, aes(x = event_date, y = value, color = strategy, group = strategy)) +
    geom_line(linewidth = 0.7) +
    geom_hline(data = data.frame(panel = "Cumulative", value = 1),
               aes(yintercept = value), inherit.aes = FALSE, linetype = 2, color = "grey50") +
    geom_hline(data = data.frame(panel = "Drawdown", value = 0),
               aes(yintercept = value), inherit.aes = FALSE, linetype = 2, color = "grey50") +
    geom_text(data = ends, aes(label = label), hjust = -0.05, size = 2.5,
              show.legend = FALSE) +
    facet_grid(panel ~ strategy, scales = "free_y") +
    labs(title = title,
         subtitle = sprintf("Date range: %s | Diagnostic event-day reaction; not causal or executable without post-event prices", date_range),
         x = NULL, y = NULL, caption = "@StockViz") +
    scale_x_date(expand = expansion(mult = c(0.01, 0.15))) +
    guides(color = "none") +
    theme_minimal(base_size = 11)
  ggsave(path, p, width = 12, height = 7, dpi = 130)
}

#' Render the available-sample metrics table as HTML and PNG.
save_metrics_table <- function(metrics_df, html_path, png_path) {
  d <- metrics_df[, c("window", "strategy", "start", "end", "n_days", "total_events",
                      "total_transaction_drag",
                      "cagr", "sharpe", "max_drawdown", "var_05", "cvar_05",
                      "positive_day_rate", "worst_day", "best_day"), drop = FALSE]
  names(d) <- c("Window", "Strategy", "Start", "End", "Days", "Events", "Transaction drag",
                "CAGR", "Sharpe", "Max DD", "VaR 5%", "CVaR 5%",
                "Positive days", "Worst day", "Best day")
  tbl <- gt(d) |>
    tab_header(
      title = "SEBI Surveillance Diagnostic Metrics",
      subtitle = "Available sample | 50 bps one-way drag per equal-weight event-date transaction | not an executable backtest"
    ) |>
    tab_source_note("@StockViz") |>
    fmt_date(columns = c(Start, End), date_style =  "iso") |>
    fmt_number(columns = c(Days, Events), decimals = 0, use_seps = TRUE) |>
    fmt_number(columns = `Transaction drag`, decimals = 2, use_seps = TRUE) |>
    fmt_number(columns = Sharpe, decimals = 2) |>
    fmt_percent(columns = c(CAGR, `Max DD`, `VaR 5%`, `CVaR 5%`, `Positive days`, `Worst day`, `Best day`), decimals = 1) |>
    cols_width(Strategy ~ px(230), everything() ~ px(100)) |>
    tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
    opt_row_striping()
  gtsave(tbl, filename = html_path)
  webshot2::webshot(html_path, png_path, vwidth = 2200, vheight = 900,
                    selector = "table.gt_table", expand = c(12, 12, 12, 12))
  invisible(tbl)
}

#' Run the diagnostic and write auditable outputs.
run_backtest <- function() {
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  events <- load_events(EVENTS_PATH)

  entry_types <- c("first_entry", "grade_transition", "re_entry")
  series <- list(
    all_long = daily_reaction(events, "All events: long", 1),
    entries_long = daily_reaction(events, "Entries/transitions: long", 1, entry_types),
    exits_long = daily_reaction(events, "Exits: long", 1, "exit"),
    entries_contrarian = daily_reaction(events, "Entries/transitions: contrarian", -1, entry_types)
  )
  series <- series[vapply(series, nrow, integer(1)) > 0L]
  daily <- do.call(rbind, series)
  rownames(daily) <- NULL
  write.csv(daily, file.path(OUT_DIR, "daily_reaction_returns.csv"), row.names = FALSE)

  windows <- list(available = c(as.Date("1900-01-01"), as.Date("9999-12-31")))
  metric_rows <- list()
  k <- 1L
  for (window in names(windows)) {
    w <- windows[[window]]
    for (nm in unique(daily$strategy)) {
      d <- daily[daily$strategy == nm & daily$event_date >= w[1] & daily$event_date <= w[2], ]
      if (nrow(d) > 0L) {
        metric_rows[[k]] <- metrics(d, window)
        k <- k + 1L
      }
    }
  }
  metric_table <- do.call(rbind, metric_rows)
  write.csv(metric_table, file.path(OUT_DIR, "metrics.csv"), row.names = FALSE)

  boot <- rbind(
    cbind(strategy = "Entries/transitions: long", bootstrap_mean(events, entry_types), stringsAsFactors = FALSE),
    cbind(strategy = "Exits: long", bootstrap_mean(events, "exit"), stringsAsFactors = FALSE),
    cbind(strategy = "All events: long", bootstrap_mean(events), stringsAsFactors = FALSE)
  )
  write.csv(boot, file.path(OUT_DIR, "bootstrap_event_mean.csv"), row.names = FALSE)

  save_chart(daily, file.path(OUT_DIR, "event_reaction_cumulative_drawdown.png"),
             "SEBI Surveillance Event-Day Reaction Diagnostic — Available Sample")
  save_metrics_table(metric_table, file.path(OUT_DIR, "metrics_table.html"),
                     file.path(OUT_DIR, "metrics_table.png"))

  stopifnot(nrow(metric_table) == length(unique(daily$strategy)))
  stopifnot(all(metric_table$n_days > 0), all(is.finite(metric_table$total_events)))
  cat(sprintf("Wrote %d daily rows, %d metric rows, %d bootstrap rows, and 1 metrics table to %s\n",
              nrow(daily), nrow(metric_table), nrow(boot), OUT_DIR))
  invisible(metric_table)
}

if (sys.nframe() == 0L) run_backtest()

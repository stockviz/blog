suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(PerformanceAnalytics)
  library(ggplot2)
  library(viridis)
  library(gt)
  library(webshot2)
})
source("build.R")
source("backtest.R")

RESULTS_PATH <- file.path(REPORT_PATH, "backtest-results.rds")
PRIMARY_COST_KEY <- "25bps"

pretty_strategy <- function(x) {
  x <- gsub("^X10M", "10M", x)
  x <- gsub("\\.\\.\\.", " - ", x)
  gsub("\\.", " ", x)
}

compute_metrics <- function(x) {
  x <- na.omit(x)
  if (NROW(x) < 20L) return(c(CAGR = NA, Volatility = NA, Sharpe = NA, MaxDD = NA,
                              Calmar = NA, RecoveryDays = NA, WorstMonth = NA,
                              PositiveMonths = NA))
  monthly <- apply.monthly(x, Return.cumulative)
  dd <- tryCatch(suppressWarnings(table.Drawdowns(x, top = 50)), error = function(e) NULL)
  recoveries <- if (is.null(dd)) numeric() else dd$Recovery
  recoveries <- recoveries[is.finite(recoveries) & recoveries > 0]
  c(
    CAGR = as.numeric(Return.annualized(x, scale = 252, geometric = TRUE)),
    Volatility = as.numeric(StdDev.annualized(x, scale = 252)),
    Sharpe = as.numeric(SharpeRatio.annualized(x, scale = 252)),
    MaxDD = as.numeric(maxDrawdown(x)),
    Calmar = as.numeric(CalmarRatio(x, scale = 252)),
    RecoveryDays = if (length(recoveries)) mean(recoveries) else NA_real_,
    WorstMonth = min(as.numeric(monthly)),
    PositiveMonths = mean(as.numeric(monthly) > 0)
  )
}

metrics_frame <- function(x) {
  rows <- lapply(seq_len(NCOL(x)), function(j) {
    m <- compute_metrics(x[, j])
    data.frame(Strategy = pretty_strategy(colnames(x)[j]), t(m), check.names = FALSE)
  })
  do.call(rbind, rows)
}

save_gt_table <- function(df, stem, title, subtitle = NULL, percent_cols = character()) {
  tbl <- gt(df) |>
    tab_header(title = title, subtitle = subtitle) |>
    tab_source_note("@StockViz") |>
    tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
    opt_row_striping()
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  ordinary <- setdiff(numeric_cols, percent_cols)
  if (length(ordinary)) tbl <- tbl |> fmt_number(columns = all_of(ordinary), decimals = 2)
  if (length(percent_cols)) tbl <- tbl |> fmt_percent(columns = all_of(percent_cols), decimals = 2)
  html <- file.path(REPORT_PATH, paste0(stem, ".html"))
  png <- file.path(REPORT_PATH, paste0(stem, ".png"))
  gtsave(tbl, html)
  webshot2::webshot(html, png, selector = "table.gt_table", expand = c(10, 10, 10, 10))
  invisible(tbl)
}

long_daily <- function(x) {
  data.frame(
    Date = rep(as.Date(index(x)), NCOL(x)),
    Strategy = rep(pretty_strategy(colnames(x)), each = NROW(x)),
    Return = as.numeric(coredata(x)),
    stringsAsFactors = FALSE
  )
}

plot_cumulative <- function(x, stem, title, subtitle) {
  wealth <- xts(apply(1 + coredata(x), 2, cumprod), index(x))
  colnames(wealth) <- colnames(x)
  df <- long_daily(wealth)
  p <- ggplot(df, aes(Date, Return, color = Strategy)) +
    geom_line(linewidth = 0.7) +
    scale_color_viridis_d(option = "D", end = 0.9) +
    scale_y_log10() +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Growth of 1 (log scale)",
         color = NULL, caption = "@StockViz") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(file.path(REPORT_PATH, paste0(stem, ".png")), p, width = 13, height = 8, dpi = 130)
}

plot_annual <- function(x, stem, title, subtitle) {
  annual <- apply.yearly(x, Return.cumulative)
  df <- long_daily(annual)
  df$Year <- factor(format(df$Date, "%Y"))
  p <- ggplot(df, aes(Year, Return * 100, fill = Strategy)) +
    geom_col(position = "dodge", width = 0.75) +
    scale_fill_viridis_d(option = "D", end = 0.9) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Return (%)",
         fill = NULL, caption = "@StockViz") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
  ggsave(file.path(REPORT_PATH, paste0(stem, ".png")), p, width = 14, height = 8, dpi = 130)
}

# Number of sectoral indices with a finite month-end level at each signal date.
sector_availability <- function(cache) {
  levels <- cache$month_ends[, setdiff(colnames(cache$month_ends), "CASH")]
  counts <- rowSums(!is.na(levels))
  xts(counts, index(levels))
}

run_analysis <- function() {
  if (!file.exists(RESULTS_PATH)) stop("Run Rscript run.R first")
  results <- readRDS(RESULTS_PATH)
  cache <- readRDS(CACHE_PATH)
  validate_cache_fingerprint(cache)
  daily <- results$primary_daily
  split_ranges <- list(
    train = paste0("/", TRAIN_END),
    test = paste0(TEST_START, "/"),
    full = paste0(first(index(daily)), "/", last(index(daily)))
  )

  for (split_name in names(split_ranges)) {
    sub <- daily[split_ranges[[split_name]]]
    metrics <- metrics_frame(sub)
    save_gt_table(metrics, paste0("metrics-", split_name),
                  paste("Performance Metrics —", tools::toTitleCase(split_name)),
                  sprintf("25 bps | %s to %s", first(index(sub)), last(index(sub))),
                  c("CAGR", "Volatility", "MaxDD", "WorstMonth", "PositiveMonths"))
    plot_cumulative(sub, paste0("cumulative-returns-", split_name),
                    paste("Cumulative Returns —", tools::toTitleCase(split_name)),
                    sprintf("25 bps | %s to %s", first(index(sub)), last(index(sub))))
    plot_annual(sub, paste0("annual-returns-", split_name),
                paste("Annual Returns —", tools::toTitleCase(split_name)), "25 bps")
  }

  # Sector availability diagnostic (staggered inclusion coverage).
  avail <- sector_availability(cache)
  availability_df <- data.frame(
    Date = as.Date(index(avail)),
    AvailableSectors = as.numeric(avail),
    stringsAsFactors = FALSE
  )
  write.csv(availability_df, file.path(REPORT_PATH, "sector-availability.csv"), row.names = FALSE)
  p_avail <- ggplot(availability_df, aes(Date, AvailableSectors)) +
    geom_step() + theme_minimal() +
    labs(title = "Sectoral Indices Available Over Time",
         subtitle = sprintf("Universe of %d TR indices; staggered inclusion", length(INDEX_NAMES)),
         x = NULL, y = "Available indices", caption = "@StockViz")
  ggsave(file.path(REPORT_PATH, "sector-availability.png"), p_avail, width = 12, height = 6, dpi = 130)

  test_month_ends <- cache$month_ends[paste0(TEST_START, "/")]
  test_start_levels <- test_month_ends[1, setdiff(colnames(cache$month_ends), "CASH")]
  cat(sprintf("Sectors with a month-end level at first test month-end (%s): %d of %d\n",
              format(index(test_month_ends)[1]), sum(!is.na(test_start_levels)), length(INDEX_NAMES)))
  cat(sprintf("Sectors available at series end: %d of %d\n",
              tail(as.numeric(avail), 1), length(INDEX_NAMES)))

  # Momentum-based availability (exact universe size used by the ranking).
  a10 <- results$audit[results$audit$strategy == "10M Top 1" &
                         results$audit$holding_start_date >= TEST_START, ]
  cat(sprintf("Sectors available (10-month momentum) in test period: %d to %d of %d\n",
              min(a10$available_count), max(a10$available_count), length(INDEX_NAMES)))

  test_metrics <- metrics_frame(daily[paste0(TEST_START, "/")])
  cat("\nTest-period metrics (25 bps):\n")
  print(test_metrics, row.names = FALSE)

  cat(sprintf("\nAnalysis complete: %d daily rows across %d strategies\n",
              NROW(daily), NCOL(daily)))
  invisible(list(metrics = test_metrics, availability = availability_df))
}

if (sys.nframe() == 0L) {
  run_analysis()
}

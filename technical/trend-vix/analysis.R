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
source("/mnt/hollandC/StockViz/R/plot.common.r")

RESULTS_PATH <- file.path(REPORT_PATH, "backtest-results.rds")
PRIMARY_COST_KEY <- "25bps"
BOOTSTRAP_REPS <- 2000L
BOOTSTRAP_BLOCK <- 4L

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

plot_common_index_comparison <- function(results, cache, date_range, split_name) {
  vix_top1 <- results$cross[[PRIMARY_COST_KEY]]$VIX_Top_1$daily[date_range]
  index_parts <- lapply(INDEX_NAMES, function(asset) cache$index_returns[date_range, asset])
  parts <- c(list(vix_top1), index_parts)
  common_dates <- Reduce(intersect, lapply(parts, function(x) as.Date(index(x))))
  if (length(common_dates) == 0L) stop("No common dates for Common chart: ", split_name)
  comparison <- do.call(merge, lapply(parts, function(x) x[as.character(common_dates)]))
  colnames(comparison) <- c("VIX Top 1", INDEX_NAMES)
  output <- file.path(REPORT_PATH, paste0("common-cumulative-returns-", split_name, ".png"))
  Common.PlotCumReturns(
    comparison,
    paste("VIX Top 1 vs Equity Indices —", tools::toTitleCase(split_name)),
    sprintf("%s to %s | VIX Top 1 net of 25 bps; indices are buy-and-hold total-return benchmarks",
            first(index(comparison)), last(index(comparison))),
    output,
    NULL
  )
  if (!file.exists(output) || file.info(output)$size == 0) {
    stop("Common chart was not created: ", output)
  }
  invisible(output)
}

monthly_pair <- function(results, adaptive = "VIX_Top_1", fixed = "10M_Top_1",
                         cost_key = PRIMARY_COST_KEY) {
  a <- apply.monthly(results$cross[[cost_key]][[adaptive]]$daily, Return.cumulative)
  f <- apply.monthly(results$cross[[cost_key]][[fixed]]$daily, Return.cumulative)
  out <- na.omit(merge(a, f, join = "inner"))
  colnames(out) <- c("Adaptive", "Fixed")
  out
}

block_bootstrap <- function(pair, reps = BOOTSTRAP_REPS, block = BOOTSTRAP_BLOCK) {
  mat <- coredata(pair)
  n <- nrow(mat)
  starts <- seq_len(n - block + 1L)
  mean_diff <- numeric(reps)
  sharpe_diff <- numeric(reps)
  set.seed(103734)
  for (b in seq_len(reps)) {
    chosen <- sample(starts, ceiling(n / block), replace = TRUE)
    idx <- unlist(lapply(chosen, function(s) s:(s + block - 1L)))[seq_len(n)]
    z <- mat[idx, , drop = FALSE]
    d <- z[, 1] - z[, 2]
    mean_diff[b] <- mean(d)
    sharpe_diff[b] <- mean(z[, 1]) / sd(z[, 1]) * sqrt(12) -
      mean(z[, 2]) / sd(z[, 2]) * sqrt(12)
  }
  list(mean_ci = quantile(mean_diff, c(0.025, 0.975), na.rm = TRUE),
       sharpe_ci = quantile(sharpe_diff, c(0.025, 0.975), na.rm = TRUE))
}

subset_exclusion <- function(pair, label) {
  dates <- as.Date(index(pair))
  keep <- rep(TRUE, NROW(pair))
  if (label == "Exclude Mar-Dec 2020") keep <- !(dates >= as.Date("2020-03-01") & dates <= as.Date("2020-12-31"))
  if (label == "Exclude 2020-2021") keep <- !(format(dates, "%Y") %in% c("2020", "2021"))
  if (label == "Exclude 2022") keep <- format(dates, "%Y") != "2022"
  pair[keep]
}

pair_summary <- function(pair, label) {
  d <- as.numeric(pair[, "Adaptive"] - pair[, "Fixed"])
  data.frame(
    Test = label,
    Months = length(d),
    MeanMonthlyExcess = mean(d),
    AdaptiveSharpe = mean(pair[, "Adaptive"]) / sd(pair[, "Adaptive"]) * sqrt(12),
    FixedSharpe = mean(pair[, "Fixed"]) / sd(pair[, "Fixed"]) * sqrt(12),
    SumExcess = sum(d),
    stringsAsFactors = FALSE
  )
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
    if (split_name %in% c("train", "test")) {
      plot_common_index_comparison(results, cache, split_ranges[[split_name]], split_name)
    }
  }

  primary_audit <- results$audit[results$audit$cost_bps == 25 &
                                   results$audit$holding_start_date >= TEST_START &
                                   results$audit$strategy %in% c("VIX Top 1", "VIX Top 2", "10M Top 1", "10M Top 2"), ]
  regime_parts <- split(primary_audit, list(primary_audit$strategy, primary_audit$regime), drop = TRUE)
  regime_df <- do.call(rbind, lapply(regime_parts, function(d) {
    r <- d$net_return
    data.frame(Strategy = d$strategy[1], Regime = d$regime[1], Months = length(r),
               CAGR = prod(1 + r)^(12 / length(r)) - 1,
               Sharpe = if (sd(r) > 0) mean(r) / sd(r) * sqrt(12) else NA,
               MaxDD = as.numeric(maxDrawdown(xts(r, as.Date(d$holding_end_date)))),
               Contribution = sum(r), stringsAsFactors = FALSE)
  }))
  rownames(regime_df) <- NULL
  save_gt_table(regime_df, "regime-metrics", "Test-Period Performance by India VIX Regime",
                sprintf("From %s | Red regime is exploratory because coverage is sparse", TEST_START),
                c("CAGR", "MaxDD", "Contribution"))

  cost_df <- do.call(rbind, lapply(names(results$cross), function(cost_key) {
    do.call(rbind, lapply(results$cross[[cost_key]], function(z) {
      test_daily <- z$daily[paste0(TEST_START, "/")]
      test_audit <- z$audit[z$audit$holding_start_date >= TEST_START, ]
      m <- compute_metrics(test_daily)
      data.frame(Cost = as.numeric(sub("bps", "", cost_key)), Strategy = z$strategy,
                 CAGR = m["CAGR"], Sharpe = m["Sharpe"], MaxDD = m["MaxDD"],
                 AvgMonthlyTurnover = mean(test_audit$turnover),
                 AnnualizedTurnover = mean(test_audit$turnover) * 12,
                 TotalCost = sum(test_audit$cost), stringsAsFactors = FALSE)
    }))
  }))
  rownames(cost_df) <- NULL
  save_gt_table(cost_df, "cost-sensitivity", "Test-Period Transaction-Cost Sensitivity",
                sprintf("From %s | cost per unit of one-way turnover", TEST_START),
                c("CAGR", "MaxDD", "AvgMonthlyTurnover", "AnnualizedTurnover", "TotalCost"))

  pair <- monthly_pair(results)[paste0(TEST_START, "/")]
  diff <- as.numeric(pair[, "Adaptive"] - pair[, "Fixed"])
  largest_idx <- order(diff, decreasing = TRUE)[c(seq_len(min(5, length(diff))),
                                                  (length(diff) - min(4, length(diff))):length(diff))]
  largest <- data.frame(Date = as.Date(index(pair))[largest_idx],
                        Adaptive = as.numeric(pair[largest_idx, "Adaptive"]),
                        Fixed = as.numeric(pair[largest_idx, "Fixed"]),
                        Difference = diff[largest_idx])
  save_gt_table(largest, "largest-relative-months", "Largest Relative Months — VIX Top 1 vs 10M Top 1",
                sprintf("From %s | five best and five worst monthly differences", TEST_START),
                c("Adaptive", "Fixed", "Difference"))

  roll_diff <- rollapply(pair[, "Adaptive"] - pair[, "Fixed"], 12, sum, fill = NA, align = "right")
  roll_df <- data.frame(Date = as.Date(index(roll_diff)), Value = as.numeric(roll_diff))
  p_rel <- ggplot(na.omit(roll_df), aes(Date, Value * 100)) + geom_line(color = viridis(1)) +
    geom_hline(yintercept = 0, linetype = 2) + theme_minimal() +
    labs(title = "Rolling 12-Month Relative Return",
         subtitle = sprintf("Test data from %s | VIX Top 1 minus fixed 10M Top 1", TEST_START),
         x = NULL, y = "Difference (percentage points)", caption = "@StockViz")
  ggsave(file.path(REPORT_PATH, "rolling-relative-returns.png"), p_rel, width = 12, height = 6, dpi = 130)

  roll_sharpe <- function(x) rollapply(x, 36, function(z) mean(z) / sd(z) * sqrt(12), fill = NA, align = "right")
  rs <- merge(roll_sharpe(pair[, "Adaptive"]), roll_sharpe(pair[, "Fixed"]))
  colnames(rs) <- c("VIX Top 1", "10M Top 1")
  rs_df <- na.omit(long_daily(rs))
  p_rs <- ggplot(rs_df, aes(Date, Return, color = Strategy)) + geom_line() +
    scale_color_viridis_d() + theme_minimal() + theme(legend.position = "bottom") +
    labs(title = "Rolling 36-Month Sharpe", subtitle = sprintf("Test data from %s", TEST_START),
         x = NULL, y = "Sharpe", color = NULL, caption = "@StockViz")
  ggsave(file.path(REPORT_PATH, "rolling-sharpe.png"), p_rs, width = 12, height = 6, dpi = 130)

  alloc <- primary_audit[primary_audit$strategy == "VIX Top 1", ]
  weight_cols <- grep("^weight_", names(alloc), value = TRUE)
  weight_cols <- weight_cols[colSums(!is.na(alloc[, weight_cols, drop = FALSE])) > 0]
  alloc_df <- data.frame(Date = rep(as.Date(alloc$holding_start_date), length(weight_cols)),
                         Asset = rep(sub("^weight_", "", weight_cols), each = nrow(alloc)),
                         Weight = unlist(alloc[, weight_cols], use.names = FALSE))
  p_alloc <- ggplot(alloc_df, aes(Date, Weight, fill = Asset)) + geom_area() +
    scale_fill_viridis_d() + theme_minimal() + theme(legend.position = "bottom") +
    labs(title = "VIX Top 1 Allocation History", subtitle = sprintf("From %s", TEST_START),
         x = NULL, y = "Weight", fill = NULL, caption = "@StockViz")
  ggsave(file.path(REPORT_PATH, "allocation-history.png"), p_alloc, width = 13, height = 6, dpi = 130)

  test_pair <- pair
  boot <- block_bootstrap(test_pair)
  stats_df <- data.frame(
    Test = c("Paired t-test p-value", "Wilcoxon p-value", "Bootstrap mean CI low",
             "Bootstrap mean CI high", "Bootstrap Sharpe-diff CI low", "Bootstrap Sharpe-diff CI high"),
    Value = c(t.test(as.numeric(test_pair[, 1]), as.numeric(test_pair[, 2]), paired = TRUE)$p.value,
              wilcox.test(as.numeric(test_pair[, 1]), as.numeric(test_pair[, 2]), paired = TRUE)$p.value,
              boot$mean_ci[1], boot$mean_ci[2], boot$sharpe_ci[1], boot$sharpe_ci[2])
  )
  save_gt_table(stats_df, "statistical-tests", "Test-Period Statistical Tests",
                sprintf("From %s | moving-block bootstrap: %d reps, %d-month blocks",
                        TEST_START, BOOTSTRAP_REPS, BOOTSTRAP_BLOCK))

  robustness <- do.call(rbind, lapply(c("Full test", "Exclude Mar-Dec 2020", "Exclude 2020-2021", "Exclude 2022"), function(label) {
    z <- if (label == "Full test") test_pair else subset_exclusion(test_pair, label)
    pair_summary(z, label)
  }))
  for (yr in sort(unique(format(index(test_pair), "%Y")))) {
    z <- test_pair[format(index(test_pair), "%Y") != yr]
    robustness <- rbind(robustness, pair_summary(z, paste("Leave out", yr)))
  }
  d <- as.numeric(test_pair[, 1] - test_pair[, 2])
  robustness <- rbind(robustness,
                      pair_summary(test_pair[-which.max(d)], "Remove best month"),
                      pair_summary(test_pair[-order(d, decreasing = TRUE)[1:3]], "Remove best 3 months"))
  save_gt_table(robustness, "robustness-tests", "VIX Top 1 Robustness — Test Period",
                sprintf("From %s | adaptive versus fixed 10-month momentum", TEST_START),
                c("MeanMonthlyExcess", "SumExcess"))

  base_signal <- build_signal_data(cache)
  grid <- expand.grid(green_prob = c(0.4, 0.5, 0.6), red_prob = c(0.85, 0.9, 0.95))
  sensitivity <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    thr <- calibrate_percentile_thresholds(base_signal, TRAIN_END, grid$green_prob[i], grid$red_prob[i])
    alt <- run_cross_portfolio(cache, TRUE, 1L, 0.0025, thr)
    test_alt <- alt$daily[paste0(TEST_START, "/")]
    m <- compute_metrics(test_alt)
    data.frame(GreenPercentile = grid$green_prob[i], RedPercentile = grid$red_prob[i],
               GreenThreshold = thr$green, RedThreshold = thr$red,
               TestCAGR = m["CAGR"], TestSharpe = m["Sharpe"], TestMaxDD = m["MaxDD"])
  }))
  rownames(sensitivity) <- NULL
  save_gt_table(sensitivity, "percentile-sensitivity", "Train-Calibrated VIX Percentile Sensitivity",
                sprintf("Thresholds estimated through 2019 only; test starts %s", TEST_START),
                c("GreenPercentile", "RedPercentile", "TestCAGR", "TestMaxDD"))

  test_diff <- as.numeric(test_pair[, "Adaptive"] - test_pair[, "Fixed"])
  concentration <- data.frame(
    Measure = c("Best 1 month share", "Best 3 months share", "Best 5 months share", "Adaptive wins"),
    Value = c(sum(sort(test_diff, decreasing = TRUE)[1]) / sum(test_diff),
              sum(sort(test_diff, decreasing = TRUE)[1:3]) / sum(test_diff),
              sum(sort(test_diff, decreasing = TRUE)[1:5]) / sum(test_diff),
              mean(test_diff > 0))
  )
  save_gt_table(concentration, "excess-return-concentration", "Excess-Return Concentration",
                sprintf("From %s | VIX Top 1 versus fixed 10M Top 1; shares can exceed 100%% when other months offset gains",
                        TEST_START),
                "Value")

  cat(sprintf("Analysis complete: %d daily rows, %d monthly comparisons, %d sensitivity cells\n",
              NROW(daily), NROW(pair), nrow(sensitivity)))
  invisible(list(regime = regime_df, costs = cost_df, robustness = robustness,
                 sensitivity = sensitivity, statistics = stats_df))
}

if (sys.nframe() == 0L) {
  run_analysis()
}

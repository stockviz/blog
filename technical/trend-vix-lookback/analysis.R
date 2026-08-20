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

RESULTS_PATH <- file.path(REPORT_PATH, "lookback-results.rds")
PRIMARY_COST <- "25"
REPRESENTATIVE <- c(1L, 3L, 6L, 10L, 12L)

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
  tbl <- tbl |> tab_style(cell_fill("#C8E6C9"),
                          cells_body(rows = Lookback == 10L))
  html <- file.path(REPORT_PATH, paste0(stem, ".html"))
  png <- file.path(REPORT_PATH, paste0(stem, ".png"))
  gtsave(tbl, html)
  webshot2::webshot(html, png, selector = "table.gt_table", expand = c(10, 10, 10, 10))
  invisible(tbl)
}

long_daily <- function(x) {
  data.frame(
    Date = rep(as.Date(index(x)), NCOL(x)),
    Series = rep(colnames(x), each = NROW(x)),
    Value = as.numeric(coredata(x)),
    stringsAsFactors = FALSE
  )
}

key_for <- function(cost_name, top_n, lookback) {
  paste(cost_name, paste0("Top", top_n), paste0("L", lookback), sep = "_")
}

# Metrics for one Top N, over the common aligned train window (the latest first
# holding date across all lookbacks, so every lookback is compared over the same
# months). FullStart records when each lookback could actually have begun.
sweep_metrics <- function(results, top_n, train_end) {
  lbs <- results$sweep_lookbacks
  series <- lapply(lbs, function(lb) results$results[[key_for(PRIMARY_COST, top_n, lb)]]$daily)
  firsts <- as.Date(vapply(series, function(x) as.character(first(index(x))), character(1)))
  common_start <- max(firsts)
  aud10 <- results$results[[key_for(PRIMARY_COST, top_n, 10L)]]$audit
  aud10 <- aud10[aud10$holding_start_date >= common_start & aud10$holding_start_date <= train_end, ]
  rows <- lapply(seq_along(lbs), function(k) {
    lb <- lbs[k]
    d <- series[[k]][paste0(common_start, "/", train_end)]
    m <- compute_metrics(d)
    aud <- results$results[[key_for(PRIMARY_COST, top_n, lb)]]$audit
    aud <- aud[aud$holding_start_date >= common_start & aud$holding_start_date <= train_end, ]
    ident <- mean(aud$selected_assets == aud10$selected_assets)
    data.frame(
      Lookback = lb,
      CAGR = m["CAGR"], Volatility = m["Volatility"], Sharpe = m["Sharpe"],
      MaxDD = m["MaxDD"], Calmar = m["Calmar"],
      AvgTurnover = mean(aud$turnover),
      IdenticalToL10 = ident,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  df$FullStart <- as.character(firsts)
  attr(df, "common_start") <- common_start
  df
}

plot_metric <- function(top1, top2, value_col, y_label, stem, title) {
  long <- rbind(
    transform(top1, TopN = "Top 1"),
    transform(top2, TopN = "Top 2")
  )
  long$TopN <- factor(long$TopN, levels = c("Top 1", "Top 2"))
  long$Value <- long[[value_col]]
  p <- ggplot(long, aes(Lookback, Value, color = TopN)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_vline(xintercept = 10, linetype = 2, color = "grey40") +
    scale_color_viridis_d(option = "D", end = 0.85) +
    scale_x_continuous(breaks = sort(unique(long$Lookback))) +
    labs(title = title,
         subtitle = sprintf("Train set, aligned common window through %s | 25 bps",
                            TRAIN_END),
         x = "Momentum lookback (months)", y = y_label, color = NULL,
         caption = "@StockViz") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(file.path(REPORT_PATH, paste0(stem, ".png")), p, width = 10, height = 6, dpi = 130)
  invisible(p)
}

plot_cumulative_sweep <- function(results, common_start, train_end) {
  series <- lapply(REPRESENTATIVE, function(lb) {
    results$results[[key_for(PRIMARY_COST, 1L, lb)]]$daily[paste0(common_start, "/", train_end)]
  })
  cd <- Reduce(intersect, lapply(series, function(x) as.Date(index(x))))
  merged <- do.call(merge, lapply(series, function(x) x[as.character(cd)]))
  colnames(merged) <- paste0("L", REPRESENTATIVE)
  wealth <- xts(apply(1 + coredata(merged), 2, cumprod), index(merged))
  colnames(wealth) <- colnames(merged)
  df <- long_daily(wealth)
  p <- ggplot(df, aes(Date, Value, color = Series)) +
    geom_line(linewidth = 0.7) +
    scale_color_viridis_d(option = "D", end = 0.9) +
    scale_y_log10() +
    labs(title = "Cumulative Return by Momentum Lookback — Top 1",
         subtitle = sprintf("Train set, aligned common window through %s | 25 bps | lookbacks %s months",
                            TRAIN_END, paste(REPRESENTATIVE, collapse = ", ")),
         x = NULL, y = "Growth of 1 (log scale)", color = "Lookback",
         caption = "@StockViz") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(file.path(REPORT_PATH, "lookback-cumulative.png"), p, width = 12, height = 7, dpi = 130)
  invisible(p)
}

spread_summary <- function(df, label) {
  c(
    Label = label,
    SharpeMin = min(df$Sharpe), SharpeMax = max(df$Sharpe), SharpeSpread = max(df$Sharpe) - min(df$Sharpe),
    SharpeL10 = df$Sharpe[df$Lookback == 10L],
    CAGRMin = min(df$CAGR), CAGRMax = max(df$CAGR), CAGRSpread = max(df$CAGR) - min(df$CAGR),
    CAGRL10 = df$CAGR[df$Lookback == 10L],
    MaxDDMin = min(df$MaxDD), MaxDDMax = max(df$MaxDD), MaxDDSpread = max(df$MaxDD) - min(df$MaxDD),
    MaxDDL10 = df$MaxDD[df$Lookback == 10L],
    SharpeRankL10 = rank(-df$Sharpe)[df$Lookback == 10L],
    BeatL10Sharpe = sum(df$Sharpe > df$Sharpe[df$Lookback == 10L])
  )
}

monthly_regime_metrics <- function(returns) {
  n <- length(returns)
  if (n == 0L) return(c(CAGR = NA, Sharpe = NA, MaxDD = NA))
  sharpe <- if (n > 1L && sd(returns) > 0) mean(returns) / sd(returns) * sqrt(12) else NA_real_
  dates <- seq(as.Date("2000-01-31"), by = "month", length.out = n)
  c(
    CAGR = prod(1 + returns)^(12 / n) - 1,
    Sharpe = sharpe,
    MaxDD = as.numeric(maxDrawdown(xts(returns, dates)))
  )
}

regime_lookback_metrics <- function(results, cache, common_start, train_end) {
  signal <- build_signal_data(cache)
  regime_map <- setNames(signal$regime, as.character(signal$dates))
  rows <- list()
  out_i <- 0L
  for (top_n in results$top_ns) {
    for (lb in results$sweep_lookbacks) {
      audit <- results$results[[key_for(PRIMARY_COST, top_n, lb)]]$audit
      audit <- audit[audit$holding_start_date >= common_start &
                       audit$holding_start_date <= train_end, ]
      audit$Regime <- unname(regime_map[as.character(audit$signal_date)])
      # India VIX starts in 2009; pre-VIX train months cannot be assigned a regime.
      audit <- audit[!is.na(audit$Regime), ]
      for (regime in c("Green", "Yellow", "Red")) {
        returns <- audit$net_return[audit$Regime == regime]
        m <- monthly_regime_metrics(returns)
        out_i <- out_i + 1L
        rows[[out_i]] <- data.frame(
          TopN = paste("Top", top_n), Regime = regime, Lookback = lb,
          Months = length(returns), CAGR = m["CAGR"], Sharpe = m["Sharpe"],
          MaxDD = m["MaxDD"], stringsAsFactors = FALSE
        )
      }
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

ideal_regime_lookbacks <- function(regime_metrics) {
  prescribed <- c(Green = 10L, Yellow = 3L, Red = 1L)
  parts <- split(regime_metrics, list(regime_metrics$TopN, regime_metrics$Regime), drop = TRUE)
  rows <- lapply(parts, function(x) {
    ranked <- x[order(x$Sharpe, decreasing = TRUE), ]
    best <- ranked[1, ]
    runner_up <- ranked[2, ]
    original <- x[x$Lookback == prescribed[[best$Regime]], ]
    data.frame(
      TopN = best$TopN,
      Regime = best$Regime,
      Months = best$Months,
      BestLookback = best$Lookback,
      Sharpe = best$Sharpe,
      CAGR = best$CAGR,
      MaxDD = best$MaxDD,
      RunnerUp = runner_up$Lookback,
      RunnerUpSharpe = runner_up$Sharpe,
      OriginalLookback = original$Lookback,
      OriginalSharpe = original$Sharpe,
      OriginalRank = match(original$Lookback, ranked$Lookback),
      Conclusion = if (best$Months < 20L) "Exploratory — insufficient coverage" else "Usable train coverage",
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$TopN <- factor(out$TopN, levels = c("Top 1", "Top 2"))
  out$Regime <- factor(out$Regime, levels = c("Green", "Yellow", "Red"))
  out <- out[order(out$TopN, out$Regime), ]
  out$TopN <- as.character(out$TopN)
  out$Regime <- as.character(out$Regime)
  rownames(out) <- NULL
  out
}

save_ideal_regime_table <- function(df) {
  tbl <- gt(df, groupname_col = "TopN") |>
    tab_header(
      title = "Best Train-Period Lookback by India VIX Regime",
      subtitle = "Highest annualized Sharpe | 25 bps | original 18/32 VIX thresholds"
    ) |>
    fmt_number(columns = c(Sharpe, RunnerUpSharpe, OriginalSharpe), decimals = 2) |>
    fmt_percent(columns = c(CAGR, MaxDD), decimals = 2) |>
    tab_style(cell_text(weight = "bold", size = "larger"), cells_row_groups()) |>
    tab_style(cell_fill("#E3F2FD"), cells_row_groups()) |>
    tab_style(cell_fill("#FFEBEE"), cells_body(rows = Regime == "Red")) |>
    tab_source_note("@StockViz") |>
    opt_row_striping()
  html <- file.path(REPORT_PATH, "ideal-lookbacks-by-regime.html")
  png <- file.path(REPORT_PATH, "ideal-lookbacks-by-regime.png")
  gtsave(tbl, html)
  webshot2::webshot(html, png, selector = "table.gt_table", expand = c(10, 10, 10, 10))
  invisible(tbl)
}

test_comparison_metrics <- function(results) {
  if (is.null(results$test_comparison)) stop("Re-run Rscript run.R to build test comparison")
  rows <- list()
  out_i <- 0L
  for (top_key in names(results$test_comparison)) {
    variants <- results$test_comparison[[top_key]]
    for (variant in names(variants)) {
      result <- variants[[variant]]
      daily <- result$daily[paste0(results$test_start, "/")]
      audit <- result$audit[result$audit$holding_start_date >= results$test_start, ]
      m <- compute_metrics(daily)
      out_i <- out_i + 1L
      rows[[out_i]] <- data.frame(
        Portfolio = sub("Top", "Top ", top_key),
        Strategy = sub(" Top [12]$", "", result$strategy),
        Days = NROW(daily),
        CAGR = m["CAGR"], Volatility = m["Volatility"], Sharpe = m["Sharpe"],
        MaxDD = m["MaxDD"], Calmar = m["Calmar"],
        AvgMonthlyTurnover = mean(audit$turnover),
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  out$Portfolio <- factor(out$Portfolio, levels = c("Top 1", "Top 2"))
  out$Strategy <- factor(out$Strategy,
                         levels = c("Train-Tuned 10/6/1", "Original 10/3/1", "Fixed 10M"))
  out <- out[order(out$Portfolio, out$Strategy), ]
  out$Portfolio <- as.character(out$Portfolio)
  out$Strategy <- as.character(out$Strategy)
  rownames(out) <- NULL
  out
}

save_test_metrics_table <- function(df) {
  tbl <- gt(df, groupname_col = "Portfolio") |>
    tab_header(
      title = "Test-Period Performance — Train-Tuned VIX Lookbacks",
      subtitle = sprintf("%s onward | 25 bps | Green 10M, Yellow 6M, Red 1M", TEST_START)
    ) |>
    fmt_number(columns = c(Sharpe, Calmar), decimals = 2) |>
    fmt_percent(columns = c(CAGR, Volatility, MaxDD, AvgMonthlyTurnover), decimals = 2) |>
    tab_style(cell_text(weight = "bold", size = "larger"), cells_row_groups()) |>
    tab_style(cell_fill("#E3F2FD"), cells_row_groups()) |>
    tab_style(cell_fill("#C8E6C9"), cells_body(rows = Strategy == "Train-Tuned 10/6/1")) |>
    tab_source_note("@StockViz") |>
    opt_row_striping()
  html <- file.path(REPORT_PATH, "metrics-test.html")
  png <- file.path(REPORT_PATH, "metrics-test.png")
  gtsave(tbl, html)
  webshot2::webshot(html, png, selector = "table.gt_table", expand = c(10, 10, 10, 10))
  invisible(tbl)
}

plot_test_cumulative <- function(results) {
  parts <- list()
  out_i <- 0L
  for (top_key in names(results$test_comparison)) {
    variants <- results$test_comparison[[top_key]]
    series <- lapply(variants, function(x) x$daily[paste0(results$test_start, "/")])
    common_dates <- Reduce(intersect, lapply(series, function(x) as.Date(index(x))))
    merged <- do.call(merge, lapply(series, function(x) x[as.character(common_dates)]))
    colnames(merged) <- c("Train-Tuned 10/6/1", "Original 10/3/1", "Fixed 10M")
    wealth <- xts(apply(1 + coredata(merged), 2, cumprod), index(merged))
    df <- long_daily(wealth)
    df$Portfolio <- sub("Top", "Top ", top_key)
    out_i <- out_i + 1L
    parts[[out_i]] <- df
  }
  plot_df <- do.call(rbind, parts)
  plot_df$Portfolio <- factor(plot_df$Portfolio, levels = c("Top 1", "Top 2"))
  p <- ggplot(plot_df, aes(Date, Value, color = Series)) +
    geom_line(linewidth = 0.75) +
    facet_wrap(~Portfolio, ncol = 1, scales = "free_y") +
    scale_color_viridis_d(option = "D", end = 0.9) +
    scale_y_log10() +
    labs(
      title = "Cumulative Returns — Test Period",
      subtitle = sprintf("%s onward | 25 bps | train-tuned 10/6/1 vs original 10/3/1 and fixed 10M",
                         TEST_START),
      x = NULL, y = "Growth of 1 (log scale)", color = NULL, caption = "@StockViz"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(file.path(REPORT_PATH, "cumulative-returns-test.png"), p,
         width = 12, height = 9, dpi = 130)
  invisible(p)
}

run_analysis <- function() {
  if (!file.exists(RESULTS_PATH)) stop("Run Rscript run.R first")
  results <- readRDS(RESULTS_PATH)
  cache <- readRDS(CACHE_PATH)
  validate_cache_fingerprint(cache)
  train_end <- as.Date(results$train_end)

  top1 <- sweep_metrics(results, 1L, train_end)
  top2 <- sweep_metrics(results, 2L, train_end)
  common_start <- attr(top1, "common_start")

  save_gt_table(top1, "metrics-top1",
                "Train-Period Metrics by Momentum Lookback — Top 1",
                sprintf("Aligned common window %s to %s | 25 bps | highlighted row is the 10-month control",
                        common_start, train_end),
                c("CAGR", "Volatility", "MaxDD", "AvgTurnover", "IdenticalToL10"))
  save_gt_table(top2, "metrics-top2",
                "Train-Period Metrics by Momentum Lookback — Top 2",
                sprintf("Aligned common window %s to %s | 25 bps | highlighted row is the 10-month control",
                        common_start, train_end),
                c("CAGR", "Volatility", "MaxDD", "AvgTurnover", "IdenticalToL10"))

  plot_metric(top1, top2, "Sharpe", "Annualized Sharpe ratio", "lookback-sharpe",
              "Sharpe vs Momentum Lookback")
  top1$CAGRpct <- top1$CAGR * 100
  top2$CAGRpct <- top2$CAGR * 100
  plot_metric(top1, top2, "CAGRpct", "CAGR (%)", "lookback-cagr",
              "CAGR vs Momentum Lookback")
  top1$MaxDDpct <- top1$MaxDD * 100
  top2$MaxDDpct <- top2$MaxDD * 100
  plot_metric(top1, top2, "MaxDDpct", "Maximum drawdown (%)", "lookback-maxdd",
              "Maximum Drawdown vs Momentum Lookback")

  plot_cumulative_sweep(results, common_start, train_end)

  regime_metrics <- regime_lookback_metrics(results, cache, common_start, train_end)
  ideal_regimes <- ideal_regime_lookbacks(regime_metrics)
  write.csv(regime_metrics, file.path(REPORT_PATH, "regime-lookback-metrics.csv"),
            row.names = FALSE)
  save_ideal_regime_table(ideal_regimes)

  test_metrics <- test_comparison_metrics(results)
  save_test_metrics_table(test_metrics)
  plot_test_cumulative(results)
  write.csv(test_metrics, file.path(REPORT_PATH, "metrics-test.csv"), row.names = FALSE)

  s1 <- spread_summary(top1, "Top 1")
  s2 <- spread_summary(top2, "Top 2")
  cat(sprintf("Common train window: %s to %s\n", common_start, train_end))
  for (s in list(s1, s2)) {
    cat(sprintf("\n%s lookback sweep (train):\n", s["Label"]))
    cat(sprintf("  Sharpe  range %.2f .. %.2f (spread %.2f) | L10 = %.2f (rank %s of 12)\n",
                as.numeric(s["SharpeMin"]), as.numeric(s["SharpeMax"]),
                as.numeric(s["SharpeSpread"]), as.numeric(s["SharpeL10"]), s["SharpeRankL10"]))
    cat(sprintf("  CAGR    range %.2f%% .. %.2f%% (spread %.2f pp) | L10 = %.2f%%\n",
                as.numeric(s["CAGRMin"]) * 100, as.numeric(s["CAGRMax"]) * 100,
                as.numeric(s["CAGRSpread"]) * 100, as.numeric(s["CAGRL10"]) * 100))
    cat(sprintf("  MaxDD   range %.2f%% .. %.2f%% (spread %.2f pp) | L10 = %.2f%%\n",
                as.numeric(s["MaxDDMin"]) * 100, as.numeric(s["MaxDDMax"]) * 100,
                as.numeric(s["MaxDDSpread"]) * 100, as.numeric(s["MaxDDL10"]) * 100))
    cat(sprintf("  Lookbacks beating L10 Sharpe: %s of 11\n", s["BeatL10Sharpe"]))
  }

  cat("\nBest lookbacks by VIX regime (highest train Sharpe):\n")
  print(ideal_regimes, row.names = FALSE)
  cat("\nHeld-out test metrics:\n")
  print(test_metrics, row.names = FALSE)
  cat("\nAnalysis complete.\n")
  invisible(list(top1 = top1, top2 = top2, common_start = common_start,
                 regime_metrics = regime_metrics, ideal_regimes = ideal_regimes,
                 test_metrics = test_metrics))
}

if (sys.nframe() == 0L) {
  run_analysis()
}

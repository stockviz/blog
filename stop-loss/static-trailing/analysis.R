suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(viridis)
  library(gt)
  library(webshot2)
})

source("build.R")

REPORT_PATH <- "/mnt/data/blog/stop-loss/static-trailing"
CACHE_PATH <- file.path(REPORT_PATH, "cache.rds")
RESULTS_PATH <- file.path(REPORT_PATH, "results.rds")

save_gt_table <- function(df, stem, title, subtitle = NULL) {
  tbl <- gt(df) |>
    tab_header(title = title, subtitle = subtitle) |>
    tab_source_note("@StockViz") |>
    tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
    opt_row_striping()
  # numeric formatting: 2 decimals except counts
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  # keep generic numeric 2 decimals for readability; counts stay integer
  # we apply fmt_number to all numeric with 2 decimals, then override N
  if (length(numeric_cols)) tbl <- tbl |> fmt_number(columns = all_of(numeric_cols), decimals = 2)
  # N / horizon / year should be integer without thousands separator
  if ("N" %in% names(df)) tbl <- tbl |> fmt_number(columns = N, decimals = 0, use_seps = FALSE)
  if ("horizon" %in% names(df)) tbl <- tbl |> fmt_number(columns = horizon, decimals = 0, use_seps = FALSE)
  if ("year" %in% names(df)) tbl <- tbl |> fmt_number(columns = year, decimals = 0, use_seps = FALSE)
  if ("Year" %in% names(df)) tbl <- tbl |> fmt_number(columns = Year, decimals = 0, use_seps = FALSE)
  html <- file.path(REPORT_PATH, paste0(stem, ".html"))
  png <- file.path(REPORT_PATH, paste0(stem, ".png"))
  gtsave(tbl, html)
  webshot2::webshot(html, png, selector = "table.gt_table", expand = c(10, 10, 10, 10))
  invisible(tbl)
}

compute_horizon_stats <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0) return(data.frame(N = 0, mean = NA, median = NA, sd = NA, pos_rate = NA,
                                 neg_rate = NA, mean_pos = NA, mean_neg = NA, t_stat = NA, p_value = NA))
  m <- mean(x)
  s <- sd(x)
  t_stat <- if (s > 0) m / (s / sqrt(n)) else NA_real_
  p_value <- if (!is.na(t_stat)) 2 * pt(-abs(t_stat), df = n - 1) else NA_real_
  data.frame(
    N = n,
    mean = m * 100,
    median = median(x) * 100,
    sd = s * 100,
    pos_rate = mean(x > 0) * 100,
    neg_rate = mean(x < 0) * 100,
    mean_pos = mean(x[x > 0]) * 100,
    mean_neg = mean(x[x < 0]) * 100,
    t_stat = t_stat,
    p_value = p_value,
    stringsAsFactors = FALSE
  )
}

run_analysis <- function() {
  if (!file.exists(CACHE_PATH)) stop("Run Rscript build.R first")
  cache <- readRDS(CACHE_PATH)
  validate_cache_fingerprint(cache)
  forward <- cache$forward
  forward <- forward[is.finite(forward$ret), ]

  # ----------------------------------------------------------------
  # 1. Combined stats by horizon (primary efficacy table)
  combined_stats <- do.call(rbind, lapply(sort(unique(forward$horizon)), function(h) {
    sub <- forward[forward$horizon == h, "ret"]
    cbind(data.frame(horizon = h), compute_horizon_stats(sub))
  }))
  # Format for display — keep raw for table but values already in %
  save_gt_table(combined_stats, "metrics-combined",
                "Forward Return After Non-Adaptive Trailing SL — Both Models Combined",
                sprintf("N=%d SL events (%s to %s) | PG: adjusted close ratio, else RETURN_SERIES_ALL cumulative | horizons are trading days after SL date",
                        nrow(cache$sl), min(cache$sl$SL_DATE), max(cache$sl$SL_DATE)))

  # 3. Annual breakdown — 5, 10 and 20-day horizons
  forward$year <- as.integer(format(forward$sl_date, "%Y"))
  annual_stats_list <- list()
  for (h in c(5L, 10L, 20L)) {
    ann <- do.call(rbind, lapply(sort(unique(forward$year)), function(y) {
      sub <- forward[forward$year == y & forward$horizon == h, "ret"]
      if (length(sub) < 20) return(NULL)
      cbind(data.frame(year = y), compute_horizon_stats(sub))
    }))
    annual_stats_list[[as.character(h)]] <- ann
    save_gt_table(ann, paste0("metrics-annual-", h, "d"),
                  sprintf("%d-Day Forward Return After SL — By Calendar Year (Combined Models)", h),
                  "Static trailing; positive mean = stop was costly (holding would have gained)")
  }
  annual_stats <- annual_stats_list[["20"]]

  # 4. Tail diagnostics — 10 and 20 day combined
  tail_df <- do.call(rbind, lapply(c(10, 20), function(h) {
    sub <- forward[forward$horizon == h, "ret"]
    data.frame(
      horizon = h,
      N = length(sub),
      pct_gt_3 = mean(sub > 0.03) * 100,
      pct_gt_5 = mean(sub > 0.05) * 100,
      pct_lt_m3 = mean(sub < -0.03) * 100,
      pct_lt_m5 = mean(sub < -0.05) * 100,
      pct_gt_10 = mean(sub > 0.10) * 100,
      pct_lt_m10 = mean(sub < -0.10) * 100,
      stringsAsFactors = FALSE
    )
  }))
  save_gt_table(tail_df, "tail-rates",
                "Tail Rates After SL — Combined Models",
                "Share of SL events where holding would have gained/lost beyond threshold")

  # ----------------------------------------------------------------
  # Plots
  # Histograms of forward returns for 5, 10, 20 trading days
  for (h in c(5L, 10L, 20L)) {
    sub_h <- forward[forward$horizon == h, ]
    p_hist <- ggplot(sub_h, aes(x = ret * 100)) +
      geom_histogram(bins = 60, fill = viridis(1, option = "D"), color = "white", linewidth = 0.2) +
      geom_vline(xintercept = 0, linetype = 2, color = "red") +
      geom_vline(data = data.frame(m = mean(sub_h$ret) * 100), aes(xintercept = m), color = "blue", linetype = 1) +
      labs(title = sprintf("Distribution of %d-Day Forward Return After Static Trailing SL", h),
           subtitle = sprintf("Both models combined — N=%d | mean %.2f%%, median %.2f%%, %.1f%% positive | blue = mean, red = 0",
                              nrow(sub_h), mean(sub_h$ret) * 100, median(sub_h$ret) * 100, mean(sub_h$ret > 0) * 100),
           x = sprintf("%d-day forward return (%%)", h), y = "Count", caption = "@StockViz") +
      theme_minimal(base_size = 11)
    ggsave(file.path(REPORT_PATH, sprintf("hist-%dd.png", h)), p_hist, width = 12, height = 7, dpi = 130)
  }

  # Boxplot by horizon
  p_box <- ggplot(forward, aes(x = factor(horizon), y = ret * 100, fill = factor(horizon))) +
    geom_boxplot(outlier.alpha = 0.15, outlier.size = 0.7) +
    geom_hline(yintercept = 0, linetype = 2, color = "red") +
    scale_fill_viridis_d(option = "D", end = 0.9) +
    labs(title = "Forward Return After SL by Horizon",
         subtitle = "Both models combined | holding 1,5,10,20 trading days after SL trigger",
         x = "Horizon (trading days)", y = "Forward return (%)", fill = NULL, caption = "@StockViz") +
    theme_minimal(base_size = 11) + theme(legend.position = "none")
  ggsave(file.path(REPORT_PATH, "box-by-horizon.png"), p_box, width = 12, height = 7, dpi = 130)

  # Mean by horizon (line)
  horizon_means <- aggregate(ret ~ horizon, forward, mean)
  p_mean <- ggplot(horizon_means, aes(x = horizon, y = ret * 100)) +
    geom_line(color = viridis(1), linewidth = 1.1) +
    geom_point(color = viridis(1), size = 3) +
    scale_x_continuous(breaks = c(1, 5, 10, 20)) +
    labs(title = "Mean Forward Return After SL Grows with Horizon",
         subtitle = "Both models | static trailing — the longer you would have held, the more you missed",
         x = "Horizon (trading days)", y = "Mean forward return (%)", caption = "@StockViz") +
    theme_minimal(base_size = 11)
  ggsave(file.path(REPORT_PATH, "mean-by-horizon.png"), p_mean, width = 10, height = 6, dpi = 130)

  # Annual distributions — violin per year for 5, 10, 20 trading days
  for (h in c(5L, 10L, 20L)) {
    fh <- forward[forward$horizon == h, ]
    fh$year_f <- factor(fh$year, levels = sort(unique(fh$year)))
    yr_n <- as.data.frame(table(fh$year_f))
    colnames(yr_n) <- c("year", "N")
    p_annual <- ggplot(fh, aes(x = year_f, y = ret * 100, fill = year_f)) +
      geom_violin(trim = FALSE, scale = "width", alpha = 0.85, linewidth = 0.3) +
      geom_boxplot(width = 0.08, outlier.size = 0.6, outlier.alpha = 0.4, fill = "white", linewidth = 0.3) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 2.2, color = "red") +
      geom_hline(yintercept = 0, linetype = 2, color = "black", linewidth = 0.4) +
      scale_fill_viridis_d(option = "D", end = 0.9) +
      labs(title = sprintf("Distribution of %d-Day Forward Return After SL — By Year", h),
           subtitle = sprintf("Each violin = full distribution that year (trading days); box = quartiles, red diamond = mean | N %s–%s per year | positive = holding would have gained",
                              min(yr_n$N), max(yr_n$N)),
           x = NULL, y = sprintf("%d-day forward return (%%)", h), fill = NULL, caption = "@StockViz") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 0, hjust = 0.5)) +
      coord_cartesian(ylim = quantile(fh$ret * 100, c(0.01, 0.99), na.rm = TRUE) * 1.15)
    ggsave(file.path(REPORT_PATH, sprintf("annual-%dd.png", h)), p_annual, width = 13, height = 7, dpi = 130)
  }

  # Save results bundle
  results <- list(
    combined = combined_stats,
    annual = annual_stats,
    annual_list = annual_stats_list,
    tail = tail_df,
    n_events = nrow(cache$sl),
    n_forward = nrow(forward)
  )
  saveRDS(results, RESULTS_PATH)
  # also export forward returns for audit
  write.csv(forward, file.path(REPORT_PATH, "forward-returns.csv"), row.names = FALSE)
  cat(sprintf("Analysis complete: %d SL events, %d forward returns (finite)\n", nrow(cache$sl), nrow(forward)))
  print(combined_stats)
  invisible(results)
}

if (sys.nframe() == 0L) {
  run_analysis()
}

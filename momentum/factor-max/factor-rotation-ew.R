# Factor rotation versus equal-weight factor basket
# Monthly signal: select the factor with the best completed prior-month return.
# The selected factor is held for the following month; benchmark is NIFTY 500 TR.

library(RODBC)
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(zoo)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(gt)
library(webshot2)

options(scipen = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("common.R")

# Fixed colors are reused in every chart and table.
series_colors <- c(
  "Factor Rotation" = "#2166AC",
  "Factor Equal Weight" = "#D6604D",
  "NIFTY 500 TR" = "#1B7837"
)

safe_num <- function(x) {
  if (length(x) == 0 || is.null(x)) return(NA_real_)
  as.numeric(x[[1]])
}

slice_window <- function(x, start = NULL, end = NULL) {
  if (is.null(start) && is.null(end)) return(x)
  if (is.null(start)) return(x[paste0("/", end)])
  if (is.null(end)) return(x[paste0(start, "/")])
  x[paste0(start, "/", end)]
}

metric_row <- function(x, name) {
  x <- na.omit(x)
  if (NROW(x) < 2) {
    return(tibble(
      Strategy = name, CAGR = NA_real_, Volatility = NA_real_, Sharpe = NA_real_,
      Sortino = NA_real_, MaxDD = NA_real_, Calmar = NA_real_,
      BestYear = NA_real_, WorstYear = NA_real_, PositiveMonths = NA_real_, N = NROW(x)
    ))
  }
  ann <- apply.yearly(x, Return.cumulative)
  cagr <- safe_num(Return.annualized(x, scale = 12))
  vol <- sd(as.numeric(x), na.rm = TRUE) * sqrt(12)
  sharpe <- safe_num(SharpeRatio.annualized(x, scale = 12))
  sortino <- tryCatch(safe_num(SortinoRatio(x, MAR = 0, scale = 12)), error = function(e) NA_real_)
  max_dd <- min(as.numeric(Drawdowns(x)), na.rm = TRUE)
  calmar <- if (is.finite(max_dd) && max_dd < 0) cagr / abs(max_dd) else NA_real_
  tibble(
    Strategy = name,
    CAGR = cagr,
    Volatility = vol,
    Sharpe = sharpe,
    Sortino = sortino,
    MaxDD = max_dd,
    Calmar = calmar,
    BestYear = max(as.numeric(ann), na.rm = TRUE),
    WorstYear = min(as.numeric(ann), na.rm = TRUE),
    PositiveMonths = sum(as.numeric(x) > 0, na.rm = TRUE) / NROW(x),
    N = NROW(x)
  )
}

plot_cum_dd <- function(rets, title, subtitle, out_file) {
  rets <- na.omit(rets)
  cum <- xts(apply(rets, 2, function(x) cumprod(1 + as.numeric(x))), index(rets))
  dd <- xts(sapply(seq_len(NCOL(rets)), function(i) as.numeric(Drawdowns(rets[, i]))), index(rets))
  colnames(cum) <- colnames(rets)
  colnames(dd) <- colnames(rets)

  cum_df <- fortify.zoo(cum) |>
    rename(Date = Index) |>
    pivot_longer(-Date, names_to = "Strategy", values_to = "Cumulative")
  dd_df <- fortify.zoo(dd) |>
    rename(Date = Index) |>
    pivot_longer(-Date, names_to = "Strategy", values_to = "Drawdown")

  end_stats <- map_dfr(colnames(rets), function(nm) {
    m <- metric_row(rets[, nm], nm)
    tibble(Strategy = nm, CAGR = m$CAGR, Sharpe = m$Sharpe,
           MaxDD = m$MaxDD)
  })
  end_cum <- cum_df |>
    group_by(Strategy) |>
    filter(Date == max(Date)) |>
    ungroup() |>
    left_join(end_stats, by = "Strategy") |>
    mutate(Label = sprintf("%s | CAGR %.1f%% | SR %.2f", Strategy, 100 * CAGR, Sharpe))
  end_dd <- dd_df |>
    group_by(Strategy) |>
    filter(Date == max(Date)) |>
    ungroup() |>
    left_join(end_stats, by = "Strategy") |>
    mutate(Label = sprintf("%s | DD %.1f%%", Strategy, 100 * MaxDD))

  p1 <- ggplot(cum_df, aes(Date, Cumulative, color = Strategy)) +
    geom_line(linewidth = 0.8) +
    geom_text_repel(data = end_cum, aes(label = Label), direction = "y",
                    hjust = 0, nudge_x = 80, segment.color = "grey60",
                    size = 3.1, show.legend = FALSE) +
    scale_color_manual(values = series_colors, drop = FALSE) +
    scale_x_date(expand = expansion(mult = c(0.01, 0.22))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Growth of $1",
         color = NULL, caption = "@StockViz") +
    theme_economist() +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

  p2 <- ggplot(dd_df, aes(Date, Drawdown, color = Strategy)) +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
    geom_line(linewidth = 0.7) +
    geom_text_repel(data = end_dd, aes(label = Label), direction = "y",
                    hjust = 0, nudge_x = 80, segment.color = "grey60",
                    size = 3.0, show.legend = FALSE) +
    scale_color_manual(values = series_colors, drop = FALSE) +
    scale_x_date(expand = expansion(mult = c(0.01, 0.22))) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = "Drawdown", color = NULL) +
    theme_economist() +
    theme(legend.position = "none")

  ggsave(out_file, p1 / p2, width = 14, height = 9, dpi = 150)
}

plot_annual_returns <- function(rets, title, out_file) {
  ann <- apply.yearly(rets, Return.cumulative)
  annual_df <- fortify.zoo(ann) |>
    rename(Year = Index) |>
    mutate(Year = format(Year, "%Y")) |>
    pivot_longer(-Year, names_to = "Strategy", values_to = "Return")
  p <- ggplot(annual_df, aes(Year, Return, fill = Strategy)) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.35) +
    geom_col(position = position_dodge(width = 0.82), width = 0.76) +
    scale_fill_manual(values = series_colors, drop = FALSE) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = title, x = NULL, y = "Annual return", fill = NULL, caption = "@StockViz") +
    theme_economist() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold"))
  ggsave(out_file, p, width = 14, height = 7, dpi = 150)
}

# Load factor-index and benchmark prices on their common history.
factor_series <- lapply(factorIndices, function(idx_name) {
  p_df <- sqlQuery(lcon, sprintf(
    "select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s'",
    idx_name, factorStDt))
  if (!is.data.frame(p_df) || NROW(p_df) == 0) stop(sprintf("No data for %s", idx_name))
  one <- sort(xts(as.numeric(p_df[, 2]), as.Date(p_df[, 1])))
  colnames(one) <- idx_name
  one
})
factor_px <- do.call(merge, c(factor_series, all = FALSE))
colnames(factor_px) <- factorIndices

bench_px <- sort(xts(as.numeric(pDf[, 2]), as.Date(pDf[, 1])))
colnames(bench_px) <- indexBench
factor_px <- do.call(merge, c(list(factor_px, bench_px), all = FALSE))
colnames(factor_px) <- c(factorIndices, indexBench)

factor_daily_ret <- do.call(merge, lapply(factorIndices, function(nm) dailyReturn(factor_px[, nm])))
colnames(factor_daily_ret) <- factorIndices
bench_daily_ret <- dailyReturn(factor_px[, indexBench])
colnames(bench_daily_ret) <- indexBench

factor_monthly_ret <- na.omit(apply.monthly(factor_daily_ret, Return.cumulative))
colnames(factor_monthly_ret) <- factorIndices
bench_monthly_ret <- apply.monthly(bench_daily_ret, Return.cumulative)
colnames(bench_monthly_ret) <- indexBench

# Causal rotation: prior completed month's winner is held during the current month.
rotation <- xts(rep(NA_real_, NROW(factor_monthly_ret)), index(factor_monthly_ret))
rotation_name <- rep(NA_character_, NROW(factor_monthly_ret))
for (i in 2:NROW(factor_monthly_ret)) {
  winner <- which.max(as.numeric(factor_monthly_ret[i - 1, ]))
  rotation_name[i] <- factorIndices[winner]
  rotation[i] <- factor_monthly_ret[i, winner]
}
colnames(rotation) <- "Factor Rotation"
rotation_switch <- c(FALSE, rotation_name[-1] != rotation_name[-NROW(rotation_name)])
rotation[rotation_switch] <- rotation[rotation_switch] - drag

factor_equal_weight <- xts(rowMeans(factor_monthly_ret, na.rm = FALSE), index(factor_monthly_ret))
colnames(factor_equal_weight) <- "Factor Equal Weight"

all_returns <- na.omit(merge(rotation, factor_equal_weight, bench_monthly_ret))
colnames(all_returns) <- c("Factor Rotation", "Factor Equal Weight", indexBench)

cat(sprintf("Common monthly sample: %s -> %s (%d observations)\n",
            first(index(all_returns)), last(index(all_returns)), NROW(all_returns)))

windows <- list(
  pre = list(start = NULL, end = "2019-12-31"),
  post = list(start = "2020-05-01", end = NULL),
  full = list(start = NULL, end = NULL)
)

all_metrics <- list()
for (window_name in names(windows)) {
  w <- windows[[window_name]]
  window_rets <- slice_window(all_returns, w$start, w$end)
  window_rets <- na.omit(window_rets)
  if (NROW(window_rets) < 2) next

  metrics <- bind_rows(lapply(colnames(window_rets), function(nm) metric_row(window_rets[, nm], nm)))
  all_metrics[[window_name]] <- metrics
  write_csv(metrics, file.path(reportPath, sprintf("metrics_%s.csv", window_name)))

  metric_table <- metrics |>
    gt() |>
    tab_header(
      title = sprintf("Factor Rotation vs Equal Weight — %s", toupper(window_name)),
      subtitle = sprintf("%s -> %s | monthly returns | @StockViz",
                         first(index(window_rets)), last(index(window_rets)))) |>
    fmt_percent(columns = c(CAGR, Volatility, MaxDD, BestYear, WorstYear, PositiveMonths), decimals = 2) |>
    fmt_number(columns = c(Sharpe, Sortino, Calmar, N), decimals = 2) |>
    cols_label(Volatility = "Vol", MaxDD = "Max DD", PositiveMonths = "Positive months") |>
    tab_source_note(source_note = "@StockViz")
  metric_table <- metric_table |>
    tab_style(style = cell_fill(color = "#e8f1fb"),
              locations = cells_body(rows = which(metrics$Strategy == "Factor Rotation"))) |>
    tab_style(style = cell_fill(color = "#fbe9e7"),
              locations = cells_body(rows = which(metrics$Strategy == "Factor Equal Weight"))) |>
    tab_style(style = cell_fill(color = "#e8f5e9"),
              locations = cells_body(rows = which(metrics$Strategy == indexBench)))

  html_file <- file.path(reportPath, sprintf("metrics_%s.html", window_name))
  png_file <- file.path(reportPath, sprintf("metrics_%s.png", window_name))
  gtsave(metric_table, html_file)
  webshot2::webshot(html_file, png_file, selector = "table.gt_table", expand = c(10, 10, 10, 10))

  subtitle <- paste(vapply(seq_len(nrow(metrics)), function(i) {
    sprintf("%s: CAGR %.1f%% / SR %.2f / MaxDD %.1f%%",
            metrics$Strategy[i], 100 * metrics$CAGR[i], metrics$Sharpe[i], 100 * metrics$MaxDD[i])
  }, character(1)), collapse = " | ")
  plot_cum_dd(window_rets,
              sprintf("Factor Rotation vs Equal-Weight Factor Basket — %s", toupper(window_name)),
              subtitle,
              file.path(reportPath, sprintf("cum_dd_%s.png", window_name)))
  plot_annual_returns(window_rets,
                      sprintf("Factor Rotation vs Equal-Weight Factor Basket — Annual Returns (%s)", toupper(window_name)),
                      file.path(reportPath, sprintf("annual_returns_%s.png", window_name)))
}

annual_returns <- apply.yearly(all_returns, Return.cumulative) |>
  fortify.zoo() |>
  rename(Year = Index)
write_csv(annual_returns, file.path(reportPath, "annual_returns.csv"))
write_csv(bind_rows(all_metrics, .id = "Window"), file.path(reportPath, "metrics_all_windows.csv"))

cat("Generated factor rotation comparison metrics and charts.\n")

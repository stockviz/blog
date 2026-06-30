library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('gt')
library('webshot2')
library('xts')

options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/data/blog/common/plot.common.r")

pdf(NULL)

# ── load cached momentum results ──────────────────────────────────────────────
cache_file <- sprintf("%s/cache.Rdata", reportPath)
if (!file.exists(cache_file))
  stop("cache.Rdata not found — run script-mom.R or script-mom-tech.R first")
load(cache_file)

mom_file    <- sprintf("%s/momentum.Rdata", reportPath)
momt_file   <- sprintf("%s/momentum-trend.Rdata", reportPath)
if (!file.exists(mom_file))
  stop("momentum.Rdata not found — run script-mom.R first")
if (!file.exists(momt_file))
  stop("momentum-trend.Rdata not found — run script-mom-tech.R first")

load(mom_file)
mom_plain <- momReturns
load(momt_file)
mom_trend <- momReturns

cat(sprintf("  plain: %d rows (%s → %s)\n",
            nrow(mom_plain),
            as.character(as.Date(first(index(mom_plain)))),
            as.character(as.Date(last(index(mom_plain))))))
cat(sprintf("  trend: %d rows (%s → %s)\n",
            nrow(mom_trend),
            as.character(as.Date(first(index(mom_trend)))),
            as.character(as.Date(last(index(mom_trend))))))

# ── rename & combine ─────────────────────────────────────────────────────────
names(mom_trend) <- paste0(names(mom_trend), "t")

# all series + both benchmarks
all_combined <- na.omit(merge(mom_plain, mom_trend,
                              bench_monthly_rets, mom_index_monthly))
colnames(all_combined)[(ncol(all_combined) - 1):ncol(all_combined)] <-
  c(benchIndexName, momIndexName)
all_cols   <- colnames(all_combined)
bench_cols <- c(benchIndexName, momIndexName)
strat_cols <- setdiff(all_cols, bench_cols)

cat(sprintf("  combined: %d rows (%s → %s)\n",
            nrow(all_combined),
            as.character(as.Date(first(index(all_combined)))),
            as.character(as.Date(last(index(all_combined))))))

# ── cumulative returns: all combined ─────────────────────────────────────────
sr_all <- paste(round(SharpeRatio.annualized(all_combined), 2), collapse = "/")
Common.PlotCumReturns(all_combined,
  "MSCI Country Index Momentum — Plain vs Trend",
  sprintf("%d/%d/EW | SR: %s", length(valid_countries), positionSize, sr_all),
  sprintf("%s/msci-compare-all.cumret.png", reportPath), NULL)

# ── cumulative returns: per lookback ──────────────────────────────────────────
momLbs <- c(50, 100, 200)
for (momLb in momLbs) {
  p_name <- paste0("MOM_", momLb)
  t_name <- paste0("MOM_", momLb, "t")
  subset <- na.omit(merge(
    all_combined[, p_name], all_combined[, t_name],
    all_combined[, bench_cols]))
  colnames(subset) <- c(sprintf("MOM_%d", momLb), sprintf("MOM_%dt", momLb),
                        bench_cols)

  sr <- sapply(colnames(subset), function(nm)
    round(SharpeRatio.annualized(subset[, nm])[1, 1], 2))
  sr_text <- paste0(colnames(subset), "=", sr, collapse = ", ")

  Common.PlotCumReturns(subset,
    sprintf("MSCI Momentum — %d-day Lookback", momLb),
    sprintf("Plain vs Trend | SR: %s", sr_text),
    sprintf("%s/msci-compare-%d.cumret.png", reportPath, momLb), NULL)
}

# ── cumulative returns: average ───────────────────────────────────────────────
avg_subset <- na.omit(merge(
  all_combined[, "MOM_AVG"], all_combined[, "MOM_AVGt"],
  all_combined[, bench_cols]))
colnames(avg_subset) <- c("MOM_AVG", "MOM_AVGt", bench_cols)
sr_avg <- sapply(colnames(avg_subset), function(nm)
  round(SharpeRatio.annualized(avg_subset[, nm])[1, 1], 2))
sr_text_avg <- paste0(colnames(avg_subset), "=", sr_avg, collapse = ", ")

Common.PlotCumReturns(avg_subset,
  "MSCI Momentum — Average (50/100/200)",
  sprintf("Plain vs Trend | SR: %s", sr_text_avg),
  sprintf("%s/msci-compare-avg.cumret.png", reportPath), NULL)

# ── summary table ─────────────────────────────────────────────────────────────
cat("\n=== Summary Metrics ===\n")
summary_tbl <- tibble()
for (nm in colnames(all_combined)) {
  sr  <- round(SharpeRatio.annualized(all_combined[, nm])[1, 1], 3)
  ret <- round(as.numeric(Return.annualized(all_combined[, nm])), 4)
  dd  <- round(as.numeric(maxDrawdown(all_combined[, nm])), 4)
  cat(sprintf("  %-30s  SR=%.2f  Ret=%.2f%%  DD=%.2f%%\n", nm, sr, ret * 100, dd * 100))
  summary_tbl <- rbind(summary_tbl,
    tibble(Strategy = nm, SR = sr, AnnRet = ret, MaxDD = dd))
}

summary_tbl |>
  gt() |>
  tab_header(
    title = sprintf("MSCI Momentum — Plain vs Trend (%d/%d/EW)", length(valid_countries), positionSize),
    subtitle = sprintf("%s → %s",
      as.character(as.Date(first(index(all_combined)))),
      as.character(as.Date(last(index(all_combined)))))) |>
  fmt_percent(columns = c(AnnRet, MaxDD), decimals = 2) |>
  fmt_number(columns = SR, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) |>
  cols_label(SR = "Sharpe", AnnRet = "Ann.Return", MaxDD = "Max Drawdown") |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"),
            locations = cells_source_notes()) ->
  tbl

# row backgrounds: plain = light green, trend = light blue, benchmarks = light yellow
plain_rows <- which(!grepl("t$", summary_tbl$Strategy) &
                    !summary_tbl$Strategy %in% bench_cols)
trend_rows <- which(grepl("t$", summary_tbl$Strategy))
bench_rows <- which(summary_tbl$Strategy %in% bench_cols)
if (length(plain_rows) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#f0fff0"),
    locations = cells_body(rows = plain_rows))
if (length(trend_rows) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#f0f4ff"),
    locations = cells_body(rows = trend_rows))
if (length(bench_rows) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#fff8e1"),
    locations = cells_body(rows = bench_rows))

for (col in c("AnnRet")) {
  neg_rows <- which(summary_tbl[[col]] < 0)
  if (length(neg_rows) > 0)
    tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
      locations = cells_body(columns = all_of(col), rows = neg_rows))
  green_rows <- which(summary_tbl[[col]] > 0.02)
  if (length(green_rows) > 0)
    tbl <- tbl |> tab_style(style = cell_text(color = "#006400"),
      locations = cells_body(columns = all_of(col), rows = green_rows))
}

tbl |> gtsave(sprintf("%s/msci-compare-summary.html", reportPath))
webshot2::webshot(
  sprintf("%s/msci-compare-summary.html", reportPath),
  sprintf("%s/msci-compare-summary.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── annual returns table ──────────────────────────────────────────────────────
annual_ret <- apply.yearly(all_combined, Return.cumulative)
ar_tbl <- fortify(annual_ret) |>
  rename(Year = Index) |>
  mutate(Year = format(Year, "%Y"))
ar_cols <- names(ar_tbl)[-1]
ar_strat <- setdiff(ar_cols, bench_cols)

tbl <- ar_tbl |>
  gt() |>
  tab_header(
    title = sprintf("MSCI Momentum — Annual Returns (%d/%d/EW)", length(valid_countries), positionSize),
    subtitle = sprintf("%s → %s",
      as.character(as.Date(first(index(all_combined)))),
      as.character(as.Date(last(index(all_combined)))))) |>
  fmt_percent(columns = -Year, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"),
            locations = cells_source_notes())

# column label backgrounds: plain = light green, trend = light blue, benchmarks = light yellow
plain_cols <- grep("^MOM_.*[^t]$", ar_cols, value = TRUE)
trend_cols <- grep("t$", ar_cols, value = TRUE)
if (length(plain_cols) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#f0fff0"),
    locations = cells_column_labels(columns = all_of(plain_cols)))
if (length(trend_cols) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#f0f4ff"),
    locations = cells_column_labels(columns = all_of(trend_cols)))
for (b in bench_cols) {
  if (b %in% ar_cols)
    tbl <- tbl |> tab_style(style = cell_fill(color = "#fff8e1"),
      locations = cells_column_labels(columns = all_of(b)))
}

# red for negative (all columns)
for (c in ar_cols) {
  neg <- which(ar_tbl[[c]] < 0)
  if (length(neg) > 0)
    tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
      locations = cells_body(columns = all_of(c), rows = neg))
}

# strategy columns: box > best bench, red < worst bench, green > best+2%
for (c in ar_strat) {
  bench_best  <- do.call(pmax, ar_tbl[, bench_cols])
  bench_worst <- do.call(pmin, ar_tbl[, bench_cols])

  below <- which(ar_tbl[[c]] < bench_worst)
  if (length(below) > 0)
    tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
      locations = cells_body(columns = all_of(c), rows = below))

  above2 <- which(ar_tbl[[c]] > bench_best + 0.02)
  if (length(above2) > 0)
    tbl <- tbl |> tab_style(style = cell_text(color = "#006400"),
      locations = cells_body(columns = all_of(c), rows = above2))

  beat <- which(ar_tbl[[c]] > bench_best)
  if (length(beat) > 0)
    tbl <- tbl |>
      tab_style(style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
                locations = cells_body(columns = all_of(c), rows = beat))
}

tbl |> gtsave(sprintf("%s/msci-compare-annual.returns.html", reportPath))
webshot2::webshot(
  sprintf("%s/msci-compare-annual.returns.html", reportPath),
  sprintf("%s/msci-compare-annual.returns.table.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── annual drawdowns table ────────────────────────────────────────────────────
annual_dd <- apply.yearly(all_combined, maxDrawdown)
dd_tbl <- fortify(annual_dd) |>
  rename(Year = Index) |>
  mutate(Year = format(Year, "%Y"))

tbl <- dd_tbl |>
  gt() |>
  tab_header(
    title = sprintf("MSCI Momentum — Max Drawdown (%d/%d/EW)", length(valid_countries), positionSize),
    subtitle = sprintf("%s → %s",
      as.character(as.Date(first(index(all_combined)))),
      as.character(as.Date(last(index(all_combined)))))) |>
  fmt_percent(columns = -Year, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"),
            locations = cells_source_notes())

# column label backgrounds
if (length(plain_cols) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#f0fff0"),
    locations = cells_column_labels(columns = all_of(plain_cols)))
if (length(trend_cols) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#f0f4ff"),
    locations = cells_column_labels(columns = all_of(trend_cols)))
for (b in bench_cols) {
  if (b %in% ar_cols)
    tbl <- tbl |> tab_style(style = cell_fill(color = "#fff8e1"),
      locations = cells_column_labels(columns = all_of(b)))
}

# red + bold for drawdowns > 20%
for (c in ar_cols) {
  severe <- which(dd_tbl[[c]] > 0.20)
  if (length(severe) > 0)
    tbl <- tbl |> tab_style(
      style = cell_text(color = "#8B0000", weight = "bold"),
      locations = cells_body(columns = all_of(c), rows = severe))
}

tbl |> gtsave(sprintf("%s/msci-compare-annual.drawdowns.html", reportPath))
webshot2::webshot(
  sprintf("%s/msci-compare-annual.drawdowns.html", reportPath),
  sprintf("%s/msci-compare-annual.drawdowns.table.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

print("Done.")

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('gt')
library('webshot2')
library('xts')

options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

pdf(NULL)

# ── load cached momentum results ──────────────────────────────────────────────
cache_file <- sprintf("%s/cache.Rdata", reportPath)
if (!file.exists(cache_file))
  stop("cache.Rdata not found — run script-mom.R first")
load(cache_file)

mom_file  <- sprintf("%s/momentum.Rdata", reportPath)
momt_file <- sprintf("%s/momentum-trend.Rdata", reportPath)
if (!file.exists(mom_file))
  stop("momentum.Rdata not found — run script-mom.R first")
if (!file.exists(momt_file))
  stop("momentum-trend.Rdata not found — run script-mom-tech.R first")

load(mom_file)
mom_plain <- momReturns
load(momt_file)
mom_trend <- momReturns

cat(sprintf("  plain momentum: %d rows (%s → %s)\n",
            nrow(mom_plain),
            as.character(as.Date(first(index(mom_plain)))),
            as.character(as.Date(last(index(mom_plain))))))
cat(sprintf("  trend momentum: %d rows (%s → %s)\n",
            nrow(mom_trend),
            as.character(as.Date(first(index(mom_trend)))),
            as.character(as.Date(last(index(mom_trend))))))

# ── load MTUM / IMTM ETF prices ───────────────────────────────────────────────
print("loading MTUM / IMTM ETF prices...")
lconUS2 <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockVizUs2", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE
)

etf_monthly <- list()
for (etf in c("MTUM", "IMTM")) {
  px <- sqlQuery(lconUS2, sprintf("
    select c, time_stamp
    from BHAV_EQ_TD
    where symbol = '%s'
    order by time_stamp", etf))

  if (nrow(px) == 0) stop(sprintf("no data for %s in BHAV_EQ_TD", etf))

  px_xts <- xts(px$c, as.Date(px$time_stamp))
  etf_monthly[[etf]] <- monthlyReturn(px_xts)
  cat(sprintf("  %s: %d daily rows, %d monthly returns (%s → %s)\n",
              etf, nrow(px), nrow(etf_monthly[[etf]]),
              as.character(as.Date(first(index(etf_monthly[[etf]])))),
              as.character(as.Date(last(index(etf_monthly[[etf]]))))))
}

odbcClose(lconUS2)

# ── find common start (both ETFs available) ────────────────────────────────────
etf_start <- max(as.Date(first(index(etf_monthly[["MTUM"]]))),
                 as.Date(first(index(etf_monthly[["IMTM"]]))))
cat(sprintf("  ETF common start: %s\n", etf_start))

# ── rename trend columns ──────────────────────────────────────────────────────
names(mom_trend) <- paste0(names(mom_trend), "t")

# ── combine all series on common ETF dates ─────────────────────────────────────
all_combined <- na.omit(merge(
  mom_plain, mom_trend,
  etf_monthly[["MTUM"]], etf_monthly[["IMTM"]]
))
colnames(all_combined)[(ncol(all_combined) - 1):ncol(all_combined)] <- c("MTUM", "IMTM")

# trim to ETF common start
all_combined <- all_combined[paste0(etf_start, "/"), ]
all_cols   <- colnames(all_combined)
bench_cols <- c("MTUM", "IMTM")
strat_cols <- setdiff(all_cols, bench_cols)

cat(sprintf("  combined: %d rows (%s → %s)\n",
            nrow(all_combined),
            as.character(as.Date(first(index(all_combined)))),
            as.character(as.Date(last(index(all_combined))))))

# ── cumulative returns: all combined ──────────────────────────────────────────
sr_all <- paste(round(SharpeRatio.annualized(all_combined), 2), collapse = "/")
Common.PlotCumReturns(all_combined,
  "MSCI Country Momentum vs MTUM / IMTM",
  sprintf("%d/%d/EW | SR: %s", length(valid_countries), positionSize, sr_all),
  sprintf("%s/msci-momentum-vs-etf-all.cumret.png", reportPath), NULL)

# ── cumulative returns: per lookback ──────────────────────────────────────────
momLbs <- c(50, 100, 200)
for (momLb in momLbs) {
  p_name <- paste0("MOM_", momLb)
  t_name <- paste0("MOM_", momLb, "t")
  subset <- na.omit(merge(
    all_combined[, p_name], all_combined[, t_name],
    all_combined[, "MTUM"], all_combined[, "IMTM"]))
  colnames(subset) <- c(sprintf("MOM_%d", momLb), sprintf("MOM_%dt", momLb),
                        "MTUM", "IMTM")

  sr <- sapply(colnames(subset), function(nm)
    round(SharpeRatio.annualized(subset[, nm])[1, 1], 2))
  sr_text <- paste0(colnames(subset), "=", sr, collapse = ", ")

  Common.PlotCumReturns(subset,
    sprintf("MSCI Momentum vs ETFs — %d-day Lookback", momLb),
    sprintf("Plain vs Trend vs MTUM/IMTM | SR: %s", sr_text),
    sprintf("%s/msci-momentum-vs-etf-%d.cumret.png", reportPath, momLb), NULL)
}

# ── cumulative returns: average ───────────────────────────────────────────────
avg_subset <- na.omit(merge(
  all_combined[, "MOM_AVG"], all_combined[, "MOM_AVGt"],
  all_combined[, "MTUM"], all_combined[, "IMTM"]))
colnames(avg_subset) <- c("MOM_AVG", "MOM_AVGt", "MTUM", "IMTM")
sr_avg <- sapply(colnames(avg_subset), function(nm)
  round(SharpeRatio.annualized(avg_subset[, nm])[1, 1], 2))
sr_text_avg <- paste0(colnames(avg_subset), "=", sr_avg, collapse = ", ")

Common.PlotCumReturns(avg_subset,
  "MSCI Momentum vs ETFs — Average (50/100/200)",
  sprintf("Plain vs Trend vs MTUM/IMTM | SR: %s", sr_text_avg),
  sprintf("%s/msci-momentum-vs-etf-avg.cumret.png", reportPath), NULL)

# ── summary table ─────────────────────────────────────────────────────────────
cat("\n=== Summary Metrics ===\n")
summary_tbl <- tibble()
for (nm in colnames(all_combined)) {
  sr  <- round(SharpeRatio.annualized(all_combined[, nm])[1, 1], 3)
  ret <- round(as.numeric(Return.annualized(all_combined[, nm])), 4)
  dd  <- round(as.numeric(maxDrawdown(all_combined[, nm])), 4)
  cat(sprintf("  %-12s  SR=%.2f  Ret=%.2f%%  DD=%.2f%%\n", nm, sr, ret * 100, dd * 100))
  summary_tbl <- rbind(summary_tbl,
    tibble(Strategy = nm, SR = sr, AnnRet = ret, MaxDD = dd))
}

summary_tbl |>
  gt() |>
  tab_header(
    title = sprintf("MSCI Momentum vs MTUM / IMTM (%d/%d/EW)", length(valid_countries), positionSize),
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

# row backgrounds: plain = light green, trend = light blue, ETFs = light yellow
plain_rows <- which(!grepl("t$", summary_tbl$Strategy) &
                    !summary_tbl$Strategy %in% bench_cols)
trend_rows <- which(grepl("t$", summary_tbl$Strategy))
etf_rows   <- which(summary_tbl$Strategy %in% bench_cols)
if (length(plain_rows) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#f0fff0"),
    locations = cells_body(rows = plain_rows))
if (length(trend_rows) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#f0f4ff"),
    locations = cells_body(rows = trend_rows))
if (length(etf_rows) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#fff8e1"),
    locations = cells_body(rows = etf_rows))

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

tbl |> gtsave(sprintf("%s/msci-momentum-vs-etf-summary.html", reportPath))
webshot2::webshot(
  sprintf("%s/msci-momentum-vs-etf-summary.html", reportPath),
  sprintf("%s/msci-momentum-vs-etf-summary.png", reportPath),
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
    title = sprintf("MSCI Momentum vs ETFs — Annual Returns (%d/%d/EW)", length(valid_countries), positionSize),
    subtitle = sprintf("%s → %s",
      as.character(as.Date(first(index(all_combined)))),
      as.character(as.Date(last(index(all_combined)))))) |>
  fmt_percent(columns = -Year, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"),
            locations = cells_source_notes())

# column label backgrounds: plain = light green, trend = light blue, ETFs = light yellow
plain_cols <- grep("^MOM_.*[^t]$", ar_cols, value = TRUE)
trend_cols <- grep("t$", ar_cols, value = TRUE)
if (length(plain_cols) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#f0fff0"),
    locations = cells_column_labels(columns = all_of(plain_cols)))
if (length(trend_cols) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#f0f4ff"),
    locations = cells_column_labels(columns = all_of(trend_cols)))
if (length(bench_cols) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#fff8e1"),
    locations = cells_column_labels(columns = all_of(bench_cols)))

# red for negative (all columns)
for (c in ar_cols) {
  neg <- which(ar_tbl[[c]] < 0)
  if (length(neg) > 0)
    tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
      locations = cells_body(columns = all_of(c), rows = neg))
}

# strategy columns: box if > best benchmark, red if < worst benchmark, green if > best+2%
for (c in ar_strat) {
  bench_best  <- pmax(ar_tbl[["MTUM"]], ar_tbl[["IMTM"]])
  bench_worst <- pmin(ar_tbl[["MTUM"]], ar_tbl[["IMTM"]])

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

tbl |> gtsave(sprintf("%s/msci-momentum-vs-etf-annual.returns.html", reportPath))
webshot2::webshot(
  sprintf("%s/msci-momentum-vs-etf-annual.returns.html", reportPath),
  sprintf("%s/msci-momentum-vs-etf-annual.returns.table.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── annual drawdowns table ────────────────────────────────────────────────────
annual_dd <- apply.yearly(all_combined, maxDrawdown)
dd_tbl <- fortify(annual_dd) |>
  rename(Year = Index) |>
  mutate(Year = format(Year, "%Y"))

tbl <- dd_tbl |>
  gt() |>
  tab_header(
    title = sprintf("MSCI Momentum vs ETFs — Max Drawdown (%d/%d/EW)", length(valid_countries), positionSize),
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
if (length(bench_cols) > 0)
  tbl <- tbl |> tab_style(style = cell_fill(color = "#fff8e1"),
    locations = cells_column_labels(columns = all_of(bench_cols)))

# red + bold for drawdowns > 20%
for (c in ar_cols) {
  severe <- which(dd_tbl[[c]] > 0.20)
  if (length(severe) > 0)
    tbl <- tbl |> tab_style(
      style = cell_text(color = "#8B0000", weight = "bold"),
      locations = cells_body(columns = all_of(c), rows = severe))
}

tbl |> gtsave(sprintf("%s/msci-momentum-vs-etf-annual.drawdowns.html", reportPath))
webshot2::webshot(
  sprintf("%s/msci-momentum-vs-etf-annual.drawdowns.html", reportPath),
  sprintf("%s/msci-momentum-vs-etf-annual.drawdowns.table.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

print("Done.")

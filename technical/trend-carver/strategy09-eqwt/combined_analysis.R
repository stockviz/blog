# Combined Analysis: Strategy Nine Long-Only vs Long-Short Portfolios
# Reads portfolio-returns-lo.csv and portfolio-returns-ls.csv,
# merges time series, generates comparison charts and tables.

library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('gt')
library('webshot2')
library('viridis')
library('ggthemes')
library('xts')

source("/mnt/data/blog/common/plot.common.r")

pdf(NULL)
options("scipen" = 100)

reportPath <- "."

# ── load portfolio returns ────────────────────────────────────────────────────
lo_csv <- sprintf("%s/portfolio-returns-lo.csv", reportPath)
ls_csv <- sprintf("%s/portfolio-returns-ls.csv", reportPath)

if (!file.exists(lo_csv)) stop("portfolio-returns-lo.csv not found — run combined_backtest_lo.R first")
if (!file.exists(ls_csv)) stop("portfolio-returns-ls.csv not found — run combined_backtest_ls.R first")

lo_df <- read.csv(lo_csv, stringsAsFactors = FALSE)
ls_df <- read.csv(ls_csv, stringsAsFactors = FALSE)

cat(sprintf("  LO: %d rows (%s → %s)\n", nrow(lo_df), lo_df$Date[1], lo_df$Date[nrow(lo_df)]))
cat(sprintf("  LS: %d rows (%s → %s)\n", nrow(ls_df), ls_df$Date[1], ls_df$Date[nrow(ls_df)]))

# ── convert to xts ───────────────────────────────────────────────────────────
lo_scaled  <- xts(lo_df$Scaled_LO,  as.Date(lo_df$Date))
lo_binary  <- xts(lo_df$Binary_LO,  as.Date(lo_df$Date))
lo_bh      <- xts(lo_df$BH,         as.Date(lo_df$Date))

ls_scaled  <- xts(ls_df$Scaled_LS,  as.Date(ls_df$Date))
ls_binary  <- xts(ls_df$Binary_LS,  as.Date(ls_df$Date))
ls_bh      <- xts(ls_df$BH,         as.Date(ls_df$Date))

# ── align on common dates ────────────────────────────────────────────────────
cd <- Reduce(intersect, list(index(lo_scaled), index(ls_scaled),
                              index(lo_binary), index(ls_binary),
                              index(lo_bh),     index(ls_bh)))
cat(sprintf("  common dates: %d (%s → %s)\n", length(cd), min(cd), max(cd)))

# slice
lo_s <- lo_scaled[cd]; lo_b <- lo_binary[cd]; lo_bh <- lo_bh[cd]
ls_s <- ls_scaled[cd]; ls_b <- ls_binary[cd]; ls_bh <- ls_bh[cd]

# ── combined returns ─────────────────────────────────────────────────────────
combined_scaled <- na.omit(merge(lo_s, ls_s))
names(combined_scaled) <- c("Scaled LO", "Scaled LS")

combined_binary <- na.omit(merge(lo_b, ls_b))
names(combined_binary) <- c("Binary LO", "Binary LS")

combined_bh <- na.omit(merge(lo_bh, ls_bh))
names(combined_bh) <- c("B&H (LO dates)", "B&H (LS dates)")

# all together for single chart
all_combined <- na.omit(merge(lo_s, lo_b, ls_s, ls_b, lo_bh))
names(all_combined) <- c("Scaled LO", "Binary LO", "Scaled LS", "Binary LS", "B&H")

# ── metrics ──────────────────────────────────────────────────────────────────
calc_metrics <- function(x, label) {
  sr  <- SharpeRatio.annualized(x)[1,1]
  ret <- as.numeric(Return.annualized(x))
  dd  <- as.numeric(maxDrawdown(x))
  cat(sprintf("  %-20s  SR=%.2f  Ret=%.2f%%  DD=%.2f%%\n", label, sr, ret*100, dd*100))
  c(sr, ret, dd)
}

cat("\n=== Metrics ===\n")
m <- list()
m[["Scaled LO"]]  <- calc_metrics(lo_s,   "Scaled LO")
m[["Binary LO"]]  <- calc_metrics(lo_b,   "Binary LO")
m[["Scaled LS"]]  <- calc_metrics(ls_s,   "Scaled LS")
m[["Binary LS"]]  <- calc_metrics(ls_b,   "Binary LS")
m[["B&H"]]        <- calc_metrics(lo_bh,  "B&H")

# ── gt metrics table ─────────────────────────────────────────────────────────
metrics_tbl <- tibble(
  Strategy = names(m),
  SR       = sapply(m, \(x) round(x[1], 3)),
  AnnRet   = sapply(m, \(x) round(x[2], 4)),
  MaxDD    = sapply(m, \(x) round(x[3], 4))
)

metrics_tbl |>
  gt() |>
  tab_header(
    title = "Strategy Nine — Portfolio Comparison",
    subtitle = sprintf("LO vs LS; %s → %s", min(cd), max(cd))
  ) |>
  fmt_percent(columns = c(AnnRet, MaxDD), decimals = 2) |>
  fmt_number(columns = SR, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  cols_label(SR = "Sharpe", AnnRet = "Ann.Return", MaxDD = "Max Drawdown") |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes()) |>
  gtsave(sprintf("%s/comparison-metrics.html", reportPath))

webshot2::webshot(
  sprintf("%s/comparison-metrics.html", reportPath),
  sprintf("%s/comparison-metrics.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── cumulative return charts ─────────────────────────────────────────────────
cat("\n=== Charts ===\n")

# all 5 lines
Common.PlotCumReturns(all_combined, "Strategy Nine Portfolio Comparison",
  sprintf("LO vs LS — Equal-Weight (%s → %s)", min(cd), max(cd)),
  sprintf("%s/comparison-all.cumret.png", reportPath), NULL)

# scaled LO vs scaled LS
Common.PlotCumReturns(combined_scaled, "Scaled: LO vs LS",
  "Strategy Nine Scaled Forecast Portfolios",
  sprintf("%s/comparison-scaled.cumret.png", reportPath), NULL)

# binary LO vs binary LS
Common.PlotCumReturns(combined_binary, "Binary: LO vs LS",
  "Strategy Nine Binary Signal Portfolios",
  sprintf("%s/comparison-binary.cumret.png", reportPath), NULL)

# LO strategies vs B&H
lo_vs_bh <- na.omit(merge(lo_s, lo_b, lo_bh))
names(lo_vs_bh) <- c("Scaled LO", "Binary LO", "B&H")
Common.PlotCumReturns(lo_vs_bh, "Long-Only Strategies vs B&H",
  "Strategy Nine Long-Only Portfolio",
  sprintf("%s/comparison-lo-vs-bh.cumret.png", reportPath), NULL)

# LS strategies vs B&H
ls_vs_bh <- na.omit(merge(ls_s, ls_b, ls_bh))
names(ls_vs_bh) <- c("Scaled LS", "Binary LS", "B&H")
Common.PlotCumReturns(ls_vs_bh, "Long-Short Strategies vs B&H",
  "Strategy Nine Long-Short Portfolio",
  sprintf("%s/comparison-ls-vs-bh.cumret.png", reportPath), NULL)

# ── annual returns table ─────────────────────────────────────────────────────
annual_ret <- apply.yearly(all_combined, Return.cumulative)
ar_tbl <- fortify(annual_ret) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))
cols <- names(ar_tbl)[-1]

tbl <- ar_tbl |>
  gt() |>
  tab_header(
    title = "Strategy Nine — Annual Returns (LO vs LS)",
    subtitle = sprintf("%s → %s", min(cd), max(cd))
  ) |>
  fmt_percent(columns = -Year, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

for (c in cols) {
  neg <- which(ar_tbl[[c]] < 0)
  if (length(neg) > 0) tbl <- tbl |>
    tab_style(style = cell_text(color = "#8B0000"),
              locations = cells_body(columns = all_of(c), rows = neg))
}

tbl |> gtsave(sprintf("%s/comparison-annual.returns.html", reportPath))
webshot2::webshot(
  sprintf("%s/comparison-annual.returns.html", reportPath),
  sprintf("%s/comparison-annual.returns.table.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── annual drawdowns table ───────────────────────────────────────────────────
annual_dd <- apply.yearly(all_combined, maxDrawdown)
dd_tbl <- fortify(annual_dd) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))

tbl <- dd_tbl |>
  gt() |>
  tab_header(
    title = "Strategy Nine — Max Drawdown (LO vs LS)",
    subtitle = sprintf("%s → %s", min(cd), max(cd))
  ) |>
  fmt_percent(columns = -Year, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

for (c in cols) {
  severe <- which(dd_tbl[[c]] > 0.20)
  mild   <- which(dd_tbl[[c]] < 0.10 & dd_tbl[[c]] > 0)
  if (length(severe) > 0) tbl <- tbl |>
    tab_style(style = cell_text(color = "#8B0000", weight = "bold"),
              locations = cells_body(columns = all_of(c), rows = severe))
  if (length(mild) > 0) tbl <- tbl |>
    tab_style(style = cell_text(color = "#006400"),
              locations = cells_body(columns = all_of(c), rows = mild))
}

tbl |> gtsave(sprintf("%s/comparison-annual.drawdowns.html", reportPath))
webshot2::webshot(
  sprintf("%s/comparison-annual.drawdowns.html", reportPath),
  sprintf("%s/comparison-annual.drawdowns.table.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── annual Sharpe table ──────────────────────────────────────────────────────
annual_sr <- apply.yearly(all_combined, SharpeRatio.annualized)
sr_tbl <- fortify(annual_sr) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))

tbl <- sr_tbl |>
  gt() |>
  tab_header(
    title = "Strategy Nine — Sharpe Ratio (LO vs LS)",
    subtitle = sprintf("%s → %s", min(cd), max(cd))
  ) |>
  fmt_number(columns = -Year, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

for (c in cols) {
  neg <- which(sr_tbl[[c]] < 0)
  if (length(neg) > 0) tbl <- tbl |>
    tab_style(style = cell_text(color = "#8B0000"),
              locations = cells_body(columns = all_of(c), rows = neg))
}

tbl |> gtsave(sprintf("%s/comparison-annual.sharpe.html", reportPath))
webshot2::webshot(
  sprintf("%s/comparison-annual.sharpe.html", reportPath),
  sprintf("%s/comparison-annual.sharpe.table.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── annual bar chart ─────────────────────────────────────────────────────────
annual_df <- fortify(annual_ret, melt = TRUE)
names(annual_df) <- c("Year", "Strategy", "Return")
annual_df$Year <- as.numeric(format(annual_df$Year, "%Y"))

p <- ggplot(annual_df, aes(x = factor(Year), y = Return, fill = Strategy)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_viridis_d(end = 0.9) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Strategy Nine — Annual Returns (LO vs LS)",
       subtitle = sprintf("%s → %s", min(cd), max(cd)),
       caption = "@StockViz", x = NULL, y = NULL) +
  theme_economist() +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(sprintf("%s/comparison-annual.returns.png", reportPath),
       plot = p, width = 14, height = 7)

print("Done.")

# Backtest: NIFTY MIDCAP SELECT — SMA(20) vs Str9 LO vs 50-50 Blend
library(xts)
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(gt)
library(webshot2)
library(viridis)
library(ggthemes)
library(RODBC)

source("strategy_nine.R")
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

pdf(NULL)
options(scipen = 100)

reportPath <- "."
drag <- 0.2/100
index_name <- "NIFTY MIDCAP SELECT"
sma_n <- 20

# ---- fetch prices ----
print("connecting...")
lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE
)

pDf <- sqlQuery(lcon, sprintf(
  "select px_close, time_stamp from bhav_index
   where index_name = '%s' and time_stamp >= '2005-04-01'
   order by time_stamp", index_name))
stopifnot(nrow(pDf) > 0)

prices <- xts(pDf$px_close, pDf$time_stamp)
rets <- dailyReturn(prices)

# ---- SMA(20) long-only ----
print("SMA(20)...")
sma_line <- SMA(prices, sma_n)
sig_sma <- ifelse(prices > sma_line, 1, 0)

retL1 <- stats::lag(rets, -1)
cd <- intersect(index(na.omit(sig_sma)), index(retL1))
sa <- sig_sma[cd]
ra <- retL1[cd]

sg <- ifelse(coredata(sa) == 1, coredata(ra), 0)
td <- coredata(sa); td <- td - c(NA, td[-length(td)])
sg <- ifelse(td != 0 & !is.na(td), sg - drag, sg)
sma_xts <- xts(sg, order.by = cd)

# ---- Strategy Nine long-only equal-weight ----
print("Strategy Nine LO...")
sig9 <- strategy_nine_signal(prices, use_cost_screen = FALSE)
cd <- intersect(index(na.omit(sig9)), index(retL1))
sa <- sig9[cd]; ra <- retL1[cd]

sg <- ifelse(coredata(sa) == 1, coredata(ra), 0)
td <- coredata(sa); td <- td - c(NA, td[-length(td)])
sg <- ifelse(td != 0 & !is.na(td), sg - drag, sg)
str9_xts <- xts(sg, order.by = cd)

# ---- 50-50 Blend ----
print("50-50 blend...")
cd <- intersect(index(sma_xts), index(str9_xts))
blend_xts <- 0.5 * sma_xts[cd] + 0.5 * str9_xts[cd]

# ---- B&H ----
bh_xts <- retL1[cd]

# ---- Combine ----
combined <- na.omit(merge(sma_xts[cd], str9_xts[cd], blend_xts, bh_xts))
names(combined) <- c(sprintf("SMA(%d)", sma_n), "Str9 LO", "50-50 Blend", "B&H")
cols <- names(combined)

# ---- Summary metrics ----
cat("\n=== Summary ===\n")
for (c in cols) {
  sr <- SharpeRatio.annualized(combined[, c])[1, 1]
  ret <- as.numeric(Return.annualized(combined[, c])) * 100
  dd <- maxDrawdown(combined[, c]) * 100
  cat(sprintf("  %-15s  SR=%.2f  Ret=%.1f%%  DD=%.1f%%\n", c, sr, ret, dd))
}

# ---- Cumulative returns ----
Common.PlotCumReturns(combined, index_name,
  sprintf("NIFTY MIDCAP SELECT: SMA(%d) vs Str9 LO vs Blend vs B&H", sma_n),
  sprintf("%s/midcap-select.cumret.png", reportPath), NULL)

# ---- Annual returns table ----
annual_ret <- apply.yearly(combined, Return.cumulative)
ar_tbl <- fortify(annual_ret) |> rename(Year = Index) |>
  mutate(Year = format(Year, "%Y"))

tbl <- ar_tbl |> gt() |>
  tab_header(title = sprintf("%s — Annual Returns", index_name),
    subtitle = sprintf("SMA(%d) vs Str9 LO vs 50-50 Blend (□ = Blend beats all by >2%%)", sma_n)) |>
  fmt_percent(columns = -Year, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

for (c in cols) {
  nr <- which(ar_tbl[[c]] < 0)
  if (length(nr) > 0) tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
    locations = cells_body(columns = all_of(c), rows = nr))
}

blend_col <- "50-50 Blend"
other_cols <- setdiff(cols, blend_col)
other_max <- do.call(pmax, c(lapply(other_cols, function(c) ar_tbl[[c]]), na.rm = TRUE))
br <- which(ar_tbl[[blend_col]] > other_max + 0.02)
if (length(br) > 0) tbl <- tbl |> tab_style(
  style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
  locations = cells_body(columns = all_of(blend_col), rows = br))

tbl |> gtsave(sprintf("%s/midcap-select.annual.returns.html", reportPath))
webshot2::webshot(sprintf("%s/midcap-select.annual.returns.html", reportPath),
  sprintf("%s/midcap-select.annual.returns.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ---- Annual drawdowns ----
annual_dd <- apply.yearly(combined, maxDrawdown)
dd_tbl <- fortify(annual_dd) |> rename(Year = Index) |>
  mutate(Year = format(Year, "%Y"))

tbl <- dd_tbl |> gt() |>
  tab_header(title = sprintf("%s — Max Drawdown", index_name),
    subtitle = sprintf("SMA(%d) vs Str9 LO vs 50-50 Blend (□ = Blend less DD than all by >2%%)", sma_n)) |>
  fmt_percent(columns = -Year, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

for (c in cols) {
  sv <- which(dd_tbl[[c]] > 0.20)
  mi <- which(dd_tbl[[c]] < 0.10 & dd_tbl[[c]] > 0)
  if (length(sv) > 0) tbl <- tbl |> tab_style(
    style = cell_text(color = "#8B0000", weight = "bold"),
    locations = cells_body(columns = all_of(c), rows = sv))
  if (length(mi) > 0) tbl <- tbl |> tab_style(
    style = cell_text(color = "#006400"),
    locations = cells_body(columns = all_of(c), rows = mi))
}

other_min <- do.call(pmin, c(lapply(other_cols, function(c) dd_tbl[[c]]), na.rm = TRUE))
br <- which(dd_tbl[[blend_col]] + 0.02 < other_min)
if (length(br) > 0) tbl <- tbl |> tab_style(
  style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
  locations = cells_body(columns = all_of(blend_col), rows = br))

tbl |> gtsave(sprintf("%s/midcap-select.annual.drawdowns.html", reportPath))
webshot2::webshot(sprintf("%s/midcap-select.annual.drawdowns.html", reportPath),
  sprintf("%s/midcap-select.annual.drawdowns.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ---- Annual Sharpe ----
annual_sr <- apply.yearly(combined, SharpeRatio.annualized)
sr_tbl <- fortify(annual_sr) |> rename(Year = Index) |>
  mutate(Year = format(Year, "%Y"))

tbl <- sr_tbl |> gt() |>
  tab_header(title = sprintf("%s — Sharpe Ratio", index_name),
    subtitle = sprintf("SMA(%d) vs Str9 LO vs 50-50 Blend (□ = Blend beats all by >2)", sma_n)) |>
  fmt_number(columns = -Year, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

for (c in cols) {
  nr <- which(sr_tbl[[c]] < 0)
  if (length(nr) > 0) tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
    locations = cells_body(columns = all_of(c), rows = nr))
}

other_max <- do.call(pmax, c(lapply(other_cols, function(c) sr_tbl[[c]]), na.rm = TRUE))
br <- which(sr_tbl[[blend_col]] > other_max + 2)
if (length(br) > 0) tbl <- tbl |> tab_style(
  style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
  locations = cells_body(columns = all_of(blend_col), rows = br))

tbl |> gtsave(sprintf("%s/midcap-select.annual.sharpe.html", reportPath))
webshot2::webshot(sprintf("%s/midcap-select.annual.sharpe.html", reportPath),
  sprintf("%s/midcap-select.annual.sharpe.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

print("Done.")

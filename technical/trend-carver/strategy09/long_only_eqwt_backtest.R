# Backtest: Strategy Nine Long-Only Equal Weight — Binary + Scaled
# Compares the binary (1/0) and scaled forecast (weight ∈ [0,1]) long-only
# variants, both built from equal-weighted EWMAC filters (no cost screen).
# Drag = 0.2% per trade (entry + exit).
#
# Outputs:
#   - summary gt table with Ann.Ret, Sharpe, MaxDD vs B&H
#   - per-index cumulative return charts (scaled vs binary vs B&H)
#   - all-indices combined cumulative charts
#   - annual returns / drawdowns / Sharpe gt tables per index

library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('gt')
library('webshot2')
library('viridis')
library('ggthemes')
library('RODBC')
library('xts')

source("strategy_nine.R")
source("/mnt/data/blog/common/plot.common.r")
source("/mnt/hollandC/StockViz/R/config.r")

pdf(NULL)
options("scipen" = 100)

reportPath <- "."
drag <- 0.2 / 100

indices <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR", "NIFTY BANK TR")

# ── connect to database ──────────────────────────────────────────────────────
print("connecting to norway...")
lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE
)

startDate <- as.Date("2005-04-01")

# ── load prices ─────────────────────────────────────────────────────────────
print("loading prices from database...")
pXts <- NULL
loaded_indices <- c()
for (iName in indices) {
  pDf <- sqlQuery(lcon, sprintf(
    "select px_close, time_stamp from bhav_index
     where index_name = '%s' and time_stamp >= '%s'",
    iName, startDate))
  if (nrow(pDf) == 0) {
    cat(sprintf("  WARNING: no data for %s\n", iName))
    next
  }
  pXts <- merge.xts(pXts, xts(pDf$px_close, pDf$time_stamp))
  loaded_indices <- c(loaded_indices, iName)
}
names(pXts) <- loaded_indices
indices <- loaded_indices

# pre-compute daily returns
dSymXts <- do.call(merge.xts, lapply(indices, \(x) dailyReturn(pXts[, x])))
names(dSymXts) <- indices

# ── run backtests: binary LO + scaled LO, equal weight only ──────────────────
results <- tibble()
binary_lo <- list()
scaled_lo <- list()
bh_strats  <- list()

cat("\n=== LONG-ONLY EQUAL WEIGHT ===\n")

for (iName in indices) {
  cat(sprintf("  %s ...\n", iName))

  sig <- tryCatch({
    strategy_nine_signal(pXts[, iName], use_cost_screen = FALSE)
  }, error = function(e) {
    cat(sprintf("    ERROR: %s\n", e$message))
    NULL
  })

  if (is.null(sig) || all(is.na(sig))) next

  # extract the FDM-adjusted forecast and scale to [-1, +1]
  forecast_xts <- attr(sig, "forecast")
  if (is.null(forecast_xts)) next
  scaled_fc <- forecast_xts / 20

  retL1 <- stats::lag(dSymXts[, iName], -1)
  common_dates <- intersect(intersect(index(na.omit(sig)), index(retL1)),
                            index(na.omit(scaled_fc)))
  sig_aligned    <- sig[common_dates]
  fc_aligned     <- scaled_fc[common_dates]
  retL1_aligned  <- retL1[common_dates]

  # ── binary long-only ──
  strat_bin <- ifelse(coredata(sig_aligned) == 1,
                      coredata(retL1_aligned), 0)
  trd <- coredata(sig_aligned)
  trd <- trd - c(NA, trd[-length(trd)])
  strat_bin <- ifelse(trd != 0 & !is.na(trd), strat_bin - drag, strat_bin)
  bin_xts <- xts(strat_bin, order.by = common_dates)
  binary_lo[[iName]] <- bin_xts

  # ── scaled long-only (weight ∈ [0, 1]) ──
  w_lo <- pmax(pmin(coredata(fc_aligned), 1), 0)
  strat_sclo <- w_lo * coredata(retL1_aligned)
  strat_sclo <- ifelse(trd != 0 & !is.na(trd), strat_sclo - drag, strat_sclo)
  sclo_xts <- xts(strat_sclo, order.by = common_dates)
  scaled_lo[[iName]] <- sclo_xts

  # B&H on same dates
  bh_xts <- retL1_aligned
  bh_strats[[iName]] <- bh_xts

  # ── metrics ──
  bin_sr   <- SharpeRatio.annualized(bin_xts)[1, 1]
  bin_ret  <- as.numeric(Return.annualized(bin_xts))
  bin_dd   <- as.numeric(maxDrawdown(bin_xts))

  sclo_sr  <- SharpeRatio.annualized(sclo_xts)[1, 1]
  sclo_ret <- as.numeric(Return.annualized(sclo_xts))
  sclo_dd  <- as.numeric(maxDrawdown(sclo_xts))

  bh_sr   <- SharpeRatio.annualized(bh_xts)[1, 1]
  bh_ret  <- as.numeric(Return.annualized(bh_xts))
  bh_dd   <- as.numeric(maxDrawdown(bh_xts))

  results <- rbind(results, tibble(
    Index       = iName,
    Type        = "binary long-only",
    Ret         = round(bin_ret, 4),
    Sharpe      = round(bin_sr, 3),
    MaxDD       = round(bin_dd, 4),
    BH_Ret      = round(bh_ret, 4),
    BH_Sharpe   = round(bh_sr, 3),
    BH_MaxDD    = round(bh_dd, 4)
  ))
  results <- rbind(results, tibble(
    Index       = iName,
    Type        = "scaled long-only",
    Ret         = round(sclo_ret, 4),
    Sharpe      = round(sclo_sr, 3),
    MaxDD       = round(sclo_dd, 4),
    BH_Ret      = round(bh_ret, 4),
    BH_Sharpe   = round(bh_sr, 3),
    BH_MaxDD    = round(bh_dd, 4)
  ))
}

# ── console summary ──────────────────────────────────────────────────────────
cat("\n=== Summary Metrics ===\n")
print(results, n = Inf)

# ── gt summary table ─────────────────────────────────────────────────────────
results |>
  gt(groupname_col = "Index") |>
  tab_header(
    title = "Strategy Nine — Long-Only Equal Weight",
    subtitle = sprintf("2005-04-01 → %s; drag = %.1f%%",
                       format(last(index(pXts)), "%Y-%m-%d"), drag * 100)
  ) |>
  tab_spanner(label = "Strategy Nine", columns = c(Ret, Sharpe, MaxDD)) |>
  tab_spanner(label = "Buy & Hold", columns = c(BH_Ret, BH_Sharpe, BH_MaxDD)) |>
  fmt_percent(columns = c(Ret, BH_Ret, MaxDD, BH_MaxDD), decimals = 2) |>
  fmt_number(columns = c(Sharpe, BH_Sharpe), decimals = 2) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  # colour-code by type
  tab_style(
    style = list(cell_fill(color = "#f0fff0"), cell_text(weight = "bold")),
    locations = cells_body(rows = Type == "scaled long-only")
  ) |>
  cols_label(
    Ret     = "Ann.Ret",  Sharpe  = "Sharpe",  MaxDD   = "MaxDD",
    BH_Ret  = "Ann.Ret",  BH_Sharpe = "Sharpe", BH_MaxDD = "MaxDD"
  ) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_source_notes()
  ) |>
  gtsave(sprintf("%s/lo-eqwt-summary.html", reportPath))

webshot2::webshot(
  sprintf("%s/lo-eqwt-summary.html", reportPath),
  sprintf("%s/lo-eqwt-summary.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10)
)

# ── per-index cumulative return charts ───────────────────────────────────────
cat("\n=== Cumulative Return Charts ===\n")
for (iName in indices) {
  if (!iName %in% names(binary_lo)) next

  combined <- na.omit(merge(scaled_lo[[iName]],
                            binary_lo[[iName]],
                            bh_strats[[iName]]))
  names(combined) <- c("Scaled LO", "Binary LO", "B&H")

  Common.PlotCumReturns(combined, iName,
    "Strategy Nine: Scaled vs Binary Long-Only (equal weight)",
    sprintf("%s/%s.lo-eqwt.cumret.png", reportPath, iName), NULL)
}

# ── combined multi-index charts ──────────────────────────────────────────────
# Binary LO — all indices together
cat("\n=== Combined Binary Long-Only ===\n")
bin_combined <- do.call(merge.xts, lapply(indices, \(iName) binary_lo[[iName]]))
names(bin_combined) <- indices
Common.PlotCumReturns(bin_combined, "All Indices",
  "Strategy Nine Binary Long-Only — Equal Weight (all indices)",
  sprintf("%s/all.lo-eqwt.binary.cumret.png", reportPath), NULL)

# Scaled LO — all indices together
cat("=== Combined Scaled Long-Only ===\n")
sclo_combined <- do.call(merge.xts, lapply(indices, \(iName) scaled_lo[[iName]]))
names(sclo_combined) <- indices
Common.PlotCumReturns(sclo_combined, "All Indices",
  "Strategy Nine Scaled Long-Only — Equal Weight (all indices)",
  sprintf("%s/all.lo-eqwt.scaled.cumret.png", reportPath), NULL)

# ── per-index annual tables ──────────────────────────────────────────────────
cat("\n=== Annual Tables ===\n")
for (iName in indices) {
  if (!iName %in% names(scaled_lo)) next

  combined <- na.omit(merge(scaled_lo[[iName]],
                            binary_lo[[iName]],
                            bh_strats[[iName]]))
  names(combined) <- c("Scaled LO", "Binary LO", "B&H")
  cols <- names(combined)

  # ─ annual returns bar chart ─
  annual_ret <- apply.yearly(combined, Return.cumulative)
  annual_df <- fortify(annual_ret, melt = TRUE)
  names(annual_df) <- c("Year", "Scenario", "Return")
  annual_df$Year <- as.numeric(format(annual_df$Year, "%Y"))

  p <- ggplot(annual_df, aes(x = factor(Year), y = Return, fill = Scenario)) +
    geom_col(position = "dodge", width = 0.8) +
    scale_fill_viridis_d(end = 0.9) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = iName,
         subtitle = "Annual Returns — Scaled vs Binary Long-Only",
         caption = "@StockViz",
         x = NULL, y = NULL) +
    theme_economist() +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 1),
          axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(sprintf("%s/%s.lo-eqwt.annual.returns.png", reportPath, iName),
         plot = p, width = 12, height = 6)

  # ─ annual returns gt table ─
  ar_tbl <- fortify(annual_ret) |>
    rename(Year = Index) |>
    mutate(Year = format(Year, "%Y"))
  scenario_cols <- setdiff(cols, "B&H")

  tbl <- ar_tbl |>
    gt() |>
    tab_header(
      title = sprintf("%s — Annual Returns", iName),
      subtitle = "Scaled LO vs Binary LO vs B&H (□ = beats B&H by >2%)"
    ) |>
    fmt_percent(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"),
              locations = cells_source_notes())

  # red for negatives
  for (c in cols) {
    neg_rows <- which(ar_tbl[[c]] < 0)
    if (length(neg_rows) > 0)
      tbl <- tbl |>
        tab_style(style = cell_text(color = "#8B0000"),
                  locations = cells_body(columns = all_of(c), rows = neg_rows))
  }

  # border cells that beat B&H by >2%
  for (c in scenario_cols) {
    beat_rows <- which(ar_tbl[[c]] - ar_tbl[["B&H"]] > 0.02)
    if (length(beat_rows) > 0)
      tbl <- tbl |>
        tab_style(
          style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
          locations = cells_body(columns = all_of(c), rows = beat_rows))
  }

  tbl |>
    gtsave(sprintf("%s/%s.lo-eqwt.annual.returns.html", reportPath, iName))
  webshot2::webshot(
    sprintf("%s/%s.lo-eqwt.annual.returns.html", reportPath, iName),
    sprintf("%s/%s.lo-eqwt.annual.returns.table.png", reportPath, iName),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # ─ annual drawdowns gt table ─
  annual_dd <- apply.yearly(combined, maxDrawdown)
  dd_tbl <- fortify(annual_dd) |>
    rename(Year = Index) |>
    mutate(Year = format(Year, "%Y"))

  tbl <- dd_tbl |>
    gt() |>
    tab_header(
      title = sprintf("%s — Max Drawdown", iName),
      subtitle = "Scaled LO vs Binary LO vs B&H (□ = less DD than B&H by >2%)"
    ) |>
    fmt_percent(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"),
              locations = cells_source_notes())

  for (c in cols) {
    severe <- which(dd_tbl[[c]] > 0.20)
    mild   <- which(dd_tbl[[c]] < 0.10 & dd_tbl[[c]] > 0)
    if (length(severe) > 0)
      tbl <- tbl |>
        tab_style(
          style = cell_text(color = "#8B0000", weight = "bold"),
          locations = cells_body(columns = all_of(c), rows = severe))
    if (length(mild) > 0)
      tbl <- tbl |>
        tab_style(
          style = cell_text(color = "#006400"),
          locations = cells_body(columns = all_of(c), rows = mild))
  }

  for (c in scenario_cols) {
    beat_rows <- which(dd_tbl[["B&H"]] - dd_tbl[[c]] > 0.02)
    if (length(beat_rows) > 0)
      tbl <- tbl |>
        tab_style(
          style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
          locations = cells_body(columns = all_of(c), rows = beat_rows))
  }

  tbl |>
    gtsave(sprintf("%s/%s.lo-eqwt.annual.drawdowns.html", reportPath, iName))
  webshot2::webshot(
    sprintf("%s/%s.lo-eqwt.annual.drawdowns.html", reportPath, iName),
    sprintf("%s/%s.lo-eqwt.annual.drawdowns.table.png", reportPath, iName),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # ─ annual Sharpe gt table ─
  annual_sr <- apply.yearly(combined, SharpeRatio.annualized)
  sr_tbl <- fortify(annual_sr) |>
    rename(Year = Index) |>
    mutate(Year = format(Year, "%Y"))

  tbl <- sr_tbl |>
    gt() |>
    tab_header(
      title = sprintf("%s — Sharpe Ratio", iName),
      subtitle = "Scaled LO vs Binary LO vs B&H (□ = beats B&H by >2)"
    ) |>
    fmt_number(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"),
              locations = cells_source_notes())

  for (c in cols) {
    neg_rows <- which(sr_tbl[[c]] < 0)
    if (length(neg_rows) > 0)
      tbl <- tbl |>
        tab_style(style = cell_text(color = "#8B0000"),
                  locations = cells_body(columns = all_of(c), rows = neg_rows))
  }

  for (c in scenario_cols) {
    beat_rows <- which(sr_tbl[[c]] - sr_tbl[["B&H"]] > 2)
    if (length(beat_rows) > 0)
      tbl <- tbl |>
        tab_style(
          style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
          locations = cells_body(columns = all_of(c), rows = beat_rows))
  }

  tbl |>
    gtsave(sprintf("%s/%s.lo-eqwt.annual.sharpe.html", reportPath, iName))
  webshot2::webshot(
    sprintf("%s/%s.lo-eqwt.annual.sharpe.html", reportPath, iName),
    sprintf("%s/%s.lo-eqwt.annual.sharpe.table.png", reportPath, iName),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  cat(sprintf("  %s done.\n", iName))
}

print("Done.")

# Backtest Strategy Nine (Robert Carver) on Nifty indices
# Compares use_cost_screen = FALSE vs TRUE (cost_per_trade_sr = 0.0088)
#
# Signal: 1 = long, 0 = short — we interpret as long-only.
# Drag: 0.2% per trade (entry + exit), same as the changepoint script.

library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('gt')
library('webshot2')
library('RODBC')
library('viridis')
library('ggthemes')

source("strategy_nine.R")
source("/mnt/data/blog/common/plot.common.r")
source("/mnt/hollandC/StockViz/R/config.r")

pdf(NULL)
options("scipen" = 100)

reportPath <- "."
drag <- 0.2/100

indices <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR", "NIFTY BANK TR")

# ---- Connect to DB and load prices ----
print("connecting to norway...")
lcon <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    ldbserver, ldbname, ldbuser, ldbpassword
  ),
  case = "nochange", believeNRows = TRUE
)

startDate <- as.Date("2005-04-01")

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
indices <- loaded_indices  # only process what we have data for

# ---- Pre-compute daily returns ----
dSymXts <- do.call(merge.xts, lapply(indices, \(x) dailyReturn(pXts[, x])))
names(dSymXts) <- indices

# ---- Run backtests ----
results <- tibble()
all_strats <- list()
bh_strats <- list()

for (use_cs in c(FALSE, TRUE)) {
  cs_label <- if (use_cs) "cost_screen" else "equal_weight"
  cat(sprintf("\n=== %s ===\n", cs_label))

  for (iName in indices) {
    cat(sprintf("  %s ...\n", iName))

    sig <- tryCatch({
      if (use_cs) {
        strategy_nine_signal(pXts[, iName],
                             use_cost_screen = TRUE,
                             cost_per_trade_sr = 0.0088)
      } else {
        strategy_nine_signal(pXts[, iName],
                             use_cost_screen = FALSE)
      }
    }, error = function(e) {
      cat(sprintf("    ERROR: %s\n", e$message))
      NULL
    })

    if (is.null(sig) || all(is.na(sig))) next

    # Align signal with returns
    retL1 <- stats::lag(dSymXts[, iName], -1)
    common_dates <- intersect(index(na.omit(sig)), index(retL1))
    sig_aligned <- sig[common_dates]
    retL1_aligned <- retL1[common_dates]

    # Strategy: long when signal=1, stay out when signal=0
    strat_gross <- ifelse(coredata(sig_aligned) == 1,
                          coredata(retL1_aligned), 0)

    # Apply drag on trades
    trd <- coredata(sig_aligned)
    trd <- trd - c(NA, trd[-length(trd)])  # trade indicator: 1 on entry, -1 on exit
    strat_net <- ifelse(trd != 0 & !is.na(trd), strat_gross - drag, strat_gross)

    strat_xts <- xts(strat_net, order.by = common_dates)

    # Store for charts
    key <- paste(iName, cs_label, sep = "_")
    all_strats[[key]] <- strat_xts
    bh_strats[[iName]] <- retL1_aligned

    # Buy & hold on same dates
    bh_xts <- retL1_aligned

    sharpe <- SharpeRatio.annualized(strat_xts)
    ann_ret <- Return.annualized(strat_xts)
    bh_sharpe <- SharpeRatio.annualized(bh_xts)
    bh_ret <- Return.annualized(bh_xts)

    results <- rbind(results, tibble(
      Index       = iName,
      Method      = cs_label,
      Type        = "long-only",
      Ret         = round(as.numeric(ann_ret), 4),
      Sharpe      = round(sharpe[1, 1], 3),
      BH_Ret      = round(as.numeric(bh_ret), 4),
      BH_Sharpe   = round(bh_sharpe[1, 1], 3)
    ))
  }
}

# ---- Long/short version ----
cat("\n=== LONG/SHORT ===\n")
ls_strats <- list()

for (use_cs in c(FALSE, TRUE)) {
  cs_label <- if (use_cs) "cost_screen" else "equal_weight"
  cat(sprintf("\n--- %s ---\n", cs_label))

  for (iName in indices) {
    cat(sprintf("  %s ...\n", iName))

    sig <- tryCatch({
      if (use_cs) {
        strategy_nine_signal(pXts[, iName],
                             use_cost_screen = TRUE,
                             cost_per_trade_sr = 0.0088)
      } else {
        strategy_nine_signal(pXts[, iName],
                             use_cost_screen = FALSE)
      }
    }, error = function(e) {
      cat(sprintf("    ERROR: %s\n", e$message))
      NULL
    })

    if (is.null(sig) || all(is.na(sig))) next

    retL1 <- stats::lag(dSymXts[, iName], -1)
    common_dates <- intersect(index(na.omit(sig)), index(retL1))
    sig_aligned <- sig[common_dates]
    retL1_aligned <- retL1[common_dates]

    # Long/short: 1 = long (+ret), 0 = short (-ret)
    strat_gross <- ifelse(coredata(sig_aligned) == 1,
                          coredata(retL1_aligned),
                          -coredata(retL1_aligned))

    # Apply drag on trades
    trd <- coredata(sig_aligned)
    trd <- trd - c(NA, trd[-length(trd)])
    strat_net <- ifelse(trd != 0 & !is.na(trd), strat_gross - drag, strat_gross)

    strat_xts <- xts(strat_net, order.by = common_dates)

    key <- paste(iName, cs_label, "ls", sep = "_")
    ls_strats[[key]] <- strat_xts

    sharpe <- SharpeRatio.annualized(strat_xts)
    ann_ret <- Return.annualized(strat_xts)

    # B&H on same dates
    bh_xts <- retL1_aligned
    bh_sharpe <- SharpeRatio.annualized(bh_xts)
    bh_ret <- Return.annualized(bh_xts)

    results <- rbind(results, tibble(
      Index       = iName,
      Method      = cs_label,
      Type        = "long/short",
      Ret         = round(as.numeric(ann_ret), 4),
      Sharpe      = round(sharpe[1, 1], 3),
      BH_Ret      = round(as.numeric(bh_ret), 4),
      BH_Sharpe   = round(bh_sharpe[1, 1], 3)
    ))
  }
}

# ---- Print + gt table ----
print(results)

results |>
  gt(groupname_col = "Type") |>
  tab_header(
    title = "Strategy Nine Backtest — Nifty Indices",
    subtitle = sprintf("2005-04-01 → %s; drag = %.1f%%; cost_per_trade_sr = 0.0088",
                       format(last(index(pXts)), "%Y-%m-%d"), drag * 100)
  ) |>
  tab_spanner(label = "Strategy Nine", columns = c(Ret, Sharpe)) |>
  tab_spanner(label = "Buy & Hold", columns = c(BH_Ret, BH_Sharpe)) |>
  fmt_percent(columns = c(Ret, BH_Ret), decimals = 2) |>
  fmt_number(columns = c(Sharpe, BH_Sharpe), decimals = 2) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  # color-code by method
  tab_style(
    style = cell_fill(color = "#f0fff0"),
    locations = cells_body(rows = Method == "equal_weight")
  ) |>
  tab_style(
    style = cell_fill(color = "#fff3e0"),
    locations = cells_body(rows = Method == "cost_screen")
  ) |>
  cols_label(
    Ret = "Ann.Ret", Sharpe = "Sharpe",
    BH_Ret = "Ann.Ret", BH_Sharpe = "Sharpe"
  ) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_source_notes()
  ) |>
  gtsave(sprintf("%s/strategy09-results.html", reportPath))

webshot2::webshot(
  sprintf("%s/strategy09-results.html", reportPath),
  sprintf("%s/strategy09-results.png", reportPath),
  selector = "table.gt_table",
  expand = c(10, 10, 10, 10)
)

# ---- Cumulative return charts: long-only ----
for (iName in indices) {
  ew_key <- paste(iName, "equal_weight", sep = "_")
  cs_key <- paste(iName, "cost_screen", sep = "_")

  if (!ew_key %in% names(all_strats)) next

  to_plot <- na.omit(merge(
    all_strats[[ew_key]],
    all_strats[[cs_key]],
    bh_strats[[iName]]
  ))
  names(to_plot) <- c("equal_weight", "cost_screen", "B&H")

  Common.PlotCumReturns(to_plot, iName,
    "Strategy Nine long-only: equal weight vs cost screen vs B&H",
    sprintf("%s/%s.strategy09.cumret.png", reportPath, iName), NULL)
}

# ---- Cumulative return charts: long/short ----
for (iName in indices) {
  ew_key <- paste(iName, "equal_weight_ls", sep = "_")
  cs_key <- paste(iName, "cost_screen_ls", sep = "_")

  if (!ew_key %in% names(ls_strats)) next

  to_plot <- na.omit(merge(
    ls_strats[[ew_key]],
    ls_strats[[cs_key]],
    bh_strats[[iName]]
  ))
  names(to_plot) <- c("equal_weight", "cost_screen", "B&H")

  Common.PlotCumReturns(to_plot, iName,
    "Strategy Nine long/short: equal weight vs cost screen vs B&H",
    sprintf("%s/%s.strategy09.ls.cumret.png", reportPath, iName), NULL)
}

# ---- Comparison charts: long-only vs long/short vs B&H ----
for (iName in indices) {
  lf_key <- paste(iName, "equal_weight", sep = "_")
  ls_key <- paste(iName, "equal_weight_ls", sep = "_")

  if (!lf_key %in% names(all_strats)) next

  to_plot <- na.omit(merge(
    all_strats[[lf_key]],
    ls_strats[[ls_key]],
    bh_strats[[iName]]
  ))
  names(to_plot) <- c("long-only", "long/short", "B&H")

  Common.PlotCumReturns(to_plot, iName,
    "Strategy Nine: long-only vs long/short vs B&H (equal weight)",
    sprintf("%s/%s.strategy09.compare.cumret.png", reportPath, iName), NULL)
}

# ---- Annual return bar plots ----
for (iName in indices) {
  lf_ew <- paste(iName, "equal_weight", sep = "_")
  lf_cs <- paste(iName, "cost_screen", sep = "_")
  ls_ew <- paste(iName, "equal_weight_ls", sep = "_")
  ls_cs <- paste(iName, "cost_screen_ls", sep = "_")

  combined <- na.omit(merge(
    all_strats[[lf_ew]],
    all_strats[[lf_cs]],
    ls_strats[[ls_ew]],
    ls_strats[[ls_cs]],
    bh_strats[[iName]]
  ))
  names(combined) <- c("long-only eq.wt", "long-only cost", "long/short eq.wt", "long/short cost", "B&H")

  annual <- apply.yearly(combined, Return.cumulative)
  annual_df <- fortify(annual, melt = TRUE)
  names(annual_df) <- c("Year", "Scenario", "Return")
  annual_df$Year <- as.numeric(format(annual_df$Year, "%Y"))

  p <- ggplot(annual_df, aes(x = factor(Year), y = Return, fill = Scenario)) +
    geom_col(position = "dodge", width = 0.8) +
    scale_fill_viridis_d(end = 0.9) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = iName,
         subtitle = "Annual Returns by Scenario",
         caption = "@StockViz",
         x = NULL, y = NULL) +
    theme_economist() +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 1),
          axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(sprintf("%s/%s.annual.returns.png", reportPath, iName),
         plot = p, width = 12, height = 6, dpi = 200)

  # gt table of annual returns
  annual_tbl <- fortify(annual) |>
    rename(Year = Index) |>
    mutate(Year = format(Year, "%Y"))

  # identify scenario columns (all except Year and B&H)
  scenario_cols <- setdiff(names(annual_tbl), c("Year", "B&H"))

  tbl <- annual_tbl |>
    gt() |>
    tab_header(
      title = sprintf("%s — Annual Returns", iName),
      subtitle = "Annual Returns by Scenario (□ = beats B&H by >2%)"
    ) |>
    fmt_percent(columns = -Year, decimals = 2) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(
      style = cell_text(align = "right"),
      locations = cells_source_notes()
    )

  # red text for negative values
  for (col in scenario_cols) {
    neg_rows <- which(annual_tbl[[col]] < 0)
    if (length(neg_rows) > 0) {
      tbl <- tbl |>
        tab_style(
          style = cell_text(color = "#8B0000"),
          locations = cells_body(columns = all_of(col), rows = neg_rows)
        )
    }
  }
  # B&H negatives too
  neg_rows <- which(annual_tbl[["B&H"]] < 0)
  if (length(neg_rows) > 0) {
    tbl <- tbl |>
      tab_style(
        style = cell_text(color = "red"),
        locations = cells_body(columns = "B&H", rows = neg_rows)
      )
  }

  # border around cells that beat B&H by >2%
  for (col in scenario_cols) {
    beat_rows <- which(annual_tbl[[col]] - annual_tbl[["B&H"]] > 0.02)
    if (length(beat_rows) > 0) {
      tbl <- tbl |>
        tab_style(
          style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
          locations = cells_body(columns = all_of(col), rows = beat_rows)
        )
    }
  }

  tbl |>
    gtsave(sprintf("%s/%s.annual.returns.html", reportPath, iName))

  webshot2::webshot(
    sprintf("%s/%s.annual.returns.html", reportPath, iName),
    sprintf("%s/%s.annual.returns.table.png", reportPath, iName),
    selector = "table.gt_table",
    expand = c(10, 10, 10, 10)
  )

  # ---- Annual max drawdown table ----
  annual_dd <- apply.yearly(combined, maxDrawdown)
  dd_tbl <- fortify(annual_dd) |>
    rename(Year = Index) |>
    mutate(Year = format(Year, "%Y"))

  tbl_dd <- dd_tbl |>
    gt() |>
    tab_header(
      title = sprintf("%s — Max Drawdown", iName),
      subtitle = "Annual Max Drawdown by Scenario (□ = less drawdown than B&H by >2%)"
    ) |>
    fmt_percent(columns = -Year, decimals = 2) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(
      style = cell_text(align = "right"),
      locations = cells_source_notes()
    )

  # color-code drawdowns: >20% = red, <10% = green
  for (col in names(dd_tbl)[-1]) {
    severe <- which(dd_tbl[[col]] > 0.20)
    mild   <- which(dd_tbl[[col]] < 0.10 & dd_tbl[[col]] > 0)
    if (length(severe) > 0) {
      tbl_dd <- tbl_dd |>
        tab_style(
          style = cell_text(color = "#8B0000", weight = "bold"),
          locations = cells_body(columns = all_of(col), rows = severe)
        )
    }
    if (length(mild) > 0) {
      tbl_dd <- tbl_dd |>
        tab_style(
          style = cell_text(color = "#006400"),
          locations = cells_body(columns = all_of(col), rows = mild)
        )
    }
  }

  # border around cells with LESS drawdown than B&H by >2%
  for (col in scenario_cols) {
    beat_rows <- which(dd_tbl[["B&H"]] - dd_tbl[[col]] > 0.02)
    if (length(beat_rows) > 0) {
      tbl_dd <- tbl_dd |>
        tab_style(
          style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
          locations = cells_body(columns = all_of(col), rows = beat_rows)
        )
    }
  }

  tbl_dd |>
    gtsave(sprintf("%s/%s.annual.drawdowns.html", reportPath, iName))

  webshot2::webshot(
    sprintf("%s/%s.annual.drawdowns.html", reportPath, iName),
    sprintf("%s/%s.annual.drawdowns.table.png", reportPath, iName),
    selector = "table.gt_table",
    expand = c(10, 10, 10, 10)
  )

  # ---- Annual Sharpe ratio table ----
  annual_sr <- apply.yearly(combined, SharpeRatio.annualized)
  sr_tbl <- fortify(annual_sr) |>
    rename(Year = Index) |>
    mutate(Year = format(Year, "%Y"))

  tbl_sr <- sr_tbl |>
    gt() |>
    tab_header(
      title = sprintf("%s — Sharpe Ratio", iName),
      subtitle = "Annual Sharpe Ratio by Scenario (□ = beats B&H by >2)"
    ) |>
    fmt_number(columns = -Year, decimals = 2) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(
      style = cell_text(align = "right"),
      locations = cells_source_notes()
    )

  # red for negative Sharpe
  for (col in names(sr_tbl)[-1]) {
    neg_rows <- which(sr_tbl[[col]] < 0)
    if (length(neg_rows) > 0) {
      tbl_sr <- tbl_sr |>
        tab_style(
          style = cell_text(color = "#8B0000"),
          locations = cells_body(columns = all_of(col), rows = neg_rows)
        )
    }
  }

  # border around cells that beat B&H by >2
  for (col in scenario_cols) {
    beat_rows <- which(sr_tbl[[col]] - sr_tbl[["B&H"]] > 2)
    if (length(beat_rows) > 0) {
      tbl_sr <- tbl_sr |>
        tab_style(
          style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
          locations = cells_body(columns = all_of(col), rows = beat_rows)
        )
    }
  }

  tbl_sr |>
    gtsave(sprintf("%s/%s.annual.sharpe.html", reportPath, iName))

  webshot2::webshot(
    sprintf("%s/%s.annual.sharpe.html", reportPath, iName),
    sprintf("%s/%s.annual.sharpe.table.png", reportPath, iName),
    selector = "table.gt_table",
    expand = c(10, 10, 10, 10)
  )
}

# =========================================================================
# SMA comparison: 50-day for NIFTY 50 / SMALLCAP, 20-day for MIDCAP / BANK
# Long/short interpretation, compared to strategy nine long/short equal-weight
# =========================================================================
print("=== SMA COMPARISON ===")
sma_periods <- c(
  "NIFTY 50 TR" = 50,
  "NIFTY MIDCAP 150 TR" = 20,
  "NIFTY SMALLCAP 250 TR" = 50,
  "NIFTY BANK TR" = 20
)

sma_strats <- list()

for (iName in indices) {
  sma_n <- sma_periods[[iName]]
  cat(sprintf("  %s SMA(%d) ...\n", iName, sma_n))

  sma_line <- SMA(pXts[, iName], sma_n)
  sig <- ifelse(pXts[, iName] > sma_line, 1, 0)

  retL1 <- stats::lag(dSymXts[, iName], -1)
  common_dates <- intersect(index(na.omit(sig)), index(retL1))
  sig_aligned <- sig[common_dates]
  retL1_aligned <- retL1[common_dates]

  # long/short: 1 = long, 0 = short
  strat_gross <- ifelse(coredata(sig_aligned) == 1,
                        coredata(retL1_aligned),
                        -coredata(retL1_aligned))
  trd <- coredata(sig_aligned)
  trd <- trd - c(NA, trd[-length(trd)])
  strat_net <- ifelse(trd != 0 & !is.na(trd), strat_gross - drag, strat_gross)

  sma_strats[[iName]] <- xts(strat_net, order.by = common_dates)

  # comparison: SMA vs strategy nine long/short equal-weight
  ls_key <- paste(iName, "equal_weight_ls", sep = "_")
  if (!ls_key %in% names(ls_strats)) next

  combined <- na.omit(merge(sma_strats[[iName]], ls_strats[[ls_key]]))
  names(combined) <- c(sprintf("SMA(%d)", sma_n), "Str9 LS eq.wt")

  # cumulative returns
  Common.PlotCumReturns(combined, iName,
    sprintf("SMA(%d) vs Strategy Nine long/short eq.wt", sma_n),
    sprintf("%s/%s.sma.compare.cumret.png", reportPath, iName), NULL)

  # annual returns table
  annual_ret <- apply.yearly(combined, Return.cumulative)
  ar_tbl <- fortify(annual_ret) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))
  sma_col <- names(ar_tbl)[2]
  str9_col <- names(ar_tbl)[3]

  tbl_ar <- ar_tbl |>
    gt() |>
    tab_header(
      title = sprintf("%s — Annual Returns", iName),
      subtitle = sprintf("SMA(%d) vs Strategy Nine LS eq.wt (□ = beats Str9 by >2%%)", sma_n)
    ) |>
    fmt_percent(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

  # color negatives in both columns
  for (col in names(ar_tbl)[-1]) {
    neg_rows <- which(ar_tbl[[col]] < 0)
    if (length(neg_rows) > 0)
      tbl_ar <- tbl_ar |> tab_style(style = cell_text(color = "#8B0000"),
        locations = cells_body(columns = all_of(col), rows = neg_rows))
  }

  beat_rows <- which(ar_tbl[[sma_col]] - ar_tbl[[str9_col]] > 0.02)
  if (length(beat_rows) > 0)
    tbl_ar <- tbl_ar |> tab_style(
      style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
      locations = cells_body(columns = all_of(sma_col), rows = beat_rows))

  tbl_ar |> gtsave(sprintf("%s/%s.sma.annual.returns.html", reportPath, iName))
  webshot2::webshot(sprintf("%s/%s.sma.annual.returns.html", reportPath, iName),
    sprintf("%s/%s.sma.annual.returns.table.png", reportPath, iName),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # annual drawdown table
  annual_dd <- apply.yearly(combined, maxDrawdown)
  dd_tbl <- fortify(annual_dd) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))

  tbl_dd <- dd_tbl |>
    gt() |>
    tab_header(
      title = sprintf("%s — Max Drawdown", iName),
      subtitle = sprintf("SMA(%d) vs Strategy Nine LS eq.wt (□ = less DD by >2%%)", sma_n)
    ) |>
    fmt_percent(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

  for (col in names(dd_tbl)[-1]) {
    severe <- which(dd_tbl[[col]] > 0.20)
    mild   <- which(dd_tbl[[col]] < 0.10 & dd_tbl[[col]] > 0)
    if (length(severe) > 0)
      tbl_dd <- tbl_dd |> tab_style(style = cell_text(color = "#8B0000", weight = "bold"),
        locations = cells_body(columns = all_of(col), rows = severe))
    if (length(mild) > 0)
      tbl_dd <- tbl_dd |> tab_style(style = cell_text(color = "#006400"),
        locations = cells_body(columns = all_of(col), rows = mild))
  }

  beat_rows <- which(dd_tbl[[str9_col]] - dd_tbl[[sma_col]] > 0.02)
  if (length(beat_rows) > 0)
    tbl_dd <- tbl_dd |> tab_style(
      style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
      locations = cells_body(columns = all_of(sma_col), rows = beat_rows))

  tbl_dd |> gtsave(sprintf("%s/%s.sma.annual.drawdowns.html", reportPath, iName))
  webshot2::webshot(sprintf("%s/%s.sma.annual.drawdowns.html", reportPath, iName),
    sprintf("%s/%s.sma.annual.drawdowns.table.png", reportPath, iName),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # annual Sharpe table
  annual_sr <- apply.yearly(combined, SharpeRatio.annualized)
  sr_tbl <- fortify(annual_sr) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))

  tbl_sr <- sr_tbl |>
    gt() |>
    tab_header(
      title = sprintf("%s — Sharpe Ratio", iName),
      subtitle = sprintf("SMA(%d) vs Strategy Nine LS eq.wt (□ = beats Str9 by >2)", sma_n)
    ) |>
    fmt_number(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

  # color negatives in both columns
  for (col in names(sr_tbl)[-1]) {
    neg_rows <- which(sr_tbl[[col]] < 0)
    if (length(neg_rows) > 0)
      tbl_sr <- tbl_sr |> tab_style(style = cell_text(color = "#8B0000"),
        locations = cells_body(columns = all_of(col), rows = neg_rows))
  }

  beat_rows <- which(sr_tbl[[sma_col]] - sr_tbl[[str9_col]] > 2)
  if (length(beat_rows) > 0)
    tbl_sr <- tbl_sr |> tab_style(
      style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
      locations = cells_body(columns = all_of(sma_col), rows = beat_rows))

  tbl_sr |> gtsave(sprintf("%s/%s.sma.annual.sharpe.html", reportPath, iName))
  webshot2::webshot(sprintf("%s/%s.sma.annual.sharpe.html", reportPath, iName),
    sprintf("%s/%s.sma.annual.sharpe.table.png", reportPath, iName),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))
}

# =========================================================================
# 50-50 blend: SMA + Strategy Nine long/short equal-weight
# =========================================================================
print("=== 50-50 BLEND ===")

for (iName in indices) {
  sma_n <- sma_periods[[iName]]
  ls_key <- paste(iName, "equal_weight_ls", sep = "_")

  if (!iName %in% names(sma_strats) || !ls_key %in% names(ls_strats)) next

  sma_xts <- sma_strats[[iName]]
  str9_xts <- ls_strats[[ls_key]]
  bh_xts <- bh_strats[[iName]]

  # align dates
  cd <- intersect(intersect(index(sma_xts), index(str9_xts)), index(bh_xts))
  blend_xts <- 0.5 * sma_xts[cd] + 0.5 * str9_xts[cd]

  combined <- na.omit(merge(sma_xts[cd], str9_xts[cd], blend_xts, bh_xts[cd]))
  names(combined) <- c(sprintf("SMA(%d)", sma_n), "Str9 LS eq.wt", "50-50 Blend", "B&H")

  cat(sprintf("  %s ...\n", iName))

  # cumulative returns
  Common.PlotCumReturns(combined, iName,
    sprintf("50-50 Blend vs SMA(%d) vs Str9 LS eq.wt vs B&H", sma_n),
    sprintf("%s/%s.blend.compare.cumret.png", reportPath, iName), NULL)

  cols <- names(combined)

  # ---- annual returns ----
  annual_ret <- apply.yearly(combined, Return.cumulative)
  ar_tbl <- fortify(annual_ret) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))

  tbl <- ar_tbl |> gt() |>
    tab_header(title = sprintf("%s — Annual Returns", iName),
      subtitle = sprintf("50-50 Blend vs SMA(%d), Str9 LS eq.wt, B&H (□ = Blend beats all by >2%%)", sma_n)) |>
    fmt_percent(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

  for (c in cols) {
    nr <- which(ar_tbl[[c]] < 0)
    if (length(nr) > 0) tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
      locations = cells_body(columns = all_of(c), rows = nr))
  }

  # box Blend if beats ALL others by >2%
  blend_col <- "50-50 Blend"
  other_cols <- setdiff(cols, blend_col)
  other_max <- pmax(ar_tbl[[other_cols[1]]], ar_tbl[[other_cols[2]]], ar_tbl[[other_cols[3]]], na.rm = TRUE)
  br <- which(ar_tbl[[blend_col]] > other_max + 0.02)
  if (length(br) > 0) tbl <- tbl |> tab_style(
    style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
    locations = cells_body(columns = all_of(blend_col), rows = br))

  tbl |> gtsave(sprintf("%s/%s.blend.annual.returns.html", reportPath, iName))
  webshot2::webshot(sprintf("%s/%s.blend.annual.returns.html", reportPath, iName),
    sprintf("%s/%s.blend.annual.returns.table.png", reportPath, iName),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # ---- annual drawdowns ----
  annual_dd <- apply.yearly(combined, maxDrawdown)
  dd_tbl <- fortify(annual_dd) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))

  tbl <- dd_tbl |> gt() |>
    tab_header(title = sprintf("%s — Max Drawdown", iName),
      subtitle = sprintf("50-50 Blend vs SMA(%d), Str9 LS eq.wt, B&H (□ = Blend less DD than all by >2%%)", sma_n)) |>
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

  other_min <- pmin(dd_tbl[[other_cols[1]]], dd_tbl[[other_cols[2]]], dd_tbl[[other_cols[3]]], na.rm = TRUE)
  br <- which(dd_tbl[[blend_col]] + 0.02 < other_min)
  if (length(br) > 0) tbl <- tbl |> tab_style(
    style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
    locations = cells_body(columns = all_of(blend_col), rows = br))

  tbl |> gtsave(sprintf("%s/%s.blend.annual.drawdowns.html", reportPath, iName))
  webshot2::webshot(sprintf("%s/%s.blend.annual.drawdowns.html", reportPath, iName),
    sprintf("%s/%s.blend.annual.drawdowns.table.png", reportPath, iName),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # ---- annual sharpe ----
  annual_sr <- apply.yearly(combined, SharpeRatio.annualized)
  sr_tbl <- fortify(annual_sr) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))

  tbl <- sr_tbl |> gt() |>
    tab_header(title = sprintf("%s — Sharpe Ratio", iName),
      subtitle = sprintf("50-50 Blend vs SMA(%d), Str9 LS eq.wt, B&H (□ = Blend beats all by >2)", sma_n)) |>
    fmt_number(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

  for (c in cols) {
    nr <- which(sr_tbl[[c]] < 0)
    if (length(nr) > 0) tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
      locations = cells_body(columns = all_of(c), rows = nr))
  }

  other_max <- pmax(sr_tbl[[other_cols[1]]], sr_tbl[[other_cols[2]]], sr_tbl[[other_cols[3]]], na.rm = TRUE)
  br <- which(sr_tbl[[blend_col]] > other_max + 2)
  if (length(br) > 0) tbl <- tbl |> tab_style(
    style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
    locations = cells_body(columns = all_of(blend_col), rows = br))

  tbl |> gtsave(sprintf("%s/%s.blend.annual.sharpe.html", reportPath, iName))
  webshot2::webshot(sprintf("%s/%s.blend.annual.sharpe.html", reportPath, iName),
    sprintf("%s/%s.blend.annual.sharpe.table.png", reportPath, iName),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))
}

# =========================================================================
# Scaled forecast strategies: forecast ∈ [-20,+20] → weight ∈ [-1,+1]
# Scaled long-only: weight ∈ [0,1]; scaled long-short: weight ∈ [-1,1]
# Drag applied on binary signal changes only
# =========================================================================
print("=== SCALED FORECAST STRATEGIES ===")
scaled_lo <- list()
scaled_ls <- list()

for (use_cs in c(FALSE, TRUE)) {
  cs_label <- if (use_cs) "cost_screen" else "equal_weight"
  cat(sprintf("\n--- %s ---\n", cs_label))

  for (iName in indices) {
    cat(sprintf("  %s ...\n", iName))

    sig <- tryCatch({
      if (use_cs) {
        strategy_nine_signal(pXts[, iName], use_cost_screen = TRUE,
                             cost_per_trade_sr = 0.0088)
      } else {
        strategy_nine_signal(pXts[, iName], use_cost_screen = FALSE)
      }
    }, error = function(e) { NULL })

    if (is.null(sig) || all(is.na(sig))) next

    forecast_xts <- attr(sig, "forecast")
    if (is.null(forecast_xts)) next

    # scale to [-1, +1]
    scaled_fc <- forecast_xts / 20

    retL1 <- stats::lag(dSymXts[, iName], -1)
    common_dates <- intersect(intersect(index(na.omit(sig)), index(retL1)),
                              index(na.omit(scaled_fc)))
    sig_aligned <- sig[common_dates]
    fc_aligned  <- scaled_fc[common_dates]
    retL1_aligned <- retL1[common_dates]

    # binary trade detection for drag
    trd <- coredata(sig_aligned)
    trd <- trd - c(NA, trd[-length(trd)])

    # scaled long-only: weight clamped to [0, 1]
    w_lo <- pmax(pmin(coredata(fc_aligned), 1), 0)
    strat_lo <- w_lo * coredata(retL1_aligned)
    strat_lo <- ifelse(trd != 0 & !is.na(trd), strat_lo - drag, strat_lo)
    lo_key <- paste(iName, cs_label, "sclo", sep = "_")
    scaled_lo[[lo_key]] <- xts(strat_lo, order.by = common_dates)

    # scaled long-short: weight clamped to [-1, 1]
    w_ls <- pmax(pmin(coredata(fc_aligned), 1), -1)
    strat_ls <- w_ls * coredata(retL1_aligned)
    strat_ls <- ifelse(trd != 0 & !is.na(trd), strat_ls - drag, strat_ls)
    ls_key <- paste(iName, cs_label, "scls", sep = "_")
    scaled_ls[[ls_key]] <- xts(strat_ls, order.by = common_dates)

    # metrics
    sharpe_lo <- SharpeRatio.annualized(xts(strat_lo, order.by = common_dates))
    sharpe_ls <- SharpeRatio.annualized(xts(strat_ls, order.by = common_dates))
    ret_lo <- Return.annualized(xts(strat_lo, order.by = common_dates))
    ret_ls <- Return.annualized(xts(strat_ls, order.by = common_dates))

    results <- rbind(results, tibble(
      Index = iName, Method = cs_label,
      Type = "scaled long-only",
      Ret = round(as.numeric(ret_lo), 4),
      Sharpe = round(sharpe_lo[1, 1], 3),
      BH_Ret = round(as.numeric(Return.annualized(retL1_aligned)), 4),
      BH_Sharpe = round(SharpeRatio.annualized(retL1_aligned)[1, 1], 3)
    ))
    results <- rbind(results, tibble(
      Index = iName, Method = cs_label,
      Type = "scaled long-short",
      Ret = round(as.numeric(ret_ls), 4),
      Sharpe = round(sharpe_ls[1, 1], 3),
      BH_Ret = round(as.numeric(Return.annualized(retL1_aligned)), 4),
      BH_Sharpe = round(SharpeRatio.annualized(retL1_aligned)[1, 1], 3)
    ))
  }
}

# ---- Scaled comparison charts (one per index per method) ----
for (iName in indices) {
  for (cs_label in c("equal_weight", "cost_screen")) {
    lo_key <- paste(iName, cs_label, "sclo", sep = "_")
    ls_key <- paste(iName, cs_label, "scls", sep = "_")
    orig_key <- if (cs_label == "equal_weight")
      paste(iName, "equal_weight_ls", sep = "_") else paste(iName, "cost_screen_ls", sep = "_")

    if (!lo_key %in% names(scaled_lo)) next

    cd <- intersect(intersect(index(scaled_lo[[lo_key]]), index(scaled_ls[[ls_key]])),
                    index(ls_strats[[orig_key]]))
    combined <- na.omit(merge(scaled_lo[[lo_key]][cd], scaled_ls[[ls_key]][cd],
                              ls_strats[[orig_key]][cd], bh_strats[[iName]][cd]))
    names(combined) <- c("Scaled LO", "Scaled LS", "Str9 LS binary", "B&H")

    Common.PlotCumReturns(combined, iName,
      sprintf("Scaled Strategies vs Binary LS (%s)", cs_label),
      sprintf("%s/%s.scaled.%s.cumret.png", reportPath, iName, cs_label), NULL)
  }
}

# Regenerate gt table with new rows
results |>
  gt(groupname_col = "Type") |>
  tab_header(
    title = "Strategy Nine Backtest — Nifty Indices",
    subtitle = sprintf("2005-04-01 → %s; drag = %.1f%%; cost_per_trade_sr = 0.0088",
                       format(last(index(pXts)), "%Y-%m-%d"), drag * 100)
  ) |>
  tab_spanner(label = "Strategy Nine", columns = c(Ret, Sharpe)) |>
  tab_spanner(label = "Buy & Hold", columns = c(BH_Ret, BH_Sharpe)) |>
  fmt_percent(columns = c(Ret, BH_Ret), decimals = 2) |>
  fmt_number(columns = c(Sharpe, BH_Sharpe), decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) |>
  tab_style(style = cell_fill(color = "#f0fff0"),
            locations = cells_body(rows = Method == "equal_weight")) |>
  tab_style(style = cell_fill(color = "#fff3e0"),
            locations = cells_body(rows = Method == "cost_screen")) |>
  cols_label(Ret = "Ann.Ret", Sharpe = "Sharpe",
             BH_Ret = "Ann.Ret", BH_Sharpe = "Sharpe") |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes()) |>
  gtsave(sprintf("%s/strategy09-results.html", reportPath))

webshot2::webshot(
  sprintf("%s/strategy09-results.html", reportPath),
  sprintf("%s/strategy09-results.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# =========================================================================
# Winning strategy: MIDCAP + SMALLCAP Scaled Long-Only (equal weight)
# =========================================================================
print("=== WINNING STRATEGY: MIDCAP + SMALLCAP ScLO ===")

mc_key  <- "NIFTY MIDCAP 150 TR_equal_weight_sclo"
sc_key  <- "NIFTY SMALLCAP 250 TR_equal_weight_sclo"
mc_lo   <- "NIFTY MIDCAP 150 TR_equal_weight"
sc_lo   <- "NIFTY SMALLCAP 250 TR_equal_weight"

if (all(c(mc_key, sc_key) %in% names(scaled_lo))) {
  mc_sclo <- scaled_lo[[mc_key]]
  sc_sclo <- scaled_lo[[sc_key]]

  cd <- intersect(index(mc_sclo), index(sc_sclo))
  comb_sclo <- 0.5 * mc_sclo[cd] + 0.5 * sc_sclo[cd]

  # comparison series
  mc_bh <- bh_strats[["NIFTY MIDCAP 150 TR"]][cd]
  sc_bh <- bh_strats[["NIFTY SMALLCAP 250 TR"]][cd]
  comb_bh <- 0.5 * mc_bh + 0.5 * sc_bh

  mc_lo_xts <- all_strats[[mc_lo]][cd]
  sc_lo_xts <- all_strats[[sc_lo]][cd]

  combined <- na.omit(merge(mc_sclo[cd], sc_sclo[cd], comb_sclo,
                            mc_lo_xts, sc_lo_xts, comb_bh))
  names(combined) <- c("MIDCAP ScLO", "SMALLCAP ScLO", "50-50 ScLO",
                        "MIDCAP LO", "SMALLCAP LO", "B&H 50-50")

  cols <- names(combined)

  # cumulative returns
  Common.PlotCumReturns(combined, "MIDCAP + SMALLCAP",
    "Winning Strategy: Scaled Long-Only vs Binary LO vs B&H",
    sprintf("%s/winning-strategy.cumret.png", reportPath), NULL)

  # annual returns
  annual_ret <- apply.yearly(combined, Return.cumulative)
  ar_tbl <- fortify(annual_ret) |> rename(Year = Index) |>
    mutate(Year = format(Year, "%Y"))

  tbl <- ar_tbl |> gt() |>
    tab_header(title = "Winning Strategy — Annual Returns",
      subtitle = "MIDCAP + SMALLCAP Scaled Long-Only (□ = 50-50 ScLO beats all by >2%)") |>
    fmt_percent(columns = -Year, decimals = 2) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_source_note(source_note = "@StockViz") |>
    tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

  for (c in cols) {
    nr <- which(ar_tbl[[c]] < 0)
    if (length(nr) > 0) tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
      locations = cells_body(columns = all_of(c), rows = nr))
  }

  blend_col <- "50-50 ScLO"
  other_cols <- setdiff(cols, blend_col)
  other_max <- do.call(pmax, c(lapply(other_cols, function(c) ar_tbl[[c]]), na.rm = TRUE))
  br <- which(ar_tbl[[blend_col]] > other_max + 0.02)
  if (length(br) > 0) tbl <- tbl |> tab_style(
    style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
    locations = cells_body(columns = all_of(blend_col), rows = br))

  tbl |> gtsave(sprintf("%s/winning-strategy.annual.returns.html", reportPath))
  webshot2::webshot(sprintf("%s/winning-strategy.annual.returns.html", reportPath),
    sprintf("%s/winning-strategy.annual.returns.png", reportPath),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # annual drawdowns
  annual_dd <- apply.yearly(combined, maxDrawdown)
  dd_tbl <- fortify(annual_dd) |> rename(Year = Index) |>
    mutate(Year = format(Year, "%Y"))

  tbl <- dd_tbl |> gt() |>
    tab_header(title = "Winning Strategy — Max Drawdown",
      subtitle = "MIDCAP + SMALLCAP Scaled Long-Only (□ = 50-50 ScLO less DD than all by >2%)") |>
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

  tbl |> gtsave(sprintf("%s/winning-strategy.annual.drawdowns.html", reportPath))
  webshot2::webshot(sprintf("%s/winning-strategy.annual.drawdowns.html", reportPath),
    sprintf("%s/winning-strategy.annual.drawdowns.png", reportPath),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # annual Sharpe
  annual_sr <- apply.yearly(combined, SharpeRatio.annualized)
  sr_tbl <- fortify(annual_sr) |> rename(Year = Index) |>
    mutate(Year = format(Year, "%Y"))

  tbl <- sr_tbl |> gt() |>
    tab_header(title = "Winning Strategy — Sharpe Ratio",
      subtitle = "MIDCAP + SMALLCAP Scaled Long-Only (□ = 50-50 ScLO beats all by >2)") |>
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

  tbl |> gtsave(sprintf("%s/winning-strategy.annual.sharpe.html", reportPath))
  webshot2::webshot(sprintf("%s/winning-strategy.annual.sharpe.html", reportPath),
    sprintf("%s/winning-strategy.annual.sharpe.png", reportPath),
    selector = "table.gt_table", expand = c(10, 10, 10, 10))

  # print summary metrics
  full_sr <- SharpeRatio.annualized(comb_sclo)
  full_ret <- Return.annualized(comb_sclo)
  full_dd <- maxDrawdown(comb_sclo)
  cat(sprintf("\n  MIDCAP+SMALLCAP 50-50 ScLO: SR=%.2f  Ret=%.1f%%  DD=%.1f%%\n",
              full_sr[1,1], as.numeric(full_ret)*100, full_dd*100))
  cat(sprintf("  At 2x leverage:           Ret=%.1f%%  DD=%.1f%%\n",
              as.numeric(full_ret)*200, full_dd*200))
}

print("Done.")

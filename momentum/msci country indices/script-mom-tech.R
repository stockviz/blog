# ── control ────────────────────────────────────────────────────────────────────
compute_sma <- TRUE
source("common.R")

# ── calculate returns for momentum calc ───────────────────────────────────────
print("computing momentum + trend returns...")

momReturns <- NULL
for(momLb in momLbs){
  cat(sprintf("    lookback: %d-day...\n", momLb))
  periodSrTb <- timetk::tk_tbl(periodSRs[[momLb]][monthEndDates, ], rename_index = "month_end")
  highMomNames <- periodSrTb |> pivot_longer(cols = -month_end, names_to = "name", values_to = "ret") |> 
    group_by(month_end) |> 
    slice_max(ret, n = positionSize) |>
    select(month_end, name) |>
    ungroup()
  
  mePrices <- do.call(merge.xts, lapply(valid_countries, \(vc) daily_prices[[vc]][monthEndDates, ]))
  names(mePrices) <- valid_countries
  
  periodSmaTb <- timetk::tk_tbl(mePrices > sma_prices[[momLb]][monthEndDates, ] , rename_index = "month_end")
  
  trendNames <- periodSmaTb |> pivot_longer(cols = -month_end, names_to = "name", values_to = "trending") |>
    filter(trending == TRUE) |>
    select(month_end, name)
  
  monthly_rets_l1 <- stats::lag(do.call(merge.xts, monthly_rets), -1)
  names(monthly_rets_l1) <- valid_countries
  
  oldNames <- c()
  momRet <- NULL
  for(i in 1:(length(monthEndDates)-1)){
    meDate <- monthEndDates[i]
    momNames <- (highMomNames |> filter(month_end == meDate) |> select(name))$name
    trndNames <- (trendNames |> filter(month_end == meDate) |> select(name))$name
    newNames <- base::intersect(momNames, trndNames)

    meRet <- mean(monthly_rets_l1[meDate, newNames])
    churn <- (positionSize - length(base::intersect(oldNames, newNames)))/positionSize
    meRetNet <- meRet*(1 - churn*drag)
    momRet <- rbind(momRet, xts(meRetNet, monthEndDates[i+1]))
    oldNames <- newNames
  }
  momReturns <- merge.xts(momReturns, momRet)
}

names(momReturns) <- unlist(lapply(momLbs, \(x) paste0("MOM_", x)))
momReturns$MOM_AVG <- rowMeans(momReturns)

save(momReturns, file=sprintf("%s/momentum-trend.Rdata", reportPath))

toPlot <- na.omit(merge(momReturns, bench_monthly_rets))
sr <- paste(round(SharpeRatio.annualized(toPlot), 2), collapse = "/")
Common.PlotCumReturns(toPlot, "MSCI Country Index Momentum w/Trend",
                      sprintf("%d/%d/EW | SR: %s", length(valid_countries), positionSize, sr),
                      sprintf("%s/msci-country-index-momentum-trend.cumret.png", reportPath), NULL)

# ── summary table ─────────────────────────────────────────────────────────────
cat("\n=== Summary Metrics ===\n")
summary_tbl <- tibble()
for (nm in colnames(toPlot)) {
  sr  <- round(SharpeRatio.annualized(toPlot[,nm])[1,1], 3)
  ret <- round(as.numeric(Return.annualized(toPlot[,nm])), 4)
  dd  <- round(as.numeric(maxDrawdown(toPlot[,nm])), 4)
  cat(sprintf("  %-20s  SR=%.2f  Ret=%.2f%%  DD=%.2f%%\n", nm, sr, ret*100, dd*100))
  summary_tbl <- rbind(summary_tbl, tibble(Strategy = nm, SR = sr, AnnRet = ret, MaxDD = dd))
}

summary_tbl |>
  gt() |>
  tab_header(
    title = sprintf("MSCI Country Index Momentum w/Trend (%d/%d/EW)", length(valid_countries), positionSize),
    subtitle = sprintf("%s → %s",
                       as.character(as.Date(first(index(toPlot)))),
                       as.character(as.Date(last(index(toPlot)))))
  ) |>
  fmt_percent(columns = c(AnnRet, MaxDD), decimals = 2) |>
  fmt_number(columns = SR, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  cols_label(SR = "Sharpe", AnnRet = "Ann.Return", MaxDD = "Max Drawdown") |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes()) ->
  tbl

for (col in c("AnnRet")) {
  neg_rows <- which(summary_tbl[[col]] < 0)
  if (length(neg_rows) > 0) tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
    locations = cells_body(columns = all_of(col), rows = neg_rows))
  green_rows <- which(summary_tbl[[col]] > 0.02)
  if (length(green_rows) > 0) tbl <- tbl |> tab_style(style = cell_text(color = "#006400"),
    locations = cells_body(columns = all_of(col), rows = green_rows))
}

tbl |> gtsave(sprintf("%s/msci-momentum-trend-summary.html", reportPath))

webshot2::webshot(
  sprintf("%s/msci-momentum-trend-summary.html", reportPath),
  sprintf("%s/msci-momentum-trend-summary.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── annual returns table ─────────────────────────────────────────────────────
annual_ret <- apply.yearly(toPlot, Return.cumulative)
ar_tbl <- fortify(annual_ret) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))
all_cols <- names(ar_tbl)[-1]
strat_cols <- setdiff(all_cols, benchIndexName)

tbl <- ar_tbl |>
  gt() |>
  tab_header(
    title = sprintf("MSCI Country Index Momentum w/Trend — Annual Returns (%d/%d/EW)", length(valid_countries), positionSize),
    subtitle = sprintf("%s → %s",
                       as.character(as.Date(first(index(toPlot)))),
                       as.character(as.Date(last(index(toPlot)))))
  ) |>
  fmt_percent(columns = -Year, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

# red for any negative returns (all columns)
for (c in all_cols) {
  neg <- which(ar_tbl[[c]] < 0)
  if (length(neg) > 0) tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
    locations = cells_body(columns = all_of(c), rows = neg))
}

# momentum columns: box if > benchmark, red if < benchmark, green if > benchmark+2%
for (c in strat_cols) {
  rel <- ar_tbl[[c]] - ar_tbl[[benchIndexName]]

  below <- which(rel < 0)
  if (length(below) > 0) tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
    locations = cells_body(columns = all_of(c), rows = below))

  above2 <- which(rel > 0.02)
  if (length(above2) > 0) tbl <- tbl |> tab_style(style = cell_text(color = "#006400"),
    locations = cells_body(columns = all_of(c), rows = above2))

  beat <- which(rel > 0)
  if (length(beat) > 0) tbl <- tbl |>
    tab_style(style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
              locations = cells_body(columns = all_of(c), rows = beat))
}

tbl |> gtsave(sprintf("%s/msci-momentum-trend-annual.returns.html", reportPath))
webshot2::webshot(
  sprintf("%s/msci-momentum-trend-annual.returns.html", reportPath),
  sprintf("%s/msci-momentum-trend-annual.returns.table.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── annual drawdowns table ────────────────────────────────────────────────────
annual_dd <- apply.yearly(toPlot, maxDrawdown)
dd_tbl <- fortify(annual_dd) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))

tbl <- dd_tbl |>
  gt() |>
  tab_header(
    title = sprintf("MSCI Country Index Momentum w/Trend — Max Drawdown (%d/%d/EW)", length(valid_countries), positionSize),
    subtitle = sprintf("%s → %s",
                       as.character(as.Date(first(index(toPlot)))),
                       as.character(as.Date(last(index(toPlot)))))
  ) |>
  fmt_percent(columns = -Year, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

# red + bold for drawdowns > 20%
for (c in all_cols) {
  severe <- which(dd_tbl[[c]] > 0.20)
  if (length(severe) > 0) tbl <- tbl |> tab_style(
    style = cell_text(color = "#8B0000", weight = "bold"),
    locations = cells_body(columns = all_of(c), rows = severe))
}

tbl |> gtsave(sprintf("%s/msci-momentum-trend-annual.drawdowns.html", reportPath))
webshot2::webshot(
  sprintf("%s/msci-momentum-trend-annual.drawdowns.html", reportPath),
  sprintf("%s/msci-momentum-trend-annual.drawdowns.table.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

print("Done.")

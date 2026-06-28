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
source("strategy_nine.R")
source("/mnt/data/blog/common/plot.common.r")

pdf(NULL)
drag <- 0.2 / 100

# ── connect ───────────────────────────────────────────────────────────────────
lconUS2 <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockVizUs2", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE
)

# ── discover MSCI country equity universe ─────────────────────────────────────
benchIndexName <- "MSCI ACWI Index"
benchIndexCode <- sqlQuery(lconUS2, sprintf("select index_code from msci_meta where index_name='%s'", benchIndexName))[[1]]

ignoreList <- c('-1')
countryCounts <- sqlQuery(lconUS2, "select index_code, count(c1.[value]) cnt from msci_meta cross apply openjson(details, '$.country') c1 group by index_code")
countryCounts <- countryCounts[countryCounts[,2] == 1,]

countryMeta <- sqlQuery(lconUS2, sprintf("select index_name, index_code, json_value(details, '$.country[0]') country 
                          from msci_meta 
                          where json_value(details, '$.assetClass') = 'EQUITY' 
                          and json_value(details, '$.indexType') = 'Flat Index Type'
                          and json_value(details, '$.sizes[0]') = 'LARGE_CAP'
                          and json_value(details, '$.taxonomyGroups[0]') <> 'Sector' 
                          and json_value(details, '$.taxonomyCategories[0]') = 'Market Cap'
                          and index_code in (%s)
                          and index_name not like '%% ex %%'
                          and index_name not like '%% imi%%'
                          and index_name not like '%% large %%'
                          and index_code not in (%s)
                          order by country", paste(countryCounts$index_code, collapse=','), paste(ignoreList, collapse=',')))

countryMeta <- countryMeta[!is.na(countryMeta$country),]

countryDates <- sqlQuery(lconUS2, sprintf("select ID, min(TIME_STAMP) stdt, max(TIME_STAMP) eddt
                                          from msci_data
                                          where INDEX_TYPE = 'G' and ID in (%s)
                                          group by ID", paste(countryMeta$index_code, collapse=",")))

countryUniverse <- countryDates |> filter(stdt <= median(stdt)) |>
  inner_join(countryMeta, join_by(ID == index_code))

cat(sprintf("  %d country indices in universe\n", nrow(countryUniverse)))

# ── load prices ───────────────────────────────────────────────────────────────
print("loading prices...")
daily_prices <- list()
daily_rets   <- list()

# benchmark
benchPx <- sqlQuery(lconUS2, sprintf("select VAL, TIME_STAMP from msci_data where INDEX_TYPE = 'G' and ID=%s order by TIME_STAMP", benchIndexCode))
bench_xts <- xts(benchPx$VAL, benchPx$TIME_STAMP)
bench_rets <- dailyReturn(bench_xts)
cat(sprintf("  ACWI: %d rows\n", nrow(bench_xts)))

for (i in 1:nrow(countryUniverse)) {
  idx_code <- countryUniverse$ID[i]
  idx_name <- countryUniverse$country[i]
  px <- sqlQuery(lconUS2, sprintf("select VAL, TIME_STAMP from msci_data where INDEX_TYPE = 'G' and ID=%s order by TIME_STAMP", idx_code))
  if (nrow(px) < 260) { cat(sprintf("  %s: too few rows (%d), skipping\n", idx_name, nrow(px))); next }
  daily_prices[[idx_name]] <- xts(px$VAL, px$TIME_STAMP)
  daily_rets[[idx_name]]   <- dailyReturn(daily_prices[[idx_name]])
  cat(sprintf("  %s: %d rows\n", idx_name, nrow(px)))
}

valid_countries <- names(daily_prices)
cat(sprintf("  %d countries loaded\n", length(valid_countries)))

# ── run Strategy Nine: 4 variants per country ─────────────────────────────────
print("computing signals...")
scaled_lo <- list()
scaled_ls <- list()
binary_lo <- list()
binary_ls <- list()
bh_rets   <- list()

for (cntry in valid_countries) {
  prices <- daily_prices[[cntry]]
  rets   <- daily_rets[[cntry]]
  cat(sprintf("  %s ...\n", cntry))

  sig <- tryCatch({
    strategy_nine_signal(prices, use_cost_screen = FALSE)
  }, error = function(e) { cat(sprintf("    ERROR: %s\n", e$message)); NULL })
  if (is.null(sig)) next

  forecast_xts <- attr(sig, "forecast")
  if (is.null(forecast_xts)) next

  retL1 <- stats::lag(rets, -1)
  common_dates <- intersect(intersect(index(na.omit(sig)), index(retL1)),
                            index(na.omit(forecast_xts)))
  if (length(common_dates) == 0) next

  sa <- sig[common_dates]
  fc <- forecast_xts[common_dates] / 20
  ra <- retL1[common_dates]
  trd <- coredata(sa); trd <- trd - c(NA, trd[-length(trd)])

  # scaled long-only (weight ∈ [0, 1])
  w_lo <- pmax(pmin(coredata(fc), 1), 0)
  sclo <- w_lo * coredata(ra)
  sclo <- ifelse(trd != 0 & !is.na(trd), sclo - drag, sclo)
  scaled_lo[[cntry]] <- xts(sclo, order.by = common_dates)

  # scaled long-short (weight ∈ [-1, 1])
  w_ls <- pmax(pmin(coredata(fc), 1), -1)
  scls <- w_ls * coredata(ra)
  scls <- ifelse(trd != 0 & !is.na(trd), scls - drag, scls)
  scaled_ls[[cntry]] <- xts(scls, order.by = common_dates)

  # binary long-only (1 → long, 0 → flat)
  blo <- ifelse(coredata(sa) == 1, coredata(ra), 0)
  blo <- ifelse(trd != 0 & !is.na(trd), blo - drag, blo)
  binary_lo[[cntry]] <- xts(blo, order.by = common_dates)

  # binary long-short (1 → long, 0 → short)
  bls <- ifelse(coredata(sa) == 1, coredata(ra), -coredata(ra))
  bls <- ifelse(trd != 0 & !is.na(trd), bls - drag, bls)
  binary_ls[[cntry]] <- xts(bls, order.by = common_dates)

  bh_rets[[cntry]] <- ra
}

# ── build equal-weight portfolios ─────────────────────────────────────────────
print("building portfolios...")
strat_sets <- list(
  "Scaled LO" = scaled_lo,
  "Scaled LS" = scaled_ls,
  "Binary LO" = binary_lo,
  "Binary LS" = binary_ls
)

valid_assets <- names(bh_rets)

# merge all assets per strategy
merged <- list()
for (nm in names(strat_sets)) {
  merged[[nm]] <- do.call(merge.xts, lapply(valid_assets, \(a) strat_sets[[nm]][[a]]))
  names(merged[[nm]]) <- valid_assets
}

# find common start
common_start <- first(index(na.omit(merged[["Scaled LO"]])))
first_valid <- sapply(valid_assets, \(a) first(index(na.omit(scaled_lo[[a]]))))
from_max_start <- max(as.Date(unlist(first_valid)))
portfolio_start <- max(common_start, from_max_start)
cat(sprintf("  portfolio start: %s\n", portfolio_start))

# equal-weight portfolio returns
portfolios <- list()
port_names <- c()

for (nm in names(strat_sets)) {
  rets <- merged[[nm]][paste0(portfolio_start, "/"), ]
  portfolios[[nm]] <- xts(rowMeans(rets, na.rm = TRUE), order.by = index(rets))
  port_names <- c(port_names, nm)
}

# ── portfolio metrics ─────────────────────────────────────────────────────────
cat("\n=== Portfolio Metrics ===\n")
summary_tbl <- tibble()
for (nm in port_names) {
  px <- portfolios[[nm]]
  sr  <- SharpeRatio.annualized(px)[1,1]
  ret <- as.numeric(Return.annualized(px))
  dd  <- as.numeric(maxDrawdown(px))
  cat(sprintf("  %-15s  SR=%.2f  Ret=%.2f%%  DD=%.2f%%\n", nm, sr, ret*100, dd*100))
  summary_tbl <- rbind(summary_tbl, tibble(
    Strategy = nm, SR = round(sr, 3), AnnRet = round(ret, 4), MaxDD = round(dd, 4)
  ))
}

# add ACWI B&H
acwi_bh <- bench_rets[paste0(portfolio_start, "/")]
acwi_sr  <- SharpeRatio.annualized(acwi_bh)[1,1]
acwi_ret <- as.numeric(Return.annualized(acwi_bh))
acwi_dd  <- as.numeric(maxDrawdown(acwi_bh))
cat(sprintf("  %-15s  SR=%.2f  Ret=%.2f%%  DD=%.2f%%\n", "ACWI B&H", acwi_sr, acwi_ret*100, acwi_dd*100))
summary_tbl <- rbind(summary_tbl, tibble(
  Strategy = "ACWI B&H", SR = round(acwi_sr, 3), AnnRet = round(acwi_ret, 4), MaxDD = round(acwi_dd, 4)
))

# ── gt summary table ─────────────────────────────────────────────────────────
summary_tbl |>
  gt() |>
  tab_header(
    title = sprintf("MSCI Country Equity — Strategy Nine (%d countries)", length(valid_assets)),
    subtitle = sprintf("Equal-weight; %s → %s; drag = %.1f%%",
                       portfolio_start, format(Sys.Date(), "%Y-%m-%d"), drag * 100)
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

tbl |> gtsave(sprintf("%s/msci-strategy-summary.html", reportPath))

webshot2::webshot(
  sprintf("%s/msci-strategy-summary.html", reportPath),
  sprintf("%s/msci-strategy-summary.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── cumulative charts ────────────────────────────────────────────────────────
# all strategies + ACWI B&H
to_plot <- do.call(merge.xts, lapply(port_names, \(nm) portfolios[[nm]]))
names(to_plot) <- port_names
to_plot <- na.omit(merge(to_plot, acwi_bh))
names(to_plot)[ncol(to_plot)] <- "ACWI B&H"

Common.PlotCumReturns(to_plot, "MSCI Country Equity — Strategy Nine",
  sprintf("%d countries, equal-weight; %s → %s", length(valid_assets), portfolio_start, format(Sys.Date(), "%Y-%m-%d")),
  sprintf("%s/msci-all-strategies.cumret.png", reportPath), NULL)

# scaled LO vs scaled LS vs ACWI
sclo_vs_scls <- na.omit(merge(portfolios[["Scaled LO"]], portfolios[["Scaled LS"]], acwi_bh))
names(sclo_vs_scls) <- c("Scaled LO", "Scaled LS", "ACWI B&H")
Common.PlotCumReturns(sclo_vs_scls, "MSCI Country Equity — Scaled Strategies",
  "Scaled Long-Only vs Scaled Long-Short vs ACWI B&H",
  sprintf("%s/msci-scaled.cumret.png", reportPath), NULL)

# binary LO vs binary LS vs ACWI
blo_vs_bls <- na.omit(merge(portfolios[["Binary LO"]], portfolios[["Binary LS"]], acwi_bh))
names(blo_vs_bls) <- c("Binary LO", "Binary LS", "ACWI B&H")
Common.PlotCumReturns(blo_vs_bls, "MSCI Country Equity — Binary Strategies",
  "Binary Long-Only vs Binary Long-Short vs ACWI B&H",
  sprintf("%s/msci-binary.cumret.png", reportPath), NULL)

# ── annual returns table ─────────────────────────────────────────────────────
annual_ret <- apply.yearly(to_plot, Return.cumulative)
ar_tbl <- fortify(annual_ret) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))
cols <- names(ar_tbl)[-1]

tbl <- ar_tbl |>
  gt() |>
  tab_header(
    title = sprintf("MSCI Country Equity — Annual Returns (%d countries)", length(valid_assets)),
    subtitle = sprintf("%s → %s", portfolio_start, format(Sys.Date(), "%Y-%m-%d"))
  ) |>
  fmt_percent(columns = -Year, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

for (c in cols) {
  neg <- which(ar_tbl[[c]] < 0)
  if (length(neg) > 0) tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
    locations = cells_body(columns = all_of(c), rows = neg))
}

# box strategy returns that beat ACWI B&H
strat_cols <- setdiff(cols, "ACWI B&H")
for (c in strat_cols) {
  beat_rows <- which(ar_tbl[[c]] > ar_tbl[["ACWI B&H"]])
  if (length(beat_rows) > 0) tbl <- tbl |>
    tab_style(
      style = cell_borders(sides = "all", color = "#333333", weight = px(1.5)),
      locations = cells_body(columns = all_of(c), rows = beat_rows))
}

tbl |> gtsave(sprintf("%s/msci-annual.returns.html", reportPath))
webshot2::webshot(
  sprintf("%s/msci-annual.returns.html", reportPath),
  sprintf("%s/msci-annual.returns.table.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── annual drawdowns ─────────────────────────────────────────────────────────
annual_dd <- apply.yearly(to_plot, maxDrawdown)
dd_tbl <- fortify(annual_dd) |> rename(Year = Index) |> mutate(Year = format(Year, "%Y"))

tbl <- dd_tbl |>
  gt() |>
  tab_header(
    title = sprintf("MSCI Country Equity — Max Drawdown (%d countries)", length(valid_assets)),
    subtitle = sprintf("%s → %s", portfolio_start, format(Sys.Date(), "%Y-%m-%d"))
  ) |>
  fmt_percent(columns = -Year, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes())

for (c in cols) {
  severe <- which(dd_tbl[[c]] > 0.20)
  mild   <- which(dd_tbl[[c]] < 0.10 & dd_tbl[[c]] > 0)
  if (length(severe) > 0) tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000", weight = "bold"),
    locations = cells_body(columns = all_of(c), rows = severe))
  if (length(mild) > 0) tbl <- tbl |> tab_style(style = cell_text(color = "#006400"),
    locations = cells_body(columns = all_of(c), rows = mild))
}

tbl |> gtsave(sprintf("%s/msci-annual.drawdowns.html", reportPath))
webshot2::webshot(
  sprintf("%s/msci-annual.drawdowns.html", reportPath),
  sprintf("%s/msci-annual.drawdowns.table.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10))

odbcClose(lconUS2)

print("Done.")

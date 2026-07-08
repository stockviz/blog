source("/mnt/data/blog/industry-rotation/common.R")

# ═══════════════════════════════════════════════════════════════════════════
#  Multi-frequency backtest: 1-week, 2-week, 4-week rebalance
# ═══════════════════════════════════════════════════════════════════════════

FREQS <- c(`1-Week` = 1L, `2-Week` = 2L, `4-Week` = 4L)

allStrats <- list()
for (nm in names(FREQS)) {
  cat(sprintf("\n--- %s rebalance ---\n", nm))
  res <- runBacktest(FREQS[nm], label = nm)
  allStrats[[paste0("Cap ", nm)]] <- res$cap
  allStrats[[paste0("Eq ", nm)]]  <- res$eq
}
allStrats <- allStrats[!sapply(allStrats, is.null)]

dailyStrats <- mergeWithBenchmarks(allStrats)
stats <- perfStats(dailyStrats)

# ── gt table ──
SCENARIO_COLORS <- c(
  `1-Week` = "#f0fff0",
  `2-Week` = "#f0f4ff",
  `4-Week` = "#fff3e0"
)
BENCH_COLOR <- "#f5f5f5"

perfTbl <- tibble(
  Strategy = names(dailyStrats),
  `Ann. Return %` = stats$ar,
  Sharpe = stats$sr
) |>
  gt() |>
  tab_header(
    title = "Industry Rotation — RRG Top 5",
    subtitle = sprintf("rsRatio > 100 & rsMomentum > 100; %.0f bps drag; %s to %s",
                       DRAG * 10000,
                       as.character(as.Date(first(index(dailyStrats)))),
                       as.character(as.Date(last(index(dailyStrats)))))) |>
  fmt_number(columns = `Ann. Return %`, decimals = 2) |>
  fmt_number(columns = Sharpe, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"),
            locations = cells_source_notes())

for (scn in names(SCENARIO_COLORS)) {
  rows <- which(grepl(scn, perfTbl$`_data`$Strategy))
  if (length(rows) > 0L)
    perfTbl <- perfTbl |>
      tab_style(style = cell_fill(color = SCENARIO_COLORS[scn]),
                locations = cells_body(rows = rows))
}
benchRows <- which(perfTbl$`_data`$Strategy %in% BENCHMARKS)
if (length(benchRows) > 0L)
  perfTbl <- perfTbl |>
    tab_style(style = cell_fill(color = BENCH_COLOR),
              locations = cells_body(rows = benchRows))

gtsave(perfTbl, sprintf("%s/industry-rotation-rrg.html", reportPath))
webshot2::webshot(sprintf("%s/industry-rotation-rrg.html", reportPath),
                  sprintf("%s/industry-rotation-rrg-tbl.png", reportPath),
                  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── plot (best of each weight type) ──
stratNames <- names(allStrats)
capNames <- grep("^Cap ", stratNames, value = TRUE)
eqNames  <- grep("^Eq ",  stratNames, value = TRUE)
bestCap  <- capNames[which.max(stats$sr[capNames])]
bestEq   <- eqNames[which.max(stats$sr[eqNames])]

plotData <- dailyStrats[, c(bestCap, bestEq, BENCHMARKS)]
srText <- paste0(c(bestCap, bestEq, BENCHMARKS), " = ",
                 stats$sr[c(bestCap, bestEq, BENCHMARKS)], collapse = "; ")

Common.PlotCumReturns(plotData,
  "Industry Rotation — RRG Top 5",
  sprintf("Best scenarios: %s, %s | Sharpe: %s", bestCap, bestEq, srText),
  sprintf("%s/industry-rotation-rrg.png", reportPath), NULL)

cat(sprintf("\nSharpe ratios:\n"))
print(stats$sr)
cat(sprintf("\nAnn. returns:\n"))
print(stats$ar)
odbcClose(lcon)

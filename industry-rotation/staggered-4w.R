source("/mnt/data/blog/industry-rotation/common.R")

# ═══════════════════════════════════════════════════════════════════════════
#  Staggered 4-week rebalance: average across 4 offset scenarios
# ═══════════════════════════════════════════════════════════════════════════

N_SCENARIOS <- 4L
FREQ_WEEKS  <- 4L

scenarioRets <- list()
for (offset in 0L:(N_SCENARIOS - 1L)) {
  label <- sprintf("4W+%d", offset)
  cat(sprintf("\n--- %s ---\n", label))
  res <- runBacktest(FREQ_WEEKS, offset, label)
  scenarioRets[[paste0("Cap ", label)]] <- res$cap
  scenarioRets[[paste0("Eq ",  label)]] <- res$eq
}

# ── average daily returns across scenarios ──
capList <- scenarioRets[grep("^Cap ", names(scenarioRets))]
eqList  <- scenarioRets[grep("^Eq ",  names(scenarioRets))]

# align to common date index across all scenarios
capDates <- Reduce(intersect, lapply(capList, index))
eqDates  <- Reduce(intersect, lapply(eqList,  index))

avgCap <- Reduce(`+`, lapply(capList, function(x) coredata(x[capDates]))) / length(capList)
avgEq  <- Reduce(`+`, lapply(eqList,  function(x) coredata(x[eqDates])))  / length(eqList)

avgCapXts <- xts(avgCap, capDates)
avgEqXts  <- xts(avgEq,  eqDates)

allStrats <- list(
  `Cap 4W Avg` = avgCapXts,
  `Eq 4W Avg`  = avgEqXts
)
# also include individual scenarios
for (nm in names(capList)) allStrats[[nm]] <- capList[[nm]]
for (nm in names(eqList))  allStrats[[nm]] <- eqList[[nm]]

dailyStrats <- mergeWithBenchmarks(allStrats)
stats <- perfStats(dailyStrats)

# ── gt table ──
AVG_COLOR    <- "#ffe0e0"   # light red for averages
BENCH_COLOR  <- "#f5f5f5"
SCEN_COLORS  <- c("#f0fff0", "#f0f4ff", "#fff3e0", "#f5f0ff")

perfTbl <- tibble(
  Strategy = names(dailyStrats),
  `Ann. Return %` = stats$ar,
  Sharpe = stats$sr
) |>
  gt() |>
  tab_header(
    title = "Industry Rotation — RRG Top 5 (Staggered 4-Week)",
    subtitle = sprintf("rsRatio > 100 & rsMomentum > 100; %.0f bps drag; avg of %d staggered scenarios; %s to %s",
                       DRAG * 10000, N_SCENARIOS,
                       as.character(as.Date(first(index(dailyStrats)))),
                       as.character(as.Date(last(index(dailyStrats)))))) |>
  fmt_number(columns = `Ann. Return %`, decimals = 2) |>
  fmt_number(columns = Sharpe, decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"),
            locations = cells_source_notes())

# color-code: average rows in red, individual scenarios, benchmarks grey
avgRows <- which(grepl("Avg", perfTbl$`_data`$Strategy))
benchRows <- which(perfTbl$`_data`$Strategy %in% BENCHMARKS)

for (j in seq_len(N_SCENARIOS)) {
  rows <- which(grepl(sprintf("4W\\+%d", j - 1L), perfTbl$`_data`$Strategy))
  if (length(rows) > 0L)
    perfTbl <- perfTbl |>
      tab_style(style = cell_fill(color = SCEN_COLORS[j]),
                locations = cells_body(rows = rows))
}
if (length(avgRows) > 0L)
  perfTbl <- perfTbl |>
    tab_style(style = cell_fill(color = AVG_COLOR),
              locations = cells_body(rows = avgRows))
if (length(benchRows) > 0L)
  perfTbl <- perfTbl |>
    tab_style(style = cell_fill(color = BENCH_COLOR),
              locations = cells_body(rows = benchRows))

gtsave(perfTbl, sprintf("%s/staggered-4w.html", reportPath))
webshot2::webshot(sprintf("%s/staggered-4w.html", reportPath),
                  sprintf("%s/staggered-4w-tbl.png", reportPath),
                  selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ── plot (averages only + benchmarks) ──
plotData <- dailyStrats[, c("Cap 4W Avg", "Eq 4W Avg", BENCHMARKS)]
srText <- paste0(c("Cap 4W Avg", "Eq 4W Avg", BENCHMARKS), " = ",
                 stats$sr[c("Cap 4W Avg", "Eq 4W Avg", BENCHMARKS)],
                 collapse = "; ")

Common.PlotCumReturns(plotData,
  "Industry Rotation — RRG Top 5 (Staggered 4-Week)",
  sprintf("Average of %d staggered scenarios | Sharpe: %s", N_SCENARIOS, srText),
  sprintf("%s/staggered-4w.png", reportPath), NULL)

cat(sprintf("\nSharpe ratios:\n"))
print(stats$sr)
cat(sprintf("\nAnn. returns:\n"))
print(stats$ar)
odbcClose(lcon)

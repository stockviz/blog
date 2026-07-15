# ============================================================================
# Drawdown-Switching Strategy
# If strategy DD < 10%: M12 (higher CAGR). If DD ≥ 10%: M6 (fastest recovery).
# Benchmarks: M6, M12, and the NIFTY500 MOMENTUM 50 TR index.
# ============================================================================

suppressPackageStartupMessages({
  library('xts')
  library('PerformanceAnalytics')
  library('tidyverse')
  library('lubridate')
  library('gt')
  library('webshot2')
  library('viridis')
})

options("scipen" = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "/mnt/data/blog/momentum/drawdown-formation"
BENCHMARK <- "NIFTY500 MOMENTUM 50 TR"

# ── Load checkpoint ──
cat("Loading checkpoint...\n")
cp <- readRDS(sprintf("%s/checkpoint_portfolios.rds", reportPath))
m6  <- cp$allPortfolios[["M6"]]
m12 <- cp$allPortfolios[["M12"]]
benchDaily <- cp$benchDaily
rm(cp)
cat(sprintf("  M6: %d days, M12: %d days, Bench: %d days\n",
            nrow(m6), nrow(m12), nrow(benchDaily)))

# ── Align all to common dates ──
cd <- Reduce(intersect, list(index(m6), index(m12), index(benchDaily)))
m6   <- m6[cd]
m12  <- m12[cd]
bench <- benchDaily[cd]

# ═══════════════════════════════════════════════════════════════
# Build drawdown-switching strategy
# ═══════════════════════════════════════════════════════════════
cat("\n=== Building DD-Switching Strategy ===\n")

eqSwitch <- 1.0
eqM6     <- 1.0
eqM12    <- 1.0
eqBench  <- 1.0

switchDaily <- numeric(length(cd))
inM12 <- TRUE
runningPeak <- 1.0
m12Fraction <- logical(length(cd))

for (i in seq_along(cd)) {
  rM6  <- as.numeric(coredata(m6[i]))
  rM12 <- as.numeric(coredata(m12[i]))
  rB   <- as.numeric(coredata(bench[i]))

  # Update standalone equity curves
  eqM6  <- eqM6  * (1 + rM6)
  eqM12 <- eqM12 * (1 + rM12)
  eqBench <- eqBench * (1 + rB)

  # Compute current drawdown of the switch strategy
  runningPeak <- max(runningPeak, eqSwitch)
  currentDD <- eqSwitch / runningPeak - 1

  # Switch logic: M12 when DD < 10%, M6 when DD ≥ 10%
  inM12 <- currentDD > -0.10

  # Apply and record
  activeRet <- if (inM12) rM12 else rM6
  eqSwitch <- eqSwitch * (1 + activeRet)
  switchDaily[i] <- activeRet
  m12Fraction[i] <- inM12
}

switchXts <- xts(switchDaily, cd)
colnames(switchXts) <- "DD Switch"

cat(sprintf("  M12 fraction: %.1f%%\n", mean(m12Fraction) * 100))

# ── Merge all strategies ──
combined <- na.omit(merge(switchXts, m6, m12, bench))
colnames(combined) <- c("DD Switch (M12⇄M6)", "M6", "M12", BENCHMARK)

# ═══════════════════════════════════════════════════════════════
# Metrics
# ═══════════════════════════════════════════════════════════════
cat("\n=== Metrics (Full Sample) ===\n")

computeMetrics <- function(rets_xts) {
  m <- list()
  for (col in colnames(rets_xts)) {
    r <- rets_xts[, col]; rv <- as.numeric(coredata(r))
    m[[col]] <- c(CAGR   = Return.annualized(r)[1,1],
                  Vol    = sd(rv, na.rm = TRUE) * sqrt(252),
                  Sharpe = SharpeRatio.annualized(r)[1,1],
                  MaxDD  = maxDrawdown(r),
                  Calmar = Return.annualized(r)[1,1] / maxDrawdown(r))
  }
  do.call(cbind, m)
}

fm <- computeMetrics(combined)
print(round(fm, 4))

# Split metrics
TRAIN_END   <- "2019-12-31"
VALID_START <- "2020-01-01"
VALID_END   <- "2021-12-31"
TEST_START  <- "2022-01-01"

for (s in c("train", "valid", "test")) {
  sr <- switch(s,
    train = paste0("/", TRAIN_END),
    valid = paste0(VALID_START, "/", VALID_END),
    test  = paste0(TEST_START, "/"))
  sub <- combined[sr]
  if (nrow(sub) < 60) next
  sm <- computeMetrics(sub)
  cat(sprintf("\n--- %s (%d days) ---\n", s, nrow(sub)))
  print(round(sm, 4))
}

# ═══════════════════════════════════════════════════════════════
# Charts
# ═══════════════════════════════════════════════════════════════
cat("\n=== Charts ===\n")
source("/mnt/hollandC/StockViz/R/plot.common.r")

# Cumulative
srAll <- sapply(colnames(combined), function(nm)
  round(SharpeRatio.annualized(combined[,nm])[1,1], 2))
Common.PlotCumReturns(combined,
  "Drawdown-Switching Momentum (M12 ⇄ M6 at 10% DD)",
  sprintf("%s → %s | SR: %s", first(index(combined)), last(index(combined)),
          paste0(colnames(combined), "=", srAll, collapse = ", ")),
  sprintf("%s/dd_switch_cumulative.png", reportPath), NULL)

# Per-split cumulative
for (s in c("train", "valid", "test")) {
  sr <- switch(s,
    train = paste0("/", TRAIN_END),
    valid = paste0(VALID_START, "/", VALID_END),
    test  = paste0(TEST_START, "/"))
  sub <- combined[sr]
  if (nrow(sub) < 60) next
  lbl <- switch(s,
    train = paste0("≤ ", TRAIN_END),
    valid = paste0(VALID_START, " – ", VALID_END),
    test  = paste0("≥ ", TEST_START))
  srs <- sapply(colnames(sub), function(nm) round(SharpeRatio.annualized(sub[,nm])[1,1], 2))
  Common.PlotCumReturns(sub,
    sprintf("DD Switch — %s", tools::toTitleCase(s)),
    sprintf("%s | SR: %s", lbl, paste0(colnames(sub), "=", srs, collapse = ", ")),
    sprintf("%s/dd_switch_%s.png", reportPath, s), NULL)
}

# Annual returns
annAll <- apply.yearly(combined, Return.cumulative)
annDf <- fortify(annAll, melt = TRUE)
names(annDf) <- c("Year", "Strategy", "Return")
annDf$Year <- as.numeric(format(annDf$Year, "%Y"))
pAnn <- ggplot(annDf, aes(x = factor(Year), y = Return * 100, fill = Strategy)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  labs(title = "Annual Returns — DD Switching Strategy",
       subtitle = sprintf("%s → %s | Flip M12⇄M6 at 10%% DD",
                          first(index(combined)), last(index(combined))),
       x = "", y = "Return (%)", caption = "@StockViz") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
ggsave(sprintf("%s/dd_switch_annual.png", reportPath), pAnn, width = 12, height = 6, dpi = 120)

# GT table
tbl <- as.data.frame(t(fm))
tbl$Strategy <- rownames(tbl)
tbl <- tbl |> select(Strategy, everything())
rownames(tbl) <- NULL
gt_tbl <- tbl |> gt() |>
  tab_header(title = "Drawdown-Switching Momentum",
             subtitle = sprintf("%s → %s | Flip M12⇄M6 at 10%% DD",
                                first(index(combined)), last(index(combined)))) |>
  fmt_percent(columns = c(CAGR, Vol, MaxDD), decimals = 2) |>
  fmt_number(columns = c(Sharpe, Calmar), decimals = 2) |>
  tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
  tab_source_note("@StockViz") |>
  tab_style(cell_fill("#f0fff0"), cells_body(rows = grepl("DD Switch", tbl$Strategy))) |>
  tab_style(cell_fill("#fff8e1"), cells_body(rows = grepl(BENCHMARK, tbl$Strategy)))
gtsave(gt_tbl, sprintf("%s/dd_switch_metrics.html", reportPath))
webshot(sprintf("%s/dd_switch_metrics.html", reportPath),
        sprintf("%s/dd_switch_metrics.png", reportPath),
        selector = "table.gt_table", expand = c(10, 10, 10, 10))

# ═══════════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════════
cat("\n===== SUMMARY =====\n")
cat(sprintf("DD Switch (M12⇄M6 at 10%%):  CAGR=%.2f%%  Sharpe=%.2f  MaxDD=%.1f%%\n",
            fm["CAGR",1]*100, fm["Sharpe",1], fm["MaxDD",1]*100))
cat(sprintf("M6:                          CAGR=%.2f%%  Sharpe=%.2f  MaxDD=%.1f%%\n",
            fm["CAGR",2]*100, fm["Sharpe",2], fm["MaxDD",2]*100))
cat(sprintf("M12:                         CAGR=%.2f%%  Sharpe=%.2f  MaxDD=%.1f%%\n",
            fm["CAGR",3]*100, fm["Sharpe",3], fm["MaxDD",3]*100))
cat(sprintf("Benchmark:                   CAGR=%.2f%%  Sharpe=%.2f  MaxDD=%.1f%%\n",
            fm["CAGR",4]*100, fm["Sharpe",4], fm["MaxDD",4]*100))
cat("\n===== END =====\n")

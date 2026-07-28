# ============================================================================
# Omega Alt — Discrete quintile exposure, lookback selection from train
# Exposures: Q1=25%, Q2=50%, Q3=75%, Q4+Q5=100%
# Sweep: lookbacks 20/50/100/200/500 on train, pick best by Sharpe
# ============================================================================

suppressPackageStartupMessages({
  library('quantmod')
  library('PerformanceAnalytics')
  library('xts')
  library('gt')
  library('webshot2')
  library('viridis')
  library('ggthemes')
})

options("scipen" = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/hollandC/StockViz/R/plot.common.r")

reportPath <- "/mnt/data/blog/momentum/volatility-switch/omega-alt"
commonPath <- "/mnt/data/blog/momentum/volatility-switch/common"

CACHE_PATH    <- file.path(commonPath, "cache.rds")
OMEGA_CACHE   <- file.path(commonPath, "omega_cache.rds")
TRAIN_END     <- "2019-12-31"
TEST_START    <- "2020-01-01"
DRAG          <- 0.5 / 100
QUINTILE_N    <- 500L

EXPOSURES     <- c(0.25, 0.50, 0.75, 1.0, 1.0)
LOOKBACKS     <- c(20L, 50L, 100L, 200L, 500L)
INDEX_NAMES   <- c("NIFTY_50_TR",
                     "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR",
                     "NIFTY MIDCAP150 MOMENTUM 50 TR", "MOMENTUM50_TR")

# ── 1. Load ──
cat("\n=== Loading caches ===\n")
cache      <- readRDS(CACHE_PATH)
omegaCache <- readRDS(OMEGA_CACHE)

CACHE_COLS <- setNames(gsub(" ", ".", INDEX_NAMES), INDEX_NAMES)
dailyRets <- na.omit(diff(log(cache[, CACHE_COLS])))
aligned   <- merge(dailyRets, cache$RF, join = "inner")
colnames(aligned) <- c(CACHE_COLS, "RF")
dailyRets <- aligned[, CACHE_COLS]
colnames(dailyRets) <- INDEX_NAMES

computeQuintiles <- function(omegaXts, retXts, lb) {
  n <- nrow(retXts)
  quintiles <- rep(NA_real_, n)
  for (i in (lb + QUINTILE_N):n) {
    omegas <- as.numeric(omegaXts[(i - QUINTILE_N):(i - 1)])
    omegas <- omegas[!is.na(omegas)]; if (length(omegas) < 100) next
    co <- as.numeric(omegaXts[i - 1]); if (is.na(co)) next
    qb <- quantile(omegas, probs = seq(0, 1, 0.2), na.rm = TRUE)
    quintiles[i] <- pmin(findInterval(co, qb, rightmost.closed = TRUE), 5L)
  }
  xts(quintiles, index(retXts))
}

discreteStrategy <- function(qVec, retXts) {
  qClean <- qVec; qClean[is.na(qClean)] <- 1
  baseExp <- xts(EXPOSURES[qClean], index(retXts))
  baseExp[is.na(qVec)] <- NA
  expChg <- abs(baseExp - lag(baseExp, 1))
  na.omit(baseExp * retXts - DRAG * expChg)
}

# ── 2. Train: sweep lookbacks ──
cat("\n=== Train: lookback sweep ===\n")

bestLookback <- list()
perLbTrain <- list()

for (idxName in INDEX_NAMES) {
  cat(sprintf("\n--- %s ---\n", idxName))
  rets <- dailyRets[, idxName, drop = FALSE]
  
  bestLB <- NA; bestSR <- -Inf
  
  for (lb in LOOKBACKS) {
    oKey <- paste0(idxName, "_L", lb)
    qXts <- computeQuintiles(omegaCache[[oKey]], rets, lb)
    qVec <- as.numeric(qXts)
    
    dRet <- discreteStrategy(qVec, rets)
    dTrain <- dRet[paste0("/", TRAIN_END)]
    if (nrow(dTrain) < 60) next
    
    sharpe <- as.numeric(SharpeRatio.annualized(dTrain)[1, 1])
    cagr   <- as.numeric(Return.annualized(dTrain)[1, 1])
    
    cat(sprintf("  L=%d  train Sharpe=%.2f CAGR=%.1f%%\n", lb, sharpe, cagr*100))
    
    perLbTrain[[paste0(idxName, "_L", lb)]] <- list(lb=lb, sharpe=sharpe, cagr=cagr)
    if (sharpe > bestSR) { bestSR <- sharpe; bestLB <- lb }
  }
  
  bestLookback[[idxName]] <- list(lb = bestLB, sharpe = bestSR)
  cat(sprintf("  BEST: L=%d train Sharpe=%.2f\n", bestLB, bestSR))
}

# ── 3. Test: apply best lookback ──
cat("\n=== Test: applying best lookbacks ===\n")

discReturns <- list()
benchReturns <- list()

for (idxName in INDEX_NAMES) {
  bt <- bestLookback[[idxName]]; lb <- bt$lb
  oKey <- paste0(idxName, "_L", lb)
  rets <- dailyRets[, idxName, drop = FALSE]
  
  cat(sprintf("  %s: L=%d\n", idxName, lb))
  
  qXts <- computeQuintiles(omegaCache[[oKey]], rets, lb)
  qVec <- as.numeric(qXts)
  
  dRet <- discreteStrategy(qVec, rets)
  discReturns[[idxName]] <- dRet
  cat(sprintf("    Discrete: %d days\n", nrow(dRet)))
  
  benchReturns[[idxName]] <- rets
}

# ── 4. Metrics (train + test) ──
cat("\n=== Computing metrics ===\n")

computeSplitMetrics <- function(stratXts) {
  train <- stratXts[paste0("/", TRAIN_END)]
  test  <- stratXts[paste0(TEST_START, "/")]
  m <- function(x) {
    if (is.null(x) || nrow(x) < 60) return(rep(NA_real_, 5))
    c(CAGR    = as.numeric(Return.annualized(x)[1, 1]),
      Vol     = as.numeric(sd(x, na.rm = TRUE) * sqrt(252)),
      Sharpe  = as.numeric(SharpeRatio.annualized(x)[1, 1]),
      MaxDD   = as.numeric(maxDrawdown(x)),
      Calmar  = as.numeric(Return.annualized(x)[1, 1] / maxDrawdown(x)))
  }
  c(m(train), m(test))
}

allMetrics <- data.frame(stringsAsFactors = FALSE)

for (idxName in INDEX_NAMES) {
  bt <- bestLookback[[idxName]]; lb <- bt$lb
  
  # Per-lookback train metrics
  for (testLb in LOOKBACKS) {
    plb <- perLbTrain[[paste0(idxName, "_L", testLb)]]
    if (is.null(plb)) next
    
    oKey <- paste0(idxName, "_L", testLb)
    qXts <- computeQuintiles(omegaCache[[oKey]], dailyRets[, idxName, drop = FALSE], testLb)
    qVec <- as.numeric(qXts)
    dLb <- discreteStrategy(qVec, dailyRets[, idxName, drop = FALSE])
    sm <- computeSplitMetrics(dLb)
    
    allMetrics <- rbind(allMetrics, data.frame(
      Index = idxName, Strategy = paste0("Discrete L", testLb), Period = "Train",
      CAGR = sm[1], Vol = sm[2], Sharpe = sm[3], MaxDD = sm[4], Calmar = sm[5],
      stringsAsFactors = FALSE
    ))
  }
  
  # Winning lookback (Test only; Train already covered)
  dm <- computeSplitMetrics(discReturns[[idxName]])
  allMetrics <- rbind(allMetrics, data.frame(
    Index = idxName, Strategy = paste0("Discrete L", lb), Period = "Test",
    CAGR = dm[6], Vol = dm[7], Sharpe = dm[8], MaxDD = dm[9], Calmar = dm[10],
    stringsAsFactors = FALSE
  ))
  
  bm <- computeSplitMetrics(benchReturns[[idxName]])
  allMetrics <- rbind(allMetrics, data.frame(
    Index = idxName, Strategy = "B&H", Period = "Train",
    CAGR = bm[1], Vol = bm[2], Sharpe = bm[3], MaxDD = bm[4], Calmar = bm[5],
    stringsAsFactors = FALSE
  ))
  allMetrics <- rbind(allMetrics, data.frame(
    Index = idxName, Strategy = "B&H", Period = "Test",
    CAGR = bm[6], Vol = bm[7], Sharpe = bm[8], MaxDD = bm[9], Calmar = bm[10],
    stringsAsFactors = FALSE
  ))
}

print(allMetrics, digits = 4)

# ── 5. GT table ──
cat("\n=== Generating GT table ===\n")

for (idxName in INDEX_NAMES) {
  df <- allMetrics[allMetrics$Index == idxName, c("Strategy", "Period", "CAGR", "Vol", "Sharpe", "MaxDD", "Calmar")]
  bt <- bestLookback[[idxName]]
  
  gtTbl <- df |> gt(groupname_col = "Period") |>
    tab_header(
      title = sprintf("Discrete Omega — %s", idxName),
      subtitle = sprintf("L=%d | Train ≤ %s | Test ≥ %s | Drag %.2f%% | 25/50/75/100/100",
                         bt$lb, TRAIN_END, TEST_START, DRAG * 100)
    ) |>
    fmt_percent(columns = c(CAGR, Vol, MaxDD), decimals = 2) |>
    fmt_number(columns = c(Sharpe, Calmar), decimals = 2) |>
    tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
    tab_style(cell_text(weight = "bold", size = "larger"), cells_row_groups()) |>
    tab_style(cell_fill("#E3F2FD"), cells_row_groups()) |>
    tab_style(cell_text(weight = "bold"), cells_body(rows = Strategy == paste0("Discrete L", bt$lb))) |>
    tab_style(cell_fill("#C8E6C9"), cells_body(rows = Strategy == paste0("Discrete L", bt$lb))) |>
    tab_style(cell_fill("#FFF8E1"), cells_body(rows = Strategy == "B&H")) |>
    tab_source_note("@StockViz")
  
  fBase <- file.path(reportPath, paste0("metrics_", gsub(" ", "_", idxName)))
  gtsave(gtTbl, paste0(fBase, ".html"))
  webshot(paste0(fBase, ".html"), paste0(fBase, ".png"),
          selector = "table.gt_table", expand = c(10, 10, 10, 10))
  cat(sprintf("  %s saved\n", idxName))
}

# ── 6. Charts ──
cat("\n=== Generating charts ===\n")

for (idxName in INDEX_NAMES) {
  bt <- bestLookback[[idxName]]; lb <- bt$lb
  
  cd <- intersect(index(discReturns[[idxName]]), index(benchReturns[[idxName]]))
  merged <- na.omit(merge(discReturns[[idxName]][cd], benchReturns[[idxName]][cd]))
  colnames(merged) <- c(paste0("Discrete L", lb), "B&H")
  
  # Test
  testMerged <- merged[paste0(TEST_START, "/")]
  if (nrow(testMerged) >= 60) {
    sr <- sapply(colnames(testMerged), function(cn) round(SharpeRatio.annualized(testMerged[, cn])[1,1], 2))
    Common.PlotCumReturns(testMerged,
      sprintf("Discrete Omega — %s (Test)", idxName),
      sprintf("≥ %s | L=%d | SR: %s", TEST_START, lb,
              paste0(colnames(testMerged), "=", sr, collapse = ", ")),
      file.path(reportPath, paste0("cumulative_test_", gsub(" ", "_", idxName), ".png")), NULL)
    cat(sprintf("  %s chart saved\n", idxName))
  }
}

# ── 7. Combined test metrics ──
cat("\n=== Generating combined test metrics ===\n")

combinedTest <- data.frame(stringsAsFactors = FALSE)
for (idxName in INDEX_NAMES) {
  bt <- bestLookback[[idxName]]; lb <- bt$lb
  df <- allMetrics[allMetrics$Index == idxName & allMetrics$Period == "Test", ]
  
  di <- df[grepl("Discrete", df$Strategy), ]
  bh <- df[df$Strategy == "B&H", ]
  
  combinedTest <- rbind(combinedTest, data.frame(
    Index = idxName,
    Lookback = lb,
    Di_CAGR = di$CAGR, Di_Sharpe = di$Sharpe, Di_MaxDD = di$MaxDD,
    BH_CAGR = bh$CAGR, BH_Sharpe = bh$Sharpe, BH_MaxDD = bh$MaxDD,
    stringsAsFactors = FALSE
  ))
}

gtTbl <- combinedTest |> gt(rowname_col = "Index") |>
  cols_label(Lookback = "L") |>
  tab_header(
    title = "Discrete Omega — Combined Test Metrics",
    subtitle = sprintf("Test ≥ %s | Drag %.2f%% | 25/50/75/100/100", TEST_START, DRAG * 100)
  ) |>
  tab_spanner(label = "Discrete", columns = starts_with("Di_")) |>
  tab_spanner(label = "B&H",      columns = starts_with("BH_")) |>
  fmt_percent(columns = ends_with("CAGR") | ends_with("MaxDD"), decimals = 2) |>
  fmt_number(columns = ends_with("Sharpe"), decimals = 2) |>
  cols_label(
    Di_CAGR = "CAGR", Di_Sharpe = "Sharpe", Di_MaxDD = "MaxDD",
    BH_CAGR = "CAGR", BH_Sharpe = "Sharpe", BH_MaxDD = "MaxDD"
  ) |>
  tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
  tab_style(cell_text(weight = "bold"), cells_column_spanners()) |>
  tab_style(cell_fill("#FFF8E1"), cells_body(columns = starts_with("BH_"))) |>
  tab_source_note("@StockViz")

# Per-row: highlight best CAGR, Sharpe, MaxDD
for (r in 1:nrow(combinedTest)) {
  cagrCols <- c("Di_CAGR", "BH_CAGR")
  bestCagr <- cagrCols[which.max(combinedTest[r, cagrCols])]
  gtTbl <- gtTbl |> tab_style(cell_text(weight = "bold", color = "#1B5E20"),
    cells_body(columns = bestCagr, rows = r))
  
  shrpCols <- c("Di_Sharpe", "BH_Sharpe")
  bestShrp <- shrpCols[which.max(combinedTest[r, shrpCols])]
  gtTbl <- gtTbl |> tab_style(cell_text(weight = "bold", color = "#1B5E20"),
    cells_body(columns = bestShrp, rows = r))
  
  ddCols <- c("Di_MaxDD", "BH_MaxDD")
  bestDD <- ddCols[which.min(combinedTest[r, ddCols])]
  gtTbl <- gtTbl |> tab_style(cell_text(weight = "bold", color = "#B71C1C"),
    cells_body(columns = bestDD, rows = r))
}

fBase <- file.path(reportPath, "metrics_combined_test")
gtsave(gtTbl, paste0(fBase, ".html"))
webshot(paste0(fBase, ".html"), paste0(fBase, ".png"),
        selector = "table.gt_table", expand = c(10, 10, 10, 10))
cat("  combined test metrics saved\n")

# ── 8. Summary ──
cat("\n===== SUMMARY =====\n")
for (idxName in INDEX_NAMES) {
  bt <- bestLookback[[idxName]]
  cat(sprintf("\n%s: best L=%d train Sharpe=%.2f\n", idxName, bt$lb, bt$sharpe))
  df <- allMetrics[allMetrics$Index == idxName, ]
  for (i in 1:nrow(df)) {
    cat(sprintf("  %-16s %-5s CAGR=%.2f%% Sharpe=%.2f MaxDD=%.2f%%\n",
                df$Strategy[i], df$Period[i], df$CAGR[i]*100, df$Sharpe[i], df$MaxDD[i]*100))
  }
}
cat(sprintf("\nOutput: %s/\n", reportPath))

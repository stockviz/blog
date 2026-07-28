# ============================================================================
# Omega Drag Sensitivity — Discrete exposure, sweep drag 0%-0.5%
# Exposures: Q1=25%, Q2=50%, Q3=75%, Q4+Q5=100%
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

reportPath <- "/mnt/data/blog/momentum/volatility-switch/omega-drag-sensitivity"
commonPath <- "/mnt/data/blog/momentum/volatility-switch/common"

CACHE_PATH    <- file.path(commonPath, "cache.rds")
OMEGA_CACHE   <- file.path(commonPath, "omega_cache.rds")
TRAIN_END     <- "2019-12-31"
TEST_START    <- "2020-01-01"
QUINTILE_N    <- 500L

EXPOSURES     <- c(0.25, 0.50, 0.75, 1.0, 1.0)
LOOKBACKS     <- c(20L, 50L, 100L, 200L, 500L)
DRAG_LEVELS   <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
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

discreteStrategy <- function(qVec, retXts, drag) {
  qClean <- qVec; qClean[is.na(qClean)] <- 1
  baseExp <- xts(EXPOSURES[qClean], index(retXts))
  baseExp[is.na(qVec)] <- NA
  expChg <- abs(baseExp - lag(baseExp, 1))
  na.omit(baseExp * retXts - (drag / 100) * expChg)
}

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

# ── 2. Sweep drag levels ──
cat("\n=== Sweeping drag levels ===\n")

allResults <- list()

for (idxName in INDEX_NAMES) {
  cat(sprintf("\n--- %s ---\n", idxName))
  rets <- dailyRets[, idxName, drop = FALSE]
  
  for (drag in DRAG_LEVELS) {
    cat(sprintf("  drag=%.1f%%: ", drag))
    
    bestLB <- NA; bestSR <- -Inf
    
    for (lb in LOOKBACKS) {
      oKey <- paste0(idxName, "_L", lb)
      qXts <- computeQuintiles(omegaCache[[oKey]], rets, lb)
      qVec <- as.numeric(qXts)
      
      dRet <- discreteStrategy(qVec, rets, drag)
      dTrain <- dRet[paste0("/", TRAIN_END)]
      if (nrow(dTrain) < 60) next
      
      sharpe <- as.numeric(SharpeRatio.annualized(dTrain)[1, 1])
      if (sharpe > bestSR) { bestSR <- sharpe; bestLB <- lb }
    }
    
    cat(sprintf("best L=%d train Sharpe=%.2f ", bestLB, bestSR))
    
    # Full period
    oKey <- paste0(idxName, "_L", bestLB)
    qXts <- computeQuintiles(omegaCache[[oKey]], rets, bestLB)
    qVec <- as.numeric(qXts)
    dRet <- discreteStrategy(qVec, rets, drag)
    sm <- computeSplitMetrics(dRet)
    
    # Test only
    cat(sprintf("test Sharpe=%.2f\n", sm[8]))
    
    allResults[[paste0(idxName, "_d", drag*100)]] <- list(
      idx = idxName, drag = drag, lb = bestLB, trainSR = bestSR,
      testCAGR = sm[6], testVol = sm[7], testSharpe = sm[8], testMaxDD = sm[9],
      dRet = dRet, trainAll = sm
    )
  }
}

# Also compute B&H once
bhReturns <- list()
for (idxName in INDEX_NAMES) {
  rets <- dailyRets[, idxName, drop = FALSE]
  sm <- computeSplitMetrics(rets)
  bhReturns[[idxName]] <- list(
    trainCAGR = sm[1], trainSharpe = sm[3], trainMaxDD = sm[4],
    testCAGR = sm[6], testSharpe = sm[8], testMaxDD = sm[9]
  )
}

# ── 3. Metrics table (all drag levels per index) ──
cat("\n=== Generating metrics ===\n")

allMetrics <- data.frame(stringsAsFactors = FALSE)

for (idxName in INDEX_NAMES) {
  for (drag in DRAG_LEVELS) {
    ar <- allResults[[paste0(idxName, "_d", drag*100)]]
    allMetrics <- rbind(allMetrics, data.frame(
      Index = idxName,
      Drag = paste0(drag, "%"),
      CAGR = ar$testCAGR, Vol = ar$testVol, Sharpe = ar$testSharpe,
      MaxDD = ar$testMaxDD, LB = ar$lb,
      stringsAsFactors = FALSE
    ))
  }
  bh <- bhReturns[[idxName]]
  allMetrics <- rbind(allMetrics, data.frame(
    Index = idxName,
    Drag = "B&H",
    CAGR = bh$testCAGR, Vol = NA, Sharpe = bh$testSharpe,
    MaxDD = bh$testMaxDD, LB = "",
    stringsAsFactors = FALSE
  ))
}

# Per-index GT tables
for (idxName in INDEX_NAMES) {
  df <- allMetrics[allMetrics$Index == idxName, c("Drag", "LB", "CAGR", "Sharpe", "MaxDD")]
  
  gtTbl <- df |> gt(rowname_col = "Drag") |>
    tab_header(
      title = sprintf("Drag Sensitivity — %s (Test ≥ %s)", idxName, TEST_START),
      subtitle = "Exposures: 25/50/75/100/100 | Best L selected from train"
    ) |>
    fmt_percent(columns = c(CAGR, MaxDD), decimals = 2) |>
    fmt_number(columns = Sharpe, decimals = 2) |>
    cols_label(LB = "L") |>
    tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
    tab_style(cell_text(weight = "bold"), cells_body(rows = Drag == "B&H")) |>
    tab_style(cell_fill("#FFF8E1"), cells_body(rows = Drag == "B&H")) |>
    tab_style(cell_text(weight = "bold"), cells_stub()) |>
    tab_source_note("@StockViz")
  
  fBase <- file.path(reportPath, paste0("metrics_", gsub(" ", "_", idxName)))
  gtsave(gtTbl, paste0(fBase, ".html"))
  webshot(paste0(fBase, ".html"), paste0(fBase, ".png"),
          selector = "table.gt_table", expand = c(10, 10, 10, 10))
  cat(sprintf("  %s saved\n", idxName))
}

# ── 4. Combined table ──
cat("\n=== Combined test metrics ===\n")

combined <- data.frame(stringsAsFactors = FALSE)
for (idxName in INDEX_NAMES) {
  for (drag in DRAG_LEVELS) {
    ar <- allResults[[paste0(idxName, "_d", drag*100)]]
    combined <- rbind(combined, data.frame(
      Index = idxName, Drag = paste0(drag, "%"), LB = ar$lb,
      CAGR = ar$testCAGR, Sharpe = ar$testSharpe, MaxDD = ar$testMaxDD,
      stringsAsFactors = FALSE
    ))
  }
  bh <- bhReturns[[idxName]]
  combined <- rbind(combined, data.frame(
    Index = idxName, Drag = "B&H", LB = "",
    CAGR = bh$testCAGR, Sharpe = bh$testSharpe, MaxDD = bh$testMaxDD,
    stringsAsFactors = FALSE
  ))
}

gtTbl <- combined |> gt(groupname_col = "Index") |>
  tab_header(
    title = "Drag Sensitivity — Combined Test Metrics",
    subtitle = sprintf("Test ≥ %s | Exposures: 25/50/75/100/100", TEST_START)
  ) |>
  fmt_percent(columns = c(CAGR, MaxDD), decimals = 2) |>
  fmt_number(columns = Sharpe, decimals = 2) |>
  cols_label(Drag = "Drag", LB = "L") |>
  tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
  tab_style(cell_text(weight = "bold", size = "larger"), cells_row_groups()) |>
  tab_style(cell_fill("#E3F2FD"), cells_row_groups()) |>
  tab_style(cell_text(weight = "bold"), cells_body(rows = Drag == "B&H")) |>
  tab_style(cell_fill("#FFF8E1"), cells_body(rows = Drag == "B&H")) |>
  tab_source_note("@StockViz")

fBase <- file.path(reportPath, "metrics_combined")
gtsave(gtTbl, paste0(fBase, ".html"))
webshot(paste0(fBase, ".html"), paste0(fBase, ".png"),
        selector = "table.gt_table", expand = c(10, 10, 10, 10))
cat("  combined saved\n")

# ── 5. Cumulative return charts ──
cat("\n=== Generating charts ===\n")

for (idxName in INDEX_NAMES) {
  cat(sprintf("  %s: ", idxName))
  
  parts <- list()
  for (drag in DRAG_LEVELS) {
    ar <- allResults[[paste0(idxName, "_d", drag*100)]]
    sr <- ar$dRet
    colnames(sr) <- paste0("d=", drag, "% L", ar$lb)
    parts[[length(parts) + 1]] <- sr
  }
  
  # Add B&H
  bh <- dailyRets[, idxName, drop = FALSE]
  colnames(bh) <- "B&H"
  parts[[length(parts) + 1]] <- bh
  
  cd <- Reduce(intersect, lapply(parts, function(x) index(na.omit(x))))
  if (length(cd) >= 60) {
    merged <- do.call(merge, lapply(parts, function(x) na.omit(x[cd])))
    colnames(merged) <- sapply(parts, colnames)
    
    testMerged <- merged[paste0(TEST_START, "/")]
    sr <- sapply(colnames(testMerged), function(cn) round(SharpeRatio.annualized(testMerged[, cn])[1,1], 2))
    
    Common.PlotCumReturns(testMerged,
      sprintf("Drag Sensitivity — %s (Test)", idxName),
      sprintf("≥ %s | SR: %s", TEST_START,
              paste0(names(sr), "=", sr, collapse = ", ")),
      file.path(reportPath, paste0("cumulative_test_", gsub(" ", "_", idxName), ".png")), NULL)
    cat("chart saved\n")
  } else {
    cat("insufficient data\n")
  }
}

# ── 6. Summary ──
cat("\n===== SUMMARY =====\n")
for (idxName in INDEX_NAMES) {
  cat(sprintf("\n%s:\n", idxName))
  cat(sprintf("  B&H: Sharpe=%.2f MaxDD=%.1f%%\n",
              bhReturns[[idxName]]$testSharpe, bhReturns[[idxName]]$testMaxDD*100))
  for (drag in DRAG_LEVELS) {
    ar <- allResults[[paste0(idxName, "_d", drag*100)]]
    cat(sprintf("  d=%.1f%% L=%d: Sharpe=%.2f MaxDD=%.1f%%\n",
                drag, ar$lb, ar$testSharpe, ar$testMaxDD*100))
  }
}
cat(sprintf("\nOutput: %s/\n", reportPath))

# ============================================================================
# consolidated.R — Combined LIQC + Momentum charts (reads saved returns)
# ============================================================================
suppressPackageStartupMessages({
  library('quantmod'); library('PerformanceAnalytics'); library('xts')
  library('tidyverse'); library('lubridate'); library('gt'); library('webshot2')
  library('viridis'); library('ggthemes'); library('scales')
})

pdf(NULL); options("scipen" = 100); options(stringsAsFactors = FALSE)
source("/mnt/data/blog/common/plot.common.r")
source("/mnt/data/blog/momentum/liquidity-improvement/liqim-common.R")

reportPath <- "/mnt/data/blog/momentum/liquidity-improvement"

# Load precomputed returns
q1 <- readRDS(sprintf("%s/q1_liqc.rds", reportPath))
mom <- readRDS(sprintf("%s/momentum.rds", reportPath))

# Combine: benchmark + Q1 + Momentum + Mom-ex-Q5(1m) + Mom-ex-Q5(12m)
combined <- na.omit(do.call(merge.xts,
  list(q1$bench, q1$q1, mom$raw, mom$filt1, mom$filt12)))

# Metrics
cat(sprintf("=== METRICS (%s → %s) ===\n", first(index(combined)), last(index(combined))))
fm <- sapply(colnames(combined), function(cn) computeMetrics(combined[, cn]))
print(round(fm, 4))

# Charts
cat("\n=== CHARTS ===\n")
makeCumretChart(combined, "LIQIM + Momentum (top 60%)",
  sprintf("%s/consolidated_cumret.png", reportPath))
makeAnnualChart(combined, "LIQIM + Momentum (top 60%)",
  sprintf("%s/consolidated_annual.png", reportPath))

# Metrics table
cat("\n=== TABLE ===\n")
makeGtTable(fm, "LIQIM + Momentum (top 60%)",
  sprintf("%s/consolidated_metrics.png", reportPath), reportPath)

cat("\n===== DONE =====\n")

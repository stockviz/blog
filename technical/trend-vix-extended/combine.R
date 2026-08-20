suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(PerformanceAnalytics)
})
source("build.R")
source("backtest.R")

# Combination test: the best single additions are broad large-cap indices.
# NIFTY 200 TR and NIFTY TOTAL MARKET TR are near-duplicates; NIFTY 100 TR and
# NIFTY 500 TR are broader/smaller complements. Test a few non-redundant combos.

BASE_NAMES <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR")
COST_RATE <- 0.0025
TMP_CACHE <- file.path(tempdir(), "trend-vix-extended-search.rds")

COMBOS <- list(
  "Base + NIFTY 200"                    = c(BASE_NAMES, "NIFTY 200 TR"),
  "Base + NIFTY TOTAL MARKET"           = c(BASE_NAMES, "NIFTY TOTAL MARKET TR"),
  "Base + NIFTY 100"                    = c(BASE_NAMES, "NIFTY 100 TR"),
  "Base + NIFTY 500"                    = c(BASE_NAMES, "NIFTY 500 TR"),
  "Base + NIFTY 200 + NIFTY 100"        = c(BASE_NAMES, "NIFTY 200 TR", "NIFTY 100 TR"),
  "Base + NIFTY 200 + NIFTY 500"        = c(BASE_NAMES, "NIFTY 200 TR", "NIFTY 500 TR"),
  "Base + NIFTY 100 + NIFTY 500"        = c(BASE_NAMES, "NIFTY 100 TR", "NIFTY 500 TR"),
  "Base + 200 + 100 + 500"              = c(BASE_NAMES, "NIFTY 200 TR", "NIFTY 100 TR", "NIFTY 500 TR"),
  "Base + 200 + TOTAL + 100"            = c(BASE_NAMES, "NIFTY 200 TR", "NIFTY TOTAL MARKET TR", "NIFTY 100 TR")
)

make_index_sql <- function(names) {
  paste0("SELECT index_name, time_stamp, px_close FROM bhav_index WHERE index_name IN (",
         paste(sprintf("'%s'", names), collapse = ","),
         ") ORDER BY time_stamp, index_name")
}
train_metrics <- function(daily) {
  x <- daily[paste0("/", TRAIN_END)]
  if (NROW(x) < 20L) return(c(CAGR = NA_real_, Sharpe = NA_real_, MaxDD = NA_real_))
  c(CAGR = as.numeric(Return.annualized(x, scale = 252, geometric = TRUE)),
    Sharpe = as.numeric(SharpeRatio.annualized(x, scale = 252)),
    MaxDD = as.numeric(maxDrawdown(x)))
}
run_strategies <- function(cache) {
  out <- list()
  for (adaptive in c(TRUE, FALSE))
    for (top_n in c(1L, 2L)) {
      r <- run_cross_portfolio(cache, adaptive, top_n, COST_RATE)
      out[[sprintf("%s Top %d", if (adaptive) "VIX" else "10M", top_n)]] <- train_metrics(r$daily)
    }
  out
}

cat("Combination test (train set, 25 bps):\n")
results <- list()
for (nm in names(COMBOS)) {
  cat(sprintf("  %-30s", nm)); flush.console()
  INDEX_NAMES <- COMBOS[[nm]]
  INDEX_SQL   <- make_index_sql(INDEX_NAMES)
  capture.output(cache <- build_cache(TMP_CACHE))
  results[[nm]] <- run_strategies(cache)
  m <- results[[nm]]
  cat(sprintf(" VIX T1: %.2f Sharpe / %.1f%% MaxDD | VIX T2: %.2f Sharpe / %.1f%% MaxDD\n",
              m[["VIX Top 1"]]["Sharpe"], m[["VIX Top 1"]]["MaxDD"]*100,
              m[["VIX Top 2"]]["Sharpe"], m[["VIX Top 2"]]["MaxDD"]*100))
  flush.console()
}

cat("\n=== Detailed (train, 25 bps) ===\n")
cat(sprintf("%-32s %10s %8s %8s | %10s %8s %8s\n",
            "Universe", "VIX1_CAGR%", "VIX1_Sh", "VIX1_DD%", "VIX2_CAGR%", "VIX2_Sh", "VIX2_DD%"))
for (nm in names(COMBOS)) {
  m <- results[[nm]]
  cat(sprintf("%-32s %10.2f %8.2f %8.1f | %10.2f %8.2f %8.1f\n",
              nm,
              m[["VIX Top 1"]]["CAGR"]*100, m[["VIX Top 1"]]["Sharpe"], m[["VIX Top 1"]]["MaxDD"]*100,
              m[["VIX Top 2"]]["CAGR"]*100, m[["VIX Top 2"]]["Sharpe"], m[["VIX Top 2"]]["MaxDD"]*100))
}

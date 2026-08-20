suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(PerformanceAnalytics)
})
source("build.R")
source("backtest.R")

# ─────────────────────────────────────────────────────────────────────────────
# Single-addition search: for each candidate broad-based TR index, add it to the
# base 3-index universe and evaluate TRAIN-set performance only (through
# TRAIN_END). Selection is made on the training set, never the test set.
# ─────────────────────────────────────────────────────────────────────────────

BASE_NAMES <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR")
CANDIDATES <- c(
  "NIFTY 500 TR",
  "NIFTY NEXT 50 TR",
  "NIFTY 100 TR",
  "NIFTY MIDCAP 100 TR",
  "NIFTY SMALLCAP 50 TR",
  "NIFTY500 MULTICAP 50:25:25 TR",
  "NIFTY 200 TR",
  "NIFTY MIDCAP 50 TR",
  "NIFTY SMALLCAP 100 TR",
  "NIFTY MICROCAP 250 TR",
  "NIFTY MIDSMALLCAP 400 TR",
  "NIFTY MIDSMALLCAP400 50:50 TR",
  "NIFTY SMALLCAP 500 TR",
  "NIFTY TOTAL MARKET TR",
  "NIFTY500 LARGEMIDSMALL EQUAL-CAP WEIGHTED TR",
  "NIFTY MIDCAP SELECT TR",
  "NIFTY LARGEMIDCAP 250 TR"
)
COST_RATE <- 0.0025  # 25 bps
TMP_CACHE <- file.path(tempdir(), "trend-vix-extended-search.rds")

make_index_sql <- function(names) {
  paste0(
    "SELECT index_name, time_stamp, px_close FROM bhav_index WHERE index_name IN (",
    paste(sprintf("'%s'", names), collapse = ","),
    ") ORDER BY time_stamp, index_name"
  )
}

train_metrics <- function(daily) {
  x <- daily[paste0("/", TRAIN_END)]
  if (NROW(x) < 20L) return(c(CAGR = NA_real_, Sharpe = NA_real_, MaxDD = NA_real_))
  c(
    CAGR   = as.numeric(Return.annualized(x, scale = 252, geometric = TRUE)),
    Sharpe = as.numeric(SharpeRatio.annualized(x, scale = 252)),
    MaxDD  = as.numeric(maxDrawdown(x))
  )
}

# run the four cross strategies on one cache, return named train metrics
run_strategies <- function(cache) {
  out <- list()
  for (adaptive in c(TRUE, FALSE)) {
    for (top_n in c(1L, 2L)) {
      r <- run_cross_portfolio(cache, adaptive, top_n, COST_RATE)
      key <- sprintf("%s Top %d", if (adaptive) "VIX" else "10M", top_n)
      out[[key]] <- train_metrics(r$daily)
    }
  }
  out
}

universes <- c(list(Baseline = BASE_NAMES),
               setNames(lapply(CANDIDATES, function(x) c(BASE_NAMES, x)), CANDIDATES))

cat(sprintf("Searching %d universes on TRAIN set (through %s) at %d bps ...\n",
            length(universes), TRAIN_END, COST_RATE * 10000))
flush.console()

results <- list()
for (nm in names(universes)) {
  cat(sprintf("  %-45s", nm)); flush.console()
  INDEX_NAMES <- universes[[nm]]
  INDEX_SQL   <- make_index_sql(INDEX_NAMES)
  capture.output(cache <- build_cache(TMP_CACHE))
  metrics <- run_strategies(cache)
  results[[nm]] <- metrics
  cat(sprintf(" VIX T1 Sharpe %.2f | VIX T2 Sharpe %.2f | T1 MaxDD %.1f%% | T2 MaxDD %.1f%%\n",
              metrics[["VIX Top 1"]]["Sharpe"], metrics[["VIX Top 2"]]["Sharpe"],
              metrics[["VIX Top 1"]]["MaxDD"] * 100, metrics[["VIX Top 2"]]["MaxDD"] * 100))
  flush.console()
}

base <- results[["Baseline"]]

rows <- lapply(names(results), function(nm) {
  m <- results[[nm]]
  data.frame(
    Universe = nm,
    VIX1_CAGR = m[["VIX Top 1"]]["CAGR"], VIX1_Sharpe = m[["VIX Top 1"]]["Sharpe"], VIX1_MaxDD = m[["VIX Top 1"]]["MaxDD"],
    VIX2_CAGR = m[["VIX Top 2"]]["CAGR"], VIX2_Sharpe = m[["VIX Top 2"]]["Sharpe"], VIX2_MaxDD = m[["VIX Top 2"]]["MaxDD"],
    M10_1_CAGR = m[["10M Top 1"]]["CAGR"], M10_1_Sharpe = m[["10M Top 1"]]["Sharpe"], M10_1_MaxDD = m[["10M Top 1"]]["MaxDD"],
    M10_2_CAGR = m[["10M Top 2"]]["CAGR"], M10_2_Sharpe = m[["10M Top 2"]]["Sharpe"], M10_2_MaxDD = m[["10M Top 2"]]["MaxDD"],
    stringsAsFactors = FALSE
  )
})
tbl <- do.call(rbind, rows)
tbl$dVIX1_MaxDD <- (tbl$VIX1_MaxDD - base[["VIX Top 1"]]["MaxDD"]) * 100
tbl$dVIX1_CAGR  <- (tbl$VIX1_CAGR  - base[["VIX Top 1"]]["CAGR"])  * 100
tbl$dVIX2_MaxDD <- (tbl$VIX2_MaxDD - base[["VIX Top 2"]]["MaxDD"]) * 100
tbl$dVIX2_CAGR  <- (tbl$VIX2_CAGR  - base[["VIX Top 2"]]["CAGR"])  * 100

write.csv(tbl, file.path(REPORT_PATH, "train-search-results.csv"), row.names = FALSE)

cat("\n=== Baseline (3 indices) train metrics, 25 bps, through", format(TRAIN_END), "===\n")
cat(sprintf("  VIX Top 1: CAGR %+.2f%%  Sharpe %+.2f  MaxDD %.2f%%\n",
            base[["VIX Top 1"]]["CAGR"]*100, base[["VIX Top 1"]]["Sharpe"], base[["VIX Top 1"]]["MaxDD"]*100))
cat(sprintf("  VIX Top 2: CAGR %+.2f%%  Sharpe %+.2f  MaxDD %.2f%%\n",
            base[["VIX Top 2"]]["CAGR"]*100, base[["VIX Top 2"]]["Sharpe"], base[["VIX Top 2"]]["MaxDD"]*100))

cat("\n=== Candidate additions ranked by VIX Top 2 MaxDD improvement ===\n")
cat(sprintf("  %-40s %8s %8s | %8s %8s\n", "Added index", "dMaxDD%", "dCAGR%", "Sharpe", "CAGR%"))
for (i in order(tbl$dVIX2_MaxDD)) {
  if (tbl$Universe[i] == "Baseline") next
  cat(sprintf("  %-40s %+8.2f %+8.2f | %8.2f %8.2f\n",
              tbl$Universe[i], tbl$dVIX2_MaxDD[i], tbl$dVIX2_CAGR[i],
              tbl$VIX2_Sharpe[i], tbl$VIX2_CAGR[i]*100))
}

cat("\n=== Candidate additions ranked by VIX Top 1 MaxDD improvement ===\n")
for (i in order(tbl$dVIX1_MaxDD)) {
  if (tbl$Universe[i] == "Baseline") next
  cat(sprintf("  %-40s %+8.2f %+8.2f | %8.2f %8.2f\n",
              tbl$Universe[i], tbl$dVIX1_MaxDD[i], tbl$dVIX1_CAGR[i],
              tbl$VIX1_Sharpe[i], tbl$VIX1_CAGR[i]*100))
}

cat(sprintf("\nResults saved to %s\n", file.path(REPORT_PATH, "train-search-results.csv")))

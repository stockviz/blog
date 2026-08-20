suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})
source("build.R")
source("backtest.R")

RESULTS_PATH <- file.path(REPORT_PATH, "lookback-results.rds")
SWEEP_LOOKBACKS <- 1:12
TOP_NS <- c(1L, 2L)
COST_LEVELS <- c(`0` = 0, `25` = 0.0025)
TUNED_REGIME_LOOKBACKS <- c(Green = 10L, Yellow = 6L, Red = 1L)

key_for <- function(cost_name, top_n, lookback) {
  paste(cost_name, paste0("Top", top_n), paste0("L", lookback), sep = "_")
}

run_sweep <- function() {
  test_status <- system2("Rscript", "tests.R")
  if (!identical(test_status, 0L)) stop("Synthetic tests failed")

  cache <- build_cache(CACHE_PATH)
  validate_cache_fingerprint(cache)

  results <- list()
  for (cost_name in names(COST_LEVELS)) {
    rate <- COST_LEVELS[[cost_name]]
    for (top_n in TOP_NS) {
      for (lb in SWEEP_LOOKBACKS) {
        res <- run_fixed_lookback_portfolio(cache, top_n, lb, rate)
        results[[key_for(cost_name, top_n, lb)]] <- list(
          cost_name = cost_name, cost_rate = rate,
          top_n = top_n, lookback = lb,
          strategy = res$strategy, daily = res$daily, audit = res$audit
        )
      }
    }
  }

  # Consistency check: the swept L=10 Top 1 must reproduce the original fixed-10M control.
  orig_fixed <- run_cross_portfolio(cache, adaptive = FALSE, top_n = 1L,
                                    cost_rate = COST_LEVELS[["25"]])
  sweep_l10 <- results[[key_for("25", 1L, 10L)]]
  stopifnot(identical(as.numeric(sweep_l10$daily), as.numeric(orig_fixed$daily)))
  stopifnot(identical(as.Date(index(sweep_l10$daily)), as.Date(index(orig_fixed$daily))))
  cat("Consistency check passed: swept L10 Top 1 == original fixed 10M Top 1\n")

  test_comparison <- list()
  for (top_n in TOP_NS) {
    top_key <- paste0("Top", top_n)
    test_comparison[[top_key]] <- list(
      tuned = run_cross_portfolio(
        cache, adaptive = TRUE, top_n = top_n, cost_rate = COST_LEVELS[["25"]],
        regime_lookbacks = TUNED_REGIME_LOOKBACKS, strategy_label = "Train-Tuned 10/6/1"
      ),
      original = run_cross_portfolio(
        cache, adaptive = TRUE, top_n = top_n, cost_rate = COST_LEVELS[["25"]],
        strategy_label = "Original 10/3/1"
      ),
      fixed = run_cross_portfolio(
        cache, adaptive = FALSE, top_n = top_n, cost_rate = COST_LEVELS[["25"]],
        strategy_label = "Fixed 10M"
      )
    )
    tuned_audit <- test_comparison[[top_key]]$tuned$audit
    observed <- unique(tuned_audit[, c("regime", "lookback_months")])
    observed <- observed[order(observed$regime), ]
    expected <- data.frame(
      regime = names(TUNED_REGIME_LOOKBACKS),
      lookback_months = as.integer(TUNED_REGIME_LOOKBACKS),
      stringsAsFactors = FALSE
    )
    expected <- expected[order(expected$regime), ]
    rownames(observed) <- NULL
    rownames(expected) <- NULL
    stopifnot(identical(observed, expected))
  }
  cat("Train-tuned 10/6/1 rule verified for Green/Yellow/Red audit rows\n")

  out <- list(
    generated_at = Sys.time(),
    cache_fingerprint = cache$fingerprint,
    train_end = TRAIN_END,
    test_start = TEST_START,
    sweep_lookbacks = SWEEP_LOOKBACKS,
    top_ns = TOP_NS,
    cost_levels = COST_LEVELS,
    tuned_regime_lookbacks = TUNED_REGIME_LOOKBACKS,
    results = results,
    test_comparison = test_comparison,
    coverage = cache$coverage
  )
  saveRDS(out, RESULTS_PATH)
  cat(sprintf("Lookback sweep complete: %d strategy runs saved to %s\n",
              length(results), RESULTS_PATH))
  invisible(out)
}

if (sys.nframe() == 0L) {
  run_sweep()
}

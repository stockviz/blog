source("build.R")
source("analysis.R")

RESULTS_PATH <- file.path(REPORT_PATH, "results.rds")

run_all <- function() {
  test_status <- system2("Rscript", "tests.R")
  if (!identical(test_status, 0L)) stop("Synthetic tests failed")
  cache <- build_cache(CACHE_PATH)
  validate_cache_fingerprint(cache)
  results <- run_analysis()
  cat(sprintf("Saved: %s, %s, forward-returns.csv + charts\n", CACHE_PATH, RESULTS_PATH))
  invisible(results)
}

if (sys.nframe() == 0L) {
  run_all()
}

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})
source("build.R")
source("backtest.R")

RESULTS_PATH <- file.path(REPORT_PATH, "backtest-results.rds")
AUDIT_PATH <- file.path(REPORT_PATH, "audit-monthly.csv")
DAILY_CSV_PATH <- file.path(REPORT_PATH, "daily-returns.csv")
DAILY_RDS_PATH <- file.path(REPORT_PATH, "daily-returns.rds")
COST_LEVELS <- c(`0` = 0, `10` = 0.001, `25` = 0.0025, `50` = 0.005)
TOP_NS <- c(1L, 2L, 3L, 4L)

rbind_fill <- function(parts) {
  all_names <- unique(unlist(lapply(parts, names)))
  padded <- lapply(parts, function(x) {
    missing <- setdiff(all_names, names(x))
    for (nm in missing) x[[nm]] <- NA
    x[, all_names, drop = FALSE]
  })
  do.call(rbind, padded)
}

merge_daily_results <- function(result_list) {
  series <- lapply(result_list, function(x) x$daily)
  common_dates <- Reduce(intersect, lapply(series, function(x) as.Date(index(x))))
  if (length(common_dates) == 0L) stop("No common dates among strategies")
  do.call(merge, lapply(series, function(x) x[as.character(common_dates)]))
}

run_all <- function() {
  test_status <- system2("Rscript", "tests.R")
  if (!identical(test_status, 0L)) stop("Synthetic tests failed")

  cache <- build_cache(CACHE_PATH)
  validate_cache_fingerprint(cache)
  cross <- list()
  all_audits <- list()
  audit_i <- 0L

  for (cost_name in names(COST_LEVELS)) {
    rate <- COST_LEVELS[[cost_name]]
    cost_key <- paste0(cost_name, "bps")
    cross[[cost_key]] <- list()
    for (adaptive in c(TRUE, FALSE)) {
      for (top_n in TOP_NS) {
        result <- run_cross_portfolio(cache, adaptive, top_n, rate)
        result_key <- gsub(" ", "_", result$strategy)
        cross[[cost_key]][[result_key]] <- result
        audit_i <- audit_i + 1L
        all_audits[[audit_i]] <- result$audit
      }
    }
  }

  primary_daily <- merge_daily_results(cross[["25bps"]])
  benchmark <- run_equal_weight(cache, first(index(primary_daily)), last(index(primary_daily)))
  common_dates <- intersect(as.Date(index(primary_daily)), as.Date(index(benchmark)))
  primary_daily <- merge(primary_daily[as.character(common_dates)], benchmark[as.character(common_dates)])

  split_counts <- c(
    train = NROW(primary_daily[paste0("/", TRAIN_END)]),
    test = NROW(primary_daily[paste0(TEST_START, "/")]),
    full = NROW(primary_daily)
  )
  if (any(split_counts == 0L)) stop("Train, test, and full periods must all be non-empty")
  if (any(!is.finite(as.numeric(primary_daily)))) stop("Primary daily returns contain non-finite values")

  audit <- rbind_fill(all_audits)
  results <- list(
    generated_at = Sys.time(),
    cache_fingerprint = cache$fingerprint,
    train_end = TRAIN_END,
    test_start = TEST_START,
    cost_levels = COST_LEVELS,
    top_ns = TOP_NS,
    primary_cost_bps = 25,
    cross = cross,
    benchmark = benchmark,
    primary_daily = primary_daily,
    audit = audit,
    split_counts = split_counts,
    coverage = cache$coverage
  )
  saveRDS(results, RESULTS_PATH)
  write.csv(audit, AUDIT_PATH, row.names = FALSE, na = "")
  write.csv(data.frame(date = as.Date(index(primary_daily)), coredata(primary_daily),
                       check.names = FALSE), DAILY_CSV_PATH, row.names = FALSE)
  saveRDS(primary_daily, DAILY_RDS_PATH)
  cat(sprintf("Backtest complete: %d primary daily rows, %d audit rows\n",
              NROW(primary_daily), nrow(audit)))
  cat(sprintf("Split rows — train: %d, test: %d, full: %d\n",
              split_counts["train"], split_counts["test"], split_counts["full"]))
  cat(sprintf("Saved: %s, %s, %s, %s\n",
              RESULTS_PATH, AUDIT_PATH, DAILY_CSV_PATH, DAILY_RDS_PATH))
  invisible(results)
}

if (sys.nframe() == 0L) {
  run_all()
}

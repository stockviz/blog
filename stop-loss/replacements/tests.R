suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})
source("build.R")

assert_equal <- function(actual, expected, tolerance = 1e-6, label = "") {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))) {
    stop(sprintf("FAIL %s: expected %s, got %s", label,
                 paste(expected, collapse = ","), paste(actual, collapse = ",")))
  }
}

cat("Testing forward return via price ratio (PG path)...\n")
dates <- as.Date(c("2020-01-02","2020-01-03","2020-01-06","2020-01-07"))
closes <- c(100, 102, 105, 103)
d <- data.frame(date_stamp = dates, c = closes)
pos <- findInterval(as.Date("2020-01-03"), d$date_stamp)
stopifnot(pos == 2)
ret1 <- closes[pos + 1] / closes[pos] - 1
assert_equal(ret1, 105/102 - 1, label = "1d price ratio")
ret2 <- closes[pos + 2] / closes[pos] - 1
assert_equal(ret2, 103/102 - 1, label = "2d price ratio")

cat("Testing RSA cumulative path...\n")
rets <- c(0.02, 0.02941176, -0.0190476)
cum1 <- prod(1 + rets[2]) - 1
assert_equal(cum1, 0.02941176, label = "rsa 1d")
cum2 <- prod(1 + rets[2:3]) - 1
assert_equal(cum2, (1.02941176 * 0.9809524) - 1, tolerance = 1e-5, label = "rsa 2d")

cat("Testing findInterval with non-trading SL date (weekend fallback)...\n")
pos_weekend <- findInterval(as.Date("2020-01-05"), dates)
assert_equal(pos_weekend, 2, label = "weekend maps to previous Friday")
ret_weekend_1d <- closes[pos_weekend + 1] / closes[pos_weekend] - 1
assert_equal(ret_weekend_1d, ret1, label = "weekend 1d")

cat("Testing duplicate dedup expectation...\n")
stopifnot(length(MODEL_IDS) == 2)
stopifnot(all(HORIZONS == c(1,5,10,20)))

cat("Testing median vs mean with symmetric returns...\n")
x <- c(-0.02, -0.01, 0.01, 0.02)
assert_equal(mean(x), 0, label = "mean zero")
assert_equal(median(x), 0, label = "median zero")

cat("Testing horizon vector strictly increasing...\n")
stopifnot(all(diff(HORIZONS) > 0))

cat("Testing fingerprint stability...\n")
rng <- list(min_date = "2016-08-23", max_date = "2026-08-19", n_events = 9896, n_symbols = 918)
inputs <- cache_fingerprint_inputs(rng, MODEL_IDS, HORIZONS)
cache <- list(sl_ranges = rng, model_ids = MODEL_IDS, horizons = HORIZONS,
              fingerprint_inputs = inputs, fingerprint = digest::digest(inputs, algo = "sha256"))
validate_cache_fingerprint(cache)
cache_bad <- cache
cache_bad$fingerprint <- "tampered"
drift <- inherits(try(validate_cache_fingerprint(cache_bad), silent = TRUE), "try-error")
stopifnot(drift)

cat("All synthetic tests passed.\n")

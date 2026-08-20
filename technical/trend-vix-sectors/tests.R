suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})
source("build.R")
source("backtest.R")

stopifnot(identical(TEST_START, as.Date("2020-05-01")))
stopifnot(length(INDEX_NAMES) == 34L)
stopifnot(all(grepl(" TR$", INDEX_NAMES)))

assert_equal <- function(actual, expected, tolerance = 1e-10, label = "") {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))) {
    stop(sprintf("FAIL %s: expected %s, got %s", label,
                 paste(expected, collapse = ","), paste(actual, collapse = ",")))
  }
}

cat("Testing point-in-time cash mapping...\n")
nav <- xts(c(10, 10.1, 10.2), as.Date(c("2020-01-01", "2020-01-03", "2020-01-06")))
equity_dates <- as.Date(c("2020-01-02", "2020-01-03", "2020-01-05", "2020-01-06"))
mapped <- map_point_in_time(equity_dates, nav)
assert_equal(as.numeric(mapped), c(10, 10.1, 10.1, 10.2), label = "cash mapping")
stopifnot(all(attr(mapped, "source_dates") <= equity_dates))

cat("Testing month-end selection...\n")
x <- xts(1:5, as.Date(c("2020-01-02", "2020-01-31", "2020-02-03", "2020-02-27", "2020-02-28")))
me <- month_end_rows(x)
assert_equal(as.numeric(as.Date(index(me))),
             as.numeric(as.Date(c("2020-01-31", "2020-02-28"))), label = "month-end dates")
assert_equal(as.numeric(me), c(2, 5), label = "month ends")

cat("Testing VIX regime boundaries...\n")
stopifnot(identical(classify_regime(20, 18), "Green"))
stopifnot(identical(classify_regime(31.999, 18.001), "Yellow"))
stopifnot(identical(classify_regime(32, 18.001), "Red"))
stopifnot(identical(classify_regime(40, 18), "Green"))

cat("Testing monthly momentum...\n")
lev <- xts(matrix(c(100,110,121,133.1,146.41), ncol = 1),
           as.Date(c("2020-01-31","2020-02-28","2020-03-31","2020-04-30","2020-05-29")))
assert_equal(as.numeric(month_momentum(lev, 1))[2:5], rep(0.1, 4), label = "1m momentum")
assert_equal(as.numeric(month_momentum(lev, 3))[4:5], rep(0.331, 2), label = "3m momentum")

cat("Testing staggered momentum (NA before inception)...\n")
# Index B only has levels from month 3 onward; 3-month momentum needs 3 prior
# month-end levels, so B's momentum is NA until month 6 (start + lookback).
d7 <- as.Date(c("2020-01-31","2020-02-28","2020-03-31","2020-04-30","2020-05-29","2020-06-30","2020-07-31"))
stag <- xts(matrix(c(100, 110, 121, 133.1, 146.41, 161.051, 177.1561,
                     NA,  NA,  100, 110,   121,    133.1,   146.41),
                   ncol = 2), d7)
colnames(stag) <- c("A", "B")
m3 <- month_momentum(stag, 3L)
stopifnot(all(is.na(as.numeric(m3[1:5, "B"]))))
stopifnot(all(is.finite(as.numeric(m3[6:7, "B"]))))
assert_equal(as.numeric(m3[6:7, "B"]), c(0.331, 0.331), label = "staggered 3m momentum")
stopifnot(all(is.finite(as.numeric(m3[4:7, "A"]))))

cat("Testing deterministic ranking and cash substitution...\n")
assets <- c("A", "B", "CASH")
w_tie <- weights_from_momentum(c(A = 0.1, B = 0.1, CASH = 0.01), 1, assets)
assert_equal(w_tie, c(A = 1, B = 0, CASH = 0), label = "tie order")
w_top2 <- weights_from_momentum(c(A = 0.2, B = 0.1, CASH = 0.01), 2, assets)
assert_equal(w_top2, c(A = 0.5, B = 0.5, CASH = 0), label = "top2")
w_neg <- weights_from_momentum(c(A = -0.1, B = -0.2, CASH = -0.3), 2, assets)
assert_equal(w_neg, c(A = 0, B = 0, CASH = 1), label = "negative risk to cash")
stopifnot(abs(sum(w_neg) - 1) < 1e-12)

cat("Testing staggered availability: NA scores are excluded...\n")
# Three sectors; C has not yet launched (NA). Top 2 must pick A and B only.
w_stag <- weights_from_momentum(c(A = 0.2, B = 0.1, C = NA_real_, CASH = 0.01), 2,
                                c("A", "B", "C", "CASH"))
assert_equal(w_stag, c(A = 0.5, B = 0.5, C = 0, CASH = 0), label = "NA excluded")

cat("Testing effective top_n caps at available assets...\n")
stopifnot(identical(effective_top_n(c(A = 0.1, B = 0.2, C = NA_real_, CASH = 0.01), 4L), 3L))
stopifnot(identical(effective_top_n(c(A = 0.1, B = 0.2, CASH = 0.01), 4L), 3L))
stopifnot(identical(effective_top_n(c(A = 0.1, B = 0.2, CASH = 0.01), 2L), 2L))

cat("Testing signal-to-holding timing...\n")
dates <- as.Date(c("2020-01-31", "2020-02-03", "2020-02-28", "2020-03-02"))
hd <- holding_dates_after_signal(as.Date("2020-01-31"), dates)
stopifnot(identical(hd, as.Date(c("2020-02-03", "2020-02-28"))))
stopifnot(all(hd > as.Date("2020-01-31")))

cat("Testing daily P&L and transaction costs...\n")
rets <- xts(matrix(c(0.01, 0.02, -0.01, 0.00), ncol = 2, byrow = TRUE,
                   dimnames = list(NULL, c("A", "CASH"))),
            as.Date(c("2020-02-03", "2020-02-04")))
w <- c(A = 0.5, CASH = 0.5)
p0 <- apply_month_weights(rets, w, old_weights = c(A = 0, CASH = 1), cost_rate = 0)
p10 <- apply_month_weights(rets, w, old_weights = c(A = 0, CASH = 1), cost_rate = 0.001)
p25 <- apply_month_weights(rets, w, old_weights = c(A = 0, CASH = 1), cost_rate = 0.0025)
p50 <- apply_month_weights(rets, w, old_weights = c(A = 0, CASH = 1), cost_rate = 0.005)
assert_equal(as.numeric(p0$gross), c(0.015, -0.005), label = "gross pnl")
assert_equal(p10$turnover, 0.5, label = "turnover")
assert_equal(as.numeric(p10$net)[1], 0.0145, label = "10bp cost")
assert_equal(as.numeric(p25$net)[1], 0.01375, label = "25bp cost")
assert_equal(as.numeric(p50$net)[1], 0.0125, label = "50bp cost")
stopifnot(prod(1 + p0$net) >= prod(1 + p10$net))
stopifnot(prod(1 + p10$net) >= prod(1 + p25$net))
stopifnot(prod(1 + p25$net) >= prod(1 + p50$net))

cat("Testing held-only selection avoids 0 * NA on staggered returns...\n")
# A is held (weight 1), B is a not-yet-launched sector (NA, weight 0).
stag_rets <- xts(matrix(c(0.01, NA, 0.02, NA), ncol = 2, byrow = TRUE),
                 as.Date(c("2020-02-03", "2020-02-04")))
colnames(stag_rets) <- c("A", "B")
w_full <- c(A = 1, B = 0, CASH = 0)
pnl <- apply_month_weights(stag_rets, w_full, old_weights = c(A = 0, B = 0, CASH = 1), cost_rate = 0)
assert_equal(as.numeric(pnl$gross), c(0.01, 0.02), label = "held-only gross")
stopifnot(all(is.finite(as.numeric(pnl$gross))))

cat("Testing bad-tick cleaning...\n")
clean_x <- xts(c(100, 110, 500, 108, 118),
               as.Date(c("2020-01-02","2020-01-03","2020-01-06","2020-01-07","2020-01-08")))
res <- clean_index_levels(clean_x, "TEST")
assert_equal(as.numeric(res$x)[3], 110, label = "spike carry-forward")
stopifnot(!anyNA(as.numeric(res$x)))
stopifnot(identical(length(res$notes), 1L))
reb <- xts(c(1684, 1000, 1005), as.Date(c("2006-12-28","2006-12-29","2007-01-02")))
r2 <- clean_index_levels(reb, "REBASE")
assert_equal(as.numeric(r2$x)[1], 1000, tolerance = 1e-6, label = "re-base back-adjust")
assert_equal(as.numeric(r2$x)[2], 1000, tolerance = 1e-6, label = "re-base day")

cat("Testing train-only percentile calibration...\n")
sig <- list(
  dates = as.Date(c("2018-01-31", "2019-01-31", "2020-01-31")),
  vix_sma20 = c(20, 30, 999),
  vix_sma40 = c(10, 20, 999)
)
thr <- calibrate_percentile_thresholds(sig, as.Date("2019-12-31"), 0.5, 0.9)
sig$vix_sma20[3] <- -999
sig$vix_sma40[3] <- -999
thr_changed_post <- calibrate_percentile_thresholds(sig, as.Date("2019-12-31"), 0.5, 0.9)
assert_equal(unlist(thr), unlist(thr_changed_post), label = "train-only thresholds")
stopifnot(identical(apply_percentile_regime(c(20, 40, 40), c(10, 20, 30),
                                             list(green = 15, red = 35)),
                    c("Green", "Red", "Red")))

cat("Testing checkpoint rejects current-configuration drift...\n")
ranges <- list(index = setNames(list(c("2000-01-01", "2020-01-01")), INDEX_NAMES[1]),
               cash = c("2000-01-01", "2020-01-01"),
               vix = c("2000-01-01", "2020-01-01"))
inputs <- cache_fingerprint_inputs(ranges)
fake_cache <- list(raw_source_ranges = ranges, fingerprint_inputs = inputs,
                   fingerprint = digest::digest(inputs, algo = "sha256"))
validate_cache_fingerprint(fake_cache)
old_code <- CASH_SCHEME_CODE
CASH_SCHEME_CODE <- 999999L
drift_rejected <- inherits(try(validate_cache_fingerprint(fake_cache), silent = TRUE), "try-error")
CASH_SCHEME_CODE <- old_code
stopifnot(drift_rejected)

cat("All synthetic tests passed.\n")

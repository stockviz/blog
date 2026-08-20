suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})
source("build.R")
source("backtest.R")

stopifnot(identical(TEST_START, as.Date("2020-05-01")))

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
assert_equal(lookbacks_for_regime(c("Green", "Yellow", "Red"),
                                  c(Green = 10L, Yellow = 6L, Red = 1L)),
             c(10L, 6L, 1L), label = "custom regime lookbacks")
invalid_lookbacks <- inherits(
  try(lookbacks_for_regime("Green", c(Green = 10L, Yellow = 6L)), silent = TRUE),
  "try-error"
)
stopifnot(invalid_lookbacks)

cat("Testing monthly momentum...\n")
lev <- xts(matrix(c(100,110,121,133.1,146.41), ncol = 1),
           as.Date(c("2020-01-31","2020-02-28","2020-03-31","2020-04-30","2020-05-29")))
assert_equal(as.numeric(month_momentum(lev, 1))[2:5], rep(0.1, 4), label = "1m momentum")
assert_equal(as.numeric(month_momentum(lev, 3))[4:5], rep(0.331, 2), label = "3m momentum")

cat("Testing deterministic ranking and cash substitution...\n")
assets <- c("A", "B", "CASH")
w_tie <- weights_from_momentum(c(A = 0.1, B = 0.1, CASH = 0.01), 1, assets)
assert_equal(w_tie, c(A = 1, B = 0, CASH = 0), label = "tie order")
w_top2 <- weights_from_momentum(c(A = 0.2, B = 0.1, CASH = 0.01), 2, assets)
assert_equal(w_top2, c(A = 0.5, B = 0.5, CASH = 0), label = "top2")
w_neg <- weights_from_momentum(c(A = -0.1, B = -0.2, CASH = -0.3), 2, assets)
assert_equal(w_neg, c(A = 0, B = 0, CASH = 1), label = "negative risk to cash")
stopifnot(abs(sum(w_neg) - 1) < 1e-12)

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

cat("Testing monthly-rebalanced equal weight benchmark...\n")
ew_rets <- xts(matrix(c(0.10, 0.00,
                         0.10, 0.00,
                         0.10, 0.00), ncol = 2, byrow = TRUE),
               as.Date(c("2020-01-02", "2020-01-03", "2020-02-03")))
colnames(ew_rets) <- c("A", "B")
ew <- run_equal_weight(list(index_returns = ew_rets), as.Date("2020-01-02"), as.Date("2020-02-03"))
assert_equal(as.numeric(ew), c(0.05, 0.55 / 1.05 * 0.10, 0.05),
             label = "monthly equal weight")

cat("Testing checkpoint rejects current-configuration drift...\n")
ranges <- list(index = list(A = c("2000-01-01", "2020-01-01")),
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

cat("Testing arbitrary fixed momentum lookback...\n")
lev4 <- xts(c(100, 110, 121, 133.1, 146.41, 161.051),
            as.Date(c("2019-01-31", "2019-02-28", "2019-03-29", "2019-04-30",
                      "2019-05-31", "2019-06-28")))
m4 <- month_momentum(lev4, 4)
assert_equal(as.numeric(m4)[5:6], c(0.4641, 0.4641), label = "4m momentum")

cat("Testing fixed-lookback portfolio...\n")
lev <- xts(matrix(c(
  100,   100,   100,
  110,   100,   100.5,
  121,   100,   101.0,
  133.1, 100,   101.5,
  146.41, 100,  102.0,
  161.051, 100, 102.5
), ncol = 3, byrow = TRUE),
as.Date(c("2019-01-31", "2019-02-28", "2019-03-29", "2019-04-30",
          "2019-05-31", "2019-06-28")))
colnames(lev) <- c("A", "B", "CASH")
td <- seq(as.Date("2019-01-02"), as.Date("2019-07-31"), by = "day")
td <- td[!format(td, "%a") %in% c("Sat", "Sun")]
idx_rets <- xts(cbind(A = rep(0.005, length(td)), B = rep(0, length(td))), td)
colnames(idx_rets) <- c("A", "B")
cash_rets <- xts(rep(0.0001, length(td)), td)
colnames(cash_rets) <- "CASH"
synth <- list(month_ends = lev, index_returns = idx_rets, cash_returns = cash_rets)

r1 <- run_fixed_lookback_portfolio(synth, top_n = 1L, lookback = 1L, cost_rate = 0)
stopifnot(identical(r1$strategy, "L1 Top 1"))
stopifnot(all(is.finite(as.numeric(r1$daily))))
stopifnot(all(r1$audit$signal_date < r1$audit$holding_start_date))
wc <- grep("^weight_", names(r1$audit), value = TRUE)
stopifnot(all(abs(rowSums(r1$audit[, wc, drop = FALSE]) - 1) < 1e-12))
stopifnot(all(r1$audit$selected_assets == "A"))  # A has highest 1-month momentum

r2 <- run_fixed_lookback_portfolio(synth, top_n = 2L, lookback = 1L, cost_rate = 0)
stopifnot(all(r2$audit$selected_assets == "A;CASH"))  # A then CASH rank above flat B

r4 <- run_fixed_lookback_portfolio(synth, top_n = 1L, lookback = 4L, cost_rate = 0)
stopifnot(nrow(r4$audit) == 2L)  # only i = 5, 6 survive the lookback warm-up

cat("All synthetic tests passed.\n")

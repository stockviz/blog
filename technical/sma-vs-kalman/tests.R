#!/usr/bin/env Rscript
# Regression tests use synthetic fixtures only, never reported market results.
suppressPackageStartupMessages({
  library(xts)
  library(PerformanceAnalytics)
})
SCRIPT_ARG <- grep("^--file=", commandArgs(), value = TRUE)
ROOT <- dirname(normalizePath(sub("^--file=", "", SCRIPT_ARG[1])))
source(file.path(ROOT, "engine.R"))

expect_close <- function(a, b, tolerance = 1e-10) {
  # Fail with numeric diagnostics rather than silently recycling lengths.
  stopifnot(length(a) == length(b), max(abs(as.numeric(a) - as.numeric(b))) < tolerance)
}

run_tests <- function() {
  # Exercise filter causality, scale invariance, execution lag and cost accounting.
  set.seed(41)
  price <- 100 * exp(cumsum(rnorm(900, 0.0005, 0.012)))
  dates <- as.Date("2018-01-01") + seq_along(price)
  x <- xts(price, dates)
  a <- kalman_filter(price)
  expect_close(a[1:650, ], kalman_filter(price[1:650]))
  changed <- price
  changed[651:900] <- changed[651:900] * seq(1.1, 2, length.out = 250)
  expect_close(a[1:650, ], kalman_filter(changed)[1:650, ])
  expect_close(a[, "Slope"], kalman_filter(price * 1000)[, "Slope"])
  # The published ~66-session horizon matches the positive impulse-weight centroid;
  # the local-linear filter also has a small negative tail, so signed lag differs.
  weights <- kalman_filter(exp(c(0, rep(0.01, 2999))))[-1, "Slope"] / 0.01
  positive_lag <- sum((0:2998) * pmax(weights, 0)) / sum(pmax(weights, 0))
  stopifnot(abs(sum(weights) - 1) < 1e-8, abs(positive_lag - 66) < 1)
  stopifnot(all(eigen(kalman_covariance(), symmetric = TRUE)$values > 0),
            all(abs(a[, "Z"]) <= 5))
  expect_close(rule_state(c(-1, 0, 1, 0, -1, 0)), c(0, 0, 1, 1, 0, 0))
  result <- simulate_index(x)
  rows <- (WARMUP + 1L):length(price)
  expect_close(result$exposure[, "Kalman"], result$signals$Kalman_Target[rows - 1L])
  expect_close(result$exposure[, "SMA200"], result$signals$SMA_Target[rows - 1L])
  expected <- price[rows] / price[rows - 1L] - 1
  expected[1] <- expected[1] - 0.0025
  expect_close(result$rets[, 1], expected)
  expect_close(result$costs, result$turnover * 0.0025)
  expect_close(result$rets, result$gross - result$costs)
  free <- simulate_index(x, 0)
  expect_close(free$rets, free$gross)
  prefix <- simulate_index(x[1:700])
  expect_close(prefix$rets, result$rets[as.character(index(prefix$rets))])
  expect_close(result$exposure, simulate_index(x * 3)$exposure)
  # Post/full slicing preserves zero cash days and never resets positions.
  m <- index_metrics(result, "fixture", "post", 25)
  stopifnot(nrow(m) == 3, all(as.Date(m$Start) >= as.Date("2020-05-01")))
  losses <- xts(c(-0.1, 0.02, -0.05), as.Date("2021-01-01") + 1:3)
  expect_close(strategy_metrics(losses)[["MaxDD"]], 0.1279)
  stopifnot(inherits(try(simulate_index(xts(c(price[-1], NA), dates)), silent = TRUE), "try-error"))
  cat("All Kalman/SMA regression tests passed.\n")
}
run_tests()

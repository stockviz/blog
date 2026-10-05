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
  # A constant per-row R vector must reproduce the scalar filter bit-for-bit.
  expect_close(a, kalman_filter(price, r = rep(KALMAN_R, length(price))))
  # The published ~66-session horizon matches the positive impulse-weight centroid;
  # the local-linear filter also has a small negative tail, so signed lag differs.
  weights <- kalman_filter(exp(c(0, rep(0.01, 2999))))[-1, "Slope"] / 0.01
  positive_lag <- sum((0:2998) * pmax(weights, 0)) / sum(pmax(weights, 0))
  stopifnot(abs(sum(weights) - 1) < 1e-8, abs(positive_lag - 66) < 1)
  expect_close(kalman_horizon(1, 1), positive_lag)
  stopifnot(all(eigen(kalman_covariance(), symmetric = TRUE)$values > 0),
            all(abs(a[, "Z"]) <= 5))
  expect_close(rule_state(c(-1, 0, 1, 0, -1, 0)), c(0, 0, 1, 1, 0, 0))
  # target_column maps display names to explicit target columns and the legacy alias.
  stopifnot(identical(target_column("SMA20"), "SMA20_Target"),
            identical(target_column("SMA200"), "SMA200_Target"),
            identical(target_column("Kalman"), "Kalman_Target"),
            identical(target_column("Kalman Calibrated"), "KalmanCalibrated_Target"),
            identical(target_column("Kalman Adaptive"), "KalmanAdaptive_Target"),
            identical(target_column("SMA200", c("SMA_Target", "Kalman_Target")), "SMA_Target"))
  result <- simulate_index(x)
  stopifnot(identical(SMA_LOOKBACKS, c(20L, 50L, 100L, 200L)),
            identical(BASE_SYSTEMS, c("Buy & Hold", paste0("SMA", SMA_LOOKBACKS), "Kalman")),
            identical(SYSTEMS, c(BASE_SYSTEMS, "Kalman Calibrated", "Kalman Adaptive")),
            identical(colnames(result$rets), BASE_SYSTEMS),
            length(BASE_SYSTEMS) == 6L, length(SYSTEMS) == 8L,
            identical(names(result$signals)[grepl("^SMA[0-9]+$", names(result$signals))],
                      paste0("SMA", SMA_LOOKBACKS)))
  rows <- (WARMUP + 1L):length(price)
  expect_close(result$exposure[, "Kalman"], result$signals$Kalman_Target[rows - 1L])
  for (lb in SMA_LOOKBACKS) {
    system <- paste0("SMA", lb)
    expect_close(result$signals[[system]][lb:length(price)],
                 as.numeric(TTR::SMA(price, n = lb))[lb:length(price)])
    expect_close(result$exposure[, system], result$signals[[paste0(system, "_Target")]][rows - 1L])
  }
  # Retain the original SMA200 path and costs exactly after expanding the arms.
  old_target <- rule_state(price - as.numeric(TTR::SMA(price, n = 200L)))
  old_position <- old_target[rows - 1L]
  old_turnover <- abs(old_position - c(0, head(old_position, -1)))
  expect_close(result$rets[, "SMA200"],
               old_position * (price[rows] / price[rows - 1L] - 1) - 0.0025 * old_turnover)
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
  stopifnot(nrow(m) == length(BASE_SYSTEMS), all(as.Date(m$Start) >= as.Date("2020-05-01")))
  falls <- fall_diagnostics(result, "fixture", threshold = 0)
  stopifnot(setequal(unique(falls$System), BASE_SYSTEMS[-1]))
  losses <- xts(c(-0.1, 0.02, -0.05), as.Date("2021-01-01") + 1:3)
  expect_close(strategy_metrics(losses)[["MaxDD"]], 0.1279)
  stopifnot(inherits(try(simulate_index(xts(c(price[-1], NA), dates)), silent = TRUE), "try-error"))

  # ---- Calibrated / adaptive mechanics ----
  # Calibration fixture spans the training cutoff with a low-vol and high-vol tail.
  set.seed(7)
  cal_n <- 1600L
  cal_all <- seq.Date(as.Date("2015-01-01"), by = "day", length.out = 2600L)
  cal_days <- cal_all[!format(cal_all, "%u") %in% c("6", "7")][seq_len(cal_n)]
  cal_vol <- rep(0.01, cal_n)
  cal_vol[seq.int(1151L, 1450L)] <- 0.0015
  cal_vol[seq.int(1451L, cal_n)] <- 0.04
  cal_price <- 100 * exp(cumsum(rnorm(cal_n, 0.0003, cal_vol)))
  cal_x <- xts(cal_price, cal_days)
  cutoff_row <- max(which(cal_days <= as.Date(CALIBRATION_END)))
  stopifnot(cutoff_row > WARMUP, cutoff_row - WARMUP >= 252L)

  cal <- calibrate_kalman(cal_x)
  stopifnot(is.list(cal), setequal(names(cal), c("settings", "candidates", "summary")),
            setequal(names(cal$settings), c("q_multiplier", "reference_variance", "training_end",
                                            "vol_half_life", "vol_bounds")),
            nrow(cal$candidates) == length(Q_MULTIPLIERS), nrow(cal$summary) == 1L)
  stopifnot(cal$settings$reference_variance > 0, is.finite(cal$settings$q_multiplier),
            cal$settings$q_multiplier %in% Q_MULTIPLIERS,
            identical(cal$settings$vol_half_life, VOL_HALF_LIFE),
            identical(cal$settings$vol_bounds, VOL_BOUNDS),
            identical(cal$settings$training_end, CALIBRATION_END))
  # Reference variance is the mean squared log return over the whole training
  # window (including the warm-up), matching the squared-return EWMA convention.
  lr_train <- diff(log(cal_price))[seq_len(cutoff_row - 1L)]
  expect_close(cal$settings$reference_variance, mean(lr_train^2))
  expect_close(cal$summary$ReferenceVolAnnualized,
               sqrt(cal$settings$reference_variance) * sqrt(252))

  res <- simulate_index(cal_x, calibration = cal)
  # Build integration also accepts the frozen settings without the fit tables.
  expect_close(simulate_index(cal_x, calibration = cal$settings)$rets, res$rets)
  stopifnot(all(cal$candidates$MaxDD <= 0), cal$summary$TrainMaxDD <= 0,
            cal$summary$TrainingRows == cal$candidates$N[cal$candidates$Selected],
            cal$summary$TrainingStart == cal$candidates$Start[cal$candidates$Selected])
  stopifnot(identical(colnames(res$rets), SYSTEMS),
            all(c("CalibratedSlope", "AdaptiveSlope", "AdaptiveVariance", "AdaptiveRMultiplier",
                  "KalmanCalibrated_Target", "KalmanAdaptive_Target") %in% names(res$signals)))
  rows_c <- (WARMUP + 1L):cal_n
  # Extra targets follow their slope signals and are applied one session later.
  expect_close(res$signals$KalmanCalibrated_Target, rule_state(res$signals$CalibratedSlope))
  expect_close(res$signals$KalmanAdaptive_Target, rule_state(res$signals$AdaptiveSlope))
  expect_close(res$exposure[, "Kalman Calibrated"], res$signals$KalmanCalibrated_Target[rows_c - 1L])
  expect_close(res$exposure[, "Kalman Adaptive"], res$signals$KalmanAdaptive_Target[rows_c - 1L])
  # Cost accounting is shared turnover * drag for every arm, including the extras.
  expect_close(res$costs, res$turnover * 0.0025)
  expect_close(res$rets, res$gross - res$costs)
  # Adaptive R multiplier is the clipped prior-close variance ratio.
  expect_close(res$signals$AdaptiveRMultiplier,
               pmax(VOL_BOUNDS[1], pmin(VOL_BOUNDS[2],
                     res$signals$AdaptiveVariance / cal$settings$reference_variance)))
  stopifnot(all(res$signals$AdaptiveRMultiplier >= VOL_BOUNDS[1] - 1e-12),
            all(res$signals$AdaptiveRMultiplier <= VOL_BOUNDS[2] + 1e-12))
  # Volatility response: floor in the low-vol span, ceiling in the high-vol tail.
  stopifnot(min(res$signals$AdaptiveRMultiplier) <= VOL_BOUNDS[1] + 1e-9,
            max(res$signals$AdaptiveRMultiplier) >= VOL_BOUNDS[2] - 1e-9,
            mean(res$signals$AdaptiveRMultiplier[seq.int(1520L, cal_n)]) >
              mean(res$signals$AdaptiveRMultiplier[seq.int(1250L, 1440L)]))
  # The calibrated arm equals the scalar filter the calibration scored (static-R
  # parity), and the vector path reproduces the scalar path bit-for-bit.
  q_sel <- KALMAN_Q * cal$settings$q_multiplier
  expect_close(res$signals$CalibratedSlope,
               kalman_filter(cal_price, q = q_sel, r = KALMAN_R)[, "Slope"])
  expect_close(kalman_filter(cal_price, q = q_sel, r = KALMAN_R),
               kalman_filter(cal_price, q = q_sel, r = rep(KALMAN_R, cal_n)))

  # ---- Invariance: the frozen calibration must be causal ----
  post_cutoff <- which(cal_days > as.Date(CALIBRATION_END))
  perturbed <- cal_price
  perturbed[post_cutoff] <- perturbed[post_cutoff] * 1.5
  cal2 <- calibrate_kalman(xts(perturbed, cal_days))
  expect_close(cal2$settings$q_multiplier, cal$settings$q_multiplier)
  expect_close(cal2$settings$reference_variance, cal$settings$reference_variance)
  expect_close(cal2$candidates$Sharpe, cal$candidates$Sharpe)
  expect_close(cal2$candidates$CAGR, cal$candidates$CAGR)
  # Changing post-cutoff prices cannot alter earlier variance rows.
  res_b <- simulate_index(xts(perturbed, cal_days), calibration = cal)
  expect_close(res_b$signals$AdaptiveVariance[seq_len(cutoff_row)],
               res$signals$AdaptiveVariance[seq_len(cutoff_row)])
  # Changing current/future prices must not alter variance[t] (excludes return at t).
  t0 <- cutoff_row
  perturbed2 <- cal_price
  perturbed2[seq.int(t0, cal_n)] <- perturbed2[seq.int(t0, cal_n)] * 1.3
  res_c <- simulate_index(xts(perturbed2, cal_days), calibration = cal)
  expect_close(res_c$signals$AdaptiveVariance[seq_len(t0)],
               res$signals$AdaptiveVariance[seq_len(t0)])
  # Prefix invariance with the calibration held fixed.
  prefix_c <- simulate_index(cal_x[1:1000], calibration = cal)
  expect_close(prefix_c$rets, res$rets[as.character(index(prefix_c$rets))])
  expect_close(prefix_c$exposure, res$exposure[as.character(index(prefix_c$exposure))])
  # Price-scale invariance: a level shift leaves every target unchanged.
  expect_close(simulate_index(cal_x * 3, calibration = cal)$exposure, res$exposure)

  # ---- Deterministic selection ----
  cal_again <- calibrate_kalman(cal_x)
  expect_close(cal_again$settings$q_multiplier, cal$settings$q_multiplier)
  expect_close(cal_again$candidates$Sharpe, cal$candidates$Sharpe)
  cand <- cal$candidates
  finite <- is.finite(cand$Sharpe) & is.finite(cand$MaxDD) & is.finite(cand$CAGR)
  best_sharpe <- max(cand$Sharpe[finite])
  tied <- which(finite & cand$Sharpe == best_sharpe)
  best <- tied[order(-cand$MaxDD[tied], tied)[1L]]
  stopifnot(sum(cand$Selected) == 1L, isTRUE(cand$Selected[best]))
  expect_close(cal$settings$q_multiplier, cand$QMultiplier[best])
  # Exact ties on a convex monotone trend fall back to grid order (smallest Q).
  drift <- seq(0.0002, 0.002, length.out = cal_n)
  mono <- calibrate_kalman(xts(100 * exp(cumsum(drift)), cal_days))
  expect_close(mono$settings$q_multiplier, Q_MULTIPLIERS[1L])

  # ---- Insufficient training and zero-variance rejection ----
  stopifnot(inherits(try(calibrate_kalman(xts(100 * exp(cumsum(rnorm(600, 0.0005, 0.01))),
                                              cal_days[1:600])), silent = TRUE), "try-error"),
            inherits(try(calibrate_kalman(xts(rep(100, 800), cal_days[1:800])),
                         silent = TRUE), "try-error"))

  cat("All Kalman/SMA regression tests passed.\n")
}
run_tests()

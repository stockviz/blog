#!/usr/bin/env Rscript
# Read-back audit of the real study artifacts, separate from synthetic tests.
suppressPackageStartupMessages({
  library(xts)
  library(PerformanceAnalytics)
})
SCRIPT_ARG <- grep("^--file=", commandArgs(), value = TRUE)
ROOT <- dirname(normalizePath(sub("^--file=", "", SCRIPT_ARG[1])))
source(file.path(ROOT, "engine.R"))
OUT <- file.path(ROOT, "output")

verify_outputs <- function() {
  # Reconcile all arms, train-only settings, adaptive paths and report artifacts.
  stopifnot(all(file.exists(file.path(OUT, c("calibration.csv", "calibration_candidates.csv",
                                            "calibration.html", "calibration.png")))))
  checkpoint <- readRDS(file.path(OUT, "checkpoint.rds"))
  metrics <- read.csv(file.path(OUT, "metrics.csv"))
  daily <- read.csv(file.path(OUT, "daily_returns.csv"))
  signals <- read.csv(file.path(OUT, "signals.csv"))
  manifest <- read.csv(file.path(OUT, "chart_manifest.csv"))
  sensitivity <- read.csv(file.path(OUT, "cost_sensitivity.csv"))
  consolidated <- read.csv(file.path(OUT, "metrics_consolidated.csv"))
  calibration <- read.csv(file.path(OUT, "calibration.csv"))
  candidates <- read.csv(file.path(OUT, "calibration_candidates.csv"))
  stopifnot(nrow(calibration) == length(checkpoint$results),
            nrow(candidates) == length(checkpoint$results) * length(Q_MULTIPLIERS),
            all(as.Date(candidates$End) <= as.Date(CALIBRATION_END)),
            all(as.Date(calibration$TrainingEnd) <= as.Date(CALIBRATION_END)))
  # Every exported system must come from the current configured universe.
  for (stem in c("metrics", "cost_sensitivity", "metrics_consolidated", "daily_returns",
                 "daily_exposure", "annual_returns", "falls", paste0("metrics_", names(WINDOWS)))) {
    exported <- read.csv(file.path(OUT, paste0(stem, ".csv")))
    expected <- if (stem == "falls") SYSTEMS[-1] else SYSTEMS
    stopifnot(setequal(unique(exported$System), expected))
  }
  stopifnot(identical(names(signals)[grepl("^SMA[0-9]+$", names(signals))],
                      paste0("SMA", SMA_LOOKBACKS)))
  expected_metrics <- length(checkpoint$results) * length(SYSTEMS) * length(WINDOWS)
  stopifnot(length(checkpoint$results) == 4L, nrow(metrics) == expected_metrics,
            nrow(sensitivity) == expected_metrics * 2L,
            nrow(consolidated) == length(checkpoint$results) * length(SYSTEMS),
            identical(checkpoint$sma_lookbacks, SMA_LOOKBACKS), identical(checkpoint$systems, SYSTEMS),
            !anyDuplicated(metrics[, c("Index", "System", "Window")]),
            !anyDuplicated(sensitivity[, c("Index", "System", "Window", "CostBps")]),
            !anyDuplicated(consolidated[, c("Index", "System")]),
            nrow(manifest) == 12L, length(list.files(OUT, "^cum_dd_.*[.]png$")) == 12L,
            all(file.exists(manifest$Path)), !anyDuplicated(daily[, c("Index", "Date", "System")]),
            all(is.finite(daily$Return)), max(abs(daily$Return - daily$Gross + daily$Cost)) < 1e-12,
            all(daily$Exposure %in% c(0, 1)), max(abs(daily$Cost - 0.0025 * daily$Turnover)) < 1e-12)
  for (nm in names(checkpoint$results)) {
    result <- checkpoint$results[[nm]]
    fitted <- checkpoint$calibrations[[nm]]
    settings <- fitted$settings
    train <- checkpoint$levels[[nm]][paste0("/", CALIBRATION_END)]
    refit <- calibrate_kalman(train)
    stopifnot(isTRUE(all.equal(refit$settings, settings, tolerance = 1e-12)),
              isTRUE(all.equal(refit$candidates, fitted$candidates, tolerance = 1e-12)))
    candidate <- candidates[candidates$Index == nm, ]
    stopifnot(setequal(candidate$QMultiplier, Q_MULTIPLIERS), sum(candidate$Selected) == 1L,
              candidate$QMultiplier[candidate$Selected] == settings$q_multiplier)
    selected <- calibration[calibration$Index == nm, ]
    train_score <- metrics[metrics$Index == nm & metrics$System == "Kalman Calibrated" &
                             metrics$Window == "pre", ]
    stopifnot(selected$QMultiplier == settings$q_multiplier,
              selected$TrainingStart == train_score$Start, selected$TrainingRows == train_score$N,
              max(abs(c(selected$TrainCAGR, selected$TrainSharpe, selected$TrainMaxDD) -
                        c(train_score$CAGR, train_score$Sharpe, train_score$MaxDD))) < 1e-12,
              all(candidate$MaxDD <= 0), selected$TrainMaxDD <= 0,
              abs(selected$ReferenceVariance - mean(diff(log(as.numeric(train)))^2)) < 1e-12,
              abs(selected$HorizonSessions - kalman_horizon(settings$q_multiplier)) < 1e-10)
    s <- signals[signals$Index == nm, ]
    # Rebuild the lagged squared-return EWMA independently from exported closes.
    prior_variance <- rep(0.01^2, nrow(s))
    log_returns <- c(0, diff(log(s$Close)))
    lambda <- 2^(-1 / settings$vol_half_life)
    for (i in seq.int(3L, nrow(s))) {
      prior_variance[i] <- lambda * prior_variance[i - 1L] + (1 - lambda) * log_returns[i - 1L]^2
    }
    r_multiplier <- pmax(settings$vol_bounds[1], pmin(settings$vol_bounds[2],
                        prior_variance / settings$reference_variance))
    stopifnot(max(abs(s$AdaptiveVariance - prior_variance)) < 1e-12,
              max(abs(s$AdaptiveRMultiplier - r_multiplier)) < 1e-10)
    original_rows <- match(as.character(as.Date(index(result$rets))), s$Date)
    stopifnot(!anyNA(original_rows))
    stopifnot(identical(colnames(result$rets), SYSTEMS))
    for (system in SYSTEMS[-1]) {
      stopifnot(max(abs(as.numeric(result$exposure[, system]) -
                        s[[target_column(system)]][original_rows - 1L])) == 0)
    }
    for (lb in SMA_LOOKBACKS) {
      actual_sma <- as.numeric(TTR::SMA(s$Close, n = lb))
      stopifnot(max(abs(s[[paste0("SMA", lb)]][lb:nrow(s)] - actual_sma[lb:nrow(s)])) < 1e-10)
    }
    for (w in names(WINDOWS)) {
      actual <- index_metrics(result, nm, w, 25)
      saved <- metrics[metrics$Index == nm & metrics$Window == w, ]
      saved <- saved[match(actual$System, saved$System), ]
      wide <- consolidated[consolidated$Index == nm, ]
      wide <- wide[match(actual$System, wide$System), ]
      for (metric in c("CAGR", "Sharpe", "MaxDD", "Exits")) {
        stopifnot(max(abs(wide[[paste0(w, "_", metric)]] - actual[[metric]])) < 1e-12)
      }
      stopifnot(identical(as.character(actual$Start), saved$Start),
                identical(as.character(actual$End), saved$End),
                max(abs(as.matrix(actual[, c("CAGR", "Sharpe", "MaxDD")]) -
                          as.matrix(saved[, c("CAGR", "Sharpe", "MaxDD")]))) < 1e-12)
      chart <- manifest[manifest$Index == nm & manifest$Window == w, ]
      stopifnot(chart$Start == actual$Start[1], chart$End == actual$End[1], chart$N == actual$N[1])
    }
  }
  stopifnot(all(as.Date(metrics$End[metrics$Window == "pre"]) <= as.Date("2019-12-31")),
            all(as.Date(metrics$Start[metrics$Window == "post"]) >= as.Date("2020-05-01")))
  for (w in names(WINDOWS)) for (ext in c("csv", "html", "png")) {
    stopifnot(file.exists(file.path(OUT, paste0("metrics_", w, ".", ext))))
  }
  for (ext in c("csv", "html", "png")) {
    stopifnot(file.exists(file.path(OUT, paste0("metrics_consolidated.", ext))))
  }
  # Reconcile each cost case to the observed gross path, not synthetic returns.
  for (bps in c(25, 2)) for (nm in names(checkpoint$results)) {
    result <- checkpoint$results[[nm]]
    result$costs <- result$turnover * bps / 10000
    result$rets <- result$gross - result$costs
    for (w in names(WINDOWS)) {
      actual <- index_metrics(result, nm, w, bps)
      saved <- sensitivity[sensitivity$Index == nm & sensitivity$Window == w & sensitivity$CostBps == bps, ]
      saved <- saved[match(actual$System, saved$System), ]
      stopifnot(max(abs(as.matrix(actual[, c("CAGR", "Sharpe", "MaxDD")]) -
                        as.matrix(saved[, c("CAGR", "Sharpe", "MaxDD")]))) < 1e-12)
    }
  }
  # Check consolidated documentation and root-relative chart links.
  readme <- readLines(file.path(ROOT, "README.md"))
  stopifnot(sum(readme == "<!-- BEGIN GENERATED RESULTS -->") == 1L,
            sum(readme == "<!-- END GENERATED RESULTS -->") == 1L,
            !file.exists(file.path(OUT, "findings.md")))
  image_lines <- readme[grepl("^!\\[", readme)]
  image_paths <- sub(".*\\]\\(([^)]+)\\)$", "\\1", image_lines)
  stopifnot(length(image_paths) == 14L, all(file.exists(file.path(ROOT, image_paths))))
  cat(sprintf("Artifact audit passed: %d daily rows, %d main metrics, %d cost-case metrics, 12 cum_dd charts, 5 HTML/PNG tables; per-index settings and adaptive paths reconciled.\n",
              nrow(daily), nrow(metrics), nrow(sensitivity)))
}
verify_outputs()

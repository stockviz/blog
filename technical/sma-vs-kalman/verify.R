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
  # Reconcile saved returns, positions, windows, metrics and chart manifests.
  checkpoint <- readRDS(file.path(OUT, "checkpoint.rds"))
  metrics <- read.csv(file.path(OUT, "metrics.csv"))
  daily <- read.csv(file.path(OUT, "daily_returns.csv"))
  signals <- read.csv(file.path(OUT, "signals.csv"))
  manifest <- read.csv(file.path(OUT, "chart_manifest.csv"))
  sensitivity <- read.csv(file.path(OUT, "cost_sensitivity.csv"))
  stopifnot(length(checkpoint$results) == 4L, nrow(metrics) == 36L, nrow(sensitivity) == 72L,
            nrow(manifest) == 12L, length(list.files(OUT, "^cum_dd_.*[.]png$")) == 12L,
            all(file.exists(manifest$Path)), !anyDuplicated(daily[, c("Index", "Date", "System")]),
            all(is.finite(daily$Return)), max(abs(daily$Return - daily$Gross + daily$Cost)) < 1e-12,
            all(daily$Exposure %in% c(0, 1)), max(abs(daily$Cost - 0.0025 * daily$Turnover)) < 1e-12)
  for (nm in names(checkpoint$results)) {
    result <- checkpoint$results[[nm]]
    s <- signals[signals$Index == nm, ]
    original_rows <- match(as.character(as.Date(index(result$rets))), s$Date)
    stopifnot(!anyNA(original_rows))
    stopifnot(max(abs(as.numeric(result$exposure[, 2]) - s$SMA_Target[original_rows - 1L])) == 0,
              max(abs(as.numeric(result$exposure[, 3]) - s$Kalman_Target[original_rows - 1L])) == 0)
    for (w in names(WINDOWS)) {
      actual <- index_metrics(result, nm, w, 25)
      saved <- metrics[metrics$Index == nm & metrics$Window == w, ]
      saved <- saved[match(actual$System, saved$System), ]
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
  # Check consolidated documentation and root-relative chart links.
  readme <- readLines(file.path(ROOT, "README.md"))
  stopifnot(sum(readme == "<!-- BEGIN GENERATED RESULTS -->") == 1L,
            sum(readme == "<!-- END GENERATED RESULTS -->") == 1L,
            !file.exists(file.path(OUT, "findings.md")))
  image_lines <- readme[grepl("^!\\[", readme)]
  image_paths <- sub(".*\\]\\(([^)]+)\\)$", "\\1", image_lines)
  stopifnot(length(image_paths) == 12L, all(file.exists(file.path(ROOT, image_paths))))
  cat(sprintf("Artifact audit passed: %d daily rows, 36 main metrics, 72 cost-case metrics, 12 cum_dd charts, 3 HTML/PNG tables.\n", nrow(daily)))
}
verify_outputs()

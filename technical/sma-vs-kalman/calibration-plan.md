# Per-index Kalman calibration implementation plan

> For Hermes: implement and review the mechanics separately from report integration.

Goal: retain the existing six controls, add per-index calibrated and volatility-adaptive Kalman arms, and rebuild the reports with auditable training-only calibration.

Architecture: choose one process-noise multiplier per index from a fixed nine-value grid using pre-2020 net Sharpe at 25 bps. Freeze the choice after December 31, 2019. The adaptive arm shares that choice and varies measurement noise using the index's lagged EWMA log-return variance relative to its pre-2020 reference variance. This is a noise-suppression hypothesis, not an estimated structural observation-error model.

Tech stack: existing R, xts, PerformanceAnalytics, gt and shared StockViz chart helpers. Do not add dependencies or change source caches.

## Task 1: mechanics and regression tests

Files: engine.R, tests.R.

Write and run failing tests before implementation. Add a nine-value Q multiplier grid 2^seq(-8, 8, by=2), a December 31, 2019 training cutoff, a 20-session EWMA half-life, and R multipliers bounded to [0.25, 4]. Choose highest finite training Sharpe, breaking ties by smaller MaxDD, then grid order. The calibration objective includes 25-bps turnover costs and the existing 504-session warm-up; fail clearly when scored training history is insufficient.

Preserve the default six-arm simulate_index result when no calibration is passed. Passing a calibration adds Kalman Calibrated and Kalman Adaptive. Compute dynamic measurement noise before each update using variance known at the preceding close. Use Joseph covariance updates and steady-state initialization for the selected Q and base R. Do not scale Q and R proportionally: that would leave the steady-state gain unchanged.

Expose selected settings, the full candidate table, reference variance, training dates and rows, and effective positive-impulse-weight horizon. Export the adaptive R multiplier with signals. Test baseline parity, fixed-R filter parity, prefix/future perturbation invariance given frozen settings, holdout exclusion from calibration, lagged variance, volatility response, bounds and per-arm cost/lag accounting.

## Task 2: report integration

Files: build.R, verify.R, README.md.

Calibrate each observed index before simulation. Use the same frozen settings in both cost cases. Save calibration.csv and calibration_candidates.csv plus a calibration PNG/HTML table; save settings in checkpoint.rds. Export all eight systems, explicit targets and adaptive noise paths. Extend consolidated metrics and all twelve charts without resetting positions or changing samples. Reconcile exports against checkpoint and recalibration on training-only input.

Document the formula R_t = R_base * clip(EWMA_variance_(t-1) / training_variance, 0.25, 4), the fixed Q grid, training objective, initialization, unchanged execution lag/costs, and that return volatility is a heuristic observation-noise proxy. Pre/full results include calibration data and are in-sample; the unchanged post window starts May 1, 2020 and omits the initial pandemic crash. Note that calibration choices may coincide across indices. Report measured improvement or deterioration rather than assuming adaptation helps.

## Task 3: execution and verification

Run Rscript tests.R, Rscript build.R, Rscript verify.R and git diff --check. Retain current metrics in scratch before rebuilding and require exact parity for all existing arms. Inspect consolidated/calibration table PNGs and a representative expanded chart. No commits, pushes or live database refresh are requested.

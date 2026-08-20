# Trend-VIX Lookback Sensitivity — Plan

## Goal

Determine whether the momentum lookback window, in isolation, meaningfully changes
strategy metrics, on the training set only.

## Scope and fixed inputs

- Universe is unchanged from `../trend-vix`: `NIFTY 50 TR`, `NIFTY MIDCAP 150 TR`,
  `NIFTY SMALLCAP 250 TR`, Quantum Liquid Fund-Growth (scheme `103734`) as cash.
- No India VIX. Each run uses a single fixed momentum lookback.
- Lookbacks swept: 1 through 12 months (integers).
- Portfolio variants: Top 1 (100% winner) and Top 2 (50/50 two winners).
- Cost: 25 bps per unit of one-way turnover (primary); 0 bps also computed.
- Period: training set only, through 2019-12-31.
- Comparison window: common start (latest first holding date across all lookbacks)
  through 2019-12-31, so every lookback is evaluated over identical months.

## Acceptance criteria

1. `tests.R` passes (fixed-lookback runner has synthetic coverage).
2. `build.R` reproduces the parent cache (5,036 common daily rows, 244 month-ends).
3. The swept 10-month Top 1 equals the parent's fixed 10M Top 1 exactly (asserted).
4. `analysis.R` emits per-lookback CAGR / Sharpe / max-drawdown / turnover tables
   for Top 1 and Top 2, lookback-vs-metric charts, and a spread summary.
5. The spread (max minus min) of Sharpe, CAGR, and max drawdown across 1–12 months
   is reported explicitly, so "meaningful" is quantified rather than asserted.

## Out of scope

- No out-of-sample (post-2019) evaluation.
- No re-optimization of the VIX thresholds or regime definitions.
- No change to the asset universe, ranking rule, cash-substitution rule, or costs.

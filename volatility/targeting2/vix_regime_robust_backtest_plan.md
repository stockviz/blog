# Robust Backtest Plan: VIX Regime Timing + Inverse-Vol Equity Weighting

## Objective

Determine whether tilting equity exposure based on VIX's trailing 6-month level and trend
(on top of an inverse-vol sizing baseline) adds *statistically robust, economically
meaningful* risk-adjusted return, or whether any apparent edge is an artifact of a few
outsized episodes, look-ahead bias, or parameter overfitting.

---

## Phase 0 — Pre-Registration (do this before looking at results)

Overfitting risk on VIX-timing strategies is high because the whole history is dominated by
a handful of known crisis episodes everyone has seen. To guard against unconsciously tuning
parameters to fit them:

- Write down the exact signal definitions (level window, trend window, percentile thresholds,
  tilt magnitude) *before* running the full-sample backtest.
- Write down the primary metric you'll judge success on (e.g., out-of-sample Sharpe
  improvement over pure inverse-vol) before seeing results.
- Decide the train/test split dates in advance.

---

## Phase 1 — Data Preparation

| Task | Detail |
|---|---|
| VIX history | Daily, CBOE or FRED `VIXCLS`, 1990–present |
| Equity series | S&P 500 **total return** index (price-only understates buy-and-hold) |
| Alignment | Inner-join on trading dates; forward-fill only isolated gaps, flag longer gaps |
| Data QA | Check for stale/duplicated values, stock splits/index reconstitution artifacts, and the Feb 2018 VIX-methodology-adjacent anomalies |
| Point-in-time integrity | Confirm no series has been revised/back-adjusted in a way that wouldn't have been observable in real time |

---

## Phase 2 — Signal Construction (No Look-Ahead)

- **Level signal:** trailing 6-month average VIX, ranked against its own **expanding trailing
  history only** (not the full sample) — this replicates what an investor could have actually
  known at each point in time.
- **Trend signal:** OLS slope of VIX over the trailing 6-month window (sign only, or normalized
  magnitude).
- **Regime flags:**
  - `high_falling` = level percentile > threshold_hi AND slope < 0
  - `low_rising` = level percentile < threshold_lo AND slope > 0
- Log the % of trading days each regime is active — if a regime is active <5% of the time,
  results will be dominated by very few episodes and confidence intervals should be treated
  with extra skepticism.

---

## Phase 3 — Portfolio Construction

Build three parallel return series, all using the same rebalancing frequency and cost model:

1. **Buy-and-hold** — 100% equity, benchmark.
2. **Inverse-vol baseline** — `weight = target_risk / VIX`, capped at a max leverage (e.g. 1.5x).
3. **Regime-tilted** — inverse-vol weight scaled up/down by a tilt factor when a regime flag is
   active.

**Implementation details that materially affect results:**
- Lag weights by at least 1 trading day (you can't trade on today's close using today's VIX
  reading instantaneously).
- Apply transaction costs proportional to turnover (bps per unit traded).
- Test both daily and monthly rebalancing — daily rebalancing on a noisy signal can generate
  costs that erase the edge.

---

## Phase 4 — In-Sample Backtest

- Run the full backtest over the full data history.
- Compute: CAGR, annualized vol, Sharpe, Sortino, max drawdown, Calmar, turnover, avg holding
  period per regime.
- Report regime-tilted vs. inverse-vol baseline, **not** just vs. buy-and-hold — the interesting
  question is whether the regime overlay adds anything beyond simple vol targeting.

---

## Phase 5 — Statistical Robustness

### 5a. Autocorrelation-aware significance testing
Standard t-tests assume i.i.d. returns; VIX-based signals violate this badly. Use:
- **Block bootstrap** (block size ≈ 1 quarter) on the Sharpe difference between regime-tilted
  and inverse-vol baseline.
- Report p-value and 95% CI on that difference, not just the point estimate.

### 5b. Subperiod / regime decomposition
- Split into distinct macro regimes: pre-2000, 2000–2009 (dot-com + GFC), 2010–2019 (low-vol
  bull), 2020–2022 (COVID + inflation shock), 2023–present.
- Report performance separately for each. A strategy that only "works" in 2008/2020/2022
  is a crisis-alpha strategy, not a general timing edge — useful to know, but a different
  claim than the original one.
- Re-run headline stats **excluding** 2008, 2020, and 2022 entirely to see if the edge survives.

### 5c. Parameter sensitivity sweep
- Vary the level/trend window (e.g. 3, 6, 9, 12 months) and percentile thresholds
  (e.g. 60/40, 70/30, 80/20).
- Plot Sharpe difference (regime-tilted − inverse-vol) as a heatmap across this grid.
- A real effect should show a smooth, plausible ridge of good performance across nearby
  parameter values — not an isolated spike at one specific combination.

### 5d. Placebo / randomization test
- Shuffle the regime flags randomly (preserving their overall frequency) and re-run the
  backtest many times to build a null distribution of Sharpe differences.
- Compare the actual observed Sharpe difference against this null distribution as an
  additional significance check, independent of the block bootstrap.

---

## Phase 6 — Out-of-Sample Validation

- **Train/test split:** fit any free parameters (thresholds, tilt magnitude) only on data
  through a cutoff date (e.g. end of 2015), then evaluate frozen parameters on 2016–present
  without further adjustment.
- **Walk-forward:** re-estimate thresholds annually using only trailing data, roll forward
  one year at a time, and stitch together the resulting out-of-sample equity curve.
- Compare in-sample vs. out-of-sample Sharpe — a large degradation is the clearest sign of
  overfitting.

---

## Phase 7 — Robustness to Implementation Details

- Sensitivity to rebalancing frequency (daily/weekly/monthly).
- Sensitivity to transaction cost assumptions (e.g. 1bp, 2bp, 5bp).
- Sensitivity to the weight cap / leverage limit.
- Sensitivity to using spot VIX vs. a smoothed/lagged VIX (spot VIX can be noisy intraday).

---

## Phase 8 — Reporting

Final writeup should include:
1. Exact signal and portfolio definitions (pre-registered in Phase 0).
2. Headline performance table (all three portfolios, full sample).
3. Block bootstrap significance result.
4. Subperiod breakdown, including "excluding crisis years."
5. Parameter sensitivity heatmap.
6. Out-of-sample / walk-forward equity curve vs. in-sample.
7. Explicit statement of which claim (regime timing vs. inverse-vol baseline) is doing the
   work, and how much confidence the evidence actually supports.

---

## Key Failure Modes to Watch For

- **Look-ahead bias** in percentile calculation (using full-sample stats instead of expanding
  trailing history).
- **Crisis-episode dependency** — edge vanishes once 2008/2020/2022 are excluded.
- **Overfit thresholds** — Sharpe difference collapses under small parameter perturbations.
- **Cost sensitivity** — edge disappears under realistic transaction costs or with daily
  rebalancing on a noisy signal.
- **In-sample vs. out-of-sample gap** — strong in-sample result, weak or negative
  out-of-sample result.

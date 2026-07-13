# Robust Backtest Plan: Long-Short VIX Regime Timing + Inverse-Vol Weighting

## Objective

Determine whether the long-short version — which allows the regime tilt to push equity
weight into net-short territory — adds statistically robust, economically meaningful
risk-adjusted return over the long-only inverse-vol baseline, and specifically whether the
**short leg** is doing real work or just diluting a long-only edge with extra risk and cost.

This version also adds a **fast-acting overlay**: the 6-month level/trend regime signal is
inherently slow (reaction latency on the order of its own window) and cannot get ahead of,
or even keep pace with, a shock that unfolds faster than that window — e.g. COVID, where
VIX went from a January 2020 average of 13.94 to a March 2020 average of 57.74 (peak 82.69)
in about six weeks. The fast overlay is a separate, short-window trigger (days, not months)
that can override the slow signal's weight during exactly these episodes. It needs to be
built and validated **independently** of the slow regime signal before being combined with
it, since the two operate on different timescales and different failure modes.

This plan follows the same phase structure as the long-only plan, with additional steps
(marked ⚠ NEW) specific to the short-side claim, and a further set (marked 🚀 OVERLAY)
specific to the fast-acting overlay.

---

## Phase 0 — Pre-Registration

- Fix in advance: level/trend window, tilt magnitude, weight bounds `[MIN_WEIGHT, MAX_WEIGHT]`.
- ⚠ NEW: Pre-register a minimum "time spent net short" threshold (e.g. >10% of trading days)
  below which you will treat the short leg as untested rather than validated, regardless of
  what the aggregate Sharpe shows.
- 🚀 OVERLAY: Fix the overlay's trigger definition, threshold, and cooldown length *before*
  looking at how it performs around known crash windows. Fitting these parameters to make
  the overlay "successfully avoid COVID" after the fact is the single easiest way to produce
  a result that won't generalize to the next, differently-shaped shock.
- 🚀 OVERLAY: Pre-register which historical windows count as the "fast shock" test set
  (e.g. Oct 1987, Feb 2018, Feb–Mar 2020) — decide this in advance rather than picking
  whichever windows make the overlay look best.
- Decide train/test split dates before looking at results.

---

## Phase 1 — Data Preparation

Same as long-only: daily VIX (FRED `VIXCLS`), daily S&P 500 total-return series, aligned and
QA'd for gaps/anomalies/point-in-time integrity.

---

## Phase 2 — Signal Construction (No Look-Ahead)

- Level score: expanding-percentile of trailing 6-month average VIX, rescaled to `[-1, +1]`.
- Trend score: rolling z-score of trailing-window VIX slope, squashed via `tanh` to `[-1, +1]`.
- Regime score: `max(level_score,0)·max(-trend_score,0) − max(-level_score,0)·max(trend_score,0)`,
  clipped to `[-1, +1]`.
- ⚠ NEW: Log the full distribution of the regime score (not just a binary flag rate) — a
  histogram showing how often the score approaches ±1 tells you upfront how "aggressive" the
  short leg will be able to get before you even run the portfolio simulation.

---

## Phase 2b — Fast-Acting Overlay Signal Construction 🚀 OVERLAY

Build and validate this on its own before combining with the slow regime signal.

- **Trigger candidates** (build and test each independently — they behave differently):
  1. **Realized-vol jump:** 5-day realized volatility of equity returns, z-scored against
     its own trailing history; trigger when z exceeds a threshold.
  2. **VIX spike:** VIX moving more than N standard deviations above its own 20-day average
     within a short number of days.
  3. **Drawdown trigger:** equity price falling more than X% from its trailing 10–20 day
     high.
- **Response function:** on trigger, override the slow-signal weight with a sharply reduced
  (or flat/zero) weight; decay back toward the slow signal's prescribed weight over a fixed
  cooldown window rather than snapping back instantly, to avoid whipsawing in and out during
  a still-unfolding shock.
- **Re-entry rule:** define explicitly (e.g. "no new trigger for M consecutive trading days")
  so the handoff back to the slow signal is unambiguous and not tuned after the fact.
- Log trigger frequency, average duration in the reduced-weight state, and how many trigger
  episodes occur outside of known crash windows (false positives) vs. within them (true
  positives) — this is essentially a precision/recall tradeoff, and both directions matter:
  too sensitive means constant whipsaw costs, too insensitive means it won't fire in time
  for the next shock either.

---

## Phase 3 — Portfolio Construction

- Base weight: `target_risk / VIX`.
- Long-only baseline: `clip(base_weight, 0, MAX_WEIGHT)`.
- Long-short (slow signal only): `clip(base_weight * (1 + tilt_magnitude * regime_score), MIN_WEIGHT, MAX_WEIGHT)`.
- 🚀 OVERLAY — Long-short + fast overlay: same as above, then override with the
  overlay's reduced weight whenever the fast trigger is active (per the response/decay
  function from Phase 2b), otherwise pass through the slow-signal weight unchanged.
- 1-day execution lag, turnover-based transaction costs, same as long-only version.
- ⚠ NEW: Also compute and store a boolean `is_short` series (`weight < 0`) so later phases can
  condition on it directly.
- 🚀 OVERLAY: Also store a boolean `overlay_active` series so later phases can condition
  performance on whether the fast overlay was engaged.

---

## Phase 4 — In-Sample Backtest

- Standard performance table for all four portfolios (buy-and-hold, long-only inverse-vol,
  long-short regime-tilted, long-short + fast overlay).
- ⚠ NEW — **Decompose the long-short return series into two conditional sub-series:**
  1. Return on days when `weight ≥ 0` (acting long-only).
  2. Return on days when `weight < 0` (acting short).
  Report CAGR/Sharpe for each sub-series separately. This is the single most important new
  diagnostic: if sub-series (2) has a negative Sharpe, the short leg is actively hurting
  performance and the aggregate number is being carried entirely by the long side.
- ⚠ NEW — Report % of days net short, and the distribution of position sizes conditional on
  being short (is it usually barely negative, or does it get deeply short?).
- 🚀 OVERLAY — **Crash-window comparison table:** for each pre-registered fast-shock window
  (Oct 1987, Feb 2018, Feb–Mar 2020), report max drawdown and time-to-recovery with vs.
  without the overlay. This is the direct test of whether the overlay does what it's for.
- 🚀 OVERLAY — Report performance **excluding** days when `overlay_active` is true, separately
  from performance **during** those days, to see whether the overlay is a net drag in normal
  times that's "paid for" by crash protection, or a drag with no offsetting benefit.

---

## Phase 5 — Statistical Robustness

### 5a. Block bootstrap
- On the Sharpe difference between long-short and long-only baseline, as in the long-only plan.
- ⚠ NEW: Also block-bootstrap the short-only conditional sub-series' mean return against zero,
  to get a confidence interval on whether the short leg's return is distinguishable from noise.

### 5b. Subperiod / regime decomposition
- Same crisis-year breakdown as the long-only plan (pre-2000, 2000–09, 2010–19, 2020–22,
  2023–present), plus:
- ⚠ NEW — **Grinding-bull stress test:** explicitly isolate long, low-vol bull stretches where
  VIX was low and slowly rising (e.g. 2017, 2021, 2024) and report performance restricted to
  those windows. This is the adversarial case for a short-biased signal — a rising-VIX-from-
  a-low-base reading is exactly what these periods produce repeatedly without the market ever
  actually falling.

### 5c. Parameter sensitivity sweep
- Two-dimensional sweep over window length **and** tilt magnitude (not just window length).
- ⚠ NEW: For each combination, also record % time net short — use this to distinguish
  "tilt magnitude increases Sharpe because it's expressing a real short-side edge" from
  "tilt magnitude increases Sharpe because it rarely triggers and mostly just looks like the
  long-only baseline with extra noise."

### 5d. Placebo / randomization test
- Shuffle regime scores (preserving their distribution) and rebuild the long-short weight
  series many times to construct a null distribution of the long-short vs. baseline Sharpe
  difference, as in the long-only plan.
- 🚀 OVERLAY: Shuffle the *timing* of overlay triggers (preserving trigger frequency and
  duration statistics) to build a null distribution for the crash-window drawdown reduction.
  If randomly-timed de-risking episodes of similar frequency/duration achieve similar
  drawdown reduction, the overlay isn't adding trigger-specific value — it's just that any
  reduction in exposure during any part of the crash window would have helped.

### 5e. Fast-overlay-specific sensitivity sweep 🚀 OVERLAY
- Sweep trigger threshold and cooldown length independently of the slow-signal sweep (do
  **not** jointly optimize slow-signal and overlay parameters together — that's the fastest
  route to a combination overfit specifically to 2020).
- For each combination, report: trigger count, false-positive rate (triggers outside known
  crash windows), crash-window drawdown reduction, and whipsaw-cost drag. A usable overlay
  should show a smooth region of "good enough" parameters, not an isolated best point.

---

## Phase 6 — Out-of-Sample Validation

- Train/test split: fix tilt magnitude and thresholds on data through a cutoff (e.g. 2015),
  evaluate frozen parameters out-of-sample.
- Walk-forward: re-estimate annually, roll forward, stitch out-of-sample equity curve.
- ⚠ NEW: Report the short-leg conditional Sharpe **separately** in-sample vs. out-of-sample —
  short-side edges are especially prone to evaporating out-of-sample since they're often
  fit to a small number of historical vol-spike-then-recovery episodes.
- 🚀 OVERLAY: This is the most important out-of-sample check in the whole plan. Fit the
  overlay's trigger parameters using data that **excludes** the most recent major shock
  (e.g. fit through 2019, test on 2020 onward, or fit excluding 2020 entirely and test on
  it), so the crash-window result isn't just "the overlay was tuned to recognize the shape
  of the shock it's being credited with avoiding."

---

## Phase 7 — Robustness to Implementation Details

- Rebalancing frequency (daily/weekly/monthly) — matters more here than long-only, since
  shorting adds borrow-cost-like frictions in practice even if not modeled explicitly.
- Transaction cost assumptions (1bp/2bp/5bp).
- Weight bounds (`MIN_WEIGHT`, `MAX_WEIGHT`) — how sensitive is the result to how much net
  short exposure is allowed?
- ⚠ NEW: Add an explicit borrow-cost/financing-cost assumption for the short leg (shorting
  equity index exposure isn't free in practice) and re-run headline stats with it included.
- 🚀 OVERLAY: Test sensitivity to the trigger's own data frequency — intraday vs. daily
  close data can change how fast a drawdown or realized-vol trigger fires, and a strategy
  that only works with intraday triggering may not be implementable as described.
- 🚀 OVERLAY: Test sensitivity to the cooldown/decay length specifically — too short
  reintroduces whipsaw, too long means missing the recovery (the same problem the slow
  signal has post-COVID, just on a shorter timescale).

---

## Phase 8 — Reporting

Include everything from the long-only report, plus:
1. Regime score distribution histogram.
2. Long-conditional vs. short-conditional performance table.
3. Grinding-bull stress test results.
4. Short-leg-only block bootstrap CI.
5. Explicit statement: is the long-short version's edge (if any) coming from the short leg,
   or is it a long-only edge that happens to also carry short exposure?
6. 🚀 OVERLAY — Crash-window drawdown comparison table (with vs. without overlay).
7. 🚀 OVERLAY — Trigger frequency, false-positive rate, and whipsaw-cost drag.
8. 🚀 OVERLAY — Out-of-sample crash-window result (parameters fit excluding the shock being
   tested).
9. 🚀 OVERLAY — Explicit statement: does the overlay's crash-window benefit survive the
   placebo/randomized-timing test, or does any similarly-timed de-risking do about as well?

---

## Key Failure Modes to Watch For (Long-Short Specific)

- **Short leg never actually triggers** — "long-short" in name only; headline numbers barely
  differ from the long-only version.
- **Short leg triggers but loses money** — aggregate Sharpe improvement is an illusion coming
  from favorable long-side timing coinciding with rare short periods, not from the short
  thesis itself.
- **Grinding-bull vulnerability** — strategy is short too often/too early during multi-year
  low-vol bull markets, bleeding return that doesn't show up as a single big drawdown but
  erodes CAGR steadily.
- **Unmodeled shorting costs** — borrow fees, financing spread, or margin requirements not
  reflected in the simple transaction-cost model make a marginal edge disappear in practice.
- **All the long-only failure modes still apply**: look-ahead bias, crisis-episode
  dependency, overfit parameters, in-sample/out-of-sample gap.

## Key Failure Modes to Watch For (Fast Overlay Specific)

- **Overfit to 2020's specific shape** — the overlay is tuned (consciously or not) to
  recognize exactly how COVID unfolded, and would misfire or arrive too late for a shock
  with a different speed or shape (e.g. a slower-building credit crisis, or an overnight
  gap-down with no preceding run-up in realized vol).
- **Whipsaw drag dominates the benefit** — the overlay fires often enough outside real
  crashes that its cumulative transaction-cost drag exceeds the drawdown it saves during
  the rare real ones.
- **Slow re-entry during recovery** — mirrors the slow signal's own post-COVID problem: an
  overlay that's too conservative about handing control back can miss a sharp recovery just
  as easily as the slow signal did.
- **In-sample-only crash protection** — the overlay looks great on the crash it was built
  and tuned against, but the out-of-sample test (parameters fit excluding that shock) shows
  little or no benefit — the clearest sign the "protection" was hindsight, not a real rule.

# Backtest Plan: Direction-Gated CP Exit

## Objective

Test whether gating the CP regime exit on trend direction — exiting the
market only when the regime is UNSTABLE **and** price is below its
trend — recovers the return sacrificed by the current CP filter while
retaining a meaningful share of its drawdown protection.

## Hypothesis

The current filter exits on UNSTABLE alone, which avoids high-volatility
periods regardless of direction. Since "unstable" days include both crash
clusters and rebound clusters, this discards upside roughly as often as
downside. Requiring the *combination* of instability and a confirmed
downtrend should let the strategy stay invested through volatile rallies
while still stepping aside for volatile selloffs.

**H0:** Direction-gating does not improve risk-adjusted return relative to
the current CP filter.
**H1:** Direction-gating improves annualized return and/or Sharpe relative
to the current CP filter, without giving back all of its drawdown
reduction relative to buy-and-hold.

## Signal definition

```r
in_downtrend <- price < SMA(price, sma_lb)     # sma_lb = 50, matching existing SMA strategy
regime_flag  <- ifelse(Regime == "UNSTABLE", 0, 1)   # from existing regime_tbl

position <- ifelse(regime_flag == 0 & in_downtrend, 0, 1)
```

Position is binary (fully in or fully out) for this test — vote-share
sizing is a separate, already-tested idea and should not be conflated with
this one, so the direction-gating hypothesis is isolated cleanly.

## Data & universe

- Same three indices as prior tests: NIFTY 50 TR, NIFTY MIDCAP 150 TR,
  NIFTY SMALLCAP 250 TR.
- Same regime cache (`window_cache`) and price data (`pXts`, `dSymXts`)
  already in use — no new data required.
- Full available history (2005 → present) for the expanding-window leg;
  same 5-year train / 1-year test structure for the sliding-window leg.

## Execution & cost assumptions (keep unchanged from existing strategies, for comparability)

- Signal computed at close of day *t*; position entered for day *t+1*'s
  return (`retL1` lead convention, unchanged from `compute_strategies()`).
- Trading cost: 0.2% applied only on days the position changes (existing
  `drag` constant) — not proportional sizing, since position is still
  binary here.
- No leverage; long-only, 0% cash return assumed while out of market.

## Test methodology

Run **both** existing harnesses, not just one, since they've already been
shown to agree closely and that agreement is itself a useful robustness
check:

1. **Expanding window** (2005 → date), single run per index.
2. **Sliding window** (5yr train / 1yr test), averaged across windows per
   index, as currently implemented in `script.R`.

Additionally, since the sliding-window harness re-derives the regime
signal daily throughout the "test" year rather than freezing it at
`train_end`, add a **third, genuinely frozen variant** as a stronger
robustness check:

3. **Frozen annual model**: at each annual anchor date, take the
   regime/direction rule as of `train_end` and hold the resulting
   position rule fixed for the following year (no daily re-derivation).
   This directly tests whether the rule generalizes to a truly unseen
   year, addressing the gap flagged in earlier analysis.

## Baselines / comparators

Report all of the following side by side, for every index and every
methodology above:

| Strategy | Description |
|---|---|
| Buy & Hold | Always fully invested |
| SMA only | Existing 50-day trend filter |
| CP only (current) | Exit whenever UNSTABLE |
| SMA+CP (AND, current) | In market only if uptrend AND stable |
| **CP, direction-gated (new)** | Exit only if UNSTABLE AND downtrend |

## Metrics

- Annualized return
- Sharpe ratio (guard against `sd == 0` → report `NA`, not silently drop
  it from an average)
- Max drawdown
- Time in market (%)
- Number of regime flips / year (turnover)
- Calmar ratio (return / |max drawdown|) as a secondary risk-adjusted
  measure, since Sharpe alone can understate the value of drawdown
  avoidance
- Longest drawdown duration (days to recover to prior peak)

## Robustness checks

- **Parameter sensitivity**: repeat with SMA lookback at 20, 50, 100, 200
  days to confirm the result isn't an artifact of the specific 50-day
  choice already used elsewhere.
- **Sub-period breakdown**: report metrics separately for known stress
  periods (2008 GFC, 2011 Euro-crisis-linked selloff, 2020 COVID crash) vs.
  the rest of the sample, to check whether the drawdown protection is
  concentrated in a couple of episodes or broad-based.
- **Per-index consistency**: confirm the direction ranking (direction-gated
  > CP-only on Sharpe) holds for all three indices individually, not just
  on average.
- **Statistical significance**: bootstrap resample the daily return series
  (block bootstrap, e.g. 20-day blocks to preserve autocorrelation) to
  produce a confidence interval on the Sharpe/return difference between
  direction-gated CP and plain CP, rather than relying on a single
  point estimate.

## Caveat on data snooping

An earlier full-history exploratory run already showed direction-gating
outperforming the current CP filter (this is what motivated the
hypothesis above). That prior look used the same data this plan proposes
to test on, so it should not be treated as independent confirmation. To
keep this test meaningful:

- Treat the frozen annual-model variant (methodology #3 above) as the
  primary result, since it's the closest to a genuine out-of-sample check
  given the constraints of a single dataset.
- If possible, hold out the most recent 1–2 years of data entirely during
  any further parameter tuning, and only score the final chosen
  configuration against that held-out period once, at the end.

## Success criteria

Direction-gating is considered a validated improvement over the current
CP filter if, across at least two of the three indices and at least two
of the three test methodologies:

- Annualized return improves versus CP-only, **and**
- Sharpe ratio improves or is statistically indistinguishable versus
  CP-only, **and**
- Max drawdown does not widen by more than roughly half the gap between
  the current CP-only drawdown and raw buy-and-hold's drawdown (i.e., it
  keeps a meaningful share of the original protection, not just a token
  amount).

## Implementation steps

1. Add `in_downtrend` and the direction-gated `position` rule as a new
   column alongside the existing SMA/CP/SMA+CP columns inside
   `compute_strategies()`, rather than a separate function, so it shares
   the exact same `retL1`, alignment, and drag handling already fixed
   there.
2. Extend the sliding-window and expanding-window result tables with the
   new column.
3. Add the frozen annual-model variant as a new, explicit code path (this
   does not currently exist in `script.R`) rather than trying to
   retrofit it into the existing sliding-window loop.
4. Re-generate `combined-metrics.html` (or an extended version of it)
   with the new strategy column included, and re-run the diagnostic
   checks (mean return conditional on regime, best/worst-day regime
   membership) already used to explain the original CP underperformance,
   applied to the direction-gated version, to confirm the mechanism
   actually shifted as expected.

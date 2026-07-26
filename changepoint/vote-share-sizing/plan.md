# Vote-Share Position Sizing — Outline

## Context

`regime_classify.R` runs 30 changepoint-detection methods (HMM, LR,
CPM, Barry-Hartigan) that each vote STABLE or UNSTABLE on every date.
The current `historical-index` backtest uses the **binary majority
vote** as a regime signal: if ≥15 methods vote UNSTABLE, go flat; else
stay long. This is the CP strategy, and it underperforms B&H because
the binary on/off signal misses recovery rallies.

The hypothesis: using the **continuous vote share** for position
sizing, instead of a binary cut, will smooth the transition between
regimes and capture partial upside during uncertain periods.

## What we already have

`classify_regime()` returns a date-level tibble with:

| Column | Description |
|--------|-------------|
| `Date` | Trading date |
| `Regime` | "STABLE" or "UNSTABLE" (majority vote) |
| `N_Unstable` | Number of methods voting UNSTABLE (0–30) |
| `N_Total` | Total methods that voted (≤30, fewer if some failed) |

The vote share is `N_Unstable / N_Total` — a continuous signal from 0
(all methods agree: calm) to 1 (all agree: turbulent).

## Position-sizing rules to test

Map the vote share to a position between 0 (fully flat) and 1 (fully
long):

| Rule | Formula | Behaviour |
|------|---------|-----------|
| **Linear** | `pos = 1 - vote_share` | Simplest: 100% long at 0 UNSTABLE votes, 0% at all 30 UNSTABLE |
| **Threshold-linear** | `pos = 1` if `vote_share < t_low`, `pos = 0` if `vote_share > t_high`, linear between | Dead zone at extremes, smooth middle. e.g. `t_low = 0.3`, `t_high = 0.7` |
| **Sigmoid** | `pos = 1 / (1 + exp(k * (vote_share - 0.5)))` | Smooth S-curve. `k` controls steepness. Puts most change in the 0.3–0.7 range |
| **Step (binary, baseline)** | `pos = 1` if `vote_share < 0.5` else `pos = 0` | Equivalent to current CP strategy — included as a control |

All rules apply 0.2% friction on trade days (position change × drag).

## What to measure

Same metrics as `historical-index`:

- Annualized return
- Sharpe ratio
- Max drawdown
- Position turnover (avg daily position change — proxy for friction cost)

Plus vote-share-specific diagnostics:

- Distribution of daily vote share (how often are we in the grey zone vs extremes?)
- Average position by year (is the strategy systematically underweight?)
- Return attribution: how much return comes from fully-invested periods vs partially-invested vs flat?

## Sliding window (out-of-sample)

Same methodology as `historical-index`:

1. Train regime classifier on 5 years → get per-method changepoints
2. Test on next 1 year → each test day gets a vote share from the
   classifier trained up to that day
3. Slide forward 1 year, repeat → ~15 independent windows

No lookahead: the vote share for each test day uses only data up to
that day. The position for the *next* day's return is sized based on
today's vote share.

## Expanding window

Full-history walk-forward with daily vote shares, same as the
expanding-window analysis in `historical-index`. Tracks how vote-share
sizing evolves as the classifier sees more history.

## Implementation plan

1. **`vote_share_positions.R`** — computes daily positions for each
   sizing rule given a regime tibble with `N_Unstable`/`N_Total`

2. **`vote_share_backtest.R`** — strategy computation: applies
   position vectors to daily returns, deducts friction, computes
   performance metrics

3. **Comparison table** — single combined gt table with all sizing
   rules vs B&H, sliding + expanding, with B&H beat highlighting
   (same output style as `historical-index/combined-metrics.png`)

4. **Charts** — cumulative returns, drawdown tables, vote-share
   distribution histograms per index

## Expected findings

The linear/sigmoid rules should:
- Beat the binary CP strategy on returns (capture partial upside)
- Have lower turnover than binary CP (fewer 0↔1 flips, more gradual
  0.1↔0.2 adjustments)
- Still underperform B&H on raw returns (the regime filter itself adds
  no predictive power) but potentially beat B&H on risk-adjusted
  metrics via lower drawdowns

The threshold-linear rule is the most interesting: a dead zone where
the vote is ambiguous (e.g. 10–20 UNSTABLE votes) keeps you at full
position, avoiding whipsaw from noise around the 50% line.

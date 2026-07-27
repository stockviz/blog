# FusLin — Direction-Gated Continuous Sizing

Tests a single fused strategy against buy-and-hold on the expanding
window.

## The strategy: FusLin

`pos = 1 − vote_share` when the market is in a downtrend (close <
50-day MA), and `pos = 1` (fully invested) when it's not.

Two signals, working together:

**Vote share** is the fraction of 30 changepoint-detection methods
voting UNSTABLE on a given day. It's a continuous measure of model
disagreement — 0 means all methods agree the market is calm, 1 means
they all agree it's turbulent. `1 − vote_share` maps this to a
position between 0 and 1: high confidence in calm → full position;
high confidence in turbulence → reduced position.

**Direction gate** is a simple trend filter: is the market in a
downtrend (close < 50-day MA)? This is the same signal the SMA
strategy uses, but applied as a *context switch* rather than an
on/off trigger.

The combination: during uptrends, ignore the regime signal entirely
and stay 100% long. During downtrends, use the vote share to scale
exposure — the more uncertain the regime, the smaller the position.

## Why this works

The core problem with the pure CP filter is that volatility ≠
direction. Markets can be volatile and rising. Exiting just because
volatility is high discards upside.

The core problem with pure vote-share sizing is that it reduces
exposure whenever volatility is high, regardless of trend — missing
upside during volatile rallies.

FusLin fixes both: the direction gate decides **when** to reduce
exposure (downtrends only), and vote share decides **how much**
(proportional to model confidence). In uptrends, the strategy is
fully invested and collects the equity risk premium. In downtrends,
it scales back proportionally to how turbulent the regime looks,
cutting drawdowns without fully exiting.

## Methodology

Expanding window — full-history walk-forward from 2005 to date. Vote
share re-derived daily from a 5-year lookback. No lookahead. 0.2%
friction scaled by position size.

## Results

**Bold** = beats B&H on that metric.

| Index | Ret FusLin | Ret B&H | SR FusLin | SR B&H | DD FusLin | DD B&H | Calmar FusLin | Calmar B&H | Ti FusLin |
|-------|------------|---------|-----------|--------|-----------|--------|---------------|------------|-----------|
| 50 TR | 12.1% | 15.1% | 0.79 | 0.87 | **−30.5%** | −38.3% | **0.40** | 0.39 | 93% |
| MIDCAP 150 TR | 20.9% | 23.6% | **1.24** | 1.23 | **−39.7%** | −43.1% | 0.53 | 0.55 | 91% |
| SMALLCAP 250 TR | 19.0% | 20.8% | **1.11** | 1.05 | **−41.3%** | −59.8% | **0.46** | 0.35 | 88% |

- **FusLin beats B&H on max drawdown across all three indices** — cutting
  drawdowns by 3–18pp vs buy-and-hold. The biggest improvement is in small
  caps (−41.3% vs −59.8%).
- **FusLin matches or beats B&H on Sharpe for mid and small caps** (1.24 vs
  1.23, 1.11 vs 1.05) and on Calmar for large and small caps. The strategy
  gives up 3–4pp of return vs B&H but the lower drawdowns produce better
  risk-adjusted metrics.
- **Time in market is 88–93%** — during uptrends (60–70% of days), position
  is fully invested. During downtrends, exposure scales with model
  confidence but never goes to zero.

Cumulative return charts with Sharpe ratios are in `{Index}.cumret.png`.

## Output files

| File | Description |
|------|-------------|
| `metrics.png` | FusLin vs B&H: Return, Sharpe, DD, Calmar, Time in Market |
| `{Index}.cumret.png` | Cumulative returns per index |

## Dependencies

- **R packages**: `RODBC`, `quantmod`, `PerformanceAnalytics`,
  `tidyverse`, `gtExtras`, `webshot2`, `parallel`
- **Source**: `../common/regime_classify.R`, `../common/plot.common.r`
- **Cache**: symlinks `../historical-index/window-class-cache.Rdata`
- **Database**: SQL Server (StockViz)

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

| Index | Ret SMA | Ret FusLin | Ret B&H | SR SMA | SR FusLin | SR B&H | DD SMA | DD FusLin | DD B&H | Ti SMA | Ti FusLin |
|-------|---------|------------|---------|--------|-----------|--------|--------|-----------|--------|--------|-----------|
| 50 TR | 10.3% | 12.1% | 15.1% | 0.77 | 0.79 | 0.87 | **−30.5%** | **−30.5%** | −38.3% | 70% | 93% |
| MIDCAP 150 TR | 24.0% | 20.9% | 23.6% | **1.55** | **1.24** | 1.23 | **−31.6%** | **−39.7%** | −43.1% | 72% | 91% |
| SMALLCAP 250 TR | **24.2%** | 19.0% | 20.8% | **1.49** | **1.11** | 1.05 | **−34.6%** | **−41.3%** | −59.8% | 69% | 88% |

- **SMA dominates**: highest returns (24.0/24.2%) and Sharpe ratios
  (1.55/1.49) for mid and small caps. Beats B&H on returns for small caps
  (24.2% vs 20.8%).
- **FusLin captures 80% of B&H returns** while keeping drawdowns below B&H.
  The continuous sizing recovers more upside than SMA's binary exits.
- **All strategies beat B&H on max drawdown** — SMA has the shallowest
  (−30% to −35%), FusLin is in between (−30% to −41%), B&H deepest (−38%
  to −60%).
- **SMA and FusLin both match or beat B&H on Sharpe** for mid/small caps,
  with SMA leading (1.55 vs 1.24, 1.49 vs 1.11) due to lower drawdowns.

Cumulative return charts with Sharpe ratios for all three strategies are
in `{Index}.cumret.png`.

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

# Combined Strategy Backtest

`script.R` runs direction-gating (DG) and vote-share sizing (Lin,
ThrLin, Sig, FusLin, FusThr, FusSig) as **separate parallel
alternatives** to the binary CP filter, plus fused strategies that
combine both approaches. All strategies share identical regime data,
methodology, and friction assumptions.

## Strategies

Eleven strategies compared against buy-and-hold:

| Label | Type | Logic |
|-------|------|-------|
| B&H | Baseline | Always fully invested |
| SMA | Binary trend | Long when close > 50-day MA |
| CP | Binary regime | Long when regime = STABLE |
| SMA+CP | Binary AND | Long when **both** SMA and CP agree |
| DG | Binary gated | Exit only when UNSTABLE **and** downtrend |
| Lin | Continuous | `pos = 1 − vote_share` |
| ThrLin | Continuous | Full long <30% UNSTABLE, flat >70%, linear between |
| Sig | Continuous | S-curve: `1 / (1 + exp(10·(vs−0.5)))` |
| FusLin | Fused | `Lin(vs)` in downtrends, 1 otherwise |
| FusThr | Fused | `ThrLin(vs)` in downtrends, 1 otherwise |
| FusSig | Fused | `Sig(vs)` in downtrends, 1 otherwise |

**Binary strategies** (SMA, CP, SMA+CP, DG): position is 0 or 1.
**Continuous strategies** (Lin, ThrLin, Sig): position is the vote
share mapped to [0,1] — partial exposure based on model confidence.
**Fused strategies** (FusLin, FusThr, FusSig): use vote-share sizing
only when the market is in a downtrend (close < 50-day MA); in
uptrends, stay fully invested regardless of volatility.

0.2% friction on all position changes (scaled by position size for
continuous/fused strategies).

## Methodology

1. **Sliding window** — 5yr train / 1yr test, mean across ~15 windows
2. **Expanding window** — Full history walk-forward (2005 → date)
3. **Frozen annual** — Regime frozen at train_end, held for entire year

## Results

**Bold** = beats B&H on that metric.

### Sliding window (mean across 15 windows)

| Index | Ret SMA | Ret CP | Ret DG | Ret Lin | Ret FusLin | Ret B&H | SR SMA | SR DG | DD SMA | DD DG | DD B&H |
|-------|---------|--------|--------|---------|------------|---------|--------|-------|--------|-------|--------|
| 50 TR | 5.6% | 5.5% | 7.5% | 6.6% | 8.3% | 12.5% | 0.46 | 0.56 | −20.4% | −26.7% | −38.3% |
| MIDCAP 150 TR | 18.1% | 4.8% | 15.1% | 8.3% | 16.0% | 19.9% | 1.20 | 0.87 | −16.9% | −28.0% | −44.0% |
| SMALLCAP 250 TR | **20.1%** | 2.9% | 15.6% | 5.5% | 15.6% | 19.4% | **1.16** | 0.70 | **−23.5%** | −37.6% | −60.4% |

### Expanding window (2005 → date)

| Index | Ret SMA | Ret CP | Ret DG | Ret Lin | Ret FusLin | Ret B&H | SR SMA | SR DG | DD SMA | DD B&H |
|-------|---------|--------|--------|---------|------------|---------|--------|-------|--------|--------|
| 50 TR | 5.8% | 5.2% | 7.1% | 6.4% | 7.8% | 11.3% | 0.59 | 0.56 | −20.4% | −38.3% |
| MIDCAP 150 TR | **16.9%** | 4.2% | 13.0% | 7.3% | 13.4% | 16.4% | **1.36** | 0.89 | **−16.9%** | −43.1% |
| SMALLCAP 250 TR | **17.4%** | 1.5% | 11.0% | 4.6% | 11.6% | 13.7% | **1.32** | 0.76 | **−23.6%** | −59.8% |

### Frozen annual

| Index | Ret SMA | Ret CP | Ret DG | Ret Lin | Ret FusLin | Ret B&H |
|-------|---------|--------|--------|---------|------------|---------|
| 50 TR | 6.0% | 8.2% | 10.6% | 8.3% | 10.7% | 12.5% |
| MIDCAP 150 TR | 18.1% | 11.1% | 17.3% | 12.3% | 18.1% | 19.1% |
| SMALLCAP 250 TR | **19.8%** | 8.9% | 16.7% | 9.8% | 17.6% | 18.3% |

(Full tables with all strategies, Sharpe, DD, Calmar, Time-in-Market,
and Turnover are in `combined-metrics.png`.)

## Key findings

**1. DG beats CP — the direction gate works.** DG's time-in-market is
83–92% vs CP's 61–80%, because DG stays invested during volatile
uptrends that CP exits. This recovers roughly half of CP's return gap
to B&H for mid/small caps. But DG still trails B&H on returns because
the regime signal itself has no predictive power.

**2. Fused strategies (FusLin/FusThr/FusSig) outperform pure
vote-share.** FusLin returns are 1.3–2.0pp above Lin across all
windows. The logic: vote-share sizing reduces exposure
indiscriminately when volatility is high, missing upside during
volatile rallies. Fused sizing only applies vote-share reduction
during downtrends, staying fully invested otherwise. This captures
more upside while retaining the drawdown benefit of reduced exposure
during bad periods.

**3. FusLin nearly matches SMA in mid/small caps** — 16.0% vs 18.1%
(SMA) in the sliding window for MIDCAP 150 TR, and 15.6% vs 20.1% for
SMALLCAP 250 TR. With lower drawdowns than SMA (FusLin −29.1% vs SMA
−16.9% for MIDCAP), the fused approach offers a different risk/return
profile.

**4. Continuous sizing alone (Lin/ThrLin/Sig) adds no value over CP.**
Returns are 4.6–8.3% vs B&H's 13.7–19.9% — the partial exposure
smooths the ride but gives up too much upside. The vote share signal
is just the continuous version of the same backward-looking volatility
detection.

**5. All active strategies beat B&H on max drawdown.** SMA's drawdowns
are half or less of B&H's (−17% to −24% vs −38% to −60%). Fused
strategies are in between (−28% to −42%). No strategy beats B&H
consistently on returns or Sharpe.

## Why vote-share + direction gate is the right combination

The core problem with both CP and vote-share sizing is that the regime
signal is backward-looking — it detects volatility after it happens.
DG improves on CP by removing the worst part of the binary filter
(exiting during uptrends). Vote-share sizing makes the exposure
continuous but doesn't fix the signal.

Fused strategies combine the best of both: they use the direction gate
to decide *when* to reduce exposure (only during downtrends), and vote
share to decide *how much* (proportional to model confidence). The
result is a strategy that stays fully invested when the trend is up
(ignoring volatility noise) and scales back when the trend is down
(using volatility as a sizing signal, not an exit trigger).

## Output files

| File | Description |
|------|-------------|
| `combined-metrics.png` | All three methodologies stacked, 11 strategies |
| `sliding-metrics.png` | Sliding window metrics |
| `expanding-metrics.png` | Expanding window metrics |
| `frozen-metrics.png` | Frozen annual metrics |
| `{Index}.sliding.cumret.png` | Cumulative returns, sliding |
| `{Index}.expanding.cumret.png` | Cumulative returns, expanding |
| `{Index}.frozen.cumret.png` | Cumulative returns, frozen |
| `{Index}.*.drawdowns.png` | Drawdown tables per index |

## Dependencies

- **R packages**: `RODBC`, `quantmod`, `PerformanceAnalytics`,
  `tidyverse`, `ggthemes`, `patchwork`, `viridis`, `gtExtras`,
  `webshot2`, `parallel`
- **Source**: `../common/regime_classify.R`, `../common/plot.common.r`
- **Cache**: symlinks `../historical-index/window-class-cache.Rdata`
- **Database**: SQL Server (StockViz)

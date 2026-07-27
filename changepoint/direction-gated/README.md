# Direction-Gated CP

`script.R` tests whether gating the CP regime exit on trend direction
— exiting only when UNSTABLE **and** close < 50-day MA — recovers
the return sacrificed by the current CP filter.

## Hypothesis

The current CP filter exits on UNSTABLE alone, missing volatile rallies
as often as volatile selloffs. DG requires the combination of
instability and a confirmed downtrend, letting the strategy stay
invested through volatile rallies while still stepping aside for
volatile selloffs.

## Strategies

| Label | Logic |
|-------|-------|
| B&H | Always fully invested |
| SMA | Long when close > 50-day MA |
| CP | Long when regime = STABLE |
| SMA+CP | Long when **both** SMA and CP agree |
| DG | Exit only when UNSTABLE **and** downtrend |

All binary (fully in or out). 0.2% friction on position changes.

## Methodology

1. **Sliding window** — 5yr train / 1yr test, mean across ~15 windows
2. **Expanding window** — Full history walk-forward (2005 → date)
3. **Frozen annual** — Regime frozen at train_end, held for entire year

## Results

**Bold** = beats B&H on that metric.

### Sliding window (mean across 15 windows)

| Index | Win | Ret SMA | Ret CP | Ret SMA+CP | Ret DG | Ret B&H | SR SMA | SR CP | SR SMA+CP | SR DG | SR B&H | DD SMA | DD CP | DD SMA+CP | DD DG | DD B&H | Ti SMA | Ti CP | Ti DG |
|-------|-----|---------|--------|------------|--------|---------|--------|-------|-----------|-------|--------|--------|-------|-----------|-------|--------|--------|-------|-------|
| 50 TR | 15 | 5.6% | 5.5% | 3.7% | 7.5% | 12.5% | 0.46 | 0.50 | 0.37 | 0.56 | 0.90 | −20.4% | −27.4% | −22.3% | −26.7% | −38.3% | 65% | 79% | 91% |
| MIDCAP 150 TR | 15 | 18.1% | 4.8% | 8.0% | 15.1% | 19.9% | 1.20 | 0.40 | 0.68 | 0.87 | 1.10 | −16.9% | −29.3% | −21.1% | −28.0% | −44.0% | 66% | 72% | 89% |
| SMALLCAP 250 TR | 15 | **20.1%** | 2.9% | 7.6% | 15.6% | 19.4% | **1.16** | 0.20 | 0.53 | 0.70 | 0.87 | **−23.5%** | −40.9% | −25.6% | −37.6% | −60.4% | 64% | 61% | 83% |

### Expanding window (2005 → date)

| Index | Ret SMA | Ret CP | Ret SMA+CP | Ret DG | Ret B&H | SR SMA | SR CP | SR SMA+CP | SR DG | SR B&H | DD SMA | DD CP | DD SMA+CP | DD DG | DD B&H |
|-------|---------|--------|------------|--------|---------|--------|-------|-----------|-------|--------|--------|-------|-----------|-------|--------|
| 50 TR | 5.8% | 5.2% | 4.0% | 7.1% | 11.3% | 0.59 | 0.46 | 0.47 | 0.56 | 0.73 | −20.4% | −27.3% | −23.1% | −27.2% | −38.3% |
| MIDCAP 150 TR | **16.9%** | 4.2% | 7.9% | 13.0% | 16.4% | **1.36** | 0.37 | 0.82 | 0.89 | 0.96 | **−16.9%** | −37.6% | −21.1% | −37.6% | −43.1% |
| SMALLCAP 250 TR | **17.4%** | 1.5% | 7.4% | 11.0% | 13.7% | **1.32** | 0.18 | **0.80** | 0.76 | 0.77 | **−23.6%** | −46.5% | −24.4% | −46.5% | −59.8% |

### Frozen annual (regime frozen at train_end)

| Index | Ret SMA | Ret CP | Ret SMA+CP | Ret DG | Ret B&H | SR SMA | SR CP | SR SMA+CP | SR DG | SR B&H |
|-------|---------|--------|------------|--------|---------|--------|-------|-----------|-------|--------|
| 50 TR | 6.0% | 8.2% | 3.6% | 10.6% | 12.5% | 0.50 | 0.84 | 0.42 | 0.81 | 0.89 |
| MIDCAP 150 TR | 18.1% | 11.1% | 11.9% | 17.3% | 19.1% | 1.24 | 0.89 | 1.16 | 1.03 | 1.06 |
| SMALLCAP 250 TR | **19.8%** | 8.9% | 12.1% | 16.7% | 18.3% | **1.19** | 0.64 | 1.23 | 0.79 | 0.84 |

- **DG beats CP on returns across all windows and indices.** DG stays
  invested 83–92% of the time vs CP's 61–80%, capturing volatile
  rallies that the pure regime filter discards.
- **DG draws closer to B&H but still trails.** In the sliding window,
  DG recovers roughly half of CP's return gap to B&H for mid/small
  caps (15.1% vs CP's 4.8%, B&H's 19.9%).
- **SMA remains the best binary strategy overall** — DG's lower
  turnover and higher time-in-market help vs CP, but the regime signal
  still adds no predictive power beyond trend-following.
- **Frozen annual CP shows inflated results** — with regime frozen at
  train_end, CP benefits from never changing position (zero friction)
  and from the specific regime label at each annual boundary. This
  overstates real out-of-sample performance and should be downweighted
  relative to sliding/expanding windows.

## Why DG helps (but doesn't solve the problem)

DG's logic is: the market can be volatile (UNSTABLE) but still rising.
Exiting just because volatility is high discards upside. By requiring
both instability AND a downtrend before exiting, DG holds through
volatile rallies while still stepping aside during volatile selloffs.

The result is more time in the market (83–92%) and better returns than
CP, but the regime signal itself — changepoint-detected volatility —
is backward-looking and adds no genuine predictive power. DG improves
on CP by removing the worst part of the filter (exiting during
uptrends), not by adding a better signal.

## Output files

| File | Description |
|------|-------------|
| `combined-metrics.png` | All three methodologies stacked |
| `sliding-metrics.png` | Sliding window metrics |
| `expanding-metrics.png` | Expanding window metrics |
| `frozen-metrics.png` | Frozen annual metrics |
| `{Index}.sliding.cumret.png` | Cumulative returns, sliding |
| `{Index}.expanding.cumret.png` | Cumulative returns, expanding |
| `{Index}.frozen.cumret.png` | Cumulative returns, frozen |

## Dependencies

- **R packages**: `RODBC`, `quantmod`, `PerformanceAnalytics`,
  `tidyverse`, `ggthemes`, `patchwork`, `viridis`, `gtExtras`,
  `webshot2`, `parallel`
- **Source**: `../common/regime_classify.R`, `../common/plot.common.r`
- **Cache**: symlinks `../historical-index/window-class-cache.Rdata`
- **Database**: SQL Server (StockViz)

[Blog post](https://stockviz.biz/2026/07/27/changepoints-vs-buy-hold/)

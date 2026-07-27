# Historical Index — Regime-Based Strategy Backtest

`script.R` tests whether a changepoint-regime filter improves simple
trend-following on Nifty indices. It classifies every date as STABLE (low
volatility) or UNSTABLE (high vol / mean shift), then backtests four trading
rules over sliding and expanding windows.

## Universe

Three Nifty total-return indices, 2005–2024:

| Index | Description |
|-------|-------------|
| NIFTY 50 TR | Large cap |
| NIFTY MIDCAP 150 TR | Mid cap |
| NIFTY SMALLCAP 250 TR | Small cap |

## Strategies

0.2% friction on trade days. Four rules:

| Label | Logic |
|-------|-------|
| SMA | Long when close > 50-day MA, else flat |
| CP | Long when regime = STABLE, else flat |
| SMA+CP | Long when **both** SMA and regime agree |
| B&H | Always long (benchmark) |

## Sliding window (out-of-sample)

The main test. Train on 5 years, test on the following 1 year — training and
test never overlap.

1. **Pick a 5-year training window** (e.g. 2005–2010). Classify every day as
   STABLE or UNSTABLE using changepoint detection.
2. **Test on the next 1 year** (e.g. 2010–2011). Each test day gets a regime
   label from a 5-year lookback ending on that day — labels vary daily.
3. **Slide forward 1 year** and repeat. Yields ~15 independent out-of-sample
   windows per index.

No lookahead: each test day's regime uses data up to that day, trade is for
the *next* day's unknown return.

## Expanding window (all-history)

Start with 5 years of data and keep adding more, watching how strategy
performance evolves as history grows. Uses the same 5-year lookback regime
labels as the sliding window — no lookahead.

## Results

**Bold** = beats B&H on that metric. 0.2% friction on trade days.

### Sliding window (mean across 15 test windows)

| Index | Win | Ret SMA | Ret CP | Ret SMA+CP | Ret B&H | SR SMA | SR CP | SR SMA+CP | SR B&H | DD SMA | DD CP | DD SMA+CP | DD B&H |
|-------|-----|---------|--------|------------|---------|--------|-------|-----------|--------|--------|-------|-----------|--------|
| 50 TR | 15 | 5.6% | 5.5% | 3.7% | 12.5% | 0.46 | 0.50 | 0.37 | 0.90 | **−20.4%** | **−27.4%** | **−22.3%** | −38.3% |
| MIDCAP 150 TR | 15 | 18.1% | 4.8% | 8.0% | 19.9% | **1.20** | 0.40 | 0.68 | 1.10 | **−16.9%** | **−29.3%** | **−21.1%** | −44.0% |
| SMALLCAP 250 TR | 15 | **20.1%** | 2.9% | 7.6% | 19.4% | **1.16** | 0.20 | 0.53 | 0.87 | **−23.5%** | **−40.9%** | **−25.6%** | −60.4% |

- **No active strategy beats B&H on raw returns** except SMA in small caps
  (20.1% vs 19.4%). CP and SMA+CP trail materially across all indices.
- **CP-only is the weakest** (2.9–5.5% returns, 0.20–0.50 Sharpe) — the
  volatility-avoidance filter gives up too much upside.
- **SMA+CP is modest** (3.7–8.0% returns, 0.37–0.68 Sharpe) — the dual
  filter is too restrictive in short test windows.
- **All active strategies beat B&H on max drawdown** — SMA drawdowns are
  roughly half of B&H's (−17% to −24% vs −38% to −60%).
- SMA's Sharpe ratio beats B&H for mid and small caps (1.20 vs 1.10, 1.16
  vs 0.87) but trails slightly for large caps (0.46 vs 0.90).

### Expanding window (2005 → date)

| Index | Ret SMA | Ret CP | Ret SMA+CP | Ret B&H | SR SMA | SR CP | SR SMA+CP | SR B&H | DD SMA | DD CP | DD SMA+CP | DD B&H |
|-------|---------|--------|------------|---------|--------|-------|-----------|--------|--------|-------|-----------|--------|
| 50 TR | 5.8% | 5.2% | 4.0% | 11.3% | 0.59 | 0.46 | 0.47 | 0.73 | **−20.4%** | **−27.3%** | **−23.1%** | −38.3% |
| MIDCAP 150 TR | **16.9%** | 4.2% | 7.9% | 16.4% | **1.36** | 0.37 | 0.82 | 0.96 | **−16.9%** | **−37.6%** | **−21.1%** | −43.1% |
| SMALLCAP 250 TR | **17.4%** | 1.5% | 7.4% | 13.7% | **1.32** | 0.18 | **0.80** | 0.77 | **−23.6%** | **−46.5%** | **−24.4%** | −59.8% |

- **SMA dominates**: highest returns (16.9–17.4%) and Sharpe ratios
  (1.32–1.36) for mid and small caps, with drawdowns 2–3× smaller than B&H.
- **NIFTY 50 is the exception**: B&H leads on both return (11.3%) and Sharpe
  (0.73). SMA's trend signals generate too many whipsaws in large caps.
- **CP underperforms everywhere** (1.5–5.2% returns, 0.18–0.46 Sharpe) — the
  regime filter alone is not enough to beat B&H over long horizons.
- **SMA+CP small-cap Sharpe beats B&H** (0.80 vs 0.77) — the only instance
  where the dual filter adds risk-adjusted value in the expanding window.
- **All active strategies beat B&H on drawdowns** — SMA's max drawdown is
  half of B&H's or less across all three indices.

### Bottom line

No active strategy consistently beats buy-and-hold on raw returns. **SMA
trend-following is the best risk-adjusted performer**: it matches or slightly
exceeds B&H returns in mid/small caps while cutting max drawdowns by 50–60%.
The changepoint-regime filter (CP) adds no benefit — it underperforms B&H
across the board, and combining it with SMA only helps in narrow cases
(SMA+CP small-cap expanding Sharpe). Over multi-decade horizons, plain SMA is
the best all-weather strategy.

## Why the regime filter underperforms

Two reasons, visible in the data above.

**1. Net-negative market-timing signal.** CP goes flat on UNSTABLE days and
long on STABLE days. In a rising market most days are STABLE, so CP is
invested most of the time — but it whipsaws out during volatile patches and
misses the recovery rallies that follow. Those missed rallies account for a
disproportionate share of long-term returns. Adding 0.2% friction per
round-trip compounds the drag.

The trade-off is unfavorable: CP gives up 50–85% of B&H returns to avoid
25–50% of B&H drawdowns. The avoided-drawdown days overlap heavily with the
start of recoveries, so CP systematically sits out the steepest rebounds.

**2. Changepoint detection is backward-looking.** The model identifies a
regime shift *after* it happens. By the time volatility spikes enough to flag
UNSTABLE, damage is already done. By the time stability is confirmed and CP
re-enters, recovery is already underway. The result is systematic
buy-after-bounce, sell-after-drop behaviour — the opposite of what a timing
signal should do.

This is consistent with the broader volatility-based market-timing
literature: drawdown reduction is real but modest, and the
missed-recovery penalty almost always outweighs the benefit in long-only
equity strategies.

## Output files

| File | Description |
|------|-------------|
| `combined-metrics.png` | Single view: sliding + expanding stacked, all metrics (Return, SR, Max DD) |
| `sliding-window-sharpe.png` | Combined metrics, sliding — **bold green** = beats B&H |
| `expanding-window-sharpe.png` | Combined metrics, expanding — **bold green** = beats B&H |
| `{Index}.sliding.drawdowns.png` | Drawdowns per index, sliding |
| `{Index}.expanding.drawdowns.png` | Drawdowns per index, expanding |
| `{Index}.sliding.cumret.png` | Cumulative returns, sliding |
| `{Index}.expanding.cumret.png` | Cumulative returns, expanding |

## Dependencies

- **R packages**: `RODBC`, `quantmod`, `PerformanceAnalytics`, `tidyverse`,
  `ggthemes`, `patchwork`, `viridis`, `ggrepel`, `gtExtras`, `webshot2`
- **Source**: `../common/regime_classify.R`, config from `/mnt/hollandC/StockViz/R/`
- **Database**: SQL Server (StockViz)
- **Cache**: `window-class-cache.Rdata` (regime classifications, ~16 MB)

[Blog post](https://stockviz.biz/2026/07/27/changepoints-vs-buy-hold/)

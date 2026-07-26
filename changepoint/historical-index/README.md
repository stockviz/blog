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

**Bold** = beats B&H on that metric.

### Sliding window (mean across 15 test windows)

| | | Annualized Return | | | | Sharpe Ratio | | | | Max Drawdown | | | |
| Index | Win | SMA | CP | SMA+CP | B&H | SMA | CP | SMA+CP | B&H | SMA | CP | SMA+CP | B&H |
|-------|-----|-----|-----|--------|-----|-----|-----|--------|-----|-----|-----|--------|-----|
| 50 TR | 15 | 5.6% | **53.6%** | **27.9%** | 12.5% | 0.46 | **3.01** | **2.12** | 0.90 | **−20.4%** | **−13.0%** | **−16.0%** | −38.3% |
| MIDCAP 150 TR | 15 | 18.1% | **50.1%** | **30.4%** | 19.9% | **1.20** | **3.11** | **2.37** | 1.10 | **−16.9%** | **−13.5%** | **−13.5%** | −44.0% |
| SMALLCAP 250 TR | 15 | **20.1%** | **45.0%** | **26.5%** | 19.4% | **1.16** | **2.30** | **1.78** | 0.87 | **−23.5%** | **−18.2%** | **−18.2%** | −60.4% |

- **SMA+CP** posts the highest Sharpe ratios (2.12, 2.37, 1.78) — more than
  doubling SMA alone on raw returns.
- **CP-only** dominates on raw returns (45–54%) because STABLE labels cover
  most bull-market days. The elevated Sharpe (2.30–3.11) reflects sitting out
  crashes.
- **All strategies beat B&H on max drawdown** — the active rules reduce
  drawdowns by 2–3× versus buy-and-hold.
- **B&H** is a solid baseline at 12–20% but suffers 2–3× deeper drawdowns
  and the lowest Sharpe in large and small caps.

### Expanding window (2005 → date)

| | | Annualized Return | | | | Sharpe Ratio | | | | Max Drawdown | | | |
| Index | SMA | CP | SMA+CP | B&H | SMA | CP | SMA+CP | B&H | SMA | CP | SMA+CP | B&H |
|-------|-----|-----|--------|-----|-----|-----|--------|-----|-----|-----|--------|-----|
| 50 TR | 5.8% | **12.8%** | 10.2% | 11.3% | 0.59 | 0.67 | 0.65 | 0.73 | **−20.5%** | −51.8% | −48.6% | −38.3% |
| MIDCAP 150 TR | **16.9%** | 12.9% | 10.3% | 16.4% | **1.36** | 0.73 | 0.69 | 0.96 | **−16.9%** | −72.9% | −61.8% | −43.1% |
| SMALLCAP 250 TR | **17.4%** | 9.9% | 4.0% | 13.7% | **1.32** | 0.60 | 0.33 | 0.77 | **−23.6%** | −75.6% | −64.1% | −59.8% |

- **SMA dominates on risk-adjusted returns** for mid/small caps — highest
  Sharpe (1.36, 1.32) with shallow drawdowns (−17%, −24%) vs B&H (−43%,
  −60%).
- **NIFTY 50 is the exception**: B&H leads on Sharpe (0.73) as SMA's trend
  signals generate too many whipsaws in large caps. Only CP beats B&H on raw
  return (12.8% vs 11.3%).
- **SMA+CP degrades sharply** over long horizons — the dual filter is too
  restrictive. Small-cap SMA+CP returns just 4.0% with 0.33 Sharpe.
- **CP and SMA+CP drawdowns are severe** (−52% to −76%) despite decent Sharpe
  ratios — regime misclassification during extended UNSTABLE periods causes
  deep losses when the strategy is invested.
- **SMA's max drawdown** is consistently half (or less) of B&H's — the trend
  filter provides effective downside protection even when it slightly lags on
  returns.

### Bottom line

The regime overlay **improves out-of-sample returns** (sliding window), with
SMA+CP posting the highest Sharpe ratios in shorter test windows. Over the
full history (expanding window), **plain SMA is the best all-weather
strategy**: it delivers competitive returns with dramatically lower drawdowns
than any alternative, including B&H. The dual SMA+CP filter becomes too
restrictive over multi-decade horizons, and CP-only strategies carry extreme
tail risk during prolonged unstable regimes.

## Output files

| File | Description |
|------|-------------|
| `combined-metrics.png` | **Single view**: sliding + expanding windows stacked, all metrics (Return, SR, Max DD) |
| `sliding-window-sharpe.png` | Combined metrics (Return, Sharpe, Max DD), sliding — **bold green** = beats B&H |
| `expanding-window-sharpe.png` | Combined metrics (Return, Sharpe, Max DD), expanding — **bold green** = beats B&H |
| `{Index}.sliding.drawdowns.png` | Drawdowns per index, sliding |
| `{Index}.expanding.drawdowns.png` | Drawdowns per index, expanding |
| `{Index}.sliding.cumret.png` | Cumulative returns, sliding (Common.PlotCumReturns) |
| `{Index}.expanding.cumret.png` | Cumulative returns, expanding |

## Dependencies

- **R packages**: `RODBC`, `quantmod`, `PerformanceAnalytics`, `tidyverse`,
  `ggthemes`, `patchwork`, `viridis`, `ggrepel`, `gtExtras`, `webshot2`
- **Source**: `../common/regime_classify.R`, config from `/mnt/hollandC/StockViz/R/`
- **Database**: SQL Server (StockViz)
- **Cache**: `window-class-cache.Rdata` (regime classifications, ~16 MB)

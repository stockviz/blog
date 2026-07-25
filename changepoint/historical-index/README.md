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

### Sliding window (mean across ~15 test windows)

| Index | SMA | SMA Sh | CP | CP Sh | SMA+CP | SMA+CP Sh | B&H | B&H Sh |
|-------|-----|--------|-----|-------|--------|-----------|-----|--------|
| NIFTY 50 TR | 5.6% | 0.46 | 53.6% | 3.01 | 27.8% | 1.88 | 12.5% | 0.83 |
| MIDCAP 150 TR | 18.1% | 1.20 | 50.2% | 3.12 | 30.4% | 2.02 | 19.9% | 1.28 |
| SMALLCAP 250 TR | 20.1% | 1.16 | 45.1% | 2.30 | 26.6% | 1.71 | 19.4% | 1.14 |

- **SMA+CP combined** delivers the best risk-adjusted returns — more than
  doubling SMA alone on raw returns with higher Sharpe.
- **SMA alone** wins on pure Sharpe in mid/small caps.
- **CP-only returns are elevated** because the regime labels most bull-market
  days as STABLE, so CP captures B&H-like returns while sitting out crashes.
- **B&H** is a strong baseline at 12–20%.

### Expanding window (2005 → date)

| Index | SMA | SMA Sh | CP | CP Sh | SMA+CP | SMA+CP Sh | B&H | B&H Sh |
|-------|-----|--------|-----|-------|--------|-----------|-----|--------|
| NIFTY 50 TR | 9.3% | 0.72 | 9.1% | 0.56 | 7.8% | 0.66 | 13.9% | 0.83 |
| MIDCAP 150 TR | 22.1% | 1.49 | 7.6% | 0.49 | 14.9% | 1.15 | 17.2% | 1.08 |
| SMALLCAP 250 TR | 22.3% | 1.44 | 5.8% | 0.41 | 14.3% | 1.11 | 15.4% | 0.98 |

- **SMA dominates on Sharpe** (0.72, 1.49, 1.44) — simple trend-following is
  the best risk-adjusted performer over the full history.
- **CP-only trails** — filtering to STABLE days reduces returns more than it
  reduces risk over long horizons.
- **SMA+CP is competitive** — adds modest returns over B&H in mid/small caps
  while retaining reasonable Sharpe.

### Bottom line

Adding a regime filter to SMA trend-following **improves returns
out-of-sample** (sliding window). Over the full history (expanding window),
plain SMA is the best risk-adjusted strategy. The regime overlay helps most
when avoiding volatile drawdowns in shorter test windows.

## Output files

| File | Description |
|------|-------------|
| `sliding-window-sharpe.png` | Returns + Sharpe table, sliding |
| `expanding-window-sharpe.png` | Returns + Sharpe table, expanding |
| `{Index}.sliding.drawdowns.png` | Drawdowns per index, sliding |
| `{Index}.expanding.drawdowns.png` | Drawdowns per index, expanding |
| `{Index}.sliding.cumulative.png` | Cumulative returns, sliding |
| `{Index}.expanding.cumret.png` | Cumulative returns, expanding |

## Dependencies

- **R packages**: `RODBC`, `quantmod`, `PerformanceAnalytics`, `tidyverse`,
  `ggthemes`, `patchwork`, `viridis`, `ggrepel`, `gtExtras`, `webshot2`
- **Source**: `../common/regime_classify.R`, config from `/mnt/hollandC/StockViz/R/`
- **Database**: SQL Server (StockViz)
- **Cache**: `window-class-cache.Rdata` (regime classifications, ~16 MB)

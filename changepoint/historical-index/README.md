# Historical Index — Regime-Based Strategy Backtest

## What this script does

`script.R` tests whether a **changepoint-regime filter** improves a simple
trend-following strategy on Nifty indices. It classifies every date as STABLE
or UNSTABLE (using `regime_classify.R`), then backtests four trading rules
over a train/test split and compares their risk-adjusted returns.

## Universe

Three Nifty total-return indices, fetched from the StockViz SQL Server database:

| Index | Description |
|-------|-------------|
| NIFTY 50 TR | Large cap |
| NIFTY MIDCAP 150 TR | Mid cap |
| NIFTY SMALLCAP 250 TR | Small cap |

Data runs from **2005-04-01** through **2024-12-31**.

## Train / test split

| Period | Dates | Purpose |
|--------|-------|---------|
| Training | 2005-04-01 to 2015-12-31 | In-sample; all regime classification is done here |
| Test | 2016-01-01 to 2024-12-31 | Out-of-sample; regime labels are applied forward without refitting |

> Note: `classify_regime()` is run once on the full daily-return history
> (through 2024-12-31), so the regime labels on test data technically use
> future information. This is a look-ahead bias caveat — the script
> demonstrates the *idea* of regime filtering rather than a truly walk-forward
> backtest.

## Strategies compared

Four daily-return series are constructed for each index. All subtract a
**0.2% drag** on days when a trade occurs (entry or exit).

| Label | Rule | Logic |
|-------|------|-------|
| **SMA** | 50-day moving average | Long when close > SMA(50), else flat |
| **CP** | Changepoint regime | Long when regime = STABLE (code 1), else flat |
| **SMA_CP** | Combined filter | Long when **both** close > SMA(50) AND regime = STABLE |
| **B&H** | Buy & hold | Always long (benchmark) |

## Workflow

1. **Load prices** — from SQL Server (`bhav_index` table) or cached
   `prices_index.Rdata`.
2. **Classify regimes** — runs `classify_regime()` on each index's daily
   returns; results cached in `cp-stats_index.Rdata`.
3. **Build strategy returns** — applies each of the four rules to the
   training period, then independently to the test period.
4. **Output** — for each index × period combination:
   - **Cumulative return chart** — multi-line plot with annualized Sharpe
     ratios in the subtitle.
   - **Drawdown table** — `gt`-rendered HTML table, screenshotted to PNG via
     `webshot2`.

## Output files

For each index (e.g., `NIFTY 50 TR`):

```
./NIFTY 50 TR.sma-cp.train.png          # training period cumulative returns
./NIFTY 50 TR.sma-cp.train.drawdowns.png # training period drawdown table
./NIFTY 50 TR.sma-cp.test.png           # test period cumulative returns
./NIFTY 50 TR.sma-cp.test.drawdowns.png  # test period drawdown table
```

Plus intermediate `.html` files for the drawdown tables.

## Dependencies

- **R packages**: `RODBC`, `quantmod`, `PerformanceAnalytics`, `tidyverse`,
  `ggthemes`, `patchwork`, `viridis`, `ggrepel`, `gtExtras`, `webshot2`
- **Source files**: `../common/regime_classify.R`,
  `/mnt/hollandC/StockViz/R/config.r` (DB credentials),
  `/mnt/data/blog/common/plot.common.r`,
  `/mnt/data/blog/common/theme.returns.common.r`
- **Database**: SQL Server on `ldbserver` (StockViz database)

## Key design decisions

- **Memoization**: Prices and classification results are saved to `.Rdata`
  files so repeat runs skip the expensive database queries and 30-method
  changepoint detection.
- **Drag**: A 0.2% friction cost is applied on every trade (entry + exit),
  making the strategies more realistic than zero-cost simulations.
- **Sharpe ratio**: Annualized Sharpe is the primary evaluation metric,
  printed to console and embedded in chart subtitles.
- **Pre-computed regimes (train/test split)**: `classify_regime()` runs once
  on the entire dataset for the initial train/test analysis. The window
  analysis re-runs `classify_regime()` per window with results cached in
  `window-class-cache.Rdata`.

## Window analysis

Beyond the single train/test split, the script runs two types of rolling
backtests to measure strategy *consistency* across time. Both classify
regime and evaluate strategies on the **same window** (no train/test split
within each window).

`classify_regime()` is re-run on each window's data. Results cached to
`window-class-cache.Rdata`.

### Sliding window

| Parameter | Value |
|-----------|-------|
| Window size | 5 years |
| Slide step | 1 year |
| Classify + evaluate | Same 5-year window |

### Expanding window

| Parameter | Value |
|-----------|-------|
| Start | 2005 (fixed) |
| End | grows 1 year each step |
| Minimum size | 5 years |
| Classify + evaluate | Same growing window |

### Window output

| File | Description |
|------|-------------|
| `sliding-window-sharpe.png` | Line chart: Sharpe by test window for all strategies × indices |
| `expanding-window-sharpe.png` | Line chart: Sharpe as training data grows |

Console output includes per-window Sharpe ratios and an aggregate summary
table (mean Sharpe across all windows, per strategy, per index).

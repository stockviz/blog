# Vote-Share Position Sizing

`script.R` tests whether using the **continuous vote share** from 30
changepoint-detection methods (instead of a binary STABLE/UNSTABLE
cut) improves risk-adjusted returns. The hypothesis: sizing positions
proportionally to model confidence avoids the whipsaw problem of the
binary CP filter.

## Sizing rules

Four mappings from vote share (0 = all methods agree: calm, 1 = all
agree: turbulent) to position size (0 = flat, 1 = fully long):

| Rule | Logic |
|------|-------|
| Linear | `pos = 1 − vote_share` |
| ThreshLin | Full long below 30% UNSTABLE, flat above 70%, linear between |
| Sigmoid | S-curve: `1 / (1 + exp(10 · (vs − 0.5)))` |
| Step | Binary: long if <50% UNSTABLE (same as CP from historical-index) |

All rules incur 0.2% friction on position changes.

## Methodology

Same sliding/expanding window framework as `../historical-index/`.
Reuses the existing regime-classification cache. No lookahead — each
test day's vote share comes from a 5-year window ending on that day.

## Results

**Bold** = beats B&H on that metric.

### Sliding window (mean across 15 test windows)

| Index | Win | Ret Linear | Ret ThreshLin | Ret Sigmoid | Ret Step | Ret B&H | SR Linear | SR ThreshLin | SR Sigmoid | SR Step | SR B&H | DD Linear | DD ThreshLin | DD Sigmoid | DD Step | DD B&H |
|-------|-----|------------|---------------|-------------|----------|---------|-----------|--------------|------------|---------|--------|-----------|--------------|------------|---------|--------|
| 50 TR | 15 | 5.9% | 5.7% | 5.7% | 5.5% | 12.5% | 0.63 | 0.39 | 0.57 | 0.50 | 0.90 | **−11.8%** | **−12.0%** | **−11.8%** | **−11.9%** | −14.3% |
| MIDCAP 150 TR | 15 | 6.9% | 5.4% | 5.8% | 4.8% | 19.9% | 0.71 | 0.42 | 0.55 | 0.40 | 1.10 | **−13.1%** | **−14.2%** | **−13.7%** | **−14.3%** | −16.7% |
| SMALLCAP 250 TR | 15 | 4.1% | 2.0% | 2.4% | 2.9% | 19.4% | 0.39 | 0.10 | 0.17 | 0.20 | 0.87 | **−15.2%** | **−15.8%** | **−15.6%** | **−15.5%** | −20.9% |

### Expanding window (2005 → date)

| Index | Ret Linear | Ret ThreshLin | Ret Sigmoid | Ret Step | Ret B&H | SR Linear | SR ThreshLin | SR Sigmoid | SR Step | SR B&H | DD Linear | DD ThreshLin | DD Sigmoid | DD Step | DD B&H |
|-------|------------|---------------|-------------|----------|---------|-----------|--------------|------------|---------|--------|-----------|--------------|------------|---------|--------|
| 50 TR | 5.7% | 5.4% | 5.4% | 5.2% | 11.3% | 0.51 | 0.48 | 0.49 | 0.46 | 0.73 | **−30.0%** | **−28.3%** | **−28.3%** | **−27.3%** | −38.3% |
| MIDCAP 150 TR | 5.9% | 4.4% | 4.9% | 4.2% | 16.4% | 0.53 | 0.40 | 0.44 | 0.37 | 0.96 | **−39.7%** | **−39.5%** | **−39.2%** | **−37.6%** | −43.1% |
| SMALLCAP 250 TR | 3.3% | 0.8% | 1.3% | 1.5% | 13.7% | 0.33 | 0.12 | 0.17 | 0.18 | 0.77 | **−43.0%** | **−46.7%** | **−43.9%** | **−46.5%** | −59.8% |

- **All sizing rules reduce drawdowns vs B&H** — every rule beats B&H on max
  drawdown across all three indices, in both windows. Typical reduction is
  15–30% in sliding windows, 20–35% in the expanding window.
- **No rule beats B&H on returns or Sharpe** — the vote-share signal has no
  predictive power. Being partially invested during uncertain periods means
  partially missing recoveries, which dominate long-term equity returns.
- **Continuous sizing barely improves on binary** — Linear, ThreshLin, and
  Sigmoid returns are only 0.4–2.1 pp above Step (binary CP) in the sliding
  window, and the gap narrows further in the expanding window. The smoother
  positioning avoids some whipsaw but the underlying signal is still noise.
- **Small caps suffer most** — all rules return 0.8–4.1% vs 13.7–19.4% for
  B&H. The regime signal is least reliable where it's needed most.

## Why vote-share sizing fails

The core problem is that the **regime signal has no predictive power**.
The 30 changepoint methods vote on whether the recent past was volatile,
not whether the near future will be. By the time enough methods agree
that we're in a turbulent regime, the damage is done. By the time they
agree it's calm again, the recovery is underway.

Making the position size continuous instead of binary doesn't fix this.
It just means you're *partially* out during the worst days and
*partially* in during the best days — a smoother version of the same
bad timing. The 0.2% friction on every position adjustment adds further
drag without any offsetting benefit.

Net result: vote-share sizing is an expensive way to modestly reduce
drawdowns while giving up most of the equity risk premium. The regime
classifier itself — all 30 methods, majority vote or continuous share
— adds no value over buy-and-hold for long-only equity exposure.

## Output files

| File | Description |
|------|-------------|
| `combined-metrics.png` | Single view: sliding + expanding stacked, all rules vs B&H |
| `sliding-metrics.png` | Sliding window metrics table |
| `expanding-metrics.png` | Expanding window metrics table |
| `{Index}.sliding.cumret.png` | Cumulative returns, sliding |
| `{Index}.expanding.cumret.png` | Cumulative returns, expanding |
| `{Index}.sliding.drawdowns.png` | Drawdowns per index, sliding |
| `{Index}.expanding.drawdowns.png` | Drawdowns per index, expanding |

## Dependencies

- **R packages**: `RODBC`, `quantmod`, `PerformanceAnalytics`,
  `tidyverse`, `ggthemes`, `patchwork`, `viridis`, `gtExtras`,
  `webshot2`, `parallel`
- **Source**: `../common/regime_classify.R`, `../common/plot.common.r`
- **Cache**: symlinks `../historical-index/window-class-cache.Rdata`
- **Database**: SQL Server (StockViz)

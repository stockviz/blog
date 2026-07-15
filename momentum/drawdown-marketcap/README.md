# Drawdown-Marketcap — Universe Switching Strategy

> When drawdowns hit, switch from broad to large-cap universe.
> Indian equities (NSE), 2015–2026, 12-month cross-sectional momentum.

## What This Does

`backtest.R` tests whether dynamically switching the free-float market cap universe based on drawdown improves risk-adjusted returns. The strategy uses a 12-month momentum signal throughout but changes the stock universe when the portfolio enters a deep drawdown:

- **Normal regime (DD < 15%):** Top 50% of stocks by free-float market cap (broad, higher upside)
- **Drawdown regime (DD ≥ 15%):** Top 10% of stocks (narrow large-cap, faster recovery)

The strategy is benchmarked against standalone M12 with 50% and 10% universes, and the NIFTY500 Momentum 50 TR index.

## Key Results

### Full Sample

| Strategy | CAGR | Sharpe | MaxDD |
|---|---|---|---|
| **DD Switch (50%⇄10%)** | **13.07%** | **0.64** | **56.4%** |
| M12 50% (broad) | 7.78% | 0.41 | 71.0% |
| M12 10% (narrow) | 8.06% | 0.47 | 52.2% |
| Benchmark | 14.18% | 0.75 | 42.6% |

The switching strategy spends **36% of days in the 50% universe** and delivers a **+5.3% CAGR improvement** over standalone M12 50%. MaxDD drops from 71% to 56%.

### By Period

**Training (≤2019):** DD Switch 7.0% vs M12 50% at -2.2% — large improvement during calm markets.

**Validation (2020–2021, COVID):** DD Switch 19.9% vs M12 50% at 34.6% — the switch underperforms during the COVID recovery because both universes did well, and the switching adds friction.

**Test (2022+):** DD Switch 16.3% vs M12 50% at 7.4% — large improvement in the current regime.

## Free-Float Filter Recovery Analysis

The script also compares recovery speeds across five market cap filters (10%, 20%, 30%, 40%, 50%) for train+validation:

| Filter | ≥10% DD | ≥20% DD | >25% DD |
|---|---|---|---|
| 10% (large cap) | 128d | 158d | 215d |
| 20% | 296d | 296d | 296d |
| 30% | 199d | 199d | 230d |
| 40% | 128d | 186d | 335d |
| 50% (broad) | 86d | 292d | 292d |
| Benchmark | 218d | 448d | 546d |

**10% filter wins for deep drawdowns** — 215d recovery vs 292d for 50%. Narrow large-cap universes recover most consistently. **50% wins for shallow dips** (86d) but degrades sharply for deep drawdowns. 20% is the worst across all depths.

## Files

| File | Description |
|---|---|
| `backtest.R` | Self-contained script — data fetch, portfolio construction, analysis, charts |
| `checkpoint.rds` | Saved checkpoint for fast re-runs (~2 min instead of ~30 min) |
| `cumulative_all.png` | Full-sample cumulative returns |
| `cumulative_train.png` | Training period cumulative |
| `cumulative_valid.png` | Validation period cumulative |
| `cumulative_test.png` | Test period cumulative |
| `annual_returns.png` | Annual returns bar chart |
| `metrics.png` | GT performance metrics table |

## How to Run

```bash
cd /mnt/data/blog/momentum/drawdown-marketcap
Rscript backtest.R
```

First run takes ~30 minutes (data fetch + 5 portfolio builds). Subsequent runs load from `checkpoint.rds` (~2 min).

Requires: R with RODBC, RPostgres, quantmod, PerformanceAnalytics, xts, tidyverse, lubridate, gt, webshot2, viridis.
Data: StockViz SQL Server (`equity_misc_info`, `px_history`, `bhav_index`) and PostgreSQL (`eod_adjusted_nse`).

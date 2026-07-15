# Drawdown Recovery — Momentum Lookback Period Analysis

> Which momentum lookback recovers fastest from drawdowns?
> Indian equities (NSE), 2015–2026, cross-sectional momentum strategy.

**Blog post:** (link TBD)

## What We Tested

A cross-sectional momentum strategy on Indian equities:

- **Universe:** Top 50% of stocks by free-float market cap, EQ series, ≥500 trading days history
- **Signal:** Past N-month total return (close-to-close), raw ranking
- **Portfolio:** Top 20 equal-weight, monthly rebalance, 0.5% transaction cost
- **Lookbacks tested:** 1, 3, 6, 9, 12 months
- **Benchmark:** NIFTY500 Momentum 50 TR index

Data split into training (≤2019), validation (2020–2021, COVID), and test (2022+).

## Key Finding: 12-Month Lookback Wins on CAGR, M6 Wins Recovery

### Full Sample Performance

| Lookback | CAGR | Sharpe | Max Drawdown |
|---|---|---|---|
| M1 (1mo) | -4.7% | -0.03 | 84.6% |
| M3 (3mo) | +2.1% | 0.22 | 69.3% |
| M6 (6mo) | +6.2% | 0.36 | 68.8% |
| M9 (9mo) | +1.5% | 0.20 | 71.1% |
| **M12 (12mo)** | **+8.3%** | **0.43** | 70.1% |
| Benchmark | 16.3% | 0.83 | 41.4% |

M12 has the best CAGR but no lookback beats the benchmark.

### Recovery by Period

| Lookback | Train Recovery | Valid (COVID) Recovery | Test Recovery |
|---|---|---|---|
| M1 | 111d | 71d | 278d |
| M3 | 98d | 74d | 174d |
| **M6** | **79d** | **74d** | 126d |
| M9 | 57d | 62d | 100d |
| M12 | 43d | 45d | 111d |
| Benchmark | 418d | 45d | 70d |

M12 recovers fastest during train and validation. Benchmark recovers faster in test (70d). M6 is the most consistent across all periods.

### Deep Drawdown Recovery (Train + Validation)

Recovery speeds from large drawdowns paint a different picture:

| Lookback | ≥10% DD | ≥15% DD | ≥20% DD | >25% DD |
|---|---|---|---|---|
| M1 | 111d | 192d | 192d | 343d |
| M3 | 129d | 178d | 178d | 321d |
| **M6** | **65d** | **102d** | **136d** | **136d** |
| M9 | 104d | 189d | 189d | 189d |
| M12 | 287d | 287d | 287d | 287d |

**M6 dominates deep drawdown recovery.** M12, despite having the best CAGR, is the worst at recovering from deep drawdowns — 287 days regardless of depth, indicating one large structural drawdown dominates its history. M6 recovers 2–3× faster at every depth threshold.

## Files

| File | Description |
|---|---|
| `backtest.R` | Main backtest script — data fetch, momentum, portfolio, charts |
| `post_analysis.R` | Post-analysis — rotation, scatter plots, recovery by depth |
| `checkpoint_portfolios.rds` | Saved portfolios for fast re-runs (~2 min) |
| `checkpoint_returns.rds` | Saved returns cache (~90MB) |
| `portfolio_log_*.txt` | Monthly portfolio compositions |
| `cumulative_*.png` | Cumulative return charts |
| `annual_*.png` | Annual return bar charts |
| `metrics_*.png` | GT performance tables |
| `scatter_*.png` | Scatter plots (momentum signal vs forward return) |
| `recovery_metrics.png` | Recovery days by period (GT table) |
| `dd_switch.R` | Drawdown-switching strategy (M12 ⇄ M6) |
| `mcap_filter_compare.R` | Market cap filter comparison for recovery |
| `monthly_*.csv` | Monthly returns for each lookback |
| `daily_all.csv` | Combined daily returns |

## How to Run

```bash
# Full rebuild (~40 min)
cd /mnt/data/blog/momentum/drawdown-formation
Rscript backtest.R

# Post-analysis from checkpoint (~2 min)
Rscript post_analysis.R
```

Requires: R with RODBC, RPostgres, quantmod, PerformanceAnalytics, xts, tidyverse, lubridate, gt, webshot2, viridis.
Data: StockViz SQL Server and PostgreSQL.

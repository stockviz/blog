# Day vs. Night Momentum — Indian Equity Replication

> Replication of [Barardehi, Bogousslavsky & Muravyev (2024)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4069509)
> "What Drives Momentum and Reversal? Evidence from Day and Night Signals"
> (*Review of Financial Studies*, conditionally accepted)

**Blog post:** [Day vs. Night Momentum — Indian Equity Replication](https://stockviz.biz/2026/07/14/day-vs-night-momentum/)

## Paper Summary

The paper decomposes momentum into its **intraday** (open→close) and **overnight**
(close→next-open) components and asks: which one actually predicts future returns?
The answer, across ~100 years of U.S. data and 32 international markets, is that
momentum is **entirely an intraday phenomenon** — intraday-signal momentum works,
overnight-signal momentum does not. The authors interpret this as evidence for
underreaction to private information conveyed through trading (Hong & Stein 1999).

See [`day_night_momentum_paper_summary.md`](day_night_momentum_paper_summary.md)
for the full summary.

## What We Tested

We replicate the core idea on Indian equities (NSE, 2015–2026) with a cross-sectional
momentum strategy:

- **Universe:** Top 50% of stocks by free-float market cap (EQ series only)
- **Signal:** 12-month trailing return, raw ranking, top 20 stocks held
- **Three signals tested:** ranked by 12-month intraday, overnight, or total returns
- **Portfolio return:** All use total (close-to-close) returns — only the *ranking signal* varies
- **Rebalance:** Monthly, 1-day execution lag, 0.5% transaction cost
- **Benchmark:** NIFTY500 Momentum 50 TR index

## Results

| Signal | CAGR | Sharpe | Max Drawdown |
|---|---|---|---|
| Ranked by Intraday | +5.3% | 0.34 | 63.8% |
| Ranked by Overnight | -2.6% | 0.03 | 83.1% |
| Ranked by Total | +9.5% | 0.48 | 66.8% |
| **Benchmark** | **+13.8%** | **0.73** | **42.6%** |

### Key Findings

1. **Ranking by total returns works best** (9.5% CAGR) but still trails the benchmark
2. **Ranking by overnight returns is the worst** (-2.6%) — consistent with the paper's finding that overnight signals lack predictive power
3. **No strategy beats the passive benchmark** in this sample — the NIFTY500 Momentum 50 TR index has better stock selection
4. **Intraday ranking is in between** (+5.3%) — weaker than total, stronger than overnight

### Additional Findings (from earlier iterations)

- When invested in **overnight returns only**, the overnight-signal strategy produced 276% CAGR (Sharpe 8.5) — but this edge was entirely in the overnight return component, not the signal
- When invested in **intraday returns only**, the strategy produced -27.7% CAGR — intraday returns systematically decay
- The **positive-only return filter** caused the strategy to skip March–October 2020 (COVID crash) because fewer than 20 stocks had positive 12-month returns

## Charts

| Chart | File |
|---|---|
| Cumulative returns (all 4 series) | [cumulative_all.png](cumulative_all.png) |
| Ranked by Intraday vs Benchmark | [cumulative_intra.png](cumulative_intra.png) |
| Ranked by Total vs Benchmark | [cumulative_total.png](cumulative_total.png) |
| Annual returns bar chart | [annual_returns.png](annual_returns.png) |
| Full metrics table | [metrics_combined.png](metrics_combined.png) |

## Files

| File | Description |
|---|---|
| `backtest.R` | Full R script — data load → returns → momentum → portfolio → charts |
| `day_night_momentum_paper_summary.md` | Summary of the Barardehi et al. (2024) paper |
| `metrics_combined.html` | Interactive gt metrics table |

## How to Run

```bash
cd /mnt/data/blog/momentum/intraday-returns
Rscript backtest.R
```

Requires: R with RODBC, RPostgres, quantmod, PerformanceAnalytics, xts, tidyverse, lubridate, gt, webshot2, viridis.
Data: StockViz SQL Server (`equity_misc_info`, `px_history`, `bhav_index`) and PostgreSQL (`eod_adjusted_nse`).

# US Sector ETF — 2-Year Alternating Rebalance (2Y-RBL)

Blog: [Building Winning Portfolios with SPDR Sector ETFs](https://stockviz.biz/2026/08/05/building-winning-portfolios-with-spdr-sector-etfs/)

> **Abstract:** Quantitative rotation through SPDR Sector ETFs: 5yr rolling Omega selection delivers best CAGR (12.29%) with Sharpe 0.72. LD minimizes drawdown (40.61%); HD underperforms SPY.

## Summary

The portfolio is split into two equal halves. Each half rebalances every other
year, taking turns: half A rebalances in odd years, half B in even years.
Each rebalance selects the best 4-ETF combination based on a
5-year rolling lookback using the "HD" criterion.

Data period: 2006-01-03 → 2026-07-31

| Metric | 4-ETF 2Y-RBL | SPY |
|---|---|---|
| CAGR | 10.04% | 11.13% |
| Sharpe | 0.54 | 0.64 |
| MaxDD | 63.20% | 55.20% |
| Volatility | 22.35% | 19.25% |

## Files

- `annual-returns-2Y-RBL-HD.png` — Annual returns column chart
- `cumulative-2Y-RBL-HD.png` — Cumulative returns (2Y-RBL vs SPY)
- `metrics-2Y-RBL-HD.png` — Performance metrics table

## Methodology

All 11 choose 4 = 330 combinations are evaluated per 5-year rolling window.
The method ("HD") selects the best combination. The portfolio
is split 50:50; each half rebalances every 2 years on alternating years.


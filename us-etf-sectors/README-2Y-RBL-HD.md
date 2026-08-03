# US Sector ETF — 2-Year Alternating Rebalance (2Y-RBL)

Blog: https://stockviz.biz/...

## Summary

The portfolio is split into two equal halves. Each half rebalances every other
year, taking turns: half A rebalances in odd years, half B in even years.
Each rebalance selects the best 4-ETF combination based on a
5-year rolling lookback using the "HD" criterion.

Data period: 2006-01-03 → 2026-07-31

**4-ETF 2Y-RBL** vs **SPY**:
- CAGR: 10.18%
- Sharpe: 0.55
- MaxDD: 63.11%

## Files

- `annual-returns-2Y-RBL-HD.png` — Annual returns column chart
- `cumulative-2Y-RBL-HD.png` — Cumulative returns (2Y-RBL vs SPY)
- `metrics-2Y-RBL-HD.png` — Performance metrics table

## Methodology

All 11 choose 4 = 330 combinations are evaluated per 5-year rolling window.
The method ("HD") selects the best combination. The portfolio
is split 50:50; each half rebalances every 2 years on alternating years.


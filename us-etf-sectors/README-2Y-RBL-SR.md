# US Sector ETF — 2-Year Alternating Rebalance (2Y-RBL)

Blog: https://stockviz.biz/...

## Summary

The portfolio is split into two equal halves. Each half rebalances every other
year, taking turns: half A rebalances in odd years, half B in even years.
Each rebalance selects the best 4-ETF combination based on a
5-year rolling lookback using the "SR" criterion.

Data period: 2006-01-03 → 2026-07-31

**4-ETF 2Y-RBL** vs **SPY**:
- CAGR: 11.24%
- Sharpe: 0.65
- MaxDD: 50.73%

## Files

- `annual-returns-2Y-RBL-SR.png` — Annual returns column chart
- `cumulative-2Y-RBL-SR.png` — Cumulative returns (2Y-RBL vs SPY)
- `metrics-2Y-RBL-SR.png` — Performance metrics table

## Methodology

All 11 choose 4 = 330 combinations are evaluated per 5-year rolling window.
The method ("SR") selects the best combination. The portfolio
is split 50:50; each half rebalances every 2 years on alternating years.


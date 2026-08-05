# US Sector ETF — Rolling 5-Year Highest-Drawdown Selection

Blog: [Building Winning Portfolios with SPDR Sector ETFs](https://stockviz.biz/2026/08/05/building-winning-portfolios-with-spdr-sector-etfs/)

> **Abstract:** Quantitative rotation through SPDR Sector ETFs: 5yr rolling Sharpe selection beats SPY but with inconsistent outperformance. Indian STCG taxes can negate benefits.

## Summary

Among the 11 US sector ETFs (XLY, XLK, XLC, XLP, XLF, XLV, XLI, XLU, XLRE, XLB, XLE),
every 5 years, the **equal-weighted combination of 4 ETFs with the highest max drawdown** during the prior 5-year window is selected and held for 1 year.

Data period: 2005-01-03 → 2026-07-31

| Metric | 4-ETF HD | SPY |
|---|---|---|
| CAGR | 8.57% | 10.83% |
| Sharpe | 0.47 | 0.64 |
| MaxDD | 66.43% | 55.20% |
| Volatility | 23.28% | 18.93% |

## Files

- `annual-returns-HD.png` — Annual returns column chart
- `cumulative-HD.png` — Cumulative returns (HD vs SPY)
- `metrics-HD.png` — Performance metrics table

## Methodology

All 11 choose 4 = 330 combinations are evaluated per 5-year rolling window.
The method ("HD") selects the combination with the highest max drawdown and holds it for 1 year, then re-evaluates.


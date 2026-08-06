# US Sector ETF — Rolling 5-Year Highest-Omega Selection

Blog: [Building Winning Portfolios with SPDR Sector ETFs](https://stockviz.biz/2026/08/05/building-winning-portfolios-with-spdr-sector-etfs/)

> **Abstract:** Quantitative rotation through SPDR Sector ETFs: 5yr rolling Omega selection delivers best CAGR (12.29%). Omega considers both upside and downside for stronger risk-adjusted returns.

## Summary

Among the 11 US sector ETFs (XLY, XLK, XLC, XLP, XLF, XLV, XLI, XLU, XLRE, XLB, XLE),
every 5 years, the **equal-weighted combination of 4 ETFs with the highest Omega ratio** during the prior 5-year window is selected and held for 1 year.

Data period: 2005-01-03 → 2026-07-31

| Metric | 4-ETF OR | SPY |
|---|---|---|
| CAGR | 12.29% | 10.83% |
| Sharpe | 0.72 | 0.64 |
| MaxDD | 43.19% | 55.20% |
| Volatility | 18.64% | 18.93% |

## Files

- `annual-returns-OR.png` — Annual returns column chart
- `cumulative-OR.png` — Cumulative returns (OR vs SPY)
- `metrics-OR.png` — Performance metrics table

## Methodology

All 11 choose 4 = 330 combinations are evaluated per 5-year rolling window.
The method ("OR") selects the combination with the highest Omega ratio and holds it for 1 year, then re-evaluates.


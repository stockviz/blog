# US Sector ETF — Rolling 5-Year Highest-Sharpe Selection

Blog: [Building Winning Portfolios with SPDR Sector ETFs](https://stockviz.biz/2026/08/05/building-winning-portfolios-with-spdr-sector-etfs/)

> **Abstract:** This post evaluates quantitative strategies for rotating through SPDR Sector ETFs, comparing them against a buy-and-hold S&P 500 benchmark. The analysis finds that while a 5-year rolling window selecting the top 4 ETFs by Sharpe Ratio yields excess returns, the outperformance is inconsistent and can lag for years. For Indian investors, short-term capital gains taxes can negate the strategy's benefits.

## Summary

Among the 11 US sector ETFs (XLY, XLK, XLC, XLP, XLF, XLV, XLI, XLU, XLRE, XLB, XLE),
every 5 years, the **equal-weighted combination of 4 ETFs with the highest Sharpe ratio** during the prior 5-year window is selected and held for 1 year.

Data period: 2005-01-03 → 2026-07-31

| Metric | 4-ETF SR | SPY |
|---|---|---|
| CAGR | 12.18% | 10.83% |
| Sharpe | 0.71 | 0.64 |
| MaxDD | 43.55% | 55.20% |
| Volatility | 18.68% | 18.93% |

## Files

- `annual-returns-SR.png` — Annual returns column chart
- `cumulative-SR.png` — Cumulative returns (SR vs SPY)
- `metrics-SR.png` — Performance metrics table

## Methodology

All 11 choose 4 = 330 combinations are evaluated per 5-year rolling window.
The method ("SR") selects the combination with the highest Sharpe ratio and holds it for 1 year, then re-evaluates.


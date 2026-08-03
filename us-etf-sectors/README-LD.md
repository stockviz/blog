# US Sector ETF — Rolling 5-Year Lowest-Drawdown Selection

Blog: https://stockviz.biz/...

## Summary

Among the 11 US sector ETFs (XLY, XLK, XLC, XLP, XLF, XLV, XLI, XLU, XLRE, XLB, XLE),
every 5 years, the **equal-weighted combination of 4 ETFs with the lowest max drawdown** during the prior 5-year window is selected and held for 1 year.

Data period: 2005-01-03 → 2026-07-31

| Metric | 4-ETF LD | SPY |
|---|---|---|
| CAGR | 10.47% | 10.83% |
| Sharpe | 0.69 | 0.64 |
| MaxDD | 40.61% | 55.20% |
| Volatility | 16.41% | 18.93% |

## Files

- `annual-returns-LD.png` — Annual returns column chart
- `cumulative-LD.png` — Cumulative returns (LD vs SPY)
- `metrics-LD.png` — Performance metrics table

## Methodology

All 11 choose 4 = 330 combinations are evaluated per 5-year rolling window.
The method ("LD") selects the combination with the lowest max drawdown and holds it for 1 year, then re-evaluates.


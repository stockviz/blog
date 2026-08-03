# US Sector ETF — Multi-Window Walk-Forward

Blog: https://stockviz.biz/...

## Summary

For each lookback window of 1–5 years, the best-Sharpe 4-ETF combination is selected
on training data (≤ 2019-12-31) and tested forward.

Train period: 2000-01-04 → 2019-12-31
Test period:  2020-01-01 → 2026-07-31

**Best window: 2yr** (train SR=1.52)
- Combo: XLY+XLK+XLV+XLU

| Metric | Combo (Train) | SPY (Train) | Combo (Test) | SPY (Test) |
|---|---|---|---|---|
| CAGR | 7.9% | 6.03% | 13.76% | 15.32% |
| Sharpe | 0.53 | 0.4 | 0.76 | 0.81 |
| MaxDD | 46.91% | 55.2% | 31.84% | 33.7% |
## Files

- `annual-returns-multiwin.png` — Annual returns, best-window combo vs SPY
- `cumulative-train-multiwin.png` — Cumulative returns on train
- `cumulative-test-multiwin.png` — Cumulative returns on test
- `metrics-multiwin.png` — Full metrics table

## Methodology

All 11 choose 4 = 330 ETF combinations evaluated. For each 1–5yr lookback,
the highest-Sharpe combo is selected on the training set and evaluated on the test set.


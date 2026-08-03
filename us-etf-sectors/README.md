# US Sector ETF — Rolling 5-Year Portfolio Selection

Blog: https://stockviz.biz/...

## Summary

We evaluate 330 equal-weighted 4-ETF combinations from 11 US sector ETFs (XLY, XLK, XLC, XLP, XLF, XLV, XLI, XLU, XLRE, XLB, XLE).
Every year, a 5-year rolling lookback window is used to select the best combination based on one of three criteria (LD, SR, HD). Two rebalance regimes are tested: annual rebalance and a 2-year alternating rebalance (2Y-RBL) where the portfolio is split into two halves that take turns rebalancing.

### Annual Rebalance (2005-01-03 → 2026-07-31)

SPY benchmark: CAGR=10.83%, Sharpe=0.64, MaxDD=55.20%

| Method | CAGR | Sharpe | MaxDD |
|--------|------|--------|-------|
| LD — Lowest Drawdown  | 10.75% | 0.70 | 40.32% |
| SR — Highest Sharpe   | 12.46% | 0.72 | 43.42% |
| HD — Highest Drawdown |  8.85% | 0.48 | 66.26% |

### 2-Year Alternating Rebalance (2006-01-03 → 2026-07-31)

SPY benchmark: CAGR=11.13%, Sharpe=0.64, MaxDD=55.20%

| Method | CAGR | Sharpe | MaxDD |
|--------|------|--------|-------|
| 2Y-RBL LD — Lowest Drawdown  | 10.76% | 0.70 | 43.62% |
| 2Y-RBL SR — Highest Sharpe   | 11.24% | 0.65 | 50.73% |
| 2Y-RBL HD — Highest Drawdown | 10.18% | 0.55 | 63.11% |

## Files

### Annual Rebalance
- `annual-returns-LD.png`, `annual-returns-SR.png`, `annual-returns-HD.png`
- `cumulative-LD.png`, `cumulative-SR.png`, `cumulative-HD.png`
- `metrics-LD.png`, `metrics-SR.png`, `metrics-HD.png`

### 2-Year Alternating Rebalance
- `annual-returns-2Y-RBL-LD.png`, `annual-returns-2Y-RBL-SR.png`, `annual-returns-2Y-RBL-HD.png`
- `cumulative-2Y-RBL-LD.png`, `cumulative-2Y-RBL-SR.png`, `cumulative-2Y-RBL-HD.png`
- `metrics-2Y-RBL-LD.png`, `metrics-2Y-RBL-SR.png`, `metrics-2Y-RBL-HD.png`

## Key Learnings

1. **Sharpe ratio (SR) is the best selection criterion.** Across both rebalance regimes, SR produced the highest CAGR (12.46% annual, 11.24% 2Y-RBL) while maintaining competitive drawdowns. It beat SPY's CAGR by 1.6% with better risk-adjusted returns.

2. **Lowest drawdown (LD) gives the best risk profile.** LD delivered MaxDD of 40.32% vs SPY's 55.20% — a 15% reduction in peak-to-trough pain — while nearly matching SPY's CAGR. The 2Y-RBL variant had slightly worse drawdown (43.62%) but similar returns.

3. **Highest drawdown (HD) is a terrible idea.** Selecting the worst-performing past combination produces predictably bad results: 8.85% CAGR with 66.26% MaxDD. Even the 2Y-RBL couldn't save it (10.18%, 63.11% MaxDD).

4. **2-year alternating rebalance (2Y-RBL) underperforms annual rebalance.** Every method lost CAGR when switching to 2Y-RBL (SR: −1.2%, LD: flat, HD: +1.3% but still terrible). The reduced turnover from 2-year holding doesn't compensate for the slower adaptation to changing market leadership.

5. **Sector rotation works but needs the right signal.** Simply picking by momentum-derived metrics (Sharpe, recent drawdown) adds value over SPY. The effect is modest but consistent — the worst-performing LD variant still ties SPY's CAGR with far less risk.

## Methodology

All 11 choose 4 = 330 combinations of US sector ETFs are evaluated over a 5-year rolling lookback window. The combination with the best score (lowest max drawdown for LD, highest Sharpe for SR, highest max drawdown for HD) is held for 1 year, then re-evaluated. For 2Y-RBL, the portfolio is split 50:50; each half rebalances every 2 years on alternating years (half A in odd years, half B in even years).

ETFs: XLY (Consumer Discretionary), XLK (Technology), XLC (Communication Services), XLP (Consumer Staples), XLF (Financials), XLV (Healthcare), XLI (Industrials), XLU (Utilities), XLRE (Real Estate), XLB (Materials), XLE (Energy)

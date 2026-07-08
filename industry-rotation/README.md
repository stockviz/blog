# Industry Rotation — RRG Backtest

Uses Relative Rotation Graphs (RRG) to rotate across NSE industries. Each week
we compute the JdK RS-Ratio and RS-Momentum for every industry with at least 10
stocks, pick the top 5 that are in the Leading quadrant (both > 100), and hold
them equal-weighted until the next rebalance.

## Files

- `common.R` — shared setup: DB connection, benchmark loading, industry return
  cache, RRG computation, and the `runBacktest()` function. All other scripts
  source this.
- `simple.R` — runs three rebalancing frequencies side by side: every week,
  every 2 weeks, and every 4 weeks.
- `staggered-4w.R` — runs four independent 4-week rebalance scenarios, each
  starting one week later than the previous. Averages the four to reduce
  path-dependency.

## How it works

1. Load NIFTY 50, MIDCAP 150, and SMALLCAP 250 TR daily prices (from 2023).
2. For ~58 industries with ≥10 stocks, load daily equal-weighted and
   cap-weighted returns from `BHAV_INDUSTRY`.
3. At each rebalance date, for each industry:
   - Compute RS = cumulative relative return vs NIFTY 50 TR
   - RS-Ratio = WMA(RS, 10) / WMA(WMA(RS, 10), 10) × 100
   - RS-Momentum = RS-Ratio / RS-Ratio<sub>4-weeks-ago</sub> × 100
4. Filter to industries where both RS-Ratio > 100 and RS-Momentum > 100
   (Leading quadrant). Sort by RS-Ratio, take top 5.
5. Hold equal-weighted for the holding period. Apply transaction cost drag
   on each rebalance. Industry universe refreshed weekly.
6. Compare cumulative returns against all three benchmarks.

## Findings

| Strategy | Sharpe | Ann. Return |
|---|---|---|
| Cap 4W Avg | 1.02 | 18.57% |
| Cap 4-Week | 0.98 | 18.05% |
| Cap 4W+0 | 1.21 | 23.68% |
| Cap 2-Week | 0.25 | 2.86% |
| Cap 1-Week | −0.37 | −8.11% |
| NIFTY MIDCAP 150 TR | 0.62 | 9.47% |
| NIFTY SMALLCAP 250 TR | 0.41 | 5.97% |
| NIFTY 50 TR | 0.12 | 0.75% |

Key takeaways:

- **Less is more.** Weekly rebalancing loses money — transaction costs eat the
  edge. Monthly (4-week) rebalancing is the sweet spot.
- **Staggering matters.** A single 4-week run is path-dependent (best slot gave
  Sharpe 1.21, worst gave 0.65). Averaging four staggered starts gives a more
  robust 1.02.
- **Cap-weighted > equal-weighted.** Cap-weighted industry returns produce
  better signals and better portfolio returns than equal-weighted.
- **Industry rotation beats all three benchmarks** over this period (Jul 2025
  to Jul 2026), both in absolute return and risk-adjusted terms.

## Output

Each script produces:
- A cumulative return chart (PNG)
- A gt table with annualized returns and Sharpe ratios (PNG + HTML)

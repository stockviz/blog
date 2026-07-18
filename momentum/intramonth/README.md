# Intramonth Futures Momentum Backtest

**Blog post:** [Intramonth Momentum in Indian Futures](https://stockviz.biz/2026/07/18/intramonth-momentum/)

**Paper:** [The Intramonth Momentum Cycle — Nathan, Suominen, and Tasa (June 2026)](intramonth_momentum_summary.md)

> Most of the equity momentum premium is a plumbing effect: investors facing month-end cash needs sell the stocks that are cheapest to part with — those with embedded losses and salient recent underperformance. Over 1980–2025, 78% of the winners-minus-losers cumulative return came from a six-day pre-month-end window.

Our backtest adapts this idea to Indian equity futures: short the worst-momentum stocks during the six-business-day window before month-end, close at month-end, and repeat monthly. Started January 2014.

## Strategy

- **Entry**: 6 business days before month-end
- **Exit**: Last trading day of the month
- **Positions**: Short 20 worst-momentum futures, 1 contract each
- **Momentum**: Past 252 trading-day returns (1 year, ascending: lowest first)
- **Drag**: 0.5% per trade
- **Filters**: Exclude symbols with ex-dates during hold period; pick next-month expiry
- **Benchmark**: NIFTY 50

## Results

| Period | Jan 2014 – Jul 2026 |
|--------|---------------------|
| Months | 151 |
| Positions | 20 short, 1 contract each |
| Avg positions | 19.7 per month |

| Metric | Strategy | NIFTY 50 |
|--------|----------|----------|
| Win Rate | 41.7% | 48.3% |
| Ann. Return | −8.40% | +0.99% |
| Ann. Vol | 12.39% | 3.53% |
| Sharpe | −0.64 | +0.30 |
| Max Drawdown | −73.49% | −10.33% |

The strategy loses money systematically. Worst-momentum stocks in Indian futures tend to mean-revert within the 6-day window rather than continue falling — shorting them catches the bounce, not the continuation.

**2025–2026 recovery**: The strategy turned positive in 2025 (+7.9%) and H1 2026 (+7.0%). This coincided with a shift in the composition of 1-year momentum losers — IT services (WIPRO, COFORGE, PERSISTENT, BSOFT) and select financials dominated the bottom decile and continued declining during the 6-day window rather than mean-reverting. These were sectors experiencing genuine secular slowdown rather than temporary selling pressure, so the momentum signal correctly predicted continued weakness. Whether this persists or reverts to the historical mean-reversion pattern is an open question.

## Real-World Gaps

The paper does not model short-selling borrow costs. Momentum losers are disproportionately small, distressed, and out-of-favor — exactly the stocks with elevated borrow fees or hard-to-borrow status. In Indian futures, these stocks may not even have liquid futures contracts, limiting the shortable universe. The paper's own net-of-spread numbers (11.5 bps/month) already show how thin the edge is before borrow costs.

## Output

- [equity_curve.csv](equity_curve.csv) — Monthly net returns
- [cumulative_returns.png](cumulative_returns.png) — Log-scale chart: Strategy vs NIFTY 50
- [annual_returns.png](annual_returns.png) — Grouped bar chart by year
- [metrics.html](metrics.html) — gt table with Sharpe, MaxDD, Win Rate

## Script

- [futures_momentum.R](futures_momentum.R) — `Rscript futures_momentum.R [lookback_days]`

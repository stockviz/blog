# Strategy 9 with Equity Indices

**Blog post:** [Strategy 9 with Equity Indices](https://stockviz.biz/2026/06/28/strategy-9-with-equity-indices/)

> Applies Strategy 9 trend-following to a basket of MSCI country equity indices. A Binary Long-only strategy earns a higher Sharpe than buy & hold, but its drawdown profile makes it unleverageable — so it ultimately trails B&H returns. More importantly, yearly returns suggest something stopped working in 2009; the backtest performance appears back-loaded. Once again, Strategy 9 fails us.

## Summary

Runs Strategy 9 (Carver multi-filter trend-following) on MSCI single-country large-cap equity indices, with four strategy variants (Scaled/Binary × Long-Only/Long-Short) at equal-weight, and compares against the MSCI ACWI benchmark.

## Scripts

### `script.R`
Loads MSCI country equity data, computes Strategy 9 signals and forecast-based position sizing for each country, builds equal-weight portfolios, and generates cumulative return charts, annual returns/drawdown tables, and a summary metrics table.

### `strategy_nine.R`
Shared Strategy 9 signal implementation — multi-filter trend-following with binary position signals and optional cost screen.

## Output

- `msci-strategy-summary.png` — Sharpe, annualized return, and max drawdown table
- `msci-all-strategies.cumret.png` — cumulative returns: all 4 strategies vs ACWI B&H
- `msci-all-strategies-2010.cumret.png` — cumulative returns from 2010 onwards
- `msci-scaled.cumret.png` — scaled LO vs scaled LS vs ACWI B&H
- `msci-binary.cumret.png` — binary LO vs binary LS vs ACWI B&H
- `msci-annual.returns.table.png` — yearly returns table
- `msci-annual.drawdowns.table.png` — yearly max drawdown table

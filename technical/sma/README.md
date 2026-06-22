# S&P 500 SMA Regimes

**Blog posts:**
- [S&P 500 SMA Regimes](https://stockviz.biz/index.php/?p=2094053)
- [SMA Distance, Part I](https://stockviz.biz/index.php/?p=2095283)
- [SMA Distance, Part II](https://stockviz.biz/index.php/?p=2095333)
- [SMA Distance, Part III Backtest](https://stockviz.biz/index.php/?p=2095453)

> Compared mixture-model regime classification against simple SMA-based (50-, 100-, 200-day) regime splits for the S&P 500. All SMA-based systems outperformed the mixture model, with the 200-day SMA being the best, concluding that simple approaches beat complex ones most of the time.

> Introduced the concept of SMA Distance — the percentage distance between a simple moving average and the index — as a more nuanced alternative to binary above/below SMA classification. Plotted distances for S&P 500 and NIFTY 50 across different look-back periods, noting apparent patterns warranting deeper investigation.

> Bucketed SMA Distance into quintiles and analyzed subsequent 20-, 50-, and 100-day returns. Found that when 50/100-day SMA Distance is in the first quintile (rising market), subsequent 20-day returns have smaller left tails; however, extremely stretched markets relative to the 200-day SMA are prone to steeper drops.

> Back-tested a long-only strategy that enters when 50/100-day SMA Distance is in the first quintile (while avoiding overextended 200-day conditions). Found it a poor long-term standalone strategy but useful for avoiding deep drawdowns, best employed as a confirming signal alongside other indicators.

## Summary

backtests SMA-based trend-following strategies on the S&P 500, analyzing the relationship between SMA distance (price deviation from moving average) and subsequent returns

## Scripts

### `sma-distance vs. returns.R`

backtests SMA-based trend-following strategies on the S&P 500, analyzing the relationship between SMA distance (price deviation from moving average) and subsequent returns

### `sma-distance-backtest.R`

backtests SMA-based trend-following strategies on the S&P 500, analyzing the relationship between SMA distance (price deviation from moving average) and subsequent returns

### `sma-distance.R`

backtests SMA-based trend-following strategies on the S&P 500, analyzing the relationship between SMA distance (price deviation from moving average) and subsequent returns

### `sp500-sma-regime.R`

backtests SMA-based trend-following strategies on the S&P 500, analyzing the relationship between SMA distance (price deviation from moving average) and subsequent returns

## Output

- `NIFTY50.sma.distance.png`
- `SP500.boxplot.100.1970-10-14.2005-12-30.png`
- `SP500.boxplot.200.1970-10-14.2005-12-30.png`
- `SP500.boxplot.50.1970-10-14.2005-12-30.png`
- `SP500.cumulative.2006-.png`
- `SP500.cumulative.all.png`
- `SP500.sma.distance.png`
- `sp500.long-only.SMA.regime.cumulative.png`
- `sp500.long-short.SMA.regime.cumulative.png`
- `sp500.sma.regime.density.png`

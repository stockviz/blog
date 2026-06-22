# SMA Strategies, Part II

**Blog post:** [SMA Strategies, Part II](https://stockviz.biz/index.php/?p=2099513)

> Added a slope-direction check to SMA strategies, going long only when the SMA is trending higher. While gross returns are lower than the raw SMA strategy, the 10-day variant shows much shallower drawdowns, making it a promising starting point for leveraged NIFTY futures trading.

## Summary

evaluates SMA slope as a trend-strength filter for timing entries and exits

## Scripts

### `backtest.R`

evaluates SMA slope as a trend-strength filter for timing entries and exits

## Output

- `NIFTY 50.index.cumulative.all.png`
- `NIFTY MIDCAP 100.index.cumulative.all.png`
- `NIFTY SMLCAP 100.index.cumulative.all.png`

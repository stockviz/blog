# SMA Strategies, Part III

**Blog post:** [SMA Strategies, Part III](https://stockviz.biz/index.php/?p=2099633)

> Tested SMA crossover strategies (SMA(N/4) > SMA(N)) versus slope-check approaches. Found the standalone slope check from Part II has lower peak drawdowns than crossover versions, as the additional averaging in crossovers introduces lag that hurts performance during rapid market declines.

## Summary

backtests SMA crossover strategies with multiple fast/slow period combinations

## Scripts

### `backtest.R`

backtests SMA crossover strategies with multiple fast/slow period combinations

## Output

- `NIFTY 50.index.X.cumulative.all.png`
- `NIFTY 50.index.XL.cumulative.all.png`
- `NIFTY MIDCAP 100.index.X.cumulative.all.png`
- `NIFTY MIDCAP 100.index.XL.cumulative.all.png`
- `NIFTY SMLCAP 100.index.X.cumulative.all.png`
- `NIFTY SMLCAP 100.index.XL.cumulative.all.png`

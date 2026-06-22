# Global Equities Momentum

**Blog posts:**
- [Global Equities Momentum](https://stockviz.biz/index.php/?p=2097853)
- [Global Equities Momentum (Update)](https://stockviz.biz/index.php/?p=40890498)

> Modified Gary Antonacci's GEM dual-momentum model by substituting market-cap indices with momentum indices. Swapping only the final US equity leg to a US Momentum index (while keeping market-cap international) improved returns with moderate drawdowns, offering the best risk-reward compromise among tested variations.

> Updated the analysis of Gary Antonacci's Global Equities Momentum (GEM) strategy and found that since 2019, GEM's momentum version underperformed its market-cap version, while simple buy-and-hold of the S&P 500 outperformed both. The lag in GEM catching up after market recoveries overshadowed the benefit of lower drawdowns.

## Summary

backtests Global Equities Momentum (GEM) strategies comparing S&P 500 and MSCI USA Momentum across different leverage levels (1x, 2x)

## Scripts

### `ETF-GEM.R`

backtests Global Equities Momentum (GEM) strategies comparing S&P 500 and MSCI USA Momentum across different leverage levels (1x, 2x)

### `sp500 and momentum.R`

backtests Global Equities Momentum (GEM) strategies comparing S&P 500 and MSCI USA Momentum across different leverage levels (1x, 2x)

## Output

- `MTUM.GEM.dd.png`
- `MTUMx2.GEM.dd.png`
- `SP500.GEM.dd.png`
- `SP500x.GEM.dd.png`
- `SP500x2.GEM.dd.png`
- `etf-GEM-returns.png`
- `etf-returns.png`
- `sp500.mom.GEM.cumulative.png`
- `sp500.mom.world.GEM.cumulative.png`
- `sp500.mtum.x2.GEM.annual.png`

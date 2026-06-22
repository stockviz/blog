# Mixture model over S&P 500 returns

**Blog post:** [Mixture model over S&amp;P 500 returns](https://stockviz.biz/index.php/?p=2094023)

> Applied Gaussian mixture models to classify S&P 500 daily returns into bull and bear regimes. While whole-period analysis looked promising with clearly separated densities, rolling-period analysis showed much less differentiation, and using the model as a trading signal produced sub-par returns.

## Summary

fits Gaussian mixture models to return distributions to identify latent bull/bear market regimes

## Scripts

### `sp500.R`

fits Gaussian mixture models to return distributions to identify latent bull/bear market regimes

### `sp500.weekly.R`

fits Gaussian mixture models to return distributions to identify latent bull/bear market regimes

## Output

- `sp500.regime.cumulative.png`
- `sp500.regime.cumulative.weekly.png`
- `sp500.regime.density.png`
- `sp500.regime.density.weekly.png`
- `sp500.regime.png`

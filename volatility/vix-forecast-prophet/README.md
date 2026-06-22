# Prophet for VIX

**Blog post:** [Prophet for VIX](https://stockviz.biz/index.php/?p=40890430)

> Compared Prophet, GARCH(1,1), and locf (last observation carried forward) for forecasting VIX 20 days ahead using 500-day rolling windows. GARCH(1,1) performed worse than Prophet, but surprisingly locf beat both models on RMSE, suggesting simple persistence often wins for VIX forecasting.

## Summary

builds VIX forecasting models using Facebook Prophet and evaluates predictive accuracy

## Scripts

### `combine.R`

builds VIX forecasting models using Facebook Prophet and evaluates predictive accuracy

### `script.R`

builds VIX forecasting models using Facebook Prophet and evaluates predictive accuracy

## Output

- `vix-forecast-rmse.png`

## Data Files

- `forecastStatsDf.Rdata`

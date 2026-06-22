# CGMM for VIX

**Blog post:** [CGMM for VIX](https://stockviz.biz/index.php/?p=40890476)

> Tested Conditional Gaussian Mixture Models (CGMM) for forecasting 20-day forward VIX and compared RMSE against the naïve LOCF (last observation carried forward) baseline. LOCF proved hard to beat, especially when VIX itself is volatile.

## Summary

applies Clustered Gaussian Mixture Models to forecast VIX from macroeconomic variables

## Scripts

### `script.R`

applies Clustered Gaussian Mixture Models to forecast VIX from macroeconomic variables

### `predict.py`

applies Clustered Gaussian Mixture Models to forecast VIX from macroeconomic variables

## Output

- `vix-forecast.error.png`
- `vix-vs-error.png`

## Data Files

- `cgmm-vix-prediction.csv`

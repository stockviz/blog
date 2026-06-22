# Macro: Using Currencies to Predict NIFTY, Part IV

**Blog posts:**
- [Macro: Using Currencies to Predict NIFTY, Part IV](https://stockviz.biz/index.php/?p=2092913)
- [Macro: Using Currencies to Predict NIFTY, Part V](https://stockviz.biz/index.php/?p=2092993)

> Created an ensemble model combining DTWEXB (degree 8) and DTWEXM (degree 4) SVMs to predict NIFTY. While the standalone DTWEXM model delivered the highest returns, the ensemble model achieved lower drawdowns, offering a better risk-adjusted profile.

> Added a 50-day SMA filter to the SVM predictions, going long/short only when both the SVM signal and the SMA regime agreed. The SMA filter significantly reduced drawdowns (especially the 2018 correction), making the DTWEXM SVM combined with the 50-day SMA the winning combination.

## Summary

applies Support Vector Regression to predict equity returns using currency and macroeconomic features

## Scripts

### `fred-cur-nifty.R`

applies Support Vector Regression to predict equity returns using currency and macroeconomic features

## Output

- `DTWEXB+DTWEXM.NIFTY.BH.drawdowns.png`
- `DTWEXB+DTWEXM.NIFTY.L.drawdowns.png`
- `DTWEXB+DTWEXM.NIFTY.L1.drawdowns.png`
- `DTWEXB+DTWEXM.NIFTY.L2.drawdowns.png`
- `DTWEXB+DTWEXM.NIFTY.LS.drawdowns.png`
- `DTWEXB+DTWEXM.NIFTY.LS1.drawdowns.png`
- `DTWEXB+DTWEXM.NIFTY.LS2.drawdowns.png`
- `DTWEXB+DTWEXM.NIFTY.cumulative.png`

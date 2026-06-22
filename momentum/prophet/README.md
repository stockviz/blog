# Prophet for Momentum

**Blog posts:**
- [Prophet for Momentum](https://stockviz.biz/index.php/?p=40890420)
- [Prophet for Momentum, Part II](https://stockviz.biz/index.php/?p=40890427)

> Tested Meta's Prophet forecasting library for predicting next-month stock returns to build a momentum portfolio. Results were better than a simple linear model but still trailed naïve momentum performance.

> Tested higher-frequency rebalancing with Prophet-based momentum portfolios. Found no benefit to rebalancing more frequently than monthly, even before costs; Prophet portfolios have low overlap which amplifies transaction costs, and monthly rebalancing works best — another win for keeping it simple.

## Summary

uses Facebook Prophet models to predict momentum signal persistence

## Scripts

### `combine-weekly.R`

uses Facebook Prophet models to predict momentum signal persistence

### `combine.R`

uses Facebook Prophet models to predict momentum signal persistence

### `script-weekly.R`

uses Facebook Prophet models to predict momentum signal persistence

### `script.R`

uses Facebook Prophet models to predict momentum signal persistence

## Output

- `symRets-PROPHET_1-weekly.png`
- `symRets-PROPHET_1-weekly.post.png`
- `symRets-PROPHET_1-weekly.pre.png`
- `symRets-PROPHET_2-weekly.png`
- `symRets-PROPHET_2-weekly.post.png`
- `symRets-PROPHET_2-weekly.pre.png`
- `symRets-PROPHET_3-weekly.png`
- `symRets-PROPHET_3-weekly.post.png`
- `symRets-PROPHET_3-weekly.pre.png`
- `symRets-PROPHET_4-weekly.png`
- `symRets-PROPHET_4-weekly.post.png`
- `symRets-PROPHET_4-weekly.pre.png`
- `symRets-weekly.gross.png`
- `symRets-weekly.net.png`
- `symRets-weekly.overlap.png`
- `symRets-weekly.post.gross.png`
- `symRets-weekly.post.net.png`
- `symRets-weekly.pre.gross.png`
- `symRets-weekly.pre.net.png`
- `symRetsAll.vol.png`
- `symRetsPost.vol.png`
- `symRetsPre.vol.png`

## Data Files

- `symRets.vol.Rdata`
- `symRets.weekly.Rdata`
- `symStatsAll-weekly.gross.csv`
- `symStatsAll-weekly.net.csv`

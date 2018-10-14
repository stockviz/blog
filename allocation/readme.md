# Asset Allocation
Scenario analysis and projections of different asset allocation schemes.

## scripts
### correlation.study.R
Looks at rolling correlations between different assets on a USD basis.

### 2-asset.R
A simple two asset (equity/bond) allocation toggled through different allocations, rebalance thresholds and taxes.

### 3-asset.equal-wt.R
An equal weighted three asset (midcap/bond/nasdaq-100) allocation toggled through different rebalance thresholds and taxes.

### 3-asset.optimized.R
Uses portfolio optimization methods to allocate a three asset (midcap/bond/nasdaq-100) portfolio. The portfolio is then toggled through different rebalance thresholds and taxes. Variance (var) and expected tail loss (ETL) are minimized while mean returns are maximized.

## blogs
* [StockViz: Allocating a Two-Asset Portfolio](https://stockviz.biz/2018/10/10/allocating-a-two-asset-portfolio/)
* [StockViz: Allocating a Three-Asset Portfolio, Equal Weighted](https://stockviz.biz/index.php/2018/10/11/allocating-a-three-asset-portfolio-equal-weighted/)
* [StockViz: Allocating a Three-Asset Portfolio, Optimized](https://stockviz.biz/2018/10/12/allocating-a-three-asset-portfolio-optimized/)
# Buying the dip vs. daily SIP
We often hear about "buying the dip." What exactly is a dip? Is buying the dip better than a daily SIP?
The following scripts do the following:
1. Invest Rs. 1 every day. In the 'sip buying' case, it buys the index.
2. In the 'dip buying' case it either accumulates cash or liquidates the cash and buys the index.

## scripts
### drawdown.study.R
An index is said to be in a drawdown if it is a THRESHOLD% below its maximum level in the last LOOKBACK days. By running a sliding 5-year window over different THRESHOLD and LOOKBACK values, one can get a sense for the advantage/disadvantage of such a strategy.
In the 'dip buying' case, the index is bought as long as it is below the THRESHOLD.

### sma.study.R
An index is bought only when the lower SMA crosses a larger SMA. By running a sliding 5-year window over different lower and upper SMAs, one can get a sense for the strategy's efficacy.
In the 'dip buying' case, the index is bough only once - when the crossover is observed.

## blogs
* [StockViz: Systematic Buy-the-Dip](https://stockviz.biz/2016/06/25/systematic-buy-dip/)
* [StockViz: Systematic Buy-the-Dip, an Update](https://stockviz.biz/2018/10/04/systematic-buy-the-dip-an-update/)
* [StockViz: Systematic Buy-the-Dip, SMA crosses](https://stockviz.biz/2018/10/04/systematic-buy-the-dip-an-update/)
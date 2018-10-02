# Dollar cost averaging vs. one-time
What are the return profiles of dollar cost averaging (DCA or SIP) vs. one-time (lumpsum) investing?

## scripts
### ls.dca.sip.R
Fits a generalized lambda distribution on a weekly return series. The model is then used to create multiple time-series to simulate alternate investment paths. The DCA and one-time investment density plots are presented.

### ls.dca.sip.2.R
Added average returns and probabilities by applying an empirical cumulative distribution function over simulated values.

## blogs
* [StockViz: Lumpsum vs. Dollar Cost Averaging (SIP)](https://stockviz.biz/2018/06/23/lumpsum-vs-dollar-cost-averaging-sip/)
* [StockViz: Lumpsum vs. SIP: Thinking in Probabilities](https://stockviz.biz/2018/10/02/lumpsum-vs-sip-thinking-in-probabilities/)
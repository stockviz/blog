# Dollar cost averaging vs. one-time
What are the return profiles of dollar cost averaging (DCA or SIP) vs. one-time (lumpsum) investing?

## scripts
### ls.dca.sip.R
Fits a generalized lambda distribution on a weekly return series. The model is then used to create multiple time-series to simulate alternate investment paths. The DCA and one-time investment density plots are presented.

### ls.dca.sip.2.R
Added average returns and probabilities by applying an empirical cumulative distribution function over simulated values.

### dca.sip.monthly.random.R
Instead of fitting returns into a model, randomly shuffle observed monthly return series to obtain a time-series of returns for the simulation.

## blogs
* [StockViz: Lumpsum vs. Dollar Cost Averaging (SIP)](https://stockviz.biz/2018/06/23/lumpsum-vs-dollar-cost-averaging-sip/)
* [StockViz: Lumpsum vs. SIP: Thinking in Probabilities](https://stockviz.biz/2018/10/02/lumpsum-vs-sip-thinking-in-probabilities/)
* [StockViz: The Path Dependency of SIP Returns](https://stockviz.biz/index.php/2018/10/23/the-path-dependency-of-sip-returns/)
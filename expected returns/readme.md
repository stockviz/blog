# Projecting Future Return Distributions

Past returns are used to setup a model. The model can either be:
1. a random shuffle of past returns (RND)
1. a normal distribution with average and standard deviation of past returns as parameters (NRM)
1. a generalized lambda distribution fit to past returns (GLD)

The model is queried for 10-, 20- or 30-year series of monthly returns 10,000 times. The annualized returns of each simulation is then plotted as a histogram.

Both lumpsum/onetime and SIP/DCA investments are run.

Blog: [Projecting Future Returns](https://stockviz.biz/index.php/2019/01/14/projecting-future-returns/)
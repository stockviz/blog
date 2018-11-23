# macro timing models
can macro instruments (commodities, currencies, rates, etc...) be used to time indian equity markets?

## scripts

### linear models
#### correlations01.R
A simple correlation chart between NIFTY 50, USDINR and OIL\
Blog: [Macro: NIFTY vs. INR/OIL Correlation, Part I](https://stockviz.biz/2018/10/29/macro-nifty-vs-inr-oil-correlation-part-i/)

#### linear.model.01.R
A simple linear model between NIFTY 50 and USDINR\
Blog: [Macro: NIFTY vs. INR/OIL Correlation, Part II](https://stockviz.biz/2018/10/29/macro-nifty-vs-inr-oil-correlation-part-ii/)

#### density.NIFTY.INR.R
Densities of weekly returns of NIFTY 50 under different USDINR and OIL thresholds\
Blog: [Macro: NIFTY vs. INR/OIL Correlation, Part III](https://stockviz.biz/2018/10/30/macro-nifty-vs-inr-oil-correlation-part-iii/)

### machine learning
An SVM can be tuned with different parameters. Principal among those is the kernel to be used.

#### svr-01/dtwexm-nifty.R
Difference in predictive power when different kernels are used to train an SVM over the same dataset.\
Blog: [Using an SVM over dollar indices to trade the NIFTY, Part I](https://stockviz.biz/2018/11/16/macro-using-currencies-to-predict-nifty-part-i/)

#### svr-2/dtwexm-nifty.R
Tune the most promising kernel from Part I\
Blog: [Using an SVM over dollar indices to trade the NIFTY, Part II](https://stockviz.biz/2018/11/20/macro-using-currencies-to-predict-nifty-part-ii/)

#### svr-3/fred.cur-nifty.R
Take all the dollar indices (including USDINR) and observe the predictive power of an SVM using a polynomial kernel with varying degree parameters\
Blog: [Using an SVM over dollar indices to trade the NIFTY, Part III](https://stockviz.biz/2018/11/20/macro-using-currencies-to-predict-nifty-part-iii/)

#### svr-4/fred.cur-nifty.R
Train an SVM using the two promising indices and their respective best-performing degree parameters from Part III\
Blog: [Using an SVM over dollar indices to trade the NIFTY, Part IV](https://stockviz.biz/2018/11/20/macro-using-currencies-to-predict-nifty-part-iv/)

#### svr-5/fred-cur-nifty-sma.R
Combine the model from Part III with a Simple Moving Average and find that it leads to lower drawdowns in long-short portfolios\
Blog: [Using an SVM over dollar indices to trade the NIFTY, Part V](https://stockviz.biz/2018/11/21/macro-using-currencies-to-predict-nifty-part-v/)
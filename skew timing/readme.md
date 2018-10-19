# skewness for market-timing
can the skewness of returns be used to time the market?

## scripts
### SPY.IVE.R
Calculates the rolling skewness of 220-day daily returns of the SPY etf and uses it to time the IVE etf.\
Two scenarios are computed. One in which cash returns 0 and another where it returns the returns of the LQD etf.\
Blog: [Is Skewness a Timing Signal?](https://stockviz.biz/2018/10/19/is-skewness-a-timing-signal/)

### NIFTY.R
Calculates the rolling skewness of 220-day daily returns of a NIFTY index and uses it to time the same index.\
Two scenarios are computed. One in which cash returns 0 and another where it returns the returns of the 0-5year bond total return index.\


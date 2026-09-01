# Short-term vs. Long-term Rates

- Difficulty level: 2
- Published: Mon, 12 Oct 2015 12:51:17 +0000
- Source: [https://stockviz.biz/2015/10/12/short-term-vs-long-term-rates/](https://stockviz.biz/2015/10/12/short-term-vs-long-term-rates/)

## Summary

This document discusses the relationship between short-term and long-term interest rates, explaining that banks have been reluctant to pass on rate-cuts primarily because the yield curve is extremely flat. The difference between short-term and long-term rates is currently near historical lows, which impacts lending behavior and monetary policy transmission. Understanding this concept requires basic knowledge of fixed income markets, yield curves, and macroeconomic rate-setting. The article serves as a concise introductory overview for observing yield curve behavior and bank responses without requiring advanced mathematical modeling or specialized financial engineering prerequisites.

## Article

Probably the reason why banks have been reluctant to pass on rate-cuts is because the yield curve has been flat as a pancake. The difference between short-term and long-term rates are near their historical lows.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Image | https://stockviz.biz/type/image/]
Probably the reason why banks have been reluctant to pass on rate-cuts is because the yield curve has been flat as a pancake. The difference between short-term and long-term rates are near their historical lows.

[Linked: ARMA + GARCH to Predict VIX | https://stockviz.biz/2015/10/11/arma-garch-to-predict-vix/]
GARCH(1,1) GARCH(1,1) is a common approach for modeling volatility. They were developed by Robert Engle to deal with the problem of auto-correlated residuals (which occurs when you have volatility clustering, for example) in time-series regression. What we did: - Picked the best fit ARIMA(p,d,q) model for historical VIX over different look back periods - Created a GARCH(1,1) model based on ARMA(p,q) - Predicted t+1 VIX 500-day lookback We found that modeling based on the previous 500-day VIX closing levels gave us the least prediction errors. The appendix has the charts for other lookback periods. Prediction vs. Actual Note how in some periods, the predicted value (red) is just the previous value. Prediction error Values less than zero implies that the model prediction overshoots the actual VIX level the next day. Prediction vs. Actual Density Plot The model bias towards higher estimation of VIX is made explicit here. Next steps We will integrate this model to our morning ‘Options Daily’ posts so that we get an idea of both the current state of VIX and the expected modeled behavior. Caveats: - The 500-day lookback is purely empirical. Maybe some other look-back period that we have not tested would have been ideal to model. We will never know. - Only the known history can be modeled. The outputs should be used along with market determined proxies of expected volatility. - There is always a probability distribution around a predicted value. We will publish this in our daily posts. Appendix VIX Model vs. Actual across various lookback periods. (pdf) Volatility Forecasting I: GARCH Models, Rob Reider (pdf)

[Linked: A quick note on bonds | https://stockviz.biz/2015/10/14/a-quick-note-on-bonds/]
We compared the total returns from the short-end of the curve to Nifty. Here’s what we found: - IRR over the last 10 years for bonds was 6.53%. - Biggest drawdown was -5.04%. - Only two years of negative correlation with NIFTY. The right place for bonds in a portfolio is for regular income. From a returns perspective, you are better off investing in equities. Bonds are no less volatile when compared to the returns they give, and are mostly correlated with equity volatility.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

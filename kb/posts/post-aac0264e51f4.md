# Can NIFTY be modeled using ARIMA?

- Difficulty level: 4
- Published: Wed, 14 Jun 2017 08:53:19 +0000
- Source: [https://stockviz.biz/2017/06/14/can-nifty-modeled-using-arima/](https://stockviz.biz/2017/06/14/can-nifty-modeled-using-arima/)

## Summary

This document discusses an empirical analysis investigating whether the NIFTY stock index can be effectively modeled as an ARIMA(1,1,1) process, as asserted by a prior research paper. The author evaluates the best-fit models across rolling windows of different sizes and finds that the majority of the time, the optimal fit is actually ARIMA(0,0,0), which corresponds to white noise. Furthermore, the author tests the forecasting ability of the ARIMA(1,1,1) model by comparing buy-and-hold annualized returns against long and short NIFTY strategies using various look-back periods, revealing a slight return advantage without transaction costs included. Ultimately, the author notes a discrepancy between the best-fit model results and the forecasting performance, concluding that the observed advantages might be random.

## Article

A recent paper on SSR, Testing Random Walk Hypothesis: An Empirical Analysis of National Stock Exchange Indices (pdf), had me wondering if the NIFTY could indeed be modeled as an ARIMA(1,1,1) process as the author asserts. As a first step, I wanted to check if ARIMA(1,1,1) is a given. What would be best fit be across rolling windows of different sizes? Turns out that for the most part, the best fit is ARIMA(0,0,0) aka, white noise. And the second best fits apply less than 20% of the time (Code and Results.) Second, I wanted to check if ARIMA(1,1,1) has any forecasting ability. It does appear so (Code and Results.) Buy & Hold Annualized return: 13.25% vs. Long/short NIFTY with different look-backs: 200: 16.75%; 500: 17.41% and 1000: 14.28% *Not including transaction costs. Although there is a slight advantage in using an ARIMA(1,1,1) model, I have a hard time reconciling the first set of results with the second. The advantage could very well be random.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Code and Results | https://github.com/stockviz/notebooks/blob/master/ARIMA%20on%20NIFTY.ipynb]
You signed in with another tab or window. Reload to refresh your session.You signed out in another tab or window. Reload to refresh your session.You switched accounts on another tab or window. Reload to refresh your session.Dismiss alert

[Linked: Code and Results | https://github.com/stockviz/notebooks/blob/master/ARIMA(1%2C1%2C1)%20on%20NIFTY.ipynb]
You signed in with another tab or window. Reload to refresh your session.You signed out in another tab or window. Reload to refresh your session.You switched accounts on another tab or window. Reload to refresh your session.Dismiss alert

[Linked: Replacing Mutual Funds with ETFs | https://stockviz.biz/2017/06/09/replacing-mutual-funds-etfs/]
Last month, we took a stab at measuring a fund’s alpha over a basket of ETFs (link.) The rationale was that the index often chosen by the mutual fund is not easily accessible to the investor. We saw how mutual fund alpha varies over time. We then asked the question: What if we just invested in the basket instead of buying the fund? We did a study of the top 10 equity mutual funds by AUM back in March-2011 and found that 4 out of 10 funds under-performed their ETF baskets and 2 out of 10 funds could be replaced by an ETF basket without compromising too much on returns. That is, only 4 out of 10 fund out-performed the ETF basket setup for them. The code, inputs and results are on github.

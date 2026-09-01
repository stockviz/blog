# Weighted Strike-Spot Ratio

- Difficulty level: 3
- Published: Mon, 10 Apr 2023 03:54:07 +0000
- Source: [https://stockviz.biz/2023/04/10/weighted-strike-spot-ratio/](https://stockviz.biz/2023/04/10/weighted-strike-spot-ratio/)

## Summary

This article applies the options volume-weighted strike-spot ratio (VWKS) from Bernile, Gao, and Hu to predict NIFTY next-day returns. It explains how VWKS measures the center of mass in options volume distribution across strike prices, using normalized moneyness (K/S minus one) weighted by traded lots. Readers must understand options mechanics, including moneyness, in-the-money and out-of-the-money calls and puts, and the distinction between strike and spot prices. The piece constructs this metric and backtests it on Indian markets, ultimately finding that net Open Interest and Value traded failed to predict returns. Prerequisites include familiarity with options terminology, volume and open interest dynamics, and basic quantitative backtesting concepts to grasp the methodology and its null results.

## Article

Can options trading predict the underlying’s returns? Center of Volume Mass: Does Options Trading Predict Stock Returns? Bernile, Gao, Hu (SSRN) tries to answer the age-old question. They construct an options volume weighted strike-spot ratio and use that the predict the underlying’s next-day returns. We rely on the volume-weighted strike-spot ratio to characterize the central location of the distribution of trading activity along the moneyness of available option contracts on the same stock. The ratio of the contract’s strike price (K) and the underlying stock price (S) measures the option moneyness, whereby call (put) options are out-of-the-money when K/S is above (below) one. After normalizing K/S by subtracting one, we calculate the weighted average of the normalized K/S ratio across available contracts using as weights the number of lots traded on each contract during the same period (V WKS, hereafter). V WKS reflects the center of mass in the options volume distribution along strike prices of available contracts and takes on higher (lower) values when the trading volume is tilted more toward OTM (ITM) calls and ITM (OTM) puts. While their results look promising, we setup a very simple backtest to see if it can be used to trade the NIFTY. Sadly, both net Open Interest and Value traded fail to show any effect on next-day returns. I guess this is one more for the #fail pile. Code and charts are on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: github | https://github.com/stockviz/blog/tree/master/options/Center%20of%20Volume%20Mass]
Blog post: Weighted Strike-Spot Ratio Tested whether an options volume-weighted strike-spot ratio (from Bernile, Gao, Hu) could predict NIFTY next-day returns. Both net Open Interest and Value traded metrics failed to show any predictive effect, landing this approach in the fail pile for Indian markets. analyzes the relationship between trading volume and returns, including the high-volume return premium - reg.OPEN_INTEREST.png - reg.VAL_IN_LAKH.png

[Linked: Mahalanobis Distance with Trend | https://stockviz.biz/2023/03/25/mahalanobis-distance-with-trend/]
Previously, we constructed a portfolio that switches between equities and bonds based on the Mahalanobis distance between them. Here, keeping everything else the same, we add a trend filter to the same set of indices. The composite regime-switching model ends up with superior Sharpe Ratios. However, if you don’t switch to bonds (and stay in cash, earning zero), then you maybe better off with a simple trend model. The alpha seems to be in earning the risk-free rate when things are “bad” and getting long equities only when things are “favorable.” Code and charts are on github.

[Linked: Volatility, Volatility of Volatility, and Momentum | https://stockviz.biz/2023/04/13/volatility-volatility-of-volatility-and-momentum/]
Momentum has proved to be the premier anomaly in different markets. And so has low-volatility. What happens if you combine both of them? Also, what if you also add low volatility of volatility into the mix? There are a couple of ways to skin this cat. You can start with low-volatility and add momentum. Or, you could go the other way – start with momentum and then add a volatility sort. tl;dr: go with low-volatility first, momentum second (VOLxMOM). While a simple momentum sort gives the highest return, adding a low-volatility filter to it gets you a better risk-adjusted return. The order of the sort – first volatility and then momentum or first momentum and then volatility – doesn’t seem to matter much for the Sharpe rankings but the former ended up with slightly better returns. Code and charts on github.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: About Us | https://stockviz.biz/2012/11/28/about-us/]
Our Mission The StockViz mission is to make modern investment practices and systems affordable to the average Indian investor to help people trade and invest better. People Shyam Sunder has over 10 years of experience in investment management, analysis and algorithmic trading. He worked at Merrill Lynch as a trader on their ABS CDS desk in New York. Having had to build his own tools specific to the Indian market to help manage his investments better, he founded StockViz to bring those tools to a wider investor base. Follow @ShyamNation Follow @ShyamNation Col Dipanshu Sinha, SM took premature retirement from the Indian Army as a Colonel in the Assam Regiment. Initially specialising as an Army Aviator, he has subsequently served with distinction in frontline infantry roles and has substantial experience in counter-insurgency operations earning his stripes with multiple tours of duty in Kashmir and various appointments in command and staff roles. A graduate of the Defence Services Staff College, Wellington, throughout his career he felt the absence of financial and tax planning information which was accentuated by peculiar service conditions and limited exposure to money management skills training in the regular professional courses. He has been one of the driving forces behind the conceptualisation and establishment of Stockviz and is a co-founder. Dipanshu is responsible for managing day-to-day operations, strategic planning, business development and oversight and policy advice for the firm’s investment activities. Follow @dipanshusinha Follow @dipanshusinha

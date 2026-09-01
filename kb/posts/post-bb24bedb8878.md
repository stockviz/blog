# Basis Trades using Futures

- Difficulty level: 4
- Published: Sun, 03 May 2015 10:24:32 +0000
- Source: [https://stockviz.biz/2015/05/03/basis-trades-using-futures/](https://stockviz.biz/2015/05/03/basis-trades-using-futures/)

## Summary

This document evaluates the viability and profitability of trading the basis between near and far expiration futures contracts using a historical back-test methodology. It examines Nifty futures price data from 2000 through the present, analyzing the stability of the contract basis and testing a quantitative trading rule based on a 50-day moving average. The text discusses the performance outcomes of long and short basis strategies across different timeframes, specifically evaluating returns from 2005 and 2010 onwards. Ultimately, the analysis concludes that the strategy yields minimal profits that are largely insufficient to cover transaction costs and taxes, indicating that such arbitrage opportunities have been eroded in recent markets.

## Article

Introduction When we discussed cash-futures basis, it was pointed out that the fair value of a futures contract is a function of the underlying price, interest rates, dividends and time to expiration. The same logic applies to the fair value of contracts across expiration dates. For example, as of close on April 30, 2015, NIFTY futures contracts had the following values: 8177.35 (April), 8244.05 (May), 8275.30 (June). Some of our clients wanted us to check if this basis can be traded. Is it possible to profit from going long the near contract and short the far contract on a consistent basis? Before we look at profitability, lets chart the basis. The basis Here is how the basis between different contracts look (2000 through now): Here is the summary statistic of the basis: Here is the same data with futures expiry dates removed: With the extreme values removed, we can now check if we can trade the nearest expiry contract with the farthest. 50-day Average Basis Trade Back-Test Lets take a look at the Near vs. Farthest basis and draw a 50-dma through it: The basis is not stable and what’s worse, it appears to be trending. Lets try our simple trading rule: go long the basis if it is above 50-dma and short if otherwise. Here’s how the back-test works out (2005 through now): Lets check the back-test on a smaller subset (2010 through now): A ~20% profit in a 10 year time-frame is barely enough to cover transaction costs. Besides, it looks like the strategy hit a wall in 2010. Conclusion It appears that the basis trade described above is not profitable enough after considering transaction costs and taxes. Also, whatever meager profits were there seem to have been arbitraged away lately.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: discussed | https://stockviz.biz/2015/01/12/nifty-cash-futures-basis/]
Fair value Equity futures have a ‘fair-value’: Futures Price = Cash Price [1+r (x/360)] – Dividends; where x = days to expiration of the futures contract Cash-futures Basis You can see this in action when you plot the NIFTY index value with its futures: Initially, x/360 is large, so futures’ trade rich to cash. As expiry approaches, futures and cash prices converge. This is the natural order of things. Interest Rate You can go one step further and back out the interest rate baked into these prices: r is usually within a tight band; roughly around where short-term rates are. So if you ever wondered why futures are trading higher than cash, now you know!

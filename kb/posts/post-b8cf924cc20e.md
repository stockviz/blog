# Internal Bar Strength

- Difficulty level: 4
- Published: Wed, 22 Jun 2016 10:34:44 +0000
- Source: [https://stockviz.biz/2016/06/22/internal-bar-strength/](https://stockviz.biz/2016/06/22/internal-bar-strength/)

## Summary

This document explores the Internal Bar Strength (IBS) trading strategy, which relies on mean-reversion by evaluating the position of a day's closing price relative to its high and low range. It outlines the theoretical framework where low IBS values theoretically correlate with positive future returns and high IBS values with negative returns. The article details a back-test conducted across 16 NSE indices, where buy and sell signals were executed at the close based on specific IBS thresholds. The findings reveal that the strategy underperformed compared to buy-and-hold approaches when tested on Indian market indices, leading to the conclusion that the IBS effect does not reliably hold for the tested equities or markets.

## Article

Definition Internal Bar Strength (IBS) is based on the position of the day’s close in relation to the day’s range: it takes a value of 0 if the closing price is the lowest price of the day, and 1 if the closing price is the highest price of the day. The IBS effect may be related to intraday over-reactions to news or market movements, which are then “corrected” the next day. IBS = (Close – Low)/(High – Low) It is a mean-reversion strategy. Back test The paper from Alexander Soffronow Pagonidis claims that low IBS values are associated with high returns, while high IBS values are associated with low returns. Average returns when IBS is below 0.20 are .35% while average returns when IBS is above 0.80 are -0.13%. We put this to the test on 16 NSE indices. Calculating IBS and trading at the close. To keep things simple, we assumed that we can trade at closing prices. Buy at the close if IBS is below 0.2, and sell at the close if IBS exceeds 0.8, exit the position at the following market close. If a back test on indices proved promising, we figured we would try this out on individual stocks next. However, IBS returns trailed buy-and-hold by a significant margin. Using IBS to trade mean reversion, as the author intended, is a losing proposition. What if we do the reverse? It “works” for about half the indices – could be pure luck. Conclusion It looks like IBS either doesn’t hold for Indian markets or for the indices we tested. Source: The IBS Effect: Mean Reversion in Equity ETFs (pdf) Equity curves: IBS Mean Reversion (pdf)

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Trading turnover throughout the day | https://stockviz.biz/2016/06/20/trading-turnover-throughout-the-day/]
Turnover, defined as volume over total number of shares outstanding, is not constant throughout the day. If you plot turnover over a trading day, it typically traces a ‘U’ shaped plot. Notice how turnover is the highest in the first half-hour and the last-half hour of trading? Turns out, it is a global phenomena. It follows that if you want liquidity, then it is enough if you show up for the last half-hour of trading. Related: Trading Day of Month Returns Equity Returns at the Turn of the Month Improving VWAP Strategies: A Dynamic Volume Approach

[Linked: Systematic Buy-the-Dip | https://stockviz.biz/2016/06/25/systematic-buy-dip/]
Introduction We often hear the term “buy-the-dip” whenever the markets are correcting. However, here are some questions that face an investor: - What exactly is a “dip?” - Where does the cash come from? - How much should I buy? The answers to these questions will determine how much alpha you will generate by employing this strategy. What is a “dip?” A dip is a percentage loss from a near-time peak (also called a drawdown.) For example, if the NIFTY posts a 50-day cumulative loss of 5%, then that is a 5% dip over where the NIFTY closed 50-days ago. To get a sense for how these 50-day dips/drawdowns are distributed, we do a density plot. As we can see, most of the NIFTY dips are at around 5%. A more than 10% dip is a “back the truck up” event where we deploy all our cash. For MIDCAPs, it is around 10% and 15%. The back test Every day, an investor has Rs. 1 that he needs needs to invest. He can either buy the NIFTY/MIDCAP or he can park it a short-term bond fund/savings account. Additionally, if it is a “back the truck up” dip, he can liquidate the bond fund and buy the NIFTY/MIDCAP. Let’s tag this as DIP. In a DIP, the investor only buys NIFTY/MIDCAP if it is in a dip. Otherwise, he buys Rs. 1 worth of bonds. The base case is that the investor buys Rs. 1 worth of NIFTY/MIDCAP every day. Let’s tag this as SIP. Should you buy the dip? Yes, buying the dip allows you to build a bigger corpus, if your transaction costs are zero. Here are the NIFTY and NIFTY MIDCAP buy the dip (DIP) vs. daily purchase (SIP) final corpus: Given how small the alpha is, net of fees/commissions/slippage/taxes, this is a losing proposition. You are better off with a SIP.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

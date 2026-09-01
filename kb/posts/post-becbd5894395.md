# Momentum Skip Month

- Difficulty level: 3
- Published: Fri, 14 Apr 2023 05:25:05 +0000
- Source: [https://stockviz.biz/2023/04/14/momentum-skip-month/](https://stockviz.biz/2023/04/14/momentum-skip-month/)

## Summary

The article investigates the "skip month" parameter in momentum strategies, originally introduced by Jegadeesh and Titman to mitigate short-term reversal effects. It empirically tests skip-month configurations ranging from one to four months. The findings confirm that skipping one month is the optimal configuration for overall performance, while skipping two months yields the best results when optimizing strictly for the Sharpe ratio. The piece also references related analyses on combining momentum with low-volatility filters and evaluating different rebalance frequencies, noting that less frequent rebalancing incurs minimal performance drag. All code and charts are available on GitHub.

## Article

The original Jegadeesh and Titman momentum paper (pdf) used a “skip month” to manage the reversal effect (quant.stackexchange). However, why is it one month and not two, or three or four? Here’s what the equity curves of different skip month configs look like. The “skip one month” is indeed a magical config. Also, if you are optimizing for Sharpe, skip two. Code and charts on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: github | https://github.com/stockviz/blog/tree/master/momentum/skip-month]
Blog post: Momentum Skip Month Examined why the traditional momentum strategy skips one month by testing different skip-month configurations (1 through 4 months). Confirmed the 1-month skip is indeed optimal, while skipping two months is best if optimizing purely for Sharpe ratio. tests the impact of skipping the most recent month in momentum signal computation - noskip-returns-distribution.png - symRets.png - symRetsAll.png - symRetsLowVol.png - symStatsAll.png - symRetDf.Rdata - symRets.Rdata - symRetsLowVol.Rdata - symStatsAll.csv

[Linked: Volatility, Volatility of Volatility, and Momentum | https://stockviz.biz/2023/04/13/volatility-volatility-of-volatility-and-momentum/]
Momentum has proved to be the premier anomaly in different markets. And so has low-volatility. What happens if you combine both of them? Also, what if you also add low volatility of volatility into the mix? There are a couple of ways to skin this cat. You can start with low-volatility and add momentum. Or, you could go the other way – start with momentum and then add a volatility sort. tl;dr: go with low-volatility first, momentum second (VOLxMOM). While a simple momentum sort gives the highest return, adding a low-volatility filter to it gets you a better risk-adjusted return. The order of the sort – first volatility and then momentum or first momentum and then volatility – doesn’t seem to matter much for the Sharpe rankings but the former ended up with slightly better returns. Code and charts on github.

[Linked: Momentum Rebalance Frequency | https://stockviz.biz/2023/04/18/momentum-rebalance-frequency/]
Previously, we found that the traditional 12_1 momentum configuration, where you look at the previous 12-month performance while skipping the most recent month and rebalancing every month, was indeed an ideal config (MOM_1_1). However, there are momentum index funds that rebalance once in 6-months (MOM_[0,1]_6). Is there any performance give-up if you rebalance infrequently? Turns out that the traditional config is quantifiably better than others. However, there’s isn’t much of a performance give-up even if you rebalance once in 6-months (MOM_0_6). Besides, the analysis here doesn’t factor in transaction costs which would be a bigger drag on the monthly rebalance config. When you add the tax-advantage and low-cost of index funds into the mix, the current crop of momentum index funds don’t look all that shabby. Code and charts on github.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

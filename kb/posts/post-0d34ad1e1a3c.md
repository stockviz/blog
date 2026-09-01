# Volatility and Returns

- Difficulty level: 3
- Published: Fri, 26 Apr 2019 08:54:37 +0000
- Source: [https://stockviz.biz/2019/04/26/volatility-and-returns/](https://stockviz.biz/2019/04/26/volatility-and-returns/)

## Summary

This document examines the return-volatility tradeoff between the Indian mid-cap index, represented by the NIFTY MIDCAP 100 TR, and the large-cap NIFTY 50 TR index. While mid-caps have historically outperformed large-caps, they also exhibit persistently higher volatility. The text evaluates how mixing bonds into a mid-cap portfolio impacts overall risk and return. Specifically, it demonstrates that a portfolio consisting of seventy-five percent mid-caps and twenty-five percent bonds achieves volatility levels comparable to or lower than an all-large-cap NIFTY portfolio, though it sacrifices approximately two percent in annualized returns compared to an unhedged mid-cap portfolio. The analysis highlights practical considerations such as taxes and transaction costs, concluding that risk reduction involves a clear tradeoff in expected returns.

## Article

Indian mid-caps, represented by the NIFTY MIDCAP 100 TR index, has out-performed its large-cap peer, the NIFTY 50 TR index. It has done so with higher volatility. Here is the rolling 200-week standard deviation of weekly returns of the two indices: MIDCAP volatility has been persistently higher than NIFTY volatility in the past: A portfolio of bonds and mid-caps should exhibit lower volatility than an all-equity portfolio. Here are the standard-deviation ratios for different allocations to bonds: B05, for example, represents a portfolio of 5% short-term bonds and 95% MIDCAP index. As allocation to bonds increases, portfolio volatility decreases. We see from the chart above that a 75% MIDCAP + 25% BOND portfolio has almost never seen volatility greater than an all NIFTY portfolio. So, what are we giving up in returns to reduce volatility? About 2% in returns: Take-away - On an annualized basis, the allocation portfolio gives up about 2% in returns compared to all MIDCAP portfolio and is on par with NIFTY’s. - After taxes and transaction costs, expect the allocation portfolio to under-perform buy-and-hold NIFTY. - No pain. No gain. Code and charts are on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: github | https://github.com/stockviz/blog/tree/master/volatility/allocation-1]
Blog post: Volatility and Returns Examined the return-volatility tradeoff between NIFTY MIDCAP 100 and NIFTY 50. A 75% MIDCAP + 25% bonds portfolio has almost always had lower volatility than an all-NIFTY portfolio, but sacrifices about 2% annualized returns — no free lunch. allocates portfolio weights based on volatility estimates to optimize risk-adjusted returns - NIFTY 50 TR.NIFTY MIDCAP 100 TR.B25.cumulative.png - NIFTY 50 TR.NIFTY MIDCAP 100 TR.SD-allocation-ratios.png - NIFTY 50 TR.NIFTY MIDCAP 100 TR.SD-ratio.png - NIFTY 50 TR.NIFTY MIDCAP 100 TR.SD.png

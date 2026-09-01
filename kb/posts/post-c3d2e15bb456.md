# Global Equities Momentum, Part IV

- Difficulty level: 4
- Published: Mon, 28 Jan 2019 06:29:26 +0000
- Source: [https://stockviz.biz/2019/01/28/global-equities-momentum-part-iv/](https://stockviz.biz/2019/01/28/global-equities-momentum-part-iv/)

## Summary

This document explores advanced quantitative variations of the Global Equities Momentum strategy, focusing specifically on alternative formation periods ranging from 6 to 12 months. The analysis evaluates how different formation lengths impact peak drawdowns and overall returns compared to the traditional 12-month model. It addresses the risk of data-mining associated with picking a single optimal window by investigating the effects of averaging all formation periods together. The text highlights that averaging formation periods effectively reduces strategy drawdowns. A virtual portfolio is planned to demonstrate this averaging methodology, and accompanying backtest code, cumulative charts, and drawdown graphics are made accessible on GitHub for practitioner implementation and further research.

## Article

Our GEM backtest in Part III used a 12-month formation period to measure momentum. Here, we look at alternative formation periods with an eye on drawdowns. 6- through 12-month formation periods Even though the 10-month version has higher returns, the 6-month one has lower peak drawdowns. The average of all The problem with picking one formation period out of 6 is that it smells of data-mining. What happens if you average them all out? The average works in reducing drawdowns compared to the traditional 12-month version. We will setup a virtual portfolio for this “averaging” strategy and post the link here when it is up and running. Code and more charts on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Part III | https://stockviz.biz/2019/01/26/global-equities-momentum-part-iii/]
We saw in our earlier posts on Global Equities Momentum (Part I, Part II) that by swapping the momentum equivalent of the equity indices in the GEM decision tree, one could significantly boost returns. Also, momentum trumped value. Correlation between momentum and base indices In the original GEM dual momentum model, the S&P 500 index was used to decide and to trade. What we claim here is that we can continue to use the S&P 500 index to decide, but we will use the momentum equivalents to trade. To back our claim, we present the correlation in the monthly returns of the base/momentum index pairs: The indices move pretty much in tandem. Robustness If dual momentum is robust, then our strategy piggybacks on its robustness through the decision tree. Where we differ is in the way we express the trade. And our backtest shows that GEM is superior to buying and holding the underlying indices themselves both in terms of returns and drawdowns: Instruments Implementing this strategy is fairly straightforward. You need to track the following ETFs: - SPY: for S&P 500 - BIL: for US T-bills - IDEV: World ex-US - MTUM: US Momentum - IMTM: World ex-US Momentum - AGG: Aggregated bond You will be long one of the last three ETFs above at any given point in time: We will setup a virtual portfolio for this strategy and post the link here when it is up and running. Code and more charts on github.

[Linked: github | https://github.com/stockviz/blog/tree/master/momentum/GEM%204.0]
Blog post: Global Equities Momentum, Part IV Tested alternative momentum formation periods (6-12 months) for the GEM strategy with a focus on drawdowns. Found that averaging across all formation periods reduces drawdowns compared to the traditional 12-month version, offering a way to mitigate data-mining concerns. backtests Global Equities Momentum strategies across country and US equity ETFs with various leverage levels - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.6-12mo.cumulative.png - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.all.cumulative.png - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.all.dd.png - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.any.cumulative.png - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.any.dd.png - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.avg.cumulative.png - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.avg.dd.png - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.m06.dd.png - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.m07.dd.png - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.m08.dd.png - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.m09.dd.png - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.m10.dd.png - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.m11.dd.png - USA MOMENTUM.WORLD ex USA MOMENTUM.GEM.m12.dd.png

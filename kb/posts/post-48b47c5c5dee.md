# Index and Funds

- Difficulty level: 3
- Published: Sat, 06 Jun 2026 07:25:42 +0000
- Source: [https://stockviz.biz/2026/06/06/index-and-funds/](https://stockviz.biz/2026/06/06/index-and-funds/)

## Summary

This article explores the post-COVID proliferation of indices and index funds, highlighting a 'problem of plenty' where most assets under management concentrate in large-cap market-weighted indices. It warns that post-launch returns often disappoint, urging investors to exercise patience before committing capital to 'hot' thematic or sectoral launches. The piece examines how momentum drives lumpy fund flows, creating trapped capital when performance cools, and introduces the mechanics of futures rollover costs, contango, and backwardation to compare ETFs versus direct futures. Readers should possess foundational knowledge of market mechanics, exchange-traded funds, and basic momentum concepts to fully grasp the analysis. The content applies these methods to evaluate the structural pitfalls of chasing new fund launches and the hidden costs embedded in commodity futures termstructures.

## Article

Index funds and ETFs proved most naysayers wrong and finally took off post-COVID. Now, we are dealing with a problem of plenty. The number of indices and index funds have skyrocketed with the vast majority of AUM concentrated in large-cap market-weighted indices. As everything in investing, it is always better to wait for things to settle down before committing capital. Index post-launch returns tend to disappoint. And these numbers are worse for index funds. While investors win by having low-cost access to a wide range of strategies and sectors, they can still lose by rushing in to “hot” launches. Patience pays. Charts and code on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: github | https://github.com/stockviz/blog/tree/master/newsletter/20260606]
Blog post: Index and Funds Observed that post-COVID, the proliferation of new indices and index funds has led to a 'problem of plenty,' with post-launch returns tending to disappoint. Investors should wait for things to settle before committing capital — patience pays. generates weekly performance charts for the StockViz newsletter covering index returns and fund flows - fund-launch-returns.png - fund-pre-post-scatter.png - index-aum-dashboard.png - index-aum-top20.png - index-count-by-category.png - index-launch-returns.png - index-launch-vs-aum.png - index-pre-post-scatter.png - index-scheme-count-dist.png - dates_with_funds_and_aum.csv

[Linked: Understanding Futures Rollover Cost | https://stockviz.biz/2026/04/15/understanding-futures-rollover-cost/]
What is the difference between buying gold through an ETF over buying the front-month futures contract and constantly rolling it over? When you buy physical gold, there is a cost of carry involved (funding rate + storage), plus an ETF will charge an asset management fee. Futures also have a similar cost of carry plus a rollover cost. At expiry, the price at which you sell the expiring contract and buy the next month contract is not the same. The differential is the rollover cost. Typically, the farther you go out on the futures termstructure, the higher the premium to spot – the commodity needs to be financed and stored for longer. This is called contango. Currently, gold futures (GC) traded on COMEX has the following termstructure: However, sometimes, the demand for near delivery is much higher than future delivery. This typically happens during a supply shock. When the near expiry futures trade at a premium to later expiries, the termstructure is said to be in backwardation. Currently, oil futures (CL) traded on NYMEX has the following termstructure: During contango, rolling over long futures incurs a positive rollover cost, negative otherwise. For Gold Minis (GOLDM) traded on the MCX, the historical rollover cost at expiry has fluctuated within a wide band: What this means for our analysis is that if we merely lined up the closing prices of the front-month contract and calculated returns, we will be off by ~2.5% (not considering brokerage, fees and CTT): So, to answer the question we posed at the beginning of this post, GOLDBEES or GOLDM? GOLDBEES, definitely. Previously: Investing in Gold Charts and code on github.

[Linked: Performance & Flows | https://stockviz.biz/2026/06/10/performance-flows/]
Our previous post examined how index providers and asset managers launch “hot” thematic/sectoral indices and funds to capitalize on stories. Who can blame them? Money always flows in to assets with strong recent performance (this is the very basis of momentum strategies). Take gold, for example. Fund flows have a near perfect correlation with performance. Flows into gold funds is nothing compared to what happened in thematic funds. If investors were rational, flows would be predictable. However, that is not nearly the case. The problem with lumpy flows in to hot assets is that once the price action cools down, the funds are trapped. Investors tend to feel the emotional pain of a loss about twice as intensely as the joy of an equivalent gain. So, they wait for the next cycle to exit. If you look the cumulative flows into Sectoral/Thematic funds, there’s a large reservoir of capital that will look for an exit when these funds come back up to par. Flows follow performance. And if the asset is illiquid enough, performance will then overshoot flows to form a spiral. Map the terrain. Understand the landscape before making your move. Code and charts on github.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

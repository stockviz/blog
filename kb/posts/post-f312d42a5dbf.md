# Strategy 9 with Crypto

- Difficulty level: 4
- Published: Fri, 26 Jun 2026 07:19:43 +0000
- Source: [https://stockviz.biz/2026/06/26/strategy-9-with-crypto/](https://stockviz.biz/2026/06/26/strategy-9-with-crypto/)

## Summary

This article extends Rob Carver's trend-following strategy to a broader cryptocurrency universe, testing the hypothesis that expanding beyond the top three coins improves performance. It applies multiple weighting schemes (equal-weight, inverse-volatility, long-only, long-short) and evaluates drawdowns and contributions. The analysis reveals that while the original three coins drove returns, the expanded set underperformed, with many coins contributing negatively. The author highlights the risk of overfitting through instrument selection and emphasizes the importance of robust backtesting. The content assumes familiarity with trend-following, portfolio construction, and performance metrics, making it suitable for readers with advanced knowledge in quantitative investing.

## Article

When we ran Carver’s Strategy 9 with 15 Instruments, we noticed how most of the returns were driven by crypto. However, that had only the three big coins – BTC, ETF and SOL. Since hand selecting instruments to trend-follow is also a form of overfitting, we expanded the universe to include all x-USDT coins listed in Binance since before the year 2019. There are 21 of those. Once you expand the universe, the sheen wears off. While the highest returns came from using a Binary Long-Only Equal-weight strategy, it came with a 60% drawdown, ruling out leverage. Digging into the coin-level metrics, we see how a fair number of coins have negative contributions. While the Big 3 coins had favorable trend-following returns, expanding the universe did not yield a better portfolio. Code and charts on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Carver’s Strategy 9 with 15 Instruments | https://stockviz.biz/2026/06/25/strategy-9-with-15-instruments/]
Our previous post on Rob Carver’s Strategy 9 experimented with four major Indian indices. We saw that only two of them contributed to out-performance while the others dragged. Can we just run those that worked and throw away the rest? The whole point of using multiple moving averages is to avoid overfitting. Hand selecting instruments to trend-follow is also a form of overfitting. Carver repeatedly says that his approach works best on a large set of instruments (start with 100 and whittle down.) However, as an Indian retail trader, we do not have many options. Realistically, we can lay our hands on at most 15 different instruments. With these 15, we played around with: scaled vs. binary x long-only vs. long-short x equal-weighted vs. inverse volatility weighted. The results are sobering. Long-only Equal-weight Long-short Equal-weight Long-only Inverse-volatility-weighting Long-short Inverse-volatility-weighting Of these, only the scaled long-only equal-weight setup looks promising. However, if you look at how individual instruments performed, it is hard to remain unbiased. The largest contributor is crypto. Charts and code are up on github (equal-weight, inverse-volatility-weight)

[Linked: github | https://github.com/stockviz/blog/tree/master/technical/trend-carver/strategy09-crypto]
You signed in with another tab or window. Reload to refresh your session.You signed out in another tab or window. Reload to refresh your session.You switched accounts on another tab or window. Reload to refresh your session.Dismiss alert

[Linked: Strategy 9 with 15 Instruments | https://stockviz.biz/2026/06/25/strategy-9-with-15-instruments/]
Our previous post on Rob Carver’s Strategy 9 experimented with four major Indian indices. We saw that only two of them contributed to out-performance while the others dragged. Can we just run those that worked and throw away the rest? The whole point of using multiple moving averages is to avoid overfitting. Hand selecting instruments to trend-follow is also a form of overfitting. Carver repeatedly says that his approach works best on a large set of instruments (start with 100 and whittle down.) However, as an Indian retail trader, we do not have many options. Realistically, we can lay our hands on at most 15 different instruments. With these 15, we played around with: scaled vs. binary x long-only vs. long-short x equal-weighted vs. inverse volatility weighted. The results are sobering. Long-only Equal-weight Long-short Equal-weight Long-only Inverse-volatility-weighting Long-short Inverse-volatility-weighting Of these, only the scaled long-only equal-weight setup looks promising. However, if you look at how individual instruments performed, it is hard to remain unbiased. The largest contributor is crypto. Charts and code are up on github (equal-weight, inverse-volatility-weight)

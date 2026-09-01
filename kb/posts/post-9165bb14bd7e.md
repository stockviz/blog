# Factors, The Famous 5

- Difficulty level: 3
- Published: Sun, 23 Aug 2020 06:30:44 +0000
- Source: [https://stockviz.biz/2020/08/23/factors-the-famous-5/](https://stockviz.biz/2020/08/23/factors-the-famous-5/)

## Summary

This article builds on the foundational Fama-French factor model by introducing the 2014 update with profitability and investment factors. It explains how investors can construct single-factor portfolios, but warns that factor returns can be negative for extended periods, making them unsuitable for buy-and-hold strategies. The article also touches on the proliferation of factors in academic research, mentioning momentum and low-volatility as persistent anomalies. It assumes familiarity with basic factor concepts and portfolio construction, but does not delve into advanced statistical methods or implementation details. Thus, it is appropriate for readers with a solid grasp of factor investing basics who are ready to explore practical applications and caveats.

## Article

Moar! In our post on Intro to Factors, we showed how Fama and French added value (HML) and small-caps (SMB) to the original market-risk model to account for the relative out-performance of small-cap/value investment strategies. The genesis of their idea was basically that certain portfolio returns deviated significantly from the market-risk-only model and they wanted to see if they could account for it systematically. In 2014, they updated their model by adding 2 more factors: profitability (RWM) and investment (CMA) – stocks with a high operating profitability perform better and stocks of companies with the high total asset growth have below average returns. No Free Lunch Investors can construct long-only portfolios with a single leg of any one of these factors to exploit it. For example, one can rank stocks by high book-to-price ratio, take the first 100 of them and create a value portfolio. Such a portfolio will have a high factor loading (ß) for HML. But just because you can do something like this, should you do it? Depends on your motivations. Factor returns ebb and flow. To visualize the cumulative effect of their spreads, you can plot them as a return series: As you can see, single factors can spend years in negative territory. During that time, plain-old, low-cost market-beta would be racing ahead while a factor portfolio will be an expensive drag. This leads us to posit that single factor portfolios are not buy-and-hold-forever investments. For example, during the final phases of a bull market, everything is expensive. So a portfolio of “pure value” stocks will be basically junk that no investor cares about. If you were to invest in such a portfolio, then when the market turns, these stocks are likely to drawdown way more than the rest of the market. If they were unloved in a bull, they will be massacred at the turn. So unless you thoroughly understand the dynamics of how different factors behave in different market environments, you should stick to market beta. The Factor Zoo Fama and French opened the flood-gates for factor research. Academics rushed to discover and publish increasingly esoteric and often overlapping factors. At last count, there were over 400 factors published in various academic journals. While some of them are a result of p-hacking and not all of them result in lasting alpha, there are a couple that have confounded academics and practitioners alike with their persistence: momentum and low-volatility. Stay tuned.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

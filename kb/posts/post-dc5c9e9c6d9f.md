# Portfolio Churn

- Difficulty level: 3
- Published: Wed, 18 Sep 2019 11:06:52 +0000
- Source: [https://stockviz.biz/2019/09/18/portfolio-churn/](https://stockviz.biz/2019/09/18/portfolio-churn/)

## Summary

This article discusses the impact of portfolio churn and transaction costs on investment returns, emphasizing that gross returns can be misleading due to the friction of taxes and trading fees. Using a quote from the Wolf of Wall Street to illustrate the unpredictability of stock performance, the author explains how taxes like the Securities Transaction Tax and capital gains taxes non-linearly reduce compounded returns over time. High-turnover strategies, such as momentum trading where large portions of a portfolio are replaced monthly, significantly diminish the final accumulated value compared to theoretical gross calculations. Furthermore, the text cautions investors against inappropriately comparing gross direct-equity returns with mutual fund net asset values, which already account for various internal costs.

## Article

There is a famous scene in the “Wolf of Wall Street” where Matthew McConaughey (Mark Hanna) is explaining to Leonardo DiCaprio (Jordan Belfort) the concept of fugazi: Mark Hanna: Number one rule of Wall Street. Nobody… and I don’t care if you’re Warren Buffet or if you’re Jimmy Buffet. Nobody knows if a stock is gonna go up, down, sideways or in f***ing circles. Least of all, stockbrokers, right? You know what a fugazi is?” Jordan Belfort: *Fugayzi*, it’s a fake. Mark Hanna: *Fugayzi*, fugazi. It’s a whazy. It’s a woozie. It’s fairy dust. it doesn’t exist. It’s never landed. It is no matter. It’s not on the elemental chart. It’s not f***ing real. IMDB Gross returns of a high turnover portfolio is just that – fugazi. Assume that there is an investment strategy that produces 12% in gross returns every year. Notionally, $1 should grow to $3.11 in 10 years. However, even if you assume brokerage charges are zero, demat charges don’t exist and there are no other taxes whatsoever, STT – Securities Transaction Tax – will take a slice of the portfolio at every churn. A x600 churn, where 25% of the portfolio is replaced every month, will leave you only $2.94 in 10 years. A x1200 churn, where 50% of the portfolio is replaced every month – not uncommon with most momentum strategies – will leave you with only $2.79. And STT is not the only tax that is paid on a direct-equity portfolio. Capital gains tax of 10-15% also apply. These taxes have a non-linear impact on a portfolio’s compounded returns. Investors should keep these in mind while comparing direct-equity portfolio returns. Also, mutual fund NAVs are net returns. It is highly inappropriate to compare gross direct-equity returns with mutual fund NAVs. Code for this analysis can be found on github. You can play around with it on pluto.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: github | https://github.com/stockviz/plutons/blob/master/transaction-cost-analysis.ipynb]
You signed in with another tab or window. Reload to refresh your session.You signed out in another tab or window. Reload to refresh your session.You switched accounts on another tab or window. Reload to refresh your session.Dismiss alert

[Linked: Mid-caps vs. Large-caps – A false choice? | https://stockviz.biz/2019/09/16/mid-caps-vs-large-caps-a-false-choice/]
It is generally believed that mid-caps give better returns than large-caps. But if you compare their historical returns, the difference is minuscule. But mid-caps have often inflicted a lot of pain on their investors – spending most of their time in drawdowns. There is no diversification benefit because both of them play in the same circus. If you are a buy-and-holder, why bother with mid-caps at all? Check out the notebook on pluto. You can play around with it once you login with your github account.

[Linked: Buy and Hold probably works only for US stocks | https://stockviz.biz/2019/09/19/buy-and-hold-probably-works-only-for-us-stocks/]
Social science research is said to have a WAGS problem. Most of their research is based on White American Graduate Students and fail to replicate in the real world. Finance has a similar problem where, thanks to the depth of the data available on the US markets, most investment research is based on American data. And US data clearly demonstrates the superiority of Buy and Hold. Over a 25+ year period, the probability of ending up with a loss is less than 0.1% If future returns are in the same vein as their past returns, US investors would be fools not to buy and hold forever. However, this does not mean that the rest of the world should do the same thing. Every market is different. The Japanese experience is a study in contrast. The probability of a negative outcome is a whopping 21% for them. i.e., there is a one-in-five chance that investors will not make any money investing in Japanese equities. If past is indeed prelude. Indian investors have been better off than their Japanese counterparts. There is only a 6% chance of not making any money investing in Indian equities. All this goes to show that the US is a statistical outlier. “Buy and Hold” working in the US is an outlier. In every other market, there is a non-trivial chance that you will not make any money buying-and-holding equities. When you look at research based on US markets, keep in mind that the probability distribution of returns that it is based on are an outlier. Anything that is long US equities will “look good.” All investing is forecasting. And these probabilities will change – we are talking about equity markets after all. But know this before you adopt the “buy and hold” mantra.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

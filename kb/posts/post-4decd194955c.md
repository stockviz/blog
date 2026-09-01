# Using SMA to Reduce Volatility of Returns

- Difficulty level: 3
- Published: Fri, 08 Aug 2014 07:13:32 +0000
- Source: [https://stockviz.biz/2014/08/08/using-sma-reduce-volatility-returns/](https://stockviz.biz/2014/08/08/using-sma-reduce-volatility-returns/)

## Summary

This article explores how utilizing simple moving average (SMA) switches can successfully reduce the volatility of returns compared to a naive buy-and-hold strategy. Using daily return data since 2010 for the CNX 100, the analysis compares a standard buy-and-hold approach against 200-day, 100-day, and 50-day SMA switch strategies. The results demonstrate that implementing these tactical SMA switches decreases the frequency of large negative daily returns and lowers standard deviation, while improving average daily returns, even after accounting for trading costs, impact costs, and tracking errors.

## Article

Introduction We saw how a CNX 100 50-day tactical investment strategy boosts returns of a naive buy-and-hold strategy (here) even while considering trading costs and other friction (here.) To visualize how this works, lets have a look at the histogram of daily returns since 2010 (1150 trading days.) Naive buy-and-hold | Daily Returns | | |---|---| | <= -2% | 36 days | | <= -1% | 165 days | | >= +2% | 41 days | | >= +1% | 188 days | | Average | +0.04% | | Std. Dev. | 1.07 | 200-day SMA switch | Daily Returns | | |---|---| | <= -2% | 16 days | | <= -2% | 85 days | | >= +2% | 21 days | | >= +1% | 122 days | | Average | +0.07% | | Std. Dev. | 0.79 | 100-day SMA switch | Daily Returns | | |---|---| | <= -2% | 11 days | | <= -2% | 66 days | | >= +2% | 21 days | | >= +1% | 114 days | | Average | +0.09% | | Std. Dev. | 0.74 | 50-day SMA switch | Daily Returns | | |---|---| | <= -2% | 7 days | | <= -2% | 53 days | | >= +2% | 24 days | | >= +1% | 110 days | | Average | +0.11% | | Std. Dev. | 0.71 | Conclusion Even after considering trading costs, impact costs and tracking error, this strategy comes out way ahead of a naive buy-and-hold strategy. Better returns than buy-and-hold with lower volatility and at a low cost! You can follow the Theme here.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: here | https://stockviz.biz/2014/08/04/cnx-100-50-day-tactical-theme/]
Escaping the worst days We had discussed how, by escaping the worst days, even if it means missing out on the best days, you can protect your portfolio from drawdowns and get superior results compared to a naive buy-and-hold strategy. See: The SMA Risk On/Off Switch We ran the same filter on the CNX 100 index. CNX 100 between 2005 and 2010 Naive Buy and Hold Cumulative Return: 1.92 DrawDowns 50-day SMA On/Off Cumulative Return: 13.12 DrawDowns CNX 100 from 2010 to now Naive Buy and Hold Cumulative Return: 0.473696 DrawDowns 50-day SMA On/Off Cumulative Return: 2.270039 DrawDowns The CNX 100 50-Day Tactical Theme The 50-day signal can be used to go “risk on” and “risk off” between the NIFTYBEES and JUNIORBEES ETFs. When “risk on”, the Theme allocates equally between NIFTYBEES and JUNIORBEES and when “risk off”, moves to LIQUIDBEES. You can follow the theme here.

[Linked: here | https://stockviz.biz/2014/08/05/50-day-sma-cnx-100-friction/]
There is always friction One of our readers made a very astute observation yesterday: It is true that every strategy has friction. Friction in terms of trading costs, tracking error, whiplash, etc. So we put the 50-Day SMA CNX 100 that we discussed yesterday through the wringer to see what happens in the real world. Modeling friction We charge a pretty low brokerage of 0.2%. A two way buy and sell would cost 0.4%. Impact cost is probably 0.2%. For a total of 0.6% in friction. Lets round it up to 1% to give us a margin of comfort. Whenever a trade happens, we will deduct 1% from the notional amount to account for this friction. From the start of 2010 to now, there were 1146 trading days, out of which, the strategy would have resulted in trades for 56 of them. Now lets compare the Raw 50-day CNX 100 with the Buy-and-Hold (B&H) and Friction scenarios. The investor still comes out as a winner with a cumulative return of 0.86 vs. 0.47 in buy-and-hold. Accounting for tracking error The above analysis used the CNX 100 index to model friction. However, in the real world, you cannot own fractional shares. This gives rise to tracking error. What would the numbers look like if we used the ETFs themselves? The investor still comes out as a winner with a cumulative return of 0.84. Conclusion Even after considering trading costs, impact costs and tracking error, this strategy comes out way ahead of a naive buy-and-hold strategy. You can follow the Theme here.

[Linked: here | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

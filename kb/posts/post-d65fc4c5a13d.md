# Relative Momentum Back-test

- Difficulty level: 3
- Published: Tue, 19 Jun 2018 09:00:43 +0000
- Source: [https://stockviz.biz/2018/06/19/relative-momentum-back-test/](https://stockviz.biz/2018/06/19/relative-momentum-back-test/)

## Summary

The article discusses a back-test of a Relative Momentum algorithm applied to about 1,000 USD denominated Total Return indices out of over 35,000 global indexes published by NASDAQ OMX on Quandl. The study investigates whether the rank of the Indian TR Index within this subset can effectively time NIFTY or MIDCAP investments. Readers are directed to download a supplementary PDF containing detailed charts and are provided contact information for high-resolution images.

## Article

Daily data of over 35,000 global indexes published by NASDAQ OMX including Global Equity, Fixed Income, Dividend, Green, Nordic, etc. are available for free download on Quandl. Out of which about 1000 indices are USD denominated Total Return (TR) indices. We were curious to find out the result of applying our Relative Momentum algorithm on this subset. We also wanted to know if the rank of the Indian TR Index within this subset can be used to time NIFTY or MIDCAP investments. To know more, download the pdf Relative Momentum Back-test. The charts in the document are low-res. Whatsapp us on +918026650232 for the high-res ones.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Book Review: Hedge Fund Market Wizards | https://stockviz.biz/2018/06/09/book-review-hedge-fund-market-wizards/]
Hedge Fund Market Wizards: How Winning Traders Win by Jack Schwager (Amazon,) is a collection of interviews of successful hedge fund mangers. I excerpted the parts I found interesting and personally applicable to me in Evernote. Read that to get an idea of what the book is about. The book would have been even better if he included some quant hedge fund managers other than Bridgewater’s Dalio. Recommendation: Read it now!

[Linked: Lumpsum vs. Dollar Cost Averaging (SIP) | https://stockviz.biz/2018/06/23/lumpsum-vs-dollar-cost-averaging-sip/]
Among Indian investors, SIPs (Systematic Investment Plans) are the rage right now. The total amount collected through SIP during May 2018 was ₹7,304 crore according to AMFI. SIPs are great for investors with a regular income – it matches the frequency of savings with the frequency of income. Structural discipline is always a welcome thing. However, for investors who have lumpy incomes or a windfall, it is often a dilemma whether to invest as a lumpsum or to setup an STP (Systematic Transfer Plan.) Both SIPs and STPs are a form of DCA (Dollar Cost Averaging) where you average into an investment over a period of time (the accumulation phase.) The thing about DCA is that it ends up under-performing a lumpsum in markets that are trending up. Intuitively, you want to buy more when the price is low (in the beginning) and less when the price is high (at the end.) So, if the market is going up, then it makes no sense to spread a lumpsum over a period of time – you are guaranteed to make the later buys at a higher level, reducing your overall returns. In the case of equity markets, the expectation is that they tend to go up over time. So if you are looking at a 10-20 year time horizon, then you are better off investing in one shot. To put this intuition to test, we modeled the returns of NIFTY, MIDCAP and GOLD as a Generalised Lambda Distribution (this works better than a normal distribution because these returns have significant skews and kurtosis) and ran a 10,000 path simulation to get a sense of the probability distribution of DCA vs lumpsum investments. Roughly, this is like assuming that the weekly return distribution is going to be the same across 10,000 different worlds. So you pick set of random weekly returns from the same distribution 10,000 different times and see how DCA and lumpsum perform over those worlds. When you plot the density of those returns, you get an idea of how they compare. To keep things simple, lets compare NIFTY MIDCAP 100 and GOLD. First, the price charts: And now the simlulated cumulative return densities of MIDCAP and GOLD, modeled with data after 2010: The area to the left of zero is that of negative returns. Lumpsums have a longer left tail compared to DCA so probability of a large negative outcome is higher for the former. However, the total area under zero is higher for DCA in MIDCAPs so the probability of negative outcomes in general is higher for DCA/SIP. Lumpsums have a fat right tail for both MIDCAPs and GOLD so the probability of large positive outcomes is higher for lumpsums. “Average” DCA returns are less than “average” lumpsum returns but they occur with a higher probability. For a prudent investor, it is the left tail that matters the most. Even though lumpsums hold out hope for higher returns (fat right tails,) they have a small probability of a big loss that is greater than that for DCA (longer left tails.) In conclusion, a prudent investor should convert a windfall into an STP and a risk-seeker should do a lumpsum. For readers curious about the code and for additional charts with longer time periods, visit github.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

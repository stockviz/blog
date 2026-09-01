# The Smirk

- Difficulty level: 3
- Published: Fri, 03 Nov 2023 06:45:10 +0000
- Source: [https://stockviz.biz/2023/11/03/the-smirk/](https://stockviz.biz/2023/11/03/the-smirk/)

## Summary

This article explains the volatility smirk, a phenomenon where implied volatility differs between OTM puts and calls, contradicting the Black-Scholes-Merton model's assumption of uniform volatility. It attributes the smirk to market crashes and fat tails, and discusses how the smirk's shape varies with strike distance. The article includes practical analysis of the NIFTY options market, with charts and code, and references further reading on risk-reversal premium and skew. It assumes familiarity with options pricing, implied volatility, and basic statistical concepts like fat tails, making it suitable for readers with intermediate knowledge who are ready to apply these concepts to real market data.

## Article

When you use the Black-Scholes-Merton (BSM) model, you end up with theoretical prices that assumes that volatility affects all strikes uniformly. i.e., strikes have no bearing on implied volatility (IV). This was largely true in the market as well until the crash of 1987. However, after the October 1987 crash, the implied volatility computed from option prices using the BSM model started differing between puts and calls. This is called “volatility smile“, or the smirk, given its actual shape. The reason for this is quite simple, markets take the stairs up and the elevator down. Fat tails, if you must. So, put options sellers require a little bit of an incentive to take on that risk. How crooked is the smirk? If you take the ratio of the IVs of OTM puts to OTM calls and plot them, you’ll notice that as you get farther away from spot, the distribution flattens out. Notice the area below 1.0? Those are the days when the calls were trading at a higher IV than the puts. On the left of zero are the calls with descending order of strikes and on the right are puts with ascending order of strikes. The farther away from zero, the more OTM they are. Also, unlike the stylized charts of IV you might have seen with sweet smiles, the reality is quite different. If this tickles your curiosity, do read The Risk-Reversal Premium, Hull and Sinclair (SSRN) Code and charts on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Fat tails | https://stockviz.biz/2020/08/03/fat-tails/]
Introduction Years of returns can get wiped out in a month in the markets. While investors mostly focus on the average, the tails end up dictating their actual returns. (Introduction) Sampling and Measurement Typically, a uniform sample is taken. The problem with this is it under-represents the tails. This leads to models that work on average but blow up on occasion. One way to overcome this problem is through stratified sampling. (Sampling) Expected shortfall (ES) is a risk measure that can be used to estimate the loss during tail-events. (Measuring) Acceptance All assets have fat tails. It is a feature, not a bug. (Historical)

[Linked: github | https://github.com/stockviz/blog/tree/master/volatility/iv-strike]
Blog post: The Smirk Examined the volatility smirk (skew) in Indian options markets, where OTM puts trade at higher implied volatility than OTM calls following the 1987 crash pattern. Found the smirk flattens farther from spot, and the real IV surface differs markedly from idealized textbook smiles; also noted rare instances where calls traded at higher IV than puts. analyzes the implied volatility surface, volatility skew, and smirk patterns across option strikes - NIFTY.iv-strike.01.png - NIFTY.iv-strike.density.png - daily.skew.density.png - weekly.skew.density.png - NIFTY.iv-strike.Rdata

[Linked: Midcap Select Index Futures, Part II | https://stockviz.biz/2023/10/24/midcap-select-index-futures-part-ii/]
At the launch of Nifty Midcap Select Index futures, we had pointed out that strategies that work on the broader Midcap 150 index should work on it as well. Since then, MIDCPNIFTY has had a colorful journey with the exchange experimenting with different tenures and expiries. However, the experimentation phase seems to be over and volumes have steadily improved with the product finding decent traction. Some quick thoughts on liquidity: - Simply don’t trade the opening and closing stubs and you are golden. - Also, stick to the nearest expiry – the spreads on the other two will make your eyes bleed. - The tightest spreads can be usually found around 15 minutes to close – great if you are taking positional trades end-of-the-day. On the face of it, MIDCPNIFTY futures look good enough to trade. Happy hunting!

[Linked: Skew | https://stockviz.biz/2023/11/08/skew/]
Our previous post discussed how the implied volatility (IV) of OTM puts are often higher than the IV of OTM calls. We would like to add that this “smirk” is very much warranted – it is not an invitation to sell OTM puts. Returns of financial instruments often have negative skew – a fancy way to say that they often take an escalator up, and an elevator down. Here are the daily and weekly return skews of the NIFTY 50 TR index and the SPY ETF: The market is willing to pay up to hedge against this risk. If you sell the skew, you’ll have to hedge against it by some other means. Otherwise, it is like picking up pennies in front of a bulldozer.

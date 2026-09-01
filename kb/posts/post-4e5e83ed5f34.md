# Understanding Nifty Volatility

- Difficulty level: 3
- Published: Sat, 19 Apr 2014 13:30:38 +0000
- Source: [https://stockviz.biz/2014/04/19/understanding-nifty-volatility/](https://stockviz.biz/2014/04/19/understanding-nifty-volatility/)

## Summary

This document explores the nature of Nifty volatility, defining historic volatility and highlighting how StockViz utilizes the Yang Zhang volatility estimator. Volatility is identified as a major contributor to option premiums, prone to significant historical spikes and fat tails that can disrupt trading strategies. The text advises traders to remain on the long side of volatility rather than solely relying on theta decay carry, as unexpected volatility surges can quickly erase months of profit. Contextual linked sources further elaborate on historical volatility estimation, theta decay dynamics in Nifty options, and implied volatility.

## Article

Definition Volatility (σ) is a measure for variation of price of a financial instrument over time. Historic volatility is derived from time series of past market prices. There are different ways of calculating volatility. At StockViz, we use Yang Zhang Volatility. σ is one of the biggest contributor of option premiums. Understanding its true nature will help you trade it better. Volatility spikes Observe the volatility spikes since 2005. Even though the average is around 0.3, its not uncommon to have huge swings. Fat tails abound Trading strategy Always try to be on the long-side of volatility. It might be tempting, while trading options, to try and clip the carry on θ-decay. But you should always be aware of the fat-tails of volatility that can crush many months of carry P&L overnight. Related articles - Forensics: NIFTY Options – Theta(θ) Decay (stockviz.biz) - Forensics: NIFTY Options – Implied Volatility(IV) (stockviz.biz)

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Yang Zhang Volatility | https://stockviz.biz/yang-zhang-volatility/]
Yang and Zhang were the first to derive an historical volatility estimator that has a minimum estimation error, is independent of the drift, and independent of opening gaps. This estimator is maximally 14 times more efficient than the close-to-close estimator. Yang and Zhang derived an extension to the Garman Glass historical volatility estimator that allows for opening jumps. It assumes Brownian motion with zero drift. Original paper: Drift‐Independent Volatility Estimation Based on High, Low, Open, and Close Prices

[Linked: Forensics: NIFTY Options – Theta(θ) Decay | https://stockviz.biz/2014/04/13/forensics-nifty-options-theta-decay/]
The most intuitive option greek is theta (θ) – a measurement of the option’s time decay. Theta measures the rate at which options lose their time value, as the expiration date draws nearer. It is usually expressed as a negative number. Simply put, theta of an option reflects the amount by which the option’s value will decrease every day. Time and Theta - Longer term options have theta of almost 0 as they do not lose value on a daily basis. - Theta is higher for shorter term options. - Theta is higher for at-the-money options. - Theta changes at an exponential rate. It goes up dramatically as options near expiration as time decay is at its greatest during that period. Theta decay in action: March 2014 NIFTY Options Since Jan First, lets look at the underlying: To capture the full move of the NIFTY, you’ll have to look at, at least, a dozen strikes between 5950 and 6900. θ of Calls: θ of Puts: Does it mean that you should go out and sell the heck out of every option you can find close to expiry? No! As expiration gets closer, the risk posed by extreme amounts of gamma outweighs the theta you’re collecting. Stay tuned for more. Related articles - Forensics: NIFTY Options (stockviz.biz) - Nifty Gap Analysis (stockviz.biz)

[Linked: Forensics: NIFTY Options – Implied Volatility(IV) | https://stockviz.biz/2014/04/14/forensics-nifty-options-implied-volatilityiv/]
Implied volatility(IV) is a measure of the market’s expectations for the underlying’s performance during the life span of the option. The IV of an option is actually backed out of the price of the option. All the inputs of an options pricing model are known (time to expiration, strike, price, interest rates) except for the volatility that the option is pricing in. So that value can be backed out and allows you to understand the relative value of the option’s price. This Khan Academy video does a good job of explaining what IV is: - When IV is high, options will be more expensive to purchase. And low IV will translate to more affordable option prices. - Heightened implied volatility correlates with bearish sentiment, while low IV suggests a bullish mood. - If you purchase an option with high IV, you need a much bigger move out of the underlying stock to profit from the trade. - IV will rise ahead of scheduled events, such as earnings reports and new product launches. Once the anticipated event occurs, IV will immediately drop. IV in Action: March 2014 NIFTY Options Since Jan First, lets look at the underlying: To capture the full move of the NIFTY, you’ll have to look at, at least, a dozen strikes between 5950 and 6900. Related articles - Forensics: NIFTY Options – Vega(κ) - Forensics: NIFTY Options – Theta(θ) Decay - Forensics: NIFTY Options – Gamma(γ) (stockviz.biz) - Forensics: NIFTY Options – Delta(δ) (stockviz.biz) - Forensics: NIFTY Options (stockviz.biz)

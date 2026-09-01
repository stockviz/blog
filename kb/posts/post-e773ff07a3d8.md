# Analysis: Bhavin Desai’s Bull Spread on ITC

- Difficulty level: 3
- Published: Thu, 17 Apr 2014 13:24:23 +0000
- Source: [https://stockviz.biz/2014/04/17/analysis-bhavin-desais-bull-spread-itc/](https://stockviz.biz/2014/04/17/analysis-bhavin-desais-bull-spread-itc/)

## Summary

This document analyzes a specific options trading strategy, namely a 350/360 long call spread on ITC suggested by Bhavin Desai. The text breaks down the mechanics of the trade, examining relevant greeks such as theta and model premiums, analyzing the payoff diagram at expiry, and determining the break-even stock price and maximum potential loss. It contrasts the stated market rationale with standard options theory regarding bull spreads, which are typically deployed to capture moderate upward movement while mitigating the upfront costs of purchasing lower strike calls through simultaneous shorting of higher strikes.

## Article

Bhavin Desai of Motilal Oswal Securities was on CNBC saying that one may buy ITC 350 Call and advises shorting 360 Call. This is a 350/360 long call spread on ITC. Let’s see how the trade works. The greeks The 360 call has a θ of -145.36 and it the model premium is 3.04. This means that the time decay will make the option worthless in a couple of days. Not bad since you are an option seller. The 350 call is already ITM (the stock closed at 352.70) and the last traded price, Rs. 6.3 is less than the model price of Rs. 7.49. Not a bad deal. Payoff diagram at expiry ITC needs to be above Rs. 354.35 at expiry for this trade to break-even. Max loss is the premium paid upfront (Rs. 4350) The right trade for the wrong reasons? The transcript on moneycontrol says: We are not really sure what that means. The reason why you would put a bull spread on is if you are moderately bullish about the stock and want to mitigate the cost of buying the lower strike by selling a higher strike. Reference Related articles - Long Call Spread - USDINR 63.5/65 Bull Spread (stockviz.biz)

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Long Call Spread | https://stockviz.biz/2014/04/16/long-call-spread/]
Introduction Suppose you are moderately bullish about a stock/index and you feel that it has room to run but its not going to be gangbusters. Then you could buy a call outright but that could be expensive. What you could do is buy the call and then sell a call at a higher strike to mitigate the cost of your (moderately) bullish outlook. A long call spread (or a bull spread) contains two calls with the same expiration but different strikes. The strike of the short call is higher than the strike of the long call. The short call’s main purpose is to help pay for the long call’s upfront cost. Example The Max loss is the net premium paid: Rs.1825.00 For the trade to break even, the NIFTY should end above 6786.50 at expiration (April 24). The Max profit at expiration is Rs.3175.00 The greeks The long call is more sensitive to changes in the underlying than the short call due to its ATM-ness. All the greeks, δ, θ, κ, and λ are higher for the long call than for the short call. The long-call will lose money faster to time decay than the short call. By freezing all other inputs, you can observe θs across different strikes of the bull spread at different values of the NIFTY as expiry approaches: Time decay is helpful to this position when it is profitable and harmful when it is loss-making. Similarly, observe how δs of the bull spread at different values of the NIFTY as expiry approaches: Exiting the trade If the trade is profitable, allow time-decay to work for you. You could even hold this to expiration. If the position is moving against you, it is best to cut your losses.

[Linked: USDINR 63.5/65 Bull Spread | https://stockviz.biz/2013/11/12/usdinr-63-565-bull-spread/]
We entered a USDINR 63.5/65 Bull Spread today. Basically you buy the ITM call and sell the OTM strike – cheaper than buying a call outright. The trade has a max payoff of Rs. 955 and costs around Rs. 565 to put on. Here’s how the pay-off looks like: The break-even is around 64 – basically the Rupee has to trade above that. But since we sold the 65 call, our returns are capped if the Rupee depreciates below 65 anytime soon. Novembers were last trading at 64.02 (+0.34) We had discussed a USDINR Condor before where we were betting on range-bound behavior… and it didn’t quite end well. Hopefully the setup works better this time… fingers crossed!

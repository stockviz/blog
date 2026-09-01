# A Simple NIFTY Short Call Butterfly Back-Test

- Difficulty level: 3
- Published: Fri, 10 Jul 2015 07:03:22 +0000
- Source: [https://stockviz.biz/2015/07/10/a-simple-nifty-short-call-butterfly-back-test/](https://stockviz.biz/2015/07/10/a-simple-nifty-short-call-butterfly-back-test/)

## Summary

This document presents a back-test of a short-call butterfly options strategy applied to the NIFTY index from 2010 through the present. The analysis evaluates monthly index moves and demonstrates that while a median move exceeding one percent generally makes the strategy profitable, flat endings can eliminate prior gains. The text highlights the importance of understanding underlying NIFTY drivers and managing risk to prevent outsized losses from wiping out incremental profits. Additionally, related notes discuss rolling returns and the impact of index price levels on required percentage moves for profitability.

## Article

Monthly moves We saw that the NIFTY’s median move over a 30-day period is over 1% and that is all that is required to make a short-call butterfly strategy profitable. Let us now do a quick back-test to check if it is indeed the case. Back-test Here is the short-call butterfly back-test from 2010 through now. You basically sell the closest expiry butterfly at each expiry (click to embiggen): Summary While it is true that a 1% move in the NIFTY results in a profitable trade, there are instances where the NIFTY doesn’t move +/-1%. When the NIFTY ends flat, you end up losing all your prior profits. Understanding what drives NIFTY to move is key to managing your risk while running this strategy.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: saw | https://stockviz.biz/2015/07/09/selling-nifty-butterflies/]
Nifty Rolling Returns In our earlier post, we saw how selling NIFTY butterflies has been profitable this year. To understand why, let’s have a look at the rolling returns of the NIFTY. Here’s the 30-day rolling returns of the NIFTY, from 2010 to the present, the whole population: Beginning of 2015-present: Median: -1.08% Profitability For a short-call butterfly to be profitable, NIFTY has to expire away from the either of the wings. Each wing is 100 points away. With NIFTY at 8500, that’s a 1.12% move. Whereas back when NIFTY was around 6000, this trade would require a 1.67% move to be profitable. So as the NIFTY rises, if they don’t widen the distance between the listed strikes, your hit ratio with selling butterflies will increase. However, the total profitability will decrease because everybody will think this way. Summary If NIFTY continues to exhibit the same pattern of returns, a rising NIFTY will make selling butterflies more profitable.

[Linked: Selling NIFTY Butterflies | https://stockviz.biz/2015/07/09/selling-nifty-butterflies/]
Nifty Rolling Returns In our earlier post, we saw how selling NIFTY butterflies has been profitable this year. To understand why, let’s have a look at the rolling returns of the NIFTY. Here’s the 30-day rolling returns of the NIFTY, from 2010 to the present, the whole population: Beginning of 2015-present: Median: -1.08% Profitability For a short-call butterfly to be profitable, NIFTY has to expire away from the either of the wings. Each wing is 100 points away. With NIFTY at 8500, that’s a 1.12% move. Whereas back when NIFTY was around 6000, this trade would require a 1.67% move to be profitable. So as the NIFTY rises, if they don’t widen the distance between the listed strikes, your hit ratio with selling butterflies will increase. However, the total profitability will decrease because everybody will think this way. Summary If NIFTY continues to exhibit the same pattern of returns, a rising NIFTY will make selling butterflies more profitable.

[Linked: BANKNIFTY Butterflies | https://stockviz.biz/2015/07/10/banknifty-butterflies/]
Introduction So far, we have focused on the NIFTY for selling butterflies (Part I, II, III.) How would this look on the BANKNIFTY? CNX BANK Index Returns First, let’s have a look at the 30-day rolling returns of the CNX BANK Index, from 2010 to the present, the whole population: Beginning of 2015-present: Median: flat Currently, the index is around 18700. That means a 100-point move translates to 100/18700 ~0.5% Expiry-to-Expiry Back-test If we do the same back-test we did to the NIFTY, this is what we find: Conclusion As with the NIFTY, the trade makes money if you know how to cut your losses. However, when the trade is live, how do we know what the future volatility of the underlying is going to look like? Without risk-management, a short-call butterfly strategy will encounter out-sized losses that wipe out all prior incremental gains.

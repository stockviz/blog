# The Inflation Drag on Bond Returns

- Difficulty level: 3
- Published: Thu, 14 Mar 2019 06:53:26 +0000
- Source: [https://stockviz.biz/2019/03/14/the-inflation-drag-on-bond-returns/](https://stockviz.biz/2019/03/14/the-inflation-drag-on-bond-returns/)

## Summary

This document examines the impact of inflation on bond returns in comparison to equity returns like the NIFTY. Specifically, it highlights how inflation at the short end of the curve completely erodes bond returns and even reduces them further. The analysis discusses how high inflation historically forced markets to demand high gross returns, and how real returns are significantly lower than nominal returns across various asset classes. The text serves as an applied financial analysis of fixed-income assets under inflationary pressures, utilizing historical data from 1991 through 2016 to demonstrate the erosion of purchasing power in debt investments.

## Article

Previously, we looked at how inflation adjusted returns for the NIFTY, from 1991 through 2016, was 5% annualized. How does it look for bonds? At the short-end of the curve, it looks like inflation ate away all of the returns… and some more. Code and charts on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: inflation adjusted returns for the NIFTY | https://stockviz.biz/2018/09/23/inflation-drag-returns/]
What do you think is the annualized inflation adjusted NIFTY 50 return is from 1991 through 2016? Hint: Gross returns were ~13% It was 5% The Midcap index was created much later. So to keep things on an even keel, if you run both NIFTY 50 and NIFTY MIDCAP 100 between 2002 and 2016, it turns out that their real returns were about 7% and 13% respectively. So, - The asset class that you pick should jive with your time horizon. No point investing in NIFTY 50 (or large-caps, for that matter) if you have a 10+ year time-horizon. - The market demanded high gross returns because of high inflation. If the RBI’s commitment of a 4-6% inflation band gets fully priced in, expect gross returns to come down in the future. - Neither market returns nor inflation is under your control. However, your lifestyle inflation is all on you. Code and additional charts are on github.

[Linked: github | https://github.com/stockviz/blog/tree/master/inflation%20drag]
Blog posts: Calculated that the inflation-adjusted annualized NIFTY 50 return from 1991-2016 was only ~5% compared to ~13% nominal, revealing that inflation erodes roughly two-thirds of Indian equity returns over long horizons. Examined inflation-adjusted bond returns from 1991-2016. Found that at the short end of the curve, inflation completely eroded all bond returns — and then some. quantifies the long-term impact of inflation and taxes on real investment returns - 0_5.gross.real.returns.2004-2017.png - 20_30.gross.real.returns.2004-2017.png - NIFTY 50.gross.real.returns.1991-2016.png - NIFTY 50.gross.real.returns.2002-2016.png - NIFTY MIDCAP 100.gross.real.returns.2002-2016.png

[Linked: Index Valuations, Part II | https://stockviz.biz/2019/03/13/index-valuations-part-ii/]
In Part I of Index Valuations, we showed how the relative PE (price-to-earnings ratio) and PB (price-to-book ratio) of the NIFTY 50 and NIFTY MIDCAP 50 indices have varied over time. What would a portfolio that weighted each of these based on the relative valuation ratio look like? Backtest Suppose, the relative ratio (R) = Ratio(MIDCAP)/Ratio(NIFTY) Then, at the end of every month, re-weight the protfolio so that portfolio (S1) = R * NIFTY + (1-R) * MIDCAP, and portfolio (S2) = (1-R) * NIFTY + R * MIDCAP Ratio can either be PE or PB It looks like: - a portfolio with PB based weights is a lot less volatile than the PE based one. - PB portfolio recovers much faster that the PE or plain-vanilla indices from deep drawdowns - PB out-performs an equal weight portfolio You can track and map this strategy to your portfolio using the PB weighted NIFTY/MIDCAP Theme. Code and charts on github.

[Linked: No Silver Bullets | https://stockviz.biz/2019/03/15/no-silver-bullets/]
Most of the time, beta swamps alpha. Take the case of the Roubini Country Insights model, for example. It claims to “rank countries based on an analysis of over 2500 data points from institutions such as the Bank of International Settlements and the World Bank.” Also, “these data points cover each country’s demographics, quality of education, healthcare and ability to innovate, and will look at the country’s growth potential in political and social spheres, as well as its top-down macro-economic situation.” It sounds like it does everything that a smart investment manager with a long-only global equities mandate should be doing. And you would expect such a smart model to add significant alpha. Thanks to Barclays, a bunch of equity indices based on this model have been available for a while now. We were curious as to how these performed vs. their corresponding plain-vanilla market-cap weighted cousins. Developed markets: MSCI World (black) vs. Insights (green) Emerging markets: MSCI EM (red) vs. Insights (blue) The value add from the smart-beta quantitative “Insights” model, roughly about 1% a year, seems skinny compared to all the work that must have gone into it. 2500 data points is a big dataset but it looks like most of them have no effect on equity returns. This also ties into the curse of dimensionality when dealing with complex adaptive systems – more data typically subtracts from the model. As an investor, it probably would have been easier to stay invested in one of the cap-weighted indices, just accepting the beta, rather than reach for that 1% extra with fancy sounding strategies.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

# The Inflation Drag on Returns

- Difficulty level: 3
- Published: Sun, 23 Sep 2018 07:45:46 +0000
- Source: [https://stockviz.biz/2018/09/23/inflation-drag-returns/](https://stockviz.biz/2018/09/23/inflation-drag-returns/)

## Summary

This document examines the long-term impact of inflation on equity and bond returns in the Indian market from 1991 through 2016. While nominal gross returns for the NIFTY 50 were approximately 13%, the annualized inflation-adjusted real return was only 5%, demonstrating that inflation erodes a significant portion of investment gains over extended periods. Comparing the NIFTY 50 and NIFTY MIDCAP 100 between 2002 and 2016 reveals real returns of 7% and 13% respectively, highlighting how asset class selection should align with an investor's time horizon. Furthermore, bond returns at the short end of the curve were entirely wiped out by inflation. The analysis emphasizes that future gross returns may decline if the Reserve Bank of India's target inflation band becomes fully priced into the market.

## Article

What do you think is the annualized inflation adjusted NIFTY 50 return is from 1991 through 2016? Hint: Gross returns were ~13% It was 5% The Midcap index was created much later. So to keep things on an even keel, if you run both NIFTY 50 and NIFTY MIDCAP 100 between 2002 and 2016, it turns out that their real returns were about 7% and 13% respectively. So, - The asset class that you pick should jive with your time horizon. No point investing in NIFTY 50 (or large-caps, for that matter) if you have a 10+ year time-horizon. - The market demanded high gross returns because of high inflation. If the RBI’s commitment of a 4-6% inflation band gets fully priced in, expect gross returns to come down in the future. - Neither market returns nor inflation is under your control. However, your lifestyle inflation is all on you. Code and additional charts are on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: github | https://github.com/stockviz/blog/tree/master/inflation%20drag]
Blog posts: Calculated that the inflation-adjusted annualized NIFTY 50 return from 1991-2016 was only ~5% compared to ~13% nominal, revealing that inflation erodes roughly two-thirds of Indian equity returns over long horizons. Examined inflation-adjusted bond returns from 1991-2016. Found that at the short end of the curve, inflation completely eroded all bond returns — and then some. quantifies the long-term impact of inflation and taxes on real investment returns - 0_5.gross.real.returns.2004-2017.png - 20_30.gross.real.returns.2004-2017.png - NIFTY 50.gross.real.returns.1991-2016.png - NIFTY 50.gross.real.returns.2002-2016.png - NIFTY MIDCAP 100.gross.real.returns.2002-2016.png

[Linked: NIFTY Index Drawdowns vs. Returns | https://stockviz.biz/2018/09/22/nifty-index-drawdowns-vs-returns/]
Things that stand out: - Sometimes, a sector doesn’t recover even after a decade – REALTY - FMCG is the other outlier. Did not drawdown much but has bested the rest in terms of returns. - PVT BANK performance is surprising. You can compare all these indices using our new tool. Code for the chart above is on github.

[Linked: Principal Component Analysis, Part I | https://stockviz.biz/2018/09/25/principal-component-analysis-part-i/]
Introduction Principal Component Analysis (PCA) is a way of summarizing data. For example, if you take financial services, there are quite a few sector indices that cover it: Bank, Pvt. Bank, Public Bank, Financial Services, etc… There will be overlap between all these indices, so the question is, in what proportion should one invest in these individual indices in order to get the most optimal exposure to financial services? PCA is one way to answer this question. To get a better understanding of what it is, see: stats.stackexchange. NASDAQ OMX India TR Indices To start this series on PCA, we will first look at the USD denominated Total Return indices published by NASDAQ-OMX. Choosing these indices helps us avoid a lot of data pre-processing steps. First, they are Total Return, so they incorporate dividends, etc. Second, they are US dollar denominated, so we don’t have to worry about being long USDINR while looking at tech stocks. And third, they start from 2001, which goes way farther than the TR indices published by the NSE. We use the following sector indices: NASDAQ India Basic Matls TR Index (NQIN1000T), NASDAQ India Cnsmr Goods TR Index (NQIN3000T), NASDAQ India Financials TR Index (NQIN8000T), NASDAQ India Health Care TR Index (NQIN4000T), NASDAQ India Inds TR Index (NQIN2000T), NASDAQ India Tech TR Index (NQIN9000T), and the NASDAQ India TR Index (NQINT) to further divide time periods when it is above and below 50-, 100- and 200-day SMA. The question we are trying to answer is that are the factor loadings stable? If they are not, then how do they change over time and across different market regimes. To answer this, we setup a sliding window of 5-year daily returns that is incremented by one year at a time. That gives us 11 datasets, starting from 2002-2007 through to 2013-2017. We run PCA on the daily returns of the sector indices listed above. We then plot the loadings of the first principal component. A few things stand out: - Dominated by Basic Materials, Financials and Industrials. - Relative importance of IT has dropped. - Financials dominate the below-SMA200 market regime implying that most of the time, the market is below 200-SMA because of financials. What we had hoped to find was some sort of stability in the loadings either in the entire dataset or in specific SMA regimes. We could have then constructed a “good times” and “bad times” portfolio and switched between them based on SMA. But it looks like it is not possible with these indices. Code and more charts are on github.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

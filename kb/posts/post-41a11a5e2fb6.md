# Can Beta Dispersion be used for Market-Timing?

- Difficulty level: 4
- Published: Thu, 22 Jun 2017 06:03:39 +0000
- Source: [https://stockviz.biz/2017/06/22/can-beta-dispersion-used-market-timing/](https://stockviz.biz/2017/06/22/can-beta-dispersion-used-market-timing/)

## Summary

This document examines the research paper titled Beta Dispersion and Market-Timing, which suggests that tracking the beta dispersion among index constituents can predict market crashes and corrections. The author raises critical methodological questions regarding the strategy's robustness, including the impact of removing crisis periods like 2000 and 2008, applicability across different markets outside the US, sensitivity to varying look-back periods, and the effect of continuous versus monthly sampled calculations. Testing the theory on the NIFTY 100 index using available data does not yield the same conclusions as the original paper, highlighting the need for cautious interpretation and further empirical validation before deploying such quantitative market-timing strategies in practical portfolio management.

## Article

The paper Beta Dispersion and Market-Timing (SSRN) argues that one can predict crashes by tracking the dispersion of betas of the constituents of an index. The intuition presented in the paper is that when beta dispersion is high, any shock to the high beta stocks could spill over to the low beta stocks and create a broad market correction. Although the paper proceeds to present a back-test on the US S&P 500 index, there some questions that need to be answered before deploying this strategy: - What is the performance if you remove 2000 and 2008 from the data? Perhaps most of the out-performance can be attributed to skipping these two periods purely due to chance? - Are the results robust over different markets? Perhaps it is unique to the US? - What happens if you change the look-back period of beta calculations? Perhaps it is being data-mined? - What happens if the calculations are continuous rather than sampled at the end of the month? Perhaps its an end-of-the-month effect? Unfortunately, we don’t have a robust data-set to put this theory to test. However, the chart of the cumulative returns of the NIFTY 100 index vs. the beta-dispersion of its components does not lead to the same conclusion made in the paper. The code for this analysis is on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: github | https://github.com/stockviz/notebooks/blob/master/Beta%20Dispersion.ipynb]
You signed in with another tab or window. Reload to refresh your session.You signed out in another tab or window. Reload to refresh your session.You switched accounts on another tab or window. Reload to refresh your session.Dismiss alert

[Linked: Mutual fund portfolio overlap and Active Share | https://stockviz.biz/2017/06/19/mutual-fund-portfolio-overlap-active-share/]
There is a problem of plenty when it comes to mutual funds – direct growth schemes alone number into the high 200s. Investors have responded to this bewildering array of choices by going in for the ‘unlimited buffet’ option. They end up making small investments into a large number of funds. By doing so, they end up owning the whole market – paying active management fee for a passive investment. There are two things investors should keep in mind before adding a new fund to their investment: - What is the new fund’s portfolio overlap with the existing investments? - How different is the new fund’s portfolio from a large-cap and mid-cap index? The first answer will tell you whether to add the new fund to your portfolio. The second will tell you if you should just replace the fund with an index ETF. For example, say you own HDFC Mid Cap Opportunities and you are wondering if you should also buy the Birla Sun Life Midcap fund. Here’s how the fund portfolios overlap: The funds have about 18 stocks in common and a fairly large number of stocks that are not in any of the indices. Given the differing styles, perhaps it makes sense to add the new fund to the portfolio. The second, also called “Active Share,” shows how different the portfolio is from an index. For example, DSP Blackrock Technology.com Fund has a 26% overlap with NIFTY 100 and a 5% overlap with NIFTY MID100 FREE. Whereas, the HDFC Large cap Fund has a 95% overlap with the NIFTY 100 index. It probably makes sense to replace the latter with an index fund. For more details about the analysis and its results, please peruse the notebook on github.

[Linked: Should You Buy What Mutual Funds Buy? [Update] | https://stockviz.biz/2017/06/23/buy-mutual-funds-buy-update/]
Back in November-2015, we had concluded that it does not make sense following mutual fund entries and exits from individual stocks. Using an expanded data set of 260 funds, we still reach the same conclusion. Median returns over 10, 20 and 50 days on additions were 0.49%, 1.81% and 4.88% whereas exits clocked -0.01%, 0.90% and 3.27%. Direct equity investors would do well to ignore what fund managers a doing. Code and results are on github.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

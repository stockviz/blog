# Quant Model in Mutual Fund Wrapper

- Difficulty level: 4
- Published: Tue, 04 Jun 2019 09:59:14 +0000
- Source: [https://stockviz.biz/2019/06/04/quant-model-in-mutual-fund-wrapper/](https://stockviz.biz/2019/06/04/quant-model-in-mutual-fund-wrapper/)

## Summary

This document provides an advanced quantitative analysis of the DSP Quant Fund using backtest data. It evaluates cumulative performance against broad-market cap indices and strategy indices, identifying key performance drivers such as market beta and the quality factor, while noting that the value factor acts as a drag on returns. The analysis compares the fund's expense ratio and liquidity with alternative options like the SBI Quality ETF. It highlights the tax efficiency and low-cost nature of mutual fund wrappers for quantitative models compared to direct-equity platforms, offering specialist insights for passive investors considering factor-based strategies.

## Article

Most quant/smart-beta model based portfolios in India are built on direct-equity platforms – PMS, RIA, Themes and DIY. Their first major drawback is the 15% capital gains tax that needs to be paid the piper every year. The second one is the ability to track the “all-in” cost of maintaining the portfolio. This is where mutual funds have an advantage. Their pass-through status means that they don’t have to pay capital gains tax on portfolio sales and the end-of-day NAV gives investors the fully baked-in value of their portfolio. That said, mutual funds that wrap quantitative models have been few and far between. A new one has entered the fray: the DSP Quant Fund. They were gracious enough to share their backtest. What follows is a 30,000 foot analysis. Cumulative performance looks vs. a broad-market cap index looks good However, excess returns seem to be tapering off… Value factor seems to be a drag If you regress the Quant Fund against the market-cap index and NIFTY strategy indices representing quality and value, you can see that returns have been primarily driven by the market (beta) and quality. Value seems to contribute negatively to overall returns. Part of the diminishing excess returns could be explained by the increasing influence of market beta to the fund’s returns. Why not just buy the NIFTY 200 Quality 30 Index Fund/ETF? The SBI Quality ETF that tracks the NIFTY 200 Quality 30 Index has an expense ratio of 50bps. So while comparing the index against the Quant Fund, we need to haircut the index performance by that amount. Also, the Quant Fund comes out at 40bps for direct investors. The former is an ETF with minimal liquidity whereas the latter is an open-ended fund that can be redeemed at NAV – matters when you want to exit. Qualitatively speaking… DSP’s Quant Fund is a low-cost alternative to investors who want something more than market beta but not a full-fledged actively managed fund. It is tax efficient compared to other direct-equity platform solutions that over-weight the quality factor. And it is of comparable cost to most other quant/smart-beta funds/etfs for direct investors. Passive investors should definitely give it a strong look. Code and charts are on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: github | https://github.com/stockviz/blog/tree/master/DSP%20Quant%20Fund]
Blog post: Quant Model in Mutual Fund Wrapper Analyzed the DSP Quant Fund's backtest and found excess returns are tapering off, with market beta dominating and value factor acting as a drag. Concluded it's a low-cost, tax-efficient alternative to direct-equity quant platforms, comparable to the NIFTY 200 Quality 30 index after expense ratios. analyzes quantitative mutual fund performance, factor exposures, and strategy drift - NIFTY 100 TR.36.rolling-monthly.return-diff.png - NIFTY 100 TR.60.rolling-monthly.return-diff.png - NIFTY200 QUALITY 30 TR.36.rolling-monthly.return-diff.png - NIFTY200 QUALITY 30 TR.60.rolling-monthly.return-diff.png - cumulative.inception.NIFTY 100 TR.png - cumulative.inception.NIFTY200 QUALITY 30 TR.png - cumulative.pb-cutoff.5.png - index.PB.png - linear-fit.36.png - linear-fit.60.png

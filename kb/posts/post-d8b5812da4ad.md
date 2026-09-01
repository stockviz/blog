# Factor Holding Periods for Excess Returns

- Difficulty level: 4
- Published: Wed, 08 May 2019 12:27:42 +0000
- Source: [https://stockviz.biz/2019/05/08/factor-holding-periods-for-excess-returns/](https://stockviz.biz/2019/05/08/factor-holding-periods-for-excess-returns/)

## Summary

The article analyzes the minimum holding periods required for various equity factor and strategy indices on the NSE, such as low-volatility, quality, momentum, value, and alpha, to consistently yield positive excess returns over the NIFTY 50 TR index. While these factor strategies generally outperform the benchmark index since inception, their excess returns are unevenly distributed over time. The analysis reveals that low-volatility and quality factors have the shortest required holding period of at least five years, whereas alpha and value indices demand roughly ten years of patience. The piece also highlights structural challenges for retail investors implementing DIY strategies, including transaction costs like the securities transaction tax, capital-gains tax, and the current lack of low-cost, liquid ETFs and index funds tracking these specific factors, emphasizing that statistical equity edges require long-term commitment.

## Article

The NSE has different “strategy” indices that reflect some well known equity factors like low-volatility, quality, momentum and value. They are all shown to out-perform the NIFTY 50 TR index since inception: However, the excess returns of these indices, like everything else in equities, is unevenly distributed. As an investor, it could get frustrating to watch their “quality” factor investment under-perform the plain-old NIFTY 50 over many months. So broadly, for a given factor/strategy, what should the minimum holding period be for an investor to see only a positive excess return? Factors take time to work. The longer the holding period, the less frustrating the experience. Low-volatility and Quality have the shortest holding periods of 5 years. The Alpha and Value indices require about 10 years for investors to see only positive excess returns. Also, given the lack of liquid, low-cost ETFs and index funds that track these factor indices, investors have to also contend with STT and capital-gains tax if they go the DIY route. The edge that statistical factors have over market-cap based indices are measured over decades and require investors to be patient. Charts and code on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: github | https://github.com/stockviz/blog/tree/master/factor%20holding%20periods]
Blog post: Factor Holding Periods for Excess Returns Analyzed minimum holding periods required for NSE factor strategy indices (low-volatility, quality, momentum, value, alpha) to deliver only positive excess returns. Found low-volatility and quality need at least 5 years, while alpha and value require roughly 10 years — these factor premiums demand patience. analyzes factor returns (size, value, momentum, quality, low-vol) and their holding period requirements - factor-index.cumulative.png - negative.holding-period.vs.NIFTY 50 TR.png

[Linked: Statistics don’t lie. Narratives do. | https://stockviz.biz/2019/05/07/statistics-dont-lie-narratives-do/]
First, a headline: As U.S. fertility rates collapse, finger-pointing and blame follow (WaPo) Fact-check: it is true! Chart from World Bank: Looks alarming! The government should respond! Mandatory paid maternity and paternity leave! Write-off education loans so that youngsters can afford to start families! But… could the aggregate reduction in fertility rate be explained by lower teen-pregnancy rate? And lower infant mortality rates? Corrected narrative: women are having lesser kids because they expect all of their kids to make it to adulthood. And are having them later in life. Don’t panic. Before subscribing to a narrative about a statistic, it is important to first figure out why the statistic was created in the first place. The raw fertility rate statistic was probably created to figure out how many midwifes to train/employ if the trend held up. While the second one was setup to measure the efficacy of sex education in schools and the last one to measure the effectiveness of primary healthcare. It is only when we go beyond the narrative and seek data that falsifies that narrative that we get the full picture. This is the fundamental difference between hypothesis testing and data-mining.

[Linked: SMA Strategy Transaction Cost Analysis | https://stockviz.biz/2019/05/10/sma-strategy-transaction-cost-analysis/]
In our previous blog post on using SMAs to trade ETFs (SMA Strategies using ETFs,) we saw how using SMAs reduced drawdowns and boosted returns. We also saw how our Tactical Midcap 100 Theme out-performed mid-cap mutual funds even after taking into account STT and brokerage costs. Given the increased interest in our newly launched Tactical Midcap 150 Theme, we added transaction cost analysis to our backtests to give investors an idea of what gross and net returns of different SMA look-backs look like over buy and hold. Annualized Returns Take-away 1) SMA strategies on the NIFTY 50 index do not produce excess returns over buy-and-hold. However, the 200-day SMA did keep an investor out of the worst of the 2008 drawdown at a reasonable cost. 2) For other indices, perhaps counter-intuitively, 20-day SMA beat 10-day SMA both in Gross and Net returns. 3) SMA strategies will under-perform buy-and-hold when markets are generally trending up. However, they will out-perform when markets turn negative. The RETFMID150 ETF tracking the NIFTY MIDCAP 150 index, continues to be well traded on the NSE. You can access the SMA(20) strategy shown above through our Tactical Midcap 150 Theme. Code and additional charts on github.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

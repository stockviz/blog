# Level 5

Specialist research and implementation: highly technical studies, complex models, code, and research design details.

## VIX and Equity Index Returns, Part III

The article explores using the VIX to time equity index positioning, specifically proposing a regime-tilted strategy where equity weight is inversely proportional to VIX during high-but-falling volatility regimes. It details long-only and long-short backtesting outlines, emphasizing the need to test regime signals against simple inverse-volatility weighting. The piece outlines rigorous statistical methodologies, including block bootstraps and Newey-West adjustments to address serial correlation, and stresses parameter sensitivity sweeps and out-of-sample testing to prevent overfitting. It also introduces a fast-reacting overlay to mitigate the latency of slow, six-month regime signals during sudden market shocks. Ultimately, the article serves as a comprehensive research and implementation blueprint for quantitatively evaluating VIX-based equity timing strategies using AI-assisted coding in R.

[Read the knowledge note](posts/post-a404feaa3756.md) · [Original](https://stockviz.biz/2026/07/13/vix-and-equity-index-returns-part-iii/) [2026]

## Reinforcement Learning for Finance

This specialist research program tests various reinforcement learning agents on Indian stocks and futures data to determine if they can outperform classical momentum and trend baselines net of trading costs. Across multiple rewards, states, actions, algorithms, universes, and venues, no RL agent successfully beat the classical baselines out-of-sample. The program identifies key market structures, such as the negative carry of intraday NIFTY trading compared to overnight gains, and categorizes RL failure modes like drift-betting, churn, and flat-optimality. The research concludes that reinforcement learning fails to provide a profitable edge over simpler systems, with its only notable reproducible contribution being drawdown avoidance.

[Read the knowledge note](posts/post-70edf6252231.md) · [Original](https://stockviz.biz/2026/08/31/reinforcement-learning-for-finance/) [2026]

## Skewness Enhanced Momentum

The article explores how cross-sectional stock anomalies are driven by extreme positive returns, proposing that explicitly selecting high expected skewness for long positions and low for shorts improves anomaly performance. It details a methodology using monthly cross-sectional regressions to forecast skewness from observable characteristics like volatility and momentum, followed by a double-sort portfolio construction. The piece also presents an NSE backtest combining this skewness overlay with liquidity improvements, showing significant performance gains pre-COVID but attenuation post-2020. It highlights that while skewness enhances risk-adjusted returns and reduces drawdowns in tough markets, its predictive power and portfolio persistence degrade in bull regimes, and turnover remains high.

[Read the knowledge note](posts/post-81c02259a26a.md) · [Original](https://stockviz.biz/2026/08/08/skewness-enhanced-momentum/) [2026]

## Global Equities Momentum, Part III

This specialist research article extends the Global Equities Momentum (GEM) framework by substituting traditional market-capitalization-weighted indices with momentum equivalents in the trading phase. Utilizing the S&P 500 index exclusively for the decision tree mechanism while executing trades via momentum exchange-traded funds such as MTUM and IMTM, the strategy achieves enhanced returns and reduced drawdowns compared to standard buy-and-hold approaches and value-based alternatives. The methodology is supported by correlation analysis of index pairs, detailed backtests across multiple scenarios, practical ETF implementation instruments, and replication code made available via GitHub.

[Read the knowledge note](posts/post-5300d3ec8336.md) · [Original](https://stockviz.biz/2019/01/26/global-equities-momentum-part-iii/) [2019]

## Bold Asset Allocation

This article evaluates the Bold Asset Allocation (BAA) strategy, a hybrid model combining elements of PAA, VAA, and DAA. It details BAA's mechanics, including its use of slow SMA relative momentum for asset selection, a fast 13612W absolute momentum filter applied to a separate four-asset canary universe, and an enlarged defensive sleeve featuring commodities and inflation-linked bonds. The analysis highlights critical implementation flaws: excessively high turnover (472–523% annually), a binary B=1 breadth trigger that increases defensive periods, and the introduction of a non-standard Keller Ratio metric. Furthermore, it provides a rigorous backtest plan using real ETFs to address prior proxy data risks, in-sample overfitting, and flat cost assumptions, offering pseudocode for data validation and strategy replication.

[Read the knowledge note](posts/post-f7a22a80b840.md) · [Original](https://stockviz.biz/2026/07/12/bold-asset-allocation/) [2026]

## Practical Momentum – Conclusion

This document concludes a series on executing practical momentum strategies using derivatives and equities within unique market constraints. It recaps key findings: a one-year lookback period is optimal; survivorship bias causes long-short portfolios to underperform long-only counterparts; hedging with single-name put options fails due to option value decay and spreads; and larger long-only portfolios successfully reduce drawdowns while improving overall performance. The article synthesizes that momentum is most effectively executed through a broad basket of stocks rather than mechanical derivative portfolios, marking the culmination of specialist quantitative research and strategy implementation in the Indian market.

[Read the knowledge note](posts/post-4c53961af893.md) · [Original](https://stockviz.biz/2015/05/21/practical-momentum-conclusion/) [2015]

## Turbulence in Financial Markets

This document investigates the application of physical turbulence concepts to financial markets, analyzing a market-turbulence strategy derived from research by Zheng and Dong. The exploratory implementation tests models on NIFTY futures, synthetic SELECT futures series, and midcap and smallcap indices. The framework evaluates various interpretations including close-based reversal, breakout systems, and trend-filtered variants using quadratic Bézier curves and turbulence thresholds. Research findings indicate that while the approach can work in specific market regimes, it tends to fail in trending markets and suffers from overfitted parameters and high dependency on numerous specified variables. The code, charts, and parameter sweeps are maintained on GitHub as an ongoing research framework.

[Read the knowledge note](posts/post-38382da5900a.md) · [Original](https://stockviz.biz/2026/09/01/turbulence-in-financial-markets/) [2026]

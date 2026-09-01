# Level 4

Advanced analysis: research requiring comfort with quantitative methods, portfolio mechanics, and statistical evidence.

## Central Bank Musings

This document explores the complex and often conflicting relationship between central banks and government fiscal authorities through the lens of economic theory, policy statements, and game mechanics. It analyzes how monetary policy limitations impact financial stability, referencing insights from Federal Reserve Chair Janet Yellen and Bank of England chief economist Andy Haldane regarding market volatility and macro-prudential regulation. Additionally, it applies a game-theoretic model derived from Alan S. Binder’s 1982 paper to illustrate the strategic interactions between a central bank governor, such as Raghuram Rajan, and politicians. The text details preference matrices, the challenges of coordination, and the realization of Nash Equilibrium where neither monetary nor fiscal authorities achieve their ideal outcomes but maintain a stable, albeit suboptimal, posture.

[Read the knowledge note](posts/post-a9de5bbd891d.md) · [Original](https://stockviz.biz/2014/07/04/central-bank-musings/) [2014]

## Mangling RSI through a Support Vector Machine

This article explores the application of a Support Vector Machine to the Relative Strength Index and the Simple Moving Average of the Nifty index to analyze market conditions. Conventional wisdom treats RSI values over 70 as overbought and under 30 as oversold, but naive implementations of these thresholds have historically underperformed simple buy-and-hold strategies and proved to be money losers. By applying an SVM model using a fourteen-day RSI and a fifty-day SMA, the analysis evaluates the bifurcation between long and short positions and questions whether trend factors overshadow RSI entirely, ultimately raising doubts about the relevance of RSI in quantitative trading strategies.

[Read the knowledge note](posts/post-ba3aec98c365.md) · [Original](https://stockviz.biz/2014/11/07/mangling-rsi-support-vector-machine/) [2014]

## Machine Learning Long-Short Trend Following

This document explores advanced quantitative finance techniques by applying Support Vector Machines to financial market trend following. It investigates how a machine learning model trained on volatility metrics and percentage distance from the 50-day simple moving average can predict daily long and short positions for indices such as NIFTY and BANKNIFTY. The article evaluates the strategy performance against traditional buy-and-hold approaches across multiple timeframes, analyzing cumulative returns, transaction costs, and taxation impact. By comparing machine learning models with baseline tactical strategies, the research highlights significant outperformance on the BankNifty index, suggesting that complex predictive algorithms can successfully capture market alpha while warranting further exploration and real-time implementation.

[Read the knowledge note](posts/post-882ecdfefa36.md) · [Original](https://stockviz.biz/2015/05/08/machine-learning-long-short-trend-following/) [2015]

## Intra-Stock Correlation and Momentum Returns

This article investigates an advanced momentum strategy that uses intra-market correlation signals (20-day vs. 200-day average correlations) to switch between long and cash positions, aiming to reduce drawdowns. It compares this correlation-based overlay with a 50-day SMA overlay and combinations thereof, using backtests on Indian equities. The analysis includes transaction cost sensitivity (25bps) and post-COVID performance. Results show modest outperformance for the combined signal but no drawdown improvement. The article assumes familiarity with momentum strategies, technical indicators like SMA, and backtesting concepts, making it suitable for readers with intermediate to advanced knowledge in quantitative finance.

[Read the knowledge note](posts/post-f902ef9b31d7.md) · [Original](https://stockviz.biz/2024/11/08/intra-stock-correlation-and-momentum-returns/) [2024]

## Volatility and VIX Charts

This document explores the analysis of market volatility and VIX charts, focusing on historical and implied volatility profiles across major global indices including the US S&P 500, Japanese Nikkei 225, and Indian NIFTY 50. It emphasizes that different indices exhibit unique volatility characteristics depending on the look-back period and measurement parameters, cautioning traders against directly transplanting strategies from one market to another. The text highlights resources and code available on GitHub for generating volatility density plots, historical volatility charts, and examining relationships between VIX indices across different global markets to better understand market risk and return profiles.

[Read the knowledge note](posts/post-4a7678bd3a89.md) · [Original](https://stockviz.biz/2018/11/09/volatility-and-vix-charts/) [2018]

## NIFTY Volatility, Historical Perspective

This document provides an advanced historical perspective on NIFTY volatility by analyzing density plots across multiple time periods, including 10, 20, 30, and 50 days. It examines the anomalous low volatility observed in 2014 compared to historical distributions from 2004 and 2011, addressing potential data skews introduced by the pre-open auction call in 2010. The analysis compares current volatility trends against long-term means and anticipates a reversion to higher volatility as initial bull-runs wear off, which impacts various investment and trading strategies. Additionally, the article discusses methodological challenges when back-testing with opening prices across the 2010 structural shift and suggests using tick-level synthetic opening prices for consistency.

[Read the knowledge note](posts/post-0c4d1137e43c.md) · [Original](https://stockviz.biz/2015/10/09/nifty-volatility-historical-perspective/) [2015]

## SMA Strategy Transaction Cost Analysis

This document provides an advanced quantitative analysis of transaction costs, brokerage fees, and securities transaction taxes applied to simple moving average (SMA) trading strategies utilizing exchange-traded funds (ETFs) on various NIFTY indices. It evaluates backtest performance metrics, comparing gross versus net returns across different SMA look-back periods such as 10-day, 20-day, and 200-day variants. The analysis highlights practical challenges in strategy implementation, including liquidity constraints, ETF premium and discount to NAV, and capital scaling limitations. Furthermore, it contrasts buy-and-hold methodologies with tactical asset allocation models to assess drawdowns, downside protection during negative market phases, and performance decay caused by high turnover in shorter look-back windows.

[Read the knowledge note](posts/post-8b6cf4ef5686.md) · [Original](https://stockviz.biz/2019/05/10/sma-strategy-transaction-cost-analysis/) [2019]

## The answer is that there is no answer

This document explores the complex, dual-natured realities of investing, highlighting how strategies like value, growth, momentum, and quality do not work simultaneously due to market regime shifts. It compiles perspectives from Howard Marks, Cam Hui, and Herb Greenberg, emphasizing that superior investing involves navigating trade-offs such as risk versus reward, concentration versus diversification, and leverage. Furthermore, it discusses quantitative modeling challenges, the difficulty of finding a model for all seasons, and how falling barriers to entry have changed quantitative analysis. The text also touches upon technical analysis limitations and market performance indicators, providing a comprehensive advanced analysis of market behaviors and investment management pitfalls.

[Read the knowledge note](posts/post-15daee1f75d2.md) · [Original](https://stockviz.biz/2014/04/11/answer-answer/) [2014]

## Practical Momentum, Part I

This document analyzes practical momentum strategies in the Indian equity market, focusing on long-only and long-short variations constrained by derivative market rules and short-selling limitations. The author investigates a universe of ninety-seven stocks that have continuously remained in the Futures and Options segment since 2004 to examine potential survivorship bias. Back-testing results demonstrate that a long-only momentum strategy utilizing a one-year look-back period significantly outperforms both the Nifty benchmark and a corresponding long-short strategy. The short portfolio consistently acted as a performance drag and increased portfolio volatility, suggesting that survivorship bias plays a substantial role in the success of the long-only approach within this specific filtered stock universe. Further investigations are proposed to enhance the efficiency of the short-side implementation.

[Read the knowledge note](posts/post-d29a8794ec5a.md) · [Original](https://stockviz.biz/2015/05/15/practical-momentum-part-i/) [2015]

## Macro: Using Currencies to Predict NIFTY, Part I

This document explores advanced quantitative finance techniques by applying a Support Vector Machine with polynomial kernels to currency index returns in order to predict subsequent weekly movements of the NIFTY 50 index. It details the methodology of dividing historical datasets from 2000 to 2018 into training, validation, and test subsets to evaluate long-only and long-short portfolio strategies. The analysis discusses performance outcomes, long biases, and limitations during specific market dislocations, such as the 2018 correction when the relationship between the U.S. dollar index and the NIFTY broke down. Furthermore, it outlines potential improvements involving hyperparameter tuning of kernel degrees and examining alternative dollar indices and exchange rates to refine predictive accuracy.

[Read the knowledge note](posts/post-20182997886e.md) · [Original](https://stockviz.biz/2018/11/16/macro-using-currencies-to-predict-nifty-part-i/) [2018]

## 93 Technical Indicators that don’t Predict Market Returns

This research paper examines 93 common technical market indicators to determine their predictive power regarding S&P 500 returns. The findings indicate that these technical indicators lack predictive capability and fail to generate profits that outperform a standard buy and hold strategy. The conclusions remain consistent even when accounting for regime-switching predictability tied to business or sentiment cycles, or when utilizing GARCH (1,1) and robust regression methods. This analysis contributes to ongoing academic and practical discussions regarding the overall usefulness of technical analysis in financial markets, suggesting that widely used indicators do not reliably forecast future market returns.

[Read the knowledge note](posts/post-593b9986f81f.md) · [Original](https://stockviz.biz/2014/07/18/93-technical-indicators-dont-predict-market-returns/) [2014]

## Is Skewness a Timing Signal?

This document investigates whether the skewness of one-year daily returns functions as a reliable market timing signal for the value factor. The author attempts to replicate and extend an existing study using simplified parameters, applying a 220-day lookback period, the SPY ETF with dividend-adjusted prices, and the IVE ETF representing the S&P 500 Value Index instead of a long-short academic portfolio. The findings reveal the exact opposite of the original premise: going long during periods of positive skewness underperforms compared to periods of negative skewness, and buy-and-hold strategies vastly outperform the timing portfolios. Furthermore, these out-performance results fail to replicate across other international indices such as NIFTY 50 and NIFTY MIDCAP 100.

[Read the knowledge note](posts/post-492ade1c5c97.md) · [Original](https://stockviz.biz/2018/10/19/is-skewness-a-timing-signal/) [2018]

## Smart Beta Strategy Return Analysis

This document analyzes the return performance, daily returns, drawdowns, and risk-adjusted metrics of smart-beta investment strategies using StockViz themes. It contrasts two specific strategies: Market Fliers, which tracks a high-volatility and high-beta portfolio, and Market Elephants, which follows a low-beta portfolio, alongside the Nifty index. The analysis evaluates metrics such as peak-to-trough drawdowns, Calmar ratios, and Sterling ratios to determine how these smart-beta themes perform relative to the broader market in terms of absolute returns and risk.

[Read the knowledge note](posts/post-d78f8e421654.md) · [Original](https://stockviz.biz/2014/05/23/smart-beta-strategy-return-analysis/) [2014]

## Global Equities Momentum, Part II

This document extends the analysis of Global Equities Momentum (GEM) models by exploring the substitution of traditional market-capitalization-based indices with value and momentum indices within the decision tree. It sets up backtests using the S&P 500 index for the primary asset allocation decision between equities and bonds, and subsequently applies MSCI USA and international prime value indices to examine whether lower-turnover value strategies mitigate the noise associated with momentum turnover. The findings indicate that while value-based GEM models demonstrate superior returns and shallower drawdowns compared to buy-and-hold benchmarks, they lack equivalent investable ETFs for practical implementation. Conversely, momentum-based GEM strategies significantly outperform value alternatives and can be readily executed using MTUM and IMTM exchange-traded funds. The analysis concludes with critical considerations regarding potential data mining and statistical validity due to limited historical index data.

[Read the knowledge note](posts/post-915740ab8b21.md) · [Original](https://stockviz.biz/2019/01/25/global-equities-momentum-part-ii/) [2019]

## Strategy 9 with Crypto

This article extends Rob Carver's trend-following strategy to a broader cryptocurrency universe, testing the hypothesis that expanding beyond the top three coins improves performance. It applies multiple weighting schemes (equal-weight, inverse-volatility, long-only, long-short) and evaluates drawdowns and contributions. The analysis reveals that while the original three coins drove returns, the expanded set underperformed, with many coins contributing negatively. The author highlights the risk of overfitting through instrument selection and emphasizes the importance of robust backtesting. The content assumes familiarity with trend-following, portfolio construction, and performance metrics, making it suitable for readers with advanced knowledge in quantitative investing.

[Read the knowledge note](posts/post-f312d42a5dbf.md) · [Original](https://stockviz.biz/2026/06/26/strategy-9-with-crypto/) [2026]

## Covered Call Strategy Cheat Sheet

This document analyzes the mechanics and risk profile of covered call strategies based on research by AQR Capital Management. A covered call involves owning an underlying stock while selling a call option against it, which caps the stock upside and provides a premium. The article explains how changing deltas in varying market environments create a net exposure that embeds elements of a reversal strategy rather than a trend-following one. Specifically, in falling markets, market exposure increases, while in rising markets, it decreases. Consequently, the cheat sheet advises against using covered calls if an investor is bearish on volatility or the market, or if they prefer trend-following strategies, highlighting that a significant portion of strategy risk stems from market timing effects.

[Read the knowledge note](posts/post-dba0be9da7ab.md) · [Original](https://stockviz.biz/2014/06/27/covered-call-strategy-cheat-sheet/) [2014]

## The Worst Mutual Funds – Quantitative

This document provides a quantitative performance analysis of the ten worst-performing mutual funds between January 2010 and May 2015, using metrics such as Sharpe ratio, bear-beta, information ratio, draw-down depth, and draw-down length. The analysis highlights that infrastructure funds performed poorly during this period and notes that many mutual funds struggle, yielding low single-digit returns over five-year time frames despite being marketed as wealth builders. The text also contrasts these findings with a related review of the top ten best mutual funds, offering investors risk metrics to evaluate fund choices among thousands of available schemes.

[Read the knowledge note](posts/post-dc373907bb5e.md) · [Original](https://stockviz.biz/2015/05/29/the-worst-mutual-funds-quantitative/) [2015]

## USDINR and Dollar Indices, Part II

This document explores the relationship between the USDINR exchange rate and trade-weighted dollar indices such as DTWEXB, DTWEXM, and DTWEXO. Building upon linear regression concepts with a forced zero intercept from Part I, the article defines the spread between USDINR and each index as A minus beta times B. It discusses pair trading methodologies, specifically testing residual stability using the augmented Dickey-Fuller test to determine p-values. Through plotting spreads across different time-frames, the author identifies a fifty-day period as the most suitable time-frame, observing considerable mean-reversion characteristics that suggest viable quantitative trading models can be developed and subsequently back-tested in future parts of the series.

[Read the knowledge note](posts/post-7b9aa04dcad1.md) · [Original](https://stockviz.biz/2018/11/05/usdinr-and-dollar-indices-part-ii/) [2018]

## Residual Momentum

This article explores the concept of residual momentum as an alternative to conventional momentum strategies in equity markets. Traditional momentum strategies rank stocks based on relative or absolute returns, which can inadvertently cause high exposure to factors like market beta, value, or small-cap premiums, leading to potential momentum crashes. To mitigate this risk, researchers Blitz, Huij, and Martens propose ranking stocks using residuals derived from fitting return series to the Fama-French Three Factor model. The article introduces an automated strategy, Momo (Residual) v1.0, which implements this approach by constructing a portfolio of equally weighted stocks exhibiting high residual returns, aiming to outperform standard vanilla momentum strategies.

[Read the knowledge note](posts/post-381dfb331d80.md) · [Original](https://stockviz.biz/2017/04/10/residual-momentum/) [2017]

## MSCI Country Momentum Index Correlations

This article updates an analysis of country index correlations by examining the rolling correlations between various momentum equity indices and the MSCI India Momentum Index. Using three-year and five-year rolling periods, the research shows that momentum serves as a loose proxy for market sentiment, demonstrating high correlations across global equity markets such as Hong Kong and emerging markets generally. The median correlations over both periods exceed plus zero point seven zero between India Momentum and Emerging Markets Momentum. The findings reinforce the concept that global equity markets are deeply interconnected and sentiment heavily influences market movements, indicating that standard equity diversification offers limited protection against drawdowns.

[Read the knowledge note](posts/post-67f807a85f24.md) · [Original](https://stockviz.biz/2020/02/10/msci-country-momentum-index-correlations/) [2020]

## SMA Strategies, Part II

This document explores advanced quantitative finance techniques by building upon simple moving average (SMA) tactical strategies using exchange-traded funds. It introduces a slope-direction filter to check whether the SMA is trending upward before initiating long positions, comparing the Nth-day value to the N/2th-day value. While this modification results in lower gross returns compared to raw SMA strategies, it successfully achieves shallower drawdowns, particularly for the 10-day SMA variant. The article highlights how reduced drawdowns enable the use of leverage, presenting this methodology as a robust starting point for NIFTY futures trading strategies and further pointing toward cross-over strategies in subsequent parts.

[Read the knowledge note](posts/post-8275e903df65.md) · [Original](https://stockviz.biz/2019/02/11/sma-strategies-part-ii/) [2019]

## Projecting Future Returns

This document explores advanced methods for projecting future investment returns by utilizing historical index data from 1991 through 2018 alongside a Generalized Lambda Distribution. The analysis models 10,000 simulations over a 20-year horizon to evaluate both lumpsum and systematic investment plans for the NIFTY 50, NIFTY 50 Dollar, and S&P 500 indices. The study investigates the impact of currency depreciation, emerging market risk premiums, and path dependency on long-term investor outcomes, providing comparative scenarios to help investors understand the variances, risks, and potential returns associated with domestic versus international equity investments.

[Read the knowledge note](posts/post-cb2a38c4f4b3.md) · [Original](https://stockviz.biz/2019/01/14/projecting-future-returns/) [2019]

## Synthetic Indices

This article introduces synthetic indices as benchmarks for long-short portfolios, referencing Lo and MacKinlay's 1990 paper on contrarian profits. It explains how to construct these indices using momentum and mean-reversion weights, and presents backtested returns. Understanding requires familiarity with portfolio theory, long-short strategies, and index construction, placing it at an advanced analysis level.

[Read the knowledge note](posts/post-93fc926734d7.md) · [Original](https://stockviz.biz/2021/06/14/synthetic-indices/) [2021]

## The Dao of Collusive Trading

This document explores the potential implications of Decentralized Autonomous Organizations on securities market regulation, specifically regarding collusive trading practices such as synchronized and circular trading. It contrasts traditional corporate hierarchies with DAOs, which utilize code, smart contracts, and anonymous participant voting to perform tasks. The article explains how an anonymous DAO could execute stock manipulation autonomously, making it extremely difficult for surveillance systems and regulators like SEBI to prove collusion and intent because transactions would occur between unacquainted participants without a discernible paper trail.

[Read the knowledge note](posts/post-48c94921be81.md) · [Original](https://stockviz.biz/2018/05/14/dao-synchronized-trading/) [2018]

## Allocating a Four-Asset Portfolio

This document extends portfolio allocation strategies from a three-asset model to a four-asset model by introducing gold alongside MIDCAP, 0-5yr bonds, and NASDAQ-100. It evaluates the impact of adding gold, noting its historical low or negative correlation with other assets and its ability to benefit from rupee depreciation. The analysis incorporates a tax drag of 10%, a securities transaction tax of 0.1%, and a 20% rebalance threshold to examine cumulative returns and drawdowns. Key findings indicate that while a four-asset equal-weight portfolio reduces maximum drawdowns during major market downturns like the 2008 financial crisis, it underperforms compared to a three-asset equal-weight portfolio in terms of absolute returns. The text also discusses the trade-offs of rebalancing thresholds in managing transaction costs and taxes, and concludes with considerations on holding gold through mechanisms such as the Sovereign Gold Bond Scheme.

[Read the knowledge note](posts/post-830021c04210.md) · [Original](https://stockviz.biz/2018/10/15/allocating-a-four-asset-portfolio/) [2018]

## Interpreting Nifty Open Interest, Part II

This document continues an advanced technical analysis of Nifty open interest data by presenting updated charts that incorporate changes in open interest to better interpret market behavior and expiry values. Building directly upon foundational work aimed at attaching probability distributions to derivative expiry values, the text examines nearest-to-expiry contracts in action. It serves as a practical continuation for quantitative analysts and traders seeking to develop sophisticated frameworks for market interpretation. The provided charts and ongoing series require a strong prior understanding of derivatives markets, open interest dynamics, and advanced technical charting methods to properly comprehend the complex relationships between price action, positioning, and future market direction.

[Read the knowledge note](posts/post-9719a5217b3b.md) · [Original](https://stockviz.biz/2014/06/16/interpreting-nifty-open-interest-part-ii/) [2014]

## The Relative Value Trap

The article explores strategies to avoid the 'relative value trap' in factor investing, specifically focusing on the value factor. It examines three approaches: timing the value factor using market price-to-book valuation bands, dynamically adjusting large and mid-cap allocations based on relative valuations, and combining value with momentum filters and stop-losses. The author backtests these methods on Indian market data (NIFTY 50 and MIDCAP indices), showing that while some tactical timing and weighting schemes improve raw returns compared to buy-and-hold, the benefits are largely eroded by transaction costs, taxes, and increased portfolio churn. The analysis highlights the challenges of implementing single-factor timing strategies and concludes that practical implementation often negates theoretical alpha.

[Read the knowledge note](posts/post-9e82a114b287.md) · [Original](https://stockviz.biz/2020/10/19/the-relative-value-trap-2/) [2020]

## Changepoints vs. Buy & Hold

This article evaluates changepoint detection for market timing on Indian indices, comparing binary and direction-gated strategies against buy-and-hold and SMA. It requires understanding of statistical changepoint methods, backtesting frameworks, and risk-adjusted metrics. The analysis involves advanced concepts like regime classification, sliding/expanding windows, and position sizing, making it suitable for readers with strong quantitative finance background.

[Read the knowledge note](posts/post-7b6239d22046.md) · [Original](https://stockviz.biz/2026/07/27/changepoints-vs-buy-hold/) [2026]

## Global Equities Momentum, Part IV

This document explores advanced quantitative variations of the Global Equities Momentum strategy, focusing specifically on alternative formation periods ranging from 6 to 12 months. The analysis evaluates how different formation lengths impact peak drawdowns and overall returns compared to the traditional 12-month model. It addresses the risk of data-mining associated with picking a single optimal window by investigating the effects of averaging all formation periods together. The text highlights that averaging formation periods effectively reduces strategy drawdowns. A virtual portfolio is planned to demonstrate this averaging methodology, and accompanying backtest code, cumulative charts, and drawdown graphics are made accessible on GitHub for practitioner implementation and further research.

[Read the knowledge note](posts/post-c3d2e15bb456.md) · [Original](https://stockviz.biz/2019/01/28/global-equities-momentum-part-iv/) [2019]

## Bonds, Rates and USDINR Update

This article provides an advanced analysis of macroeconomic indicators in India, focusing on the zero-coupon yield curve, the spread between Indian and US 10-year bonds, and historical total return indices for long bonds since 2000 and 2010. It discusses the implications of the Modi euphoria, RBI's anticipated easing monetary policy, and bond market volatility. Additionally, it examines the USDINR currency dynamics following stabilization in inflation and NDA-II government policies aimed at economic growth, making it relevant for sophisticated fixed-income and currency market analysts.

[Read the knowledge note](posts/post-6db613ebd7dc.md) · [Original](https://stockviz.biz/2014/07/28/bonds-rates-usdinr-update/) [2014]

## Trending Momentum Models

This article applies trend overlays, such as simple moving averages, to existing homegrown momentum models including Momentum, Velocity, and Acceleration strategies originally built between 2013 and 2015. It compares these trend-timed variants against automated trailing stop-loss versions called Momo strategies, evaluating performance during sudden crashes versus grinding bear markets. The analysis finds that trend overlays reduce drawdowns effectively but may sacrifice returns, especially in Velocity, while removing crash data reveals stronger performance in Momentum and Acceleration. The piece also references earlier work on trend overlays applied to NIFTY momentum indices and discusses how transaction costs and taxes erode trend-following returns. Readers should be familiar with momentum investing, relative versus absolute momentum, trailing stop-losses, simple moving averages, drawdown analysis, backtesting, and portfolio rebalancing concepts.

[Read the knowledge note](posts/post-d16df427a72d.md) · [Original](https://stockviz.biz/2022/11/28/trending-momentum-models/) [2022]

## Strategy 9 – Conclusion

This article concludes a series on Rob Carver's Strategy 9, a composite trend-following strategy, tested across various asset universes from an Indian retail perspective. It synthesizes findings from multiple backtests, evaluating scaled vs. binary, long-only vs. long-short, and equal vs. inverse-volatility weighting. Key results: scaled long-only is the only viable variant, but the strategy's drawdowns prevent safe leverage, making it inferior to buy-and-hold on absolute returns. Expanding the universe yields diminishing returns, and performance may be period-dependent. The article requires understanding of trend-following, backtesting, Sharpe ratio, drawdown, leverage, and portfolio construction, as well as familiarity with Carver's methodology. It is an advanced analysis of strategy implementation and limitations, suitable for readers with prior knowledge of quantitative trading concepts.

[Read the knowledge note](posts/post-cf3fd9626282.md) · [Original](https://stockviz.biz/2026/06/29/strategy-9-conclusion/) [2026]

## Macro: NIFTY vs. INR/OIL Correlation, Part II

This document analyzes the macroeconomic correlation between the NIFTY 50 index, the USD/INR currency exchange rate, and crude oil prices using weekly return time-series data. It evaluates the construction of a simple linear model based on the weak negative correlation found between NIFTY 50 and USD/INR. The dataset is split into a training set from 2010 to 2015 and a test set from 2016 to 2018 to predict weekly index returns. The findings reveal that the resulting linear model is heavily bullish, consistently generating positive predictions that perform no better than a standard buy-and-hold strategy. The analysis highlights the limitations of applying linear modeling to weak macro-variable relationships and suggests that alternative non-linear approaches or higher frequency evaluations may be necessary to capture the complex dynamics between currency movements and equity returns.

[Read the knowledge note](posts/post-0cb7e3973e75.md) · [Original](https://stockviz.biz/2018/10/29/macro-nifty-vs-inr-oil-correlation-part-ii/) [2018]

## Hedging Momos

This article discusses advanced hedging strategies for high-turnover momentum portfolios (Momos). It compares a naive full hedge using rolling beta against a smarter hedge that minimizes basis risk, showing the latter improves returns by 3-4% and significantly reduces drawdowns, albeit with added costs and complexity. The analysis requires understanding of momentum strategies, beta, basis risk, and hedging mechanics, as well as familiarity with backtesting and risk metrics. It builds on prior discussions of trend overlays and momentum models, indicating a need for solid grounding in quantitative finance and portfolio management.

[Read the knowledge note](posts/post-f9fc6709d18a.md) · [Original](https://stockviz.biz/2023/06/03/hedging-momos/) [2023]

## Liquidity Improvements and Momentum

This article applies the liquidity improvement factor (LIQIM) from a recent SSRN paper to Indian equities, testing whether liquidity changes can enhance momentum strategies. It explains the Amihud illiquidity measure and constructs LIQC (liquidity change) quintiles, finding that stocks with improving liquidity (Q1) outperform, while those with deteriorating liquidity (Q5) underperform. However, a standalone long-only Q1 portfolio underperforms the benchmark. The key result is that excluding Q5 stocks from a momentum portfolio improves returns by about 50 bps annually, with a 1-month lookback being most effective. The analysis includes backtests, performance metrics, and code on GitHub, requiring understanding of factor construction, portfolio sorting, and performance evaluation.

[Read the knowledge note](posts/post-901763ccf84e.md) · [Original](https://stockviz.biz/2026/08/08/liquidity-improvements-and-momentum/) [2026]

## RSI through a Support Vector Machine, Part Deux

This document explores the performance of a Support Vector Machine trained on a 14-day RSI and a 50-day SMA of the Nifty index to establish potential trading rules. Analyzing predictions from 2006, the article highlights that the machine learning model relied more heavily on market trend than on RSI levels, often issuing long signals during overbought conditions and short signals during oversold periods. By running yearly training data through the SVM, the author demonstrates that the resulting contours lack year-to-year stability, indicating that a trading strategy based purely on RSI proves to be random and ineffective for consistent predictive modeling.

[Read the knowledge note](posts/post-d92b6b4e2384.md) · [Original](https://stockviz.biz/2014/11/08/rsi-support-vector-machine-part-deux/) [2014]

## Short Butterflies with a Delayed Fuse

This article discusses an advanced strategy modification for trading short-call NIFTY butterflies by introducing a delayed entry mechanism. The previous analysis showed that standard mechanical expiry-to-expiry butterfly strategies with a stop-loss suffered from a high frequency of triggered stop-losses during the first half of the trade while the position remained idle. To mitigate the risk of the underlying NIFTY index moving out of the wings and returning to the center before expiration, the article proposes delaying the entry until 10 days after expiry. However, a mechanical backtest of this delayed-entry strategy concluded that it still does not form a viable approach that can be reliably applied on an ongoing basis for trading the volatile index.

[Read the knowledge note](posts/post-0793de29e95d.md) · [Original](https://stockviz.biz/2015/07/11/short-butterflies-with-a-delayed-fuse/) [2015]

## Interpreting Nifty Open Interest

The article discusses the initial steps in developing a quantitative framework to interpret open interest in order to calculate a probability distribution for Nifty expiry values. The author outlines the process of charting open interest for the nearest-to-expiry contract to analyze market behavior and mentions upcoming charts for Friday Nifty open interest data. The content is analytical and geared toward advanced derivatives research and modeling, requiring familiarity with options markets, open interest dynamics, and quantitative finance concepts to fully comprehend the proposed methodology and its implementation.

[Read the knowledge note](posts/post-38d13ef3d725.md) · [Original](https://stockviz.biz/2014/06/14/interpreting-nifty-open-interest/) [2014]

## Transaction Cost Analysis of a Momentum Strategy

This document provides a transaction cost analysis of a momentum trading strategy, addressing common criticisms regarding the difference between gross returns and post-cost mutual fund returns. Because the author offers varying brokerage slabs to different clients, a single cost cannot be uniformly applied. Instead, the analysis demonstrates the impact of different brokerage structures by showing that a gross return of 83.82 percent translates into net returns of 74.20 percent, 69.58 percent, and 65.08 percent under specific brokerage slabs of 0.1 percent, 0.05 percent, and 0 percent respectively, assuming a securities transaction tax of 0.1 percent. The findings illustrate that momentum strategies continue to outperform even after accounting for these transaction costs.

[Read the knowledge note](posts/post-07ffd8d870d0.md) · [Original](https://stockviz.biz/2017/12/16/transaction-cost-analysis-momentum-strategy/) [2017]

## Practical Momentum, Part II – Volatility Adjustment

This document explores the practical implementation of momentum strategies in equity markets, specifically evaluating the effects of adding a volatility adjustment to smooth out draw-downs in long-only and long-short portfolios. The author reviews previous back-tests comparing look-back periods and portfolio types within Indian market constraints, such as F&O segment limitations and survivorship bias. By introducing volatility metrics, the analysis observes performance improvements in long-only momentum over long-short strategies, though steep draw-downs during specific years like 2008 and 2013 remain challenging for leverage and derivatives. The article concludes by setting up future explorations into hedged long-only momentum strategies to manage risk and mitigate severe portfolio declines.

[Read the knowledge note](posts/post-e313a203b8a1.md) · [Original](https://stockviz.biz/2015/05/16/practical-momentum-part-ii-volatility-adjustment/) [2015]

## Intramonth Momentum

This article discusses the intramonth momentum cycle, a phenomenon where most of the equity momentum premium is concentrated in a six-day window before month-end, driven by institutional cash needs and 'dispensability' selling of losers. It presents evidence from a paper by Nathan, Suominen, and Tasa, including the mechanism, causal evidence from settlement changes, and international replication. The blog then applies this to Indian equity futures, detailing a backtest that shorts worst-momentum stocks during the pre-month-end window. The results are negative, highlighting practical constraints like borrow costs and shorting restrictions. Understanding this requires familiarity with momentum strategies, portfolio construction, regression analysis, and market microstructure, placing it at an advanced applied level.

[Read the knowledge note](posts/post-88bcd9d3f2c1.md) · [Original](https://stockviz.biz/2026/07/18/intramonth-momentum/) [2026]

## Lumpsum vs. Dollar Cost Averaging (SIP)

This article explores the comparative performance and probability distributions of lumpsum investments versus Dollar Cost Averaging (DCA), commonly known as Systematic Investment Plans (SIP) or Systematic Transfer Plans (STP). Using simulation techniques and historical data for NIFTY, MIDCAP, and GOLD modeled through a Generalized Lambda Distribution, the text analyzes 10,000 paths to understand return trajectories. The findings demonstrate that while lumpsum investments typically yield higher average returns and feature fat right tails in upward-trending markets, they also present a longer left tail indicating a higher probability of severe negative outcomes. Conversely, DCA methods generally reduce large loss probabilities for prudent investors, despite higher total frequencies of minor negative returns. The document concludes with strategic recommendations for risk-seekers versus prudent investors receiving windfalls.

[Read the knowledge note](posts/post-96aaae58a819.md) · [Original](https://stockviz.biz/2018/06/23/lumpsum-vs-dollar-cost-averaging-sip/) [2018]

## Probabilistic Sharpe Ratio

The article addresses the inherent instability of traditional mutual fund performance metrics like alpha, beta, and the information ratio, emphasizing their retrospective nature. It critiques the standard Sharpe Ratio for relying on the flawed assumption of normally distributed returns. To overcome this, the piece introduces Marcos López de Prado’s Probabilistic Sharpe Ratio (PSR), which adjusts the traditional metric by accounting for skewness and kurtosis. The text explains the mathematical intuition behind PSR, noting that it increases with lower standard deviation of the Sharpe Ratio and positive skewness, while decreasing with fatter tails. A practical comparison between two large-cap mutual funds and a basic strategy demonstrates how PSR provides a confidence level when historical Sharpe Ratios are similar. Ultimately, the article positions PSR as a crucial tool for differentiating between equally attractive investment strategies, requiring readers to possess foundational knowledge of statistical distribution and standard performance metrics to fully grasp its application.

[Read the knowledge note](posts/post-584f58d6c5ec.md) · [Original](https://stockviz.biz/2020/05/23/probabilistic-sharpe-ratio/) [2020]

## Single Stock Momentum

This article adapts the Ammann, Moellenbeck, and Schmid single-stock momentum strategy for the Indian equity market. It details a backtesting methodology that buys the top-performing large-cap stock over a six-month formation period and shorts the NIFTY index, holding positions for three months using overlapping portfolio strands to manage turnover. The analysis highlights that raw returns are insufficient; beta hedging is required because momentum stocks are high-beta, and the Omega ratio outperforms the Sharpe ratio by penalizing only downside volatility relative to the benchmark. By extending the holding period and diversifying across three stocks instead of one, the strategy achieves higher risk-adjusted returns. The piece serves as an advanced analytical guide, bridging academic quantitative research with practical, cost-aware implementation in emerging markets.

[Read the knowledge note](posts/post-3d1597b4303d.md) · [Original](https://stockviz.biz/2026/07/25/single-stock-momentum/) [2026]

## Mahalanobis Distance with Trend

This article extends a previous Mahalanobis distance-based regime-switching model by adding a trend filter. It compares the composite model to simpler alternatives, finding that the alpha primarily comes from earning the risk-free rate during unfavorable periods and going long equities only when conditions are favorable. The analysis involves backtesting on multiple NIFTY indices, with code and charts provided. Understanding requires familiarity with Mahalanobis distance, regime-switching models, trend-following strategies, and performance metrics like Sharpe Ratio. The article assumes prior knowledge of the linked previous work and basic quantitative finance concepts, making it suitable for advanced readers.

[Read the knowledge note](posts/post-63c2720f3187.md) · [Original](https://stockviz.biz/2023/03/25/mahalanobis-distance-with-trend/) [2023]

## Strategy 9 with Dynamic Universe Selection

This article extends a prior analysis of Carver's Strategy 9 on a fixed crypto universe to a dynamic universe selection. It details a walk-forward backtest where, each month, coins with at least 500 days of history and a positive Sharpe ratio under Strategy 9 are selected for the next month. The results are underwhelming, and attempts to improve via Hurst exponent filtering show little benefit. The article includes technical implementation details, code, and charts, and discusses the challenges of universe expansion and overfitting. Understanding requires familiarity with trend-following strategies, walk-forward analysis, Sharpe ratio, and basic crypto market data handling.

[Read the knowledge note](posts/post-aba56f812aac.md) · [Original](https://stockviz.biz/2026/06/27/strategy-9-with-dynamic-universe-selection/) [2026]

## Basis Trades using Futures

This document evaluates the viability and profitability of trading the basis between near and far expiration futures contracts using a historical back-test methodology. It examines Nifty futures price data from 2000 through the present, analyzing the stability of the contract basis and testing a quantitative trading rule based on a 50-day moving average. The text discusses the performance outcomes of long and short basis strategies across different timeframes, specifically evaluating returns from 2005 and 2010 onwards. Ultimately, the analysis concludes that the strategy yields minimal profits that are largely insufficient to cover transaction costs and taxes, indicating that such arbitrage opportunities have been eroded in recent markets.

[Read the knowledge note](posts/post-bb24bedb8878.md) · [Original](https://stockviz.biz/2015/05/03/basis-trades-using-futures/) [2015]

## Trending Momo Models

This article extends prior work on trend overlays to the 'Momo' versions of homegrown momentum models, which use trailing stop-losses. It compares the performance of trend-filtered momo strategies against their monthly rebalanced counterparts, focusing on drawdown reduction and return trade-offs. The analysis highlights that while trended momo strategies can sidestep deep drawdowns, raw returns favor monthly rebalancing, and the higher turnover introduces operational risks requiring automated trading. The post includes performance metrics, charts, and code links, and discusses implications for different market conditions, such as the post-2020 rally. Understanding this requires familiarity with momentum strategies, trend filters, and backtesting concepts, making it suitable for advanced analysis.

[Read the knowledge note](posts/post-27b359687bb7.md) · [Original](https://stockviz.biz/2022/12/04/trending-momo-models/) [2022]

## India VIX vs. SPX VIX

This document examines the relationship between the India VIX and the S&P 500 VIX indices to determine if they exhibit any predictive correlation during periods of market turbulence. The analysis begins by plotting the two indices since 2009, identifying multiple instances where the India VIX experienced sharp upward movements without a corresponding reaction in the SPX VIX. It then evaluates their statistical properties through density plots, demonstrating that the India VIX is significantly more dispersed than its US counterpart, reflecting distinct underlying market behaviors. Finally, a cross-correlation analysis of daily changes is conducted to test for lead-lag relationships or joint movements. The study concludes that despite sharing a common name and serving as volatility gauges, the India VIX and SPX VIX cannot be reliably used to predict each other's market moves, highlighting structural differences between the two equity markets.

[Read the knowledge note](posts/post-8883d09b7217.md) · [Original](https://stockviz.biz/2016/02/07/india-vix-vs-spx-vix/) [2016]

## Diversification and its Malcontents

The article challenges the universal appeal of diversification by exposing the hidden assumptions behind popular strategies like the 60/40 portfolio. It highlights how U.S.‑centric research, data limitations, and currency dynamics create divergent outcomes for investors outside America, especially in emerging markets such as India. Volatility, drawdowns, and the financialization of assets like commodities are shown to erode the “free lunch” promise of simple diversification. The piece warns against blindly adopting U.S. models, urging readers to consider local regulatory, political, and market structures, and to construct portfolios that reflect real‑world risks rather than idealized academic conclusions.

[Read the knowledge note](posts/post-cbebde681852.md) · [Original](https://stockviz.biz/2020/11/08/diversification-and-its-malcontents/) [2020]

## Beyond Payoff Diagrams

This document explores advanced option trading strategies beyond static payoff diagrams, focusing on the dynamic behavior of option spreads over time. It examines a Nifty May 6600/6750 Long Put Spread used to express a bearish market view during an election period, highlighting how daily mark-to-market values fluctuate due to random underlying price paths and time decay. The text explains that option values only get pulled toward the payoff diagram values near the very end of their lifespan primarily because of theta-decay. Key takeaways advise that if positions are in-the-money, keeping them open until expiry is advantageous since most gains accrue at the fag-end of the term. The document also integrates linked materials covering historical option usage, pricing models like Black-Scholes-Merton, put-call parity, theta behavior, investment theme performances, and political contexts affecting market volatility.

[Read the knowledge note](posts/post-42d815efca6e.md) · [Original](https://stockviz.biz/2014/05/11/beyond-payoff-diagrams/) [2014]

## ARMA + GARCH to Predict VIX

This document outlines an advanced quantitative analysis approach using ARMA and GARCH(1,1) models to predict the VIX index. The methodology involves selecting the best fit ARIMA model for historical VIX data and applying a GARCH(1,1) model to forecast t+1 VIX values. Empirical testing revealed that a 500-day lookback period produced the least prediction errors, though the model exhibits a slight bias toward overestimating future VIX levels. The article details plans to integrate these volatility forecasts into daily options publications, while acknowledging practical caveats such as the empirical nature of the lookback window, limitations of historical modeling, and the inherent probability distributions surrounding predicted values.

[Read the knowledge note](posts/post-0abd8e2d9477.md) · [Original](https://stockviz.biz/2015/10/11/arma-garch-to-predict-vix/) [2015]

## Vigilant Asset Allocation

This article critically evaluates the Vigilant Asset Allocation (VAA) strategy from Keller and Keuning's 2017 paper, which combines relative momentum with breadth-based crash protection. The author reproduces the strategy using ETFs and finds that while it performs well in-sample, it underperforms out-of-sample, particularly during bull markets. The article discusses the trade-offs of drawdown avoidance, including opportunity costs and model risk, and compares VAA to simpler alternatives like trend-following and static asset allocation. It requires understanding of momentum, drawdown metrics, backtesting, and parameter optimization, making it suitable for advanced readers.

[Read the knowledge note](posts/post-e9070998a20f.md) · [Original](https://stockviz.biz/2026/07/12/vigilant-asset-allocation/) [2026]

## Defensive Asset Allocation

This article critiques the Defensive Asset Allocation (DAA) strategy by Keller and Keuning, which uses a separate 'canary' universe to signal crash protection via breadth momentum. The author reproduces the strategy with ETFs, finding poor out-of-sample performance, and suggests improvements like using SPHB instead of EEM. The discussion covers advanced topics such as walk-forward validation, parameter overfitting, transaction costs, and the use of non-tradable proxies. Understanding requires familiarity with momentum strategies, asset allocation, backtesting methodology, and statistical pitfalls like data snooping. The article assumes knowledge of prior work (VAA) and references technical details like breadth parameters and leveraged ETFs. It is aimed at readers who can critically evaluate quantitative investment research and implement or modify such strategies.

[Read the knowledge note](posts/post-dda526701343.md) · [Original](https://stockviz.biz/2026/07/12/defensive-asset-allocation/) [2026]

## Protective Asset Allocation

This article critiques the Protective Asset Allocation (PAA) strategy, a momentum-based approach for portfolio protection, and compares it with its successors VAA and DAA. It highlights PAA's design, including its use of a slow SMA filter, equal-weight top-6 asset selection, and a gradual bond allocation based on breadth. The critique points out limitations such as in-sample overfitting, circular selection criteria, synthetic data issues, and limited stress episodes. The author then proposes a rigorous walk-forward backtest using real ETFs, multiple-testing corrections, and pre-registered utility functions to address these flaws. Understanding this article requires familiarity with momentum strategies, asset allocation, backtesting methodologies, and statistical concepts like multiple testing and walk-forward analysis.

[Read the knowledge note](posts/post-b838e5af5544.md) · [Original](https://stockviz.biz/2026/07/12/protective-asset-allocation/) [2026]

## CGMM for VIX

This article evaluates Conditional Gaussian Mixture Models (CGMM) for forecasting 20-day forward VIX, comparing RMSE against the naive LOCF baseline. It builds on prior work with Prophet and GARCH, and references advanced statistical concepts like conditional distributions and mixture models. The analysis involves implementing a specialized library (cgmm), interpreting error metrics, and discussing model limitations in volatile regimes. Understanding of time series forecasting, GMMs, and backtesting is required, placing it at an advanced applied level.

[Read the knowledge note](posts/post-7746fc40f964.md) · [Original](https://stockviz.biz/2025/09/14/cgmm-for-vix/) [2025]

## Is it rational to be irrational?

This article explores the intersection of behavioral economics and investment strategy, questioning whether loss aversion is truly irrational. It critiques traditional economic models and prospect theory through the lens of non-ergodicity and geometric compounding, arguing that avoiding catastrophic losses is mathematically rational when facing ruin. Readers must understand probability, expected returns, and compounding to grasp why low-probability, high-severity events invalidate context-independent rationality. The piece evaluates the practical failure of behavioral funds to beat passive ETFs, suggesting that flawed models, rather than flawed investors, explain poor outcomes. It requires advanced comprehension of how time-series risk differs from ensemble averages, making it suitable for readers analyzing the mathematical foundations of risk and decision-making under uncertainty.

[Read the knowledge note](posts/post-2bb080be2e20.md) · [Original](https://stockviz.biz/2020/07/19/is-it-rational-to-be-irrational/) [2020]

## Moats are for never

This article challenges the durability of competitive advantages and the "never sell" investment dogma by applying base-rate analysis and historical simulations. It examines the lifecycle of corporate moats, citing examples like Intel, GE, and Kodak to illustrate how creative destruction and organizational entropy erode market leadership regardless of past dominance. The piece utilizes McKinsey data on declining corporate lifespans and probabilistic models of country-index returns to demonstrate that buy-and-hold strategies carry significant long-term risks, with many nations posting negative returns. It argues that survivorship bias inflates the perceived success of concentrated stock-picking, urging investors to assess the embedded growth expectations in valuations and the probability of a company's irrelevance. By framing investment longevity through the lens of systemic decay and statistical base rates, the text provides a method for critically evaluating the sustainability of market leaders rather than relying on conventional wisdom.

[Read the knowledge note](posts/post-9ae8ec9e80ec.md) · [Original](https://stockviz.biz/2020/12/20/moats-are-for-never/) [2020]

## Risk Management is Not Free, Part II

This document provides an advanced analysis of risk management strategies within equity portfolios, specifically focusing on the implementation and performance of trailing stop losses (TSLs) on momentum portfolios. It contrasts the mechanics of index investing risk management with portfolio-level stock strategies, evaluating parameters such as exit criteria, re-entry rules, and stock replacement versus holding cash. Through empirical performance data from both Indian and US markets, the text examines the trade-offs between mitigating downside risk and incurring transaction costs, STT, brokerage, and capital gains taxes. The analysis demonstrates how dynamic strategies like Momo 1.1 affect cumulative returns, annualized performance, and cash allocation during different market phases. Ultimately, it discusses the quantitative impact of stop-losses across bull and bear cycles, questioning their aggregate value after accounting for all frictions and highlighting the psychological and structural costs inherent in active risk mitigation.

[Read the knowledge note](posts/post-d07924a444c3.md) · [Original](https://stockviz.biz/2020/03/22/risk-management-is-not-free-part-ii/) [2020]

## Stop-losses or Stop-profits?

This article critically evaluates the efficacy of trailing stop-losses in momentum trading strategies, using empirical data from two live models. It compares the forward returns of stopped positions versus their replacements, finding that holding the original stocks would have yielded better results on average. The analysis covers statistical significance, tail risks, and year-by-year performance, concluding that static stops are costly in bull markets and only marginally helpful in bear markets. The content assumes familiarity with momentum strategies, backtesting, and performance metrics, and includes code and detailed statistical analysis, placing it at an advanced level.

[Read the knowledge note](posts/post-c3baca434a3c.md) · [Original](https://stockviz.biz/2026/08/23/stop-losses-or-stop-profits/) [2026]

## Market-Cap Deciles, Part III

This document explores the performance and accessibility of small-cap versus mega-cap stock portfolios based on market-capitalization deciles. A monthly rebalanced portfolio of approximately 150 equal-weight small-cap stocks yielded an impressive return of 111% from 2015 onward, compared to only 11% for a similar mega-cap portfolio, albeit with higher volatility. The text analyzes the accessibility of this small-cap alpha, noting that meager trading volumes, narrow circuit breakers, and intra-day price volatility create high impact costs for large positions. However, smaller allocations per stock make theoretical portfolio sizes between 15 lakh and 75 lakh rupees viable for capturing these returns. The appendix provides references to cumulative wealth charts and box plots for each market-capitalization decile.

[Read the knowledge note](posts/post-727bf2ae48ab.md) · [Original](https://stockviz.biz/2016/04/24/market-cap-deciles-part-iii/) [2016]

## Market-cap Deciles and Circuit Limits

This article examines how liquidity varies across market-cap deciles and the practical implications of circuit limits for micro and small-cap stocks. It explains that while median liquidity correlates with market capitalization, mid and small caps carry an embedded illiquidity premium that becomes dangerous during market stress. The piece highlights how direct equity investors face exit risks that mutual funds do not, since funds must honor daily NAV redemptions despite being unable to trade underlying illiquid stocks. Historical examples from December 2017 illustrate how momentum stocks can hit lower circuits immediately after market opens, leaving prices 40-50% below previous levels for weeks. The article also briefly touches on applying trend-following systems to commodities like silver and natural gas on the MCX, noting their concentrated trading hours and volatility profiles.

[Read the knowledge note](posts/post-96aae9f9dd05.md) · [Original](https://stockviz.biz/2024/02/25/market-cap-deciles-and-circuit-limits/) [2024]

## Profiting from PE Ratio Obsession

This document explores the phenomenon of price-to-earnings ratio obsession among market participants and investigates whether a profitable arbitrage strategy can exploit this anomaly. Drawing on research from the United States spanning 1974 to 2013, the article examines how decile long-short portfolios formed on characteristics of P/E rankings and rebalanced monthly or daily can earn significant excess returns with high Sharpe ratios. These excess returns remain robust against various factors such as size, value, profitability, and momentum. Furthermore, changes in a stock's P/E ranking can predict excess returns independently of changes in the P/E ratio itself, offering a compelling quantitative anomaly driven by investor attention and limited capital constraints.

[Read the knowledge note](posts/post-de8691013277.md) · [Original](https://stockviz.biz/2015/07/26/profiting-from-pe-ratio-obsession/) [2015]

## Strategy 9 with Equity Indices

This article applies Carver's Strategy 9 trend-following to MSCI country equity indices, comparing four variants (scaled/binary, long-only/long-short) against buy-and-hold. It finds that while binary long-only achieves a higher Sharpe, its drawdowns prevent leverage, making it inferior to buy-and-hold. More critically, yearly returns indicate the strategy stopped working after 2009, suggesting backtest performance is back-loaded. The analysis requires understanding of trend-following, backtesting, Sharpe ratio, drawdown, and leverage, as well as familiarity with prior experiments on crypto and other universes. It involves evaluating strategy variants and interpreting performance metrics, placing it at an advanced applied level.

[Read the knowledge note](posts/post-7f3ae40cf651.md) · [Original](https://stockviz.biz/2026/06/28/strategy-9-with-equity-indices/) [2026]

## Multiple MADs

This article critically evaluates the robustness of the Moving Average Distance (MAD) crossover strategy by conducting an exhaustive parameter search. It compares the published 21/200 lookback parameters against all possible combinations, analyzing pre- and post-COVID performance. The findings suggest that while the original parameters appear legitimate pre-COVID, they do not rank in the top 5 post-COVID, and removing the sigma threshold significantly reduces returns. The analysis requires understanding of backtesting, parameter optimization, overfitting, and performance metrics like Sharpe ratio and drawdowns. It builds on prior posts introducing MAD and rolling window analysis, making it suitable for readers with advanced knowledge of quantitative strategy evaluation.

[Read the knowledge note](posts/post-e9216da95f56.md) · [Original](https://stockviz.biz/2023/12/31/multiple-mads/) [2023]

## Backtesting a Pair Trading Strategy

This document explores the practical backtesting and implementation details of a quantitative pairs trading strategy using financial futures like BANKNIFTY and ICICIBANK. It details a mean-reversion strategy based on buying or selling the spread when it deviates by one standard deviation from the average. The text evaluates simulation results, highlighting the cumulative profit and loss in terms of unit-spreads over specific timeframes. Furthermore, it introduces an asymmetric strategy focusing solely on long positions due to disparities in short profitability. The analysis addresses critical caveats such as execution risks, hedge ratios, lot-size constraints, and the necessity of continuous algorithmic execution to capture alpha, while noting the absence of stop-loss risk management in the simulation.

[Read the knowledge note](posts/post-577aafd5f430.md) · [Original](https://stockviz.biz/2014/05/01/backtesting-pair-trading-strategy/) [2014]

## State of PMS – December 2016

This document provides a monthly update regarding the performance of SEBI-registered Portfolio Management Services (PMS) as of December 2016. It discusses performance diffusion and cumulative returns, comparing PMS offerings with mutual funds, noting that average PMS returns rarely beat large-cap mutual funds after accounting for fees, brokerage, profit sharing, and tax implications on trading accounts. The text highlights how SEBI's publication of PMS details enables transparent tracking of these investment managers. Additionally, the update touches upon broader market movements during the period, encompassing commodity pricing, international exchange-traded funds (ETFs), yield curves, bond indices, and various investment theme performances such as momentum strategies.

[Read the knowledge note](posts/post-9e2b8fdc1ab1.md) · [Original](https://stockviz.biz/2017/02/02/state-pms-december-2016/) [2017]

## Implied Volatility around Earnings Announcements

This document examines the common trading belief that option implied volatility (IV) predictably decreases following earnings announcements. By observing at-the-money IVs five days before announcements and analyzing their subsequent behavior, the study tests the validity of selling options specifically to profit from post-earnings IV drops. The findings reveal that implied volatility only falls 44 percent of the time, demonstrating that IV dissipation is not guaranteed and earnings alone cannot reliably predict the outcome. However, the study notes that when the initial IV is greater than 50, it dissipates 70 percent of the time, though further research is required to determine if these odds differ from non-earnings periods. The analysis concludes that traders should avoid blanket assumptions regarding IV behavior after earnings and carefully evaluate their odds before executing option-selling strategies.

[Read the knowledge note](posts/post-88f03c44e99a.md) · [Original](https://stockviz.biz/2015/07/02/implied-volatility-around-earnings-announcements/) [2015]

## Are Stop-Losses Worth It? Part II

This document analyzes the effectiveness of stop-loss mechanisms in momentum investment portfolios, specifically evaluating performance from mid-2016 through April-2019 across bull and bear market phases. It examines subsequent returns of stop-lossed positions to determine if the trading costs and taxes associated with frequent exits are justified by avoided losses. The author contrasts single-position outcomes with aggregate portfolio performance, demonstrating that while stop-losses provide psychological comfort and can help during severe bear phases, they ultimately act as a performance drag after accounting for transaction costs. The text concludes that investors seeking to reduce volatility should focus on overall asset allocation rather than relying on position-level stop-losses for high-risk momentum strategies.

[Read the knowledge note](posts/post-65312fda628c.md) · [Original](https://stockviz.biz/2019/05/02/are-stop-losses-worth-it-part-ii/) [2019]

## Streaks, Part II – Backtest

This article details a quantitative backtest exploring whether monthly return streaks can predict subsequent market directions for the NIFTY 50 index. The strategy entails going long for one month following two consecutive negative months, capitalizing on the empirical rarity of three successive down months. The strategy yields a 190% gross return, which scales to approximately 1330% when leveraged through NIFTY futures, benefiting from notably shallow drawdowns. However, the author provides critical context regarding data-mining concerns, noting that these favorable results do not hold up when incorporating historical data prior to 2005. The piece includes references to GitHub repositories housing the corresponding code and charts.

[Read the knowledge note](posts/post-156b2d1988e5.md) · [Original](https://stockviz.biz/2019/01/03/streaks-part-ii-backtest/) [2019]

## SMA Strategies using ETFs

This document analyzes quantitative backtests of simple moving average tactical strategies using Exchange Traded Funds in Indian markets. It explores toggling between equity indices like NIFTY 50, NIFTY MIDCAP 100, and NIFTY SMLCAP 100 and cash equivalents based on moving average look-back periods. The analysis covers the impact of shorter look-backs on performance and trade frequency, transaction costs including brokerage and securities transaction taxes, and practical implementation challenges. Specifically, it highlights scalability constraints caused by low ETF trading volumes and additional risks stemming from ETFs trading at significant premiums or discounts relative to their net asset values. The text introduces alternative fund options, trend-confirmation checks for moving averages to reduce drawdowns, and references related research parts.

[Read the knowledge note](posts/post-1e81453b6848.md) · [Original](https://stockviz.biz/2019/02/07/sma-strategies-using-etfs/) [2019]

## Dual Momentum: NIFTY vs MIDCAP

This article explores the application of a dual momentum strategy to toggle between NIFTY 50, MIDCAP 100, and bonds. The author analyzes rolling 200-day cumulative returns to evaluate the correlation of excess returns over bonds and the sticky relative performance between the two equity indices. Backtests over different look-back periods indicate that 3- and 4-month look-backs yield higher returns and lower drawdowns compared to buy-and-hold strategies, though risking over-fitting. Additionally, a model checking 'any' lookback signal provides reduced drawdowns and returns superior to NIFTY 50, albeit lower than MIDCAP 100 buy-and-hold. Operating on a monthly check frequency helps minimize transaction costs for long-term investors.

[Read the knowledge note](posts/post-ec8947e5850d.md) · [Original](https://stockviz.biz/2019/03/07/dual-momentum-nifty-vs-midcap/) [2019]

## Industry Momentum

This article discusses the implementation and backtesting of an industry momentum strategy using Relative Rotation Graphs (RRG). It covers the construction of RRG indicators (RS-Ratio and RS-Momentum), the selection of top industries based on relative strength, and the impact of rebalancing frequency and weighting schemes. The backtest results show that a cap-weighted, 4-week rebalanced strategy in the 'Leading' quadrant outperforms benchmarks, while the 'Improving' quadrant fails. The article emphasizes the importance of trend (RS-Ratio) over momentum (RS-Momentum) and highlights practical challenges in execution, such as rebalance timing and weighting. It assumes familiarity with technical analysis, momentum investing, and basic portfolio construction, making it suitable for advanced readers.

[Read the knowledge note](posts/post-20307e71af39.md) · [Original](https://stockviz.biz/2026/07/09/industry-momentum/) [2026]

## SVM for Momentum

This article evaluates the use of Support Vector Machines (SVMs) for momentum trading, comparing them to simpler strategies. It highlights the pitfalls of overfitting through extensive parameter tuning, such as selecting a 5th-degree polynomial kernel based on backtest performance. The author expresses skepticism about AI-driven strategies, noting that many tuned models fail in live trading. The content assumes familiarity with machine learning concepts, backtesting, and portfolio construction, making it suitable for advanced readers.

[Read the knowledge note](posts/post-561c31a10625.md) · [Original](https://stockviz.biz/2024/07/11/svm-for-momentum/) [2024]

## Volatility as Beta

This article explains the volatility risk premium (VRP) as the difference between implied and realized volatility, and argues that short volatility strategies are implicitly long equity risk, thus earning a premium. It introduces option Greeks (delta, gamma, vega, theta) and describes how delta-hedged short option positions can isolate volatility exposure. The author suggests treating volatility as a beta that can be allocated to, similar to equity risk premium, and mentions building models to time VRP. The content requires understanding of options pricing, Greeks, and portfolio construction, making it suitable for advanced readers.

[Read the knowledge note](posts/post-5c8fe4260809.md) · [Original](https://stockviz.biz/2025/06/12/volatility-as-beta/) [2025]

## Fractional Momentum

This article introduces Fractional Momentum, a strategy that uses fractional differencing to incorporate price path information into momentum signals, aiming to reduce crash risk. It requires understanding of momentum strategies, time series analysis, fractional calculus, and backtesting. The article discusses the theoretical foundation, empirical results, and practical implementation, making it suitable for advanced readers familiar with quantitative finance and statistical methods.

[Read the knowledge note](posts/post-8d2431902f22.md) · [Original](https://stockviz.biz/2022/12/15/fractional-momentum/) [2022]

## Budget-day Options

This article analyzes trading strategies around Indian budget days, focusing on options. It notes that 16 of the last 26 budgets ended red, suggesting a short bias. It evaluates delta-hedged short strangles on NIFTY, held overnight and closed on budget day, showing decent profit potential but with significant execution risk due to intraday volatility. The analysis includes backtested P&L tables and code on GitHub. Understanding requires familiarity with options (strangles, delta hedging), market microstructure, and event-driven trading. It goes beyond basic concepts, applying advanced options strategies and statistical analysis, but does not delve into specialist research or implementation details.

[Read the knowledge note](posts/post-320ebb7f7018.md) · [Original](https://stockviz.biz/2026/01/15/budget-day-options/) [2026]

## Trend Following vs. Trend Prediction, Part II

This document discusses the 100-day performance milestone of machine-learning trend-following models compared to traditional momentum algorithms. The machine-learning models, incorporating Neural Networks and Support Vector Machines, have successfully outperformed the majority of traditional algorithms. The text highlights the anticipation of evaluating these models over a 200-day period to minimize the influence of short-term randomness or luck. It builds upon previous explorations of trend prediction versus trend following, emphasizing the practical implementation and comparative performance of quantitative trading strategies using advanced machine-learning techniques rather than basic momentum indices.

[Read the knowledge note](posts/post-34788f4f7c23.md) · [Original](https://stockviz.biz/2018/05/17/trend-following-vs-trend-prediction-part-ii/) [2018]

## Buy and Hold probably works only for US stocks

This document examines the widespread assumption that the buy-and-hold investment strategy is universally effective, arguing instead that its perceived superiority stems heavily from an over-reliance on United States market data. The analysis highlights that US equities represent a statistical outlier with exceptionally low probabilities of long-term losses due to unique historical, economic, and regulatory tailwinds. In contrast, evaluating international markets reveals significantly higher risks of negative outcomes, such as a 21 percent probability in Japan and a 6 percent chance in India, while numerous other countries experienced single-digit or stagnant equity returns over decades. The text cautions investors and researchers against generalizing US-centric performance metrics globally, emphasizing that foreign equity markets operate under vastly different economic probabilities and government structures. Consequently, adopting a passive buy-and-hold strategy requires careful consideration of local market dynamics rather than relying blindly on American historical precedents.

[Read the knowledge note](posts/post-4a7ab6273647.md) · [Original](https://stockviz.biz/2019/09/19/buy-and-hold-probably-works-only-for-us-stocks/) [2019]

## Do Superstitious Traders Lose Money?

This document explores a financial research paper examining the relationship between superstition and trading performance among market participants in the Taiwan Futures Exchange. The authors construct a superstition index to categorize traders and evaluate whether superstitious beliefs correlate with cognitive disabilities in financial decision-making. The findings indicate that highly superstitious traders consistently underperform non-superstitious counterparts, with performance deficits expanding from a trading day to five-day windows following transactions. Additionally, the text includes a historical anecdote regarding India's Independence Day selection based on astrological guidance. The referenced content provides advanced insights into behavioral finance, trader psychology, and quantitative performance disparities driven by cognitive biases.

[Read the knowledge note](posts/post-9b5de9219f22.md) · [Original](https://stockviz.biz/2014/08/21/superstitious-traders-lose-money/) [2014]

## Can Beta Dispersion be used for Market-Timing?

This document examines the research paper titled Beta Dispersion and Market-Timing, which suggests that tracking the beta dispersion among index constituents can predict market crashes and corrections. The author raises critical methodological questions regarding the strategy's robustness, including the impact of removing crisis periods like 2000 and 2008, applicability across different markets outside the US, sensitivity to varying look-back periods, and the effect of continuous versus monthly sampled calculations. Testing the theory on the NIFTY 100 index using available data does not yield the same conclusions as the original paper, highlighting the need for cautious interpretation and further empirical validation before deploying such quantitative market-timing strategies in practical portfolio management.

[Read the knowledge note](posts/post-41a11a5e2fb6.md) · [Original](https://stockviz.biz/2017/06/22/can-beta-dispersion-used-market-timing/) [2017]

## Trend Following vs. Trend Prediction, Part I

This article discusses the differences and similarities between traditional equity momentum strategies, which follow existing market trends, and machine learning-based trend prediction models. While basic momentum strategies rely solely on price series and often encounter limitations when incorporating multiple factors like volatility, machine learning algorithms can efficiently process and weigh numerous variables simultaneously. However, a major drawback of using complex machine learning models is their lack of transparency and explainability as more features are added. To explore this approach practically, the author details the establishment of four initial machine learning algorithms utilizing Support Vector Regressors and Linear Regression, trained either on return series exclusively or a combination of returns and volatility, marking an advanced step toward algorithmic trading integration.

[Read the knowledge note](posts/post-bff10172ac88.md) · [Original](https://stockviz.biz/2018/03/22/trend-following-vs-trend-prediction/) [2018]

## MSCI: Momentum trumps Value

This document analyzes the performance comparison between MSCI Value and MSCI Momentum factor indices across various global markets, including developed and emerging regions, dating back to 1995. It details the underlying index construction methodologies, noting that Prime Value indices incorporate quality scores and valuation descriptors like P/E, P/B, P/S, and P/CE, while Momentum indices rely on risk-adjusted, volatility-corrected 6-month and 12-month price performance. Through multi-market performance evaluations, the text addresses the overarching investment dilemma of choosing between value and momentum strategies, ultimately concluding that momentum generally outperforms value in most regions, except in specific long-term deflationary environments.

[Read the knowledge note](posts/post-897170a5faaa.md) · [Original](https://stockviz.biz/2019/01/24/msci-momentum-trumps-value/) [2019]

## Are Stop-Losses Worth It?

This document examines the effectiveness of stop-loss strategies in equity trading through a quantitative evaluation of StockViz's Momentum investment themes. The author explores how a Chinese market crash prompted the creation of 'Momo' strategies featuring a 5 percent trailing stop-loss to mitigate downside risk. After analyzing multi-year performance data, volatility impacts, and the hidden costs of increased portfolio turnover, the article concludes that stop-losses are generally not worth it for high-risk strategies. Instead, it suggests that investors should rely on disciplined asset allocation to manage overall portfolio volatility rather than depending on intrinsic security-level safety nets like stop-losses, which often erode returns through additional taxes and transaction fees.

[Read the knowledge note](posts/post-c3901ccfc02d.md) · [Original](https://stockviz.biz/2019/04/25/are-stop-losses-worth-it/) [2019]

## Strategy 9 with 15 Instruments

This article explores Rob Carver's Strategy 9, applying multiple moving average trend-following rules to Indian market indices like NIFTY 50, BANK, MIDCAP, and SMALLCAP. It examines various decision points including long-only versus long-short, binary versus scaled signals, and equal-weight versus inverse-volatility-weighting. The analysis reveals that only the scaled long-only equal-weight setup appears promising, though heavily influenced by crypto. For MIDCAP futures, a binary long-short approach without a cost-screen works best, but carries significant leverage risk with periodic 20% drawdowns. Since SMALLCAP lacks listed futures, a binary long-only approach is required. The piece highlights practical constraints for Indian retail traders, such as high trading costs rendering cost-screens useless and capital requirements making scaled positions unfeasible, ultimately adapting a complex Western futures strategy to a limited local instrument set.

[Read the knowledge note](posts/post-b6338459fa4e.md) · [Original](https://stockviz.biz/2026/06/25/strategy-9-with-15-instruments/) [2026]

## Lessons from the Latin American Debt Crisis

This document examines the structural lessons learned from major historical banking crises, specifically focusing on the Latin American debt crisis. It analyzes systemic vulnerabilities, including how bankers prioritize short-term profits over long-term risks and the reliance on debt rollovers. Furthermore, it details the interventionist tools employed by central bankers during times of effective systemic bankruptcy, such as taxpayer bailouts of non-performing loans, interest rate manipulation to boost net interest margins, and the enforcement of economic austerity measures to stabilize the financial sector.

[Read the knowledge note](posts/post-4f0715ba1d06.md) · [Original](https://stockviz.biz/2014/09/25/lessons-latin-american-debt-crisis/) [2014]

## Funds that (also) invest in foreign markets

This article evaluates the practice of Indian mutual funds investing in foreign equities under the guise of portfolio diversification. By analyzing monthly return correlations between major global indices like the S&P 500, Nasdaq, FTSE 100, Nikkei 225, and the Indian CNX 500, the author demonstrates that international equities are not negatively or zero-correlated with Indian stocks, thereby undermining the true diversification claim. While historical depreciation of the Indian rupee against the US dollar justifies holding dollar-denominated assets, the text questions the competency and effectiveness of Indian asset managers attempting active stock-picking in foreign markets where developed regions have largely shifted toward indexing. The author concludes that investors seeking currency exposure should instead buy an S&P 500 ETF or hold net short rupee positions directly, rather than relying on mixed-in foreign stock portfolios.

[Read the knowledge note](posts/post-90c326089bc4.md) · [Original](https://stockviz.biz/2015/02/26/funds-also-invest-foreign-markets/) [2015]

## Does Momentum Trend or Mean-Revert?

This article investigates whether long-only momentum returns exhibit trending or mean-reverting behavior by examining excess returns over 5-day and 10-day periods using the Hurst exponent across Barclays Euro-zone, UK, Japan, and US momentum indices. The author outlines a methodology using rolling windows to calculate the Hurst exponent to time entries and exits, testing the hypothesis that momentum excess returns are predictable. The results indicate that incorporating the Hurst exponent did not improve momentum returns compared to simpler median-based or buy-and-hold strategies, and the findings suggest that momentum excess returns generally tend to trend rather than mean-revert within the tested configurations, highlighting the risks of data-mining when attempting to build trading strategies based on these exponents.

[Read the knowledge note](posts/post-8828f9c19b4f.md) · [Original](https://stockviz.biz/2019/09/26/does-momentum-trend-or-mean-revert/) [2019]

## Annual Drawdowns and Subsequent Returns

This article examines the relationship between annual max drawdowns and subsequent returns across major equity indices such as the S&P 500 and NIFTY 50. It questions the newsworthiness of typical market drops by showing that maximum drawdowns happen frequently. Furthermore, the analysis demonstrates that experiencing a significant drawdown does not guarantee immediate or rapid subsequent positive returns. Historical charts evaluating forward returns indicate that a bull case is difficult to justify purely based on a recent market dip. The text emphasizes that patience is the primary requirement, as it takes considerable time for statistical odds to favor investors following a major drawdown. Additional resources and reproducible code are provided via linked collections and GitHub repositories covering buying-the-dip strategies and market timing models.

[Read the knowledge note](posts/post-00681bd33605.md) · [Original](https://stockviz.biz/2018/12/27/annual-drawdowns-and-subsequent-returns/) [2018]

## Nifty Gaps

This article investigates the time alignment problems inherent in global financial data when dealing with rolling closes between Nifty and S&P indices. It examines the characteristics of Nifty opening gaps, distinguishing between regular Mondays, holidays, and standard trading days, and analyzes the performance differences between close-to-close and open-to-close returns. The author explores the impact of holding positions overnight versus buying at the open, and compares market volatility and return summary metrics between the first and last half-hours of trading. Ultimately, the piece concludes that avoiding weekend carryover positions offers a net benefit for global macro models using weekly time series, allowing traders to bypass the rolling close issue by opening positions at Monday open and closing them at Friday close.

[Read the knowledge note](posts/post-1005acd87f24.md) · [Original](https://stockviz.biz/2018/11/29/nifty-gaps/) [2018]

## Long-Short Trend Following

This article explores long-short trend following strategies using the 50-day simple moving average and volatility metrics on indices like CNX 100, Nifty, and Bank Nifty. Building upon prior work involving tactical SMA on/off switches, the author evaluates naive long-short strategies against long-only tactical approaches. The findings indicate that while a naive long-short approach does not significantly outperform long-only methods, combining a volatility signal with the 50-day moving average generates meaningful long-term alpha. The piece concludes by introducing a real-time tracking theme called Trend Long-Short to demonstrate the practical application of this combined methodology.

[Read the knowledge note](posts/post-49d46d3305b8.md) · [Original](https://stockviz.biz/2015/04/30/long-short-trend-following/) [2015]

## Lumpsum vs. SIP: Thinking in Probabilities

This article analyzes the probabilistic outcomes of lumpsum versus Systematic Investment Plan (SIP) or Dollar Cost Averaging (DCA) strategies across indices like NIFTY 50, MIDCAP, and SMLCAP. Utilizing a Generalized Lambda Distribution and 10,000-path simulations, the author models weekly returns to build empirical cumulative distribution functions and evaluate probabilities under specific return thresholds. The discussion contrasts synthetic average return series against real volatile market paths, observing that while lumpsums offer a higher probability of superior and average returns, they also exhibit fatter left tails of risk. Conversely, SIPs present a non-trivial possibility of negative returns over long periods, though their maximum potential losses are often constrained compared to lumpsum counterparts. The findings provide advanced comparative probabilistic insights for portfolio allocation and investment execution.

[Read the knowledge note](posts/post-558fd072f3f0.md) · [Original](https://stockviz.biz/2018/10/02/lumpsum-vs-sip-thinking-in-probabilities/) [2018]

## Mixture model over S&P 500 returns

This document examines the application of Gaussian mixture models to classify daily S&P 500 returns into bull and bear market regimes. While a whole-period analysis shows distinct differences between the return distributions of the two regimes, a rolling-period analysis reveals far less differentiation between the densities. Furthermore, when the identified regimes are used as a timing signal in a systematic trading strategy to navigate market downturns and uptrends, the resulting overall return profile is sub-par. The article concludes that although mixture models are interesting quantitative tools, using them to time trades in a linear fashion is ineffective. The analysis connects to related quantitative concepts such as market skewness as a timing signal, momentum investing, and growth investing frameworks.

[Read the knowledge note](posts/post-c8e61a3a04d2.md) · [Original](https://stockviz.biz/2018/12/04/mixture-model-over-sp-500-returns/) [2018]

## Quant Model in Mutual Fund Wrapper

This document provides an advanced quantitative analysis of the DSP Quant Fund using backtest data. It evaluates cumulative performance against broad-market cap indices and strategy indices, identifying key performance drivers such as market beta and the quality factor, while noting that the value factor acts as a drag on returns. The analysis compares the fund's expense ratio and liquidity with alternative options like the SBI Quality ETF. It highlights the tax efficiency and low-cost nature of mutual fund wrappers for quantitative models compared to direct-equity platforms, offering specialist insights for passive investors considering factor-based strategies.

[Read the knowledge note](posts/post-b04a8c6d70d7.md) · [Original](https://stockviz.biz/2019/06/04/quant-model-in-mutual-fund-wrapper/) [2019]

## Chart: One and Two Percent Moves

This document analyzes the frequency and trends of one and two percent intraday and overnight price movements for NIFTY 50 and BANK NIFTY indices. It breaks down market behavior into several key components including Prev Close-to-Open overnight events, Close-to-Close fundamentals, Open-to-Close sentiment, and High-to-Low uncertainty. The author observes that overnight moves and Open-to-Close ranges have experienced a noticeable decline over the years, which may be a characteristic of the prevailing bull market. The text also touches upon broader macroeconomic narratives surrounding the low volatility regime, such as central bank liquidity, post-2008 regulatory changes, systematic investment plans, derivatives market activity, and political stability. Potential risks of a market regime shift back to historical volatility norms are highlighted, supported by data code and charts available externally.

[Read the knowledge note](posts/post-64fd11d7b42c.md) · [Original](https://stockviz.biz/2019/03/05/chart-one-and-two-percent-moves/) [2019]

## USDINR and Dollar Indices, Part III

This document covers Part III of the USDINR and dollar indices series, focusing on back-testing spread-trading strategies using daily time-series. The text defines spread-trading mechanics by creating two legs: long USDINR and short beta times a dollar index. Three back-test scenarios are evaluated, including convergence, momentum, and divergence models. Results indicate that betting on momentum where the spread diverges beyond one sigma is the most profitable approach. Specifically, pairing USDINR with DTWEXM yields the strongest performance. The analysis highlights practical implementation challenges and theoretical limitations, noting that dollar indices cannot be directly traded, and points toward subsequent weekly time-series analysis.

[Read the knowledge note](posts/post-1faece8361bb.md) · [Original](https://stockviz.biz/2018/11/06/usdinr-and-dollar-indices-part-iii/) [2018]

## USDINR and Dollar Indices, Part IV

This document concludes the four-part series on analyzing USDINR and trade-weighted dollar indices by shifting from daily to weekly returns to mitigate issues associated with 24/7 global market trading and closing prices. By applying adf-tests and spread-trading back-tests to weekly series, the author confirms that the findings mirror daily results. Specifically, momentum-based strategies on the USDINR and DTWEXM pairs continue to demonstrate profitability, reinforcing confidence in the previously tested models. The article concludes with important operational caveats regarding spread trading, emphasizing that trades involve two legs and that buying USDINR implies going long on USD and short on INR, while noting that the analysis does not inherently determine the relative fundamental valuation of either currency in isolation.

[Read the knowledge note](posts/post-952fe5124d5e.md) · [Original](https://stockviz.biz/2018/11/06/usdinr-and-dollar-indices-part-iv/) [2018]

## World Markets and the NIFTY 50

This document explores the potential of constructing a world markets indicator to time and short the NIFTY 50 index during global sell-offs. By analyzing dollar-based proxies of world index ETFs listed on the NYSE, the author constructs simple moving average (SMA) indices tracking the fraction of markets trading above their 5-day, 10-day, and 50-day SMAs. The study tests trading strategies that go long or short based on whether these fractions fall below historical medians. Performance comparisons against buy-and-hold strategies reveal that while short-term SMA strategies largely underperform, the 50-day SMA strategy successfully helps short the 2008 crisis and subsequent market sell-offs. However, due to high trading costs, persistent underperformance in multiple consecutive years, and overall volatility, the strategy presents significant practical challenges for consistent trading implementation.

[Read the knowledge note](posts/post-40b0cf0098a4.md) · [Original](https://stockviz.biz/2016/02/09/world-markets-and-the-nifty-50/) [2016]

## Mutual Funds: A quick note on performance metrics

This document examines the stability and reliability of common mutual fund performance metrics such as alpha, beta, and information ratio over time and across various market environments. Using a 200-week sliding window of midcap mutual fund returns, specifically analyzing the HDFC Mid-Cap Opportunities Fund, the analysis demonstrates that these metrics exhibit zero stability and vary constantly. Consequently, using them to pick winning funds or dropping funds based on declining alpha is shown to be ineffective and only indicative of past performance. Additional observations note that negative beta is inescapable, managers demonstrate asymmetric risk and reward capabilities by outperforming on the upside while avoiding drastic under-performance on the downside, and historical outperformance is no guarantee of future results.

[Read the knowledge note](posts/post-8d9b91224855.md) · [Original](https://stockviz.biz/2018/10/09/mutual-funds-a-quick-note-on-performance-metrics/) [2018]

## VIX and Equity Index Returns, Part II

This document extends the analysis of VIX and equity index relationships by examining alternate holding periods beyond a single day and testing whether changes in the VIX can effectively time the equity index. Through bucketing VIX returns into deciles and observing subsequent 5, 10, 15, and 20-day index returns, the author investigates predictive patterns between volatility and equity movements. The findings indicate no determinable predictive pattern, suggesting that the VIX and the equity index are largely co-incident rather than one holding predictive power over the other. The article also references additional charts and code available in a GitHub repository for Nikkei 225 and NIFTY 50 datasets, building upon foundational concepts established in Part I of the series.

[Read the knowledge note](posts/post-be57f90bd302.md) · [Original](https://stockviz.biz/2018/11/13/vix-and-equity-index-returns-part-ii/) [2018]

## Are stocks an inflation hedge?

This document examines the empirical relationship between inflation and stock market returns in India using monthly data from 1994 to 2014. The analysis discusses findings from a recent study which indicates a lack of significant pro-cyclical inter-dependency between inflation and stock returns. However, it critiques the underlying research paper for omitting formal statistical significance tests, relying instead on visual interpretations of wavelet transforms. This leaves readers to evaluate the graphical representations independently without rigorous quantitative confirmation.

[Read the knowledge note](posts/post-b97cad0e39fa.md) · [Original](https://stockviz.biz/2017/02/14/stocks-inflation-hedge/) [2017]

## Day vs. Night Momentum

This article summarizes a research paper that decomposes momentum into intraday and overnight components, finding that momentum is driven by intraday returns. It then replicates the study on Indian equities, comparing strategies ranked by intraday, overnight, and total returns. The replication shows that total-return ranking performs best, but all strategies underperform the benchmark. The content requires understanding of momentum strategies, return decomposition, and portfolio backtesting, making it suitable for advanced analysis.

[Read the knowledge note](posts/post-d7ce8f80640a.md) · [Original](https://stockviz.biz/2026/07/14/day-vs-night-momentum/) [2026]

## What can Modi do?

This document analyzes excerpts from a JP Morgan special report regarding the Indian elections, equity market exuberance, and the economic realities facing the country. It highlights that market optimism is driven by expectations of a stable post-election government and a dramatic economic pivot to jumpstart the capital expenditure cycle. However, the report points out major structural hurdles, such as stalled projects under state government jurisdictions, weak corporate balance sheets requiring significant deleveraging among infrastructure companies, and undercapitalized public sector banks saddled with impaired loans. Consequently, the transition from political stability to actual economic performance is expected to be lagged, uncertain, and incomplete compared to current market sentiment.

[Read the knowledge note](posts/post-b12d9163de1d.md) · [Original](https://stockviz.biz/2014/04/15/can-modi/) [2014]

## Rates and USDINR Update

This article analyzes the relationship between Indian 10-year interest rates, US Treasuries, and the USDINR exchange rate following political shifts and geopolitical events. It examines how interest rate spread compression between US Treasuries and Indian Government Securities reflects market bullishness tied to the Modi administration. Additionally, it evaluates the short-term impact of the ISIS-induced conflict in Iraq on global oil prices, export disruptions, and the flight to safety. The author posits that once the knee-jerk market reactions to tail-risks surrounding Iraq, Russia-Ukraine, and the Chinese financial crisis subside, existing macroeconomic trends involving bond yields and currency valuations will likely reassert themselves.

[Read the knowledge note](posts/post-682dfc8d171f.md) · [Original](https://stockviz.biz/2014/06/17/rates-usdinr-update/) [2014]

## Macro: Using Currencies to Predict NIFTY, Part II

This document explores advanced machine learning techniques applied to quantitative finance, specifically predicting NIFTY 50 index movements using currency returns through Support Vector Machines with polynomial kernels. The author investigates hyperparameter tuning of the degree parameter across different time horizons, specifically comparing datasets spanning 2000-2018 and 2005-2018 to evaluate how historical data depth impacts predictive performance and market correction side-stepping. The analysis highlights the sensitivity of models to dataset selection windows, indicating that older historical data can occasionally be counter-productive when structural market changes occur. The methodology includes training, validation, and testing divisions, performance tabulation, and cumulative return charting for long-only, long-short, and buy-and-hold strategies.

[Read the knowledge note](posts/post-be31e96d86ba.md) · [Original](https://stockviz.biz/2018/11/20/macro-using-currencies-to-predict-nifty-part-ii/) [2018]

## Macro: Using Currencies to Predict NIFTY, Part III

This document is part of a quantitative finance series exploring the use of currency indices to predict NIFTY 50 returns using Support Vector Machines with polynomial kernels. Building on prior analyses of major dollar indices, this installment trains models on alternative FRED datasets, specifically DTWEXB, DTWEXO, and USDINR. The author evaluates returns across different time horizons, notably comparing 2000-2018 and 2005-2018 datasets. Findings indicate that DTWEXO and USDINR are less effective predictors, whereas the DTWEXB index with specific degree parameters successfully side-steps market corrections. The analysis highlights dataset selection sensitivity and outlines plans to combine top-performing models in subsequent research.

[Read the knowledge note](posts/post-a49b4bf32f89.md) · [Original](https://stockviz.biz/2018/11/20/macro-using-currencies-to-predict-nifty-part-iii/) [2018]

## Macro: Using Currencies to Predict NIFTY, Part IV

This document covers Part IV of an advanced series on predicting the NIFTY 50 index using Support Vector Machines applied to dollar indices. It details the creation of an ensemble model combining two distinct SVM configurations: one using the DTWEXB index with an 8th-degree polynomial kernel and another using the DTWEXM index with a 4th-degree polynomial kernel. The article examines the performance of these standalone models versus the ensemble in constructing long-only and long-short portfolios. Results indicate that while the standalone DTWEXM long-short model outperforms others in cumulative returns, the ensemble model achieves lower drawdowns. The text also highlights drawdowns exceeding five percent across the test dataset and sets up the final post, which introduces technical signals like simple moving averages to further optimize portfolio returns and risk metrics.

[Read the knowledge note](posts/post-dfee9ccde968.md) · [Original](https://stockviz.biz/2018/11/20/macro-using-currencies-to-predict-nifty-part-iv/) [2018]

## Global Equities Momentum

This document explores Gary Antonacci's Global Equities Momentum (GEM) model, which utilizes dual momentum to toggle between stocks and bonds based on trailing returns. The article discusses substituting traditional market-capitalization-based indices like the S&P 500 and World ex USA with momentum-based indices across various scenarios. By replacing standard indices with US and International momentum strategies, the author evaluates the impact on cumulative returns, annual breakdowns, and drawdowns. The linked source from Newfound Research further analyzes GEM through the lens of model specification risk and strategy fragility, highlighting how varying lookback horizons and implementation details can lead to significant performance dispersion and differing tactical outcomes.

[Read the knowledge note](posts/post-58c8dbbecdcb.md) · [Original](https://stockviz.biz/2019/01/22/global-equities-momentum/) [2019]

## Strategy 9

This article applies Rob Carver's Strategy Nine, a composite trend-following system, to Indian indices, evaluating long-only vs. long-short, binary vs. scaled signals, and cost screens. It requires understanding of futures trading, leverage, drawdowns, and backtesting. The analysis compares performance across NIFTY 50, BANK, MIDCAP, and SMALLCAP, concluding that a 50-50 blend of scaled long-only on MIDCAP and SMALLCAP is optimal. The content assumes familiarity with moving averages, Sharpe ratios, and trading costs, making it suitable for advanced practitioners.

[Read the knowledge note](posts/post-00cc2d7790ba.md) · [Original](https://stockviz.biz/2026/06/23/strategy-9/) [2026]

## The Path Dependency of SIP Returns

This article explores the path dependency of Systematic Investment Plan and Dollar Cost Averaging returns by analyzing how the sequence of monthly returns influences final investment outcomes. Moving beyond statistical models that fit weekly returns into a Generalised Lambda Distribution, the analysis involves randomly shuffling an observed set of monthly returns for indices like NIFTY 50, MIDCAP, and SMALLCAP to generate multiple return series with the exact same constituent returns in varying orders. The resulting simulations demonstrate that reordering monthly returns yields vastly different performance distributions, highlighting why individual investors can experience poor returns despite strong overall index performance due to path dependency.

[Read the knowledge note](posts/post-fdeb30ea0c80.md) · [Original](https://stockviz.biz/2018/10/23/the-path-dependency-of-sip-returns/) [2018]

## SMA Distance, Part II

This article continues the exploration of Simple Moving Average (SMA) Distance by analyzing how current distance metrics relate to future market returns. The author buckets 50-, 100-, and 200-day SMA distances into quintiles to plot the distribution of subsequent 20-, 50-, and 100-day returns. The findings indicate that markets in rising trends with lower 50- and 100-day distances tend to avoid sharp short-term reversals, as evidenced by smaller negative tails on 20-day returns. Conversely, extreme deviations from the 200-day SMA suggest over-extended markets that are vulnerable to steeper drops. The analysis bridges basic moving average concepts with advanced return distribution modeling and sets the stage for future backtesting of trading strategies using these quantitative thresholds.

[Read the knowledge note](posts/post-4b7fd102ef48.md) · [Original](https://stockviz.biz/2018/12/25/sma-distance-part-ii/) [2018]

## Principal Component Analysis, Part I

This article explores Principal Component Analysis (PCA) as a method for summarizing and optimizing financial sector index exposure using USD-denominated Total Return indices published by NASDAQ-OMX from 2001 onward. The author investigates whether factor loadings remain stable across different time periods and market regimes, utilizing a sliding window of 5-year daily returns across 11 datasets spanning 2002 to 2017. By running PCA on sector indices such as Basic Materials, Consumer Goods, Financials, Health Care, Industrials, and Tech, the study plots the loadings of the first principal component. The findings reveal that the loadings are dominated by Basic Materials, Financials, and Industrials, while the relative importance of IT has declined, and Financials dominate the below-SMA200 market regime. Ultimately, the analysis concludes that stable factor loadings across the entire dataset or specific Simple Moving Average regimes are not achievable with these indices for constructing reliable switching portfolios.

[Read the knowledge note](posts/post-eebcfb2fbc77.md) · [Original](https://stockviz.biz/2018/09/25/principal-component-analysis-part-i/) [2018]

## Is there a correlation between USDINR and Tech stocks? [Update]

This document updates an analysis from May 2015 regarding the relationship between USDINR currency movements and the performance of technology stocks, specifically the NIFTY IT index. By examining updated weekly and monthly correlation charts, the study reaffirms that there is no direct relationship between currency fluctuations and subsequent IT index returns. The analysis challenges the common media narrative that attributes tech stock movements solely to currency depreciation, demonstrating through empirical scatter and cross-correlation plots that currency moves cannot serve as a reliable standalone explanation for technology stock fluctuations. The associated code and visualization assets are made available on GitHub for further technical review and replication by researchers and quantitative analysts.

[Read the knowledge note](posts/post-9040474d7c0d.md) · [Original](https://stockviz.biz/2019/02/26/is-there-a-correlation-between-usdinr-and-tech-stocks-update/) [2019]

## MSCI Country Index Momentum

This article tests cross-sectional momentum on MSCI single-country ETFs, comparing plain momentum and momentum with a trend filter across 50, 100, and 200-day lookbacks. It finds that longer lookbacks improve Sharpe and returns, with the 200-day variant outperforming both the MSCI ACWI and ACWI Momentum indices. The trend filter reduces drawdowns only for long lookbacks. The analysis requires understanding of momentum strategies, rolling Sharpe ratios, SMA filters, portfolio construction, and performance metrics like Sharpe ratio and drawdown. It also involves backtesting and code implementation, indicating advanced applied knowledge.

[Read the knowledge note](posts/post-03a03799b49a.md) · [Original](https://stockviz.biz/2026/06/30/msci-country-index-momentum/) [2026]

## Macro: NIFTY vs. INR/OIL Correlation, Part III

This document explores the macroeconomic correlations and relationships between the Indian NIFTY 50 benchmark index, the USD/INR currency exchange rate, and crude oil prices. Utilizing historical weekly returns and statistical modeling, the study investigates common market narratives regarding oil price shocks and currency fluctuations affecting equities. It demonstrates that while a basic linear regression model fails to capture the dynamic relationship between NIFTY and USDINR due to weak linear correlation and heavy-tailed residuals, advanced visual analysis using return densities reveals directional skews based on rupee appreciation or depreciation. The research provides practical insights into time-series alignment, data frequency limitations, and the complexities of modeling multi-variable macroeconomic interactions in emerging markets.

[Read the knowledge note](posts/post-a8f88c6276d9.md) · [Original](https://stockviz.biz/2018/10/30/macro-nifty-vs-inr-oil-correlation-part-iii/) [2018]

## Game Theory: Rajan vs. Government

This article applies game theory concepts to analyze the strategic relationship between central bank leadership and government fiscal policy makers, specifically focusing on the dynamic between RBI governor Raghuram Rajan and Indian politicians. Using an economic framework derived from Alan S. Blinder's 1982 research paper, the text explores how divergent objectives—such as the central bank's focus on controlling inflation versus politicians' desires for economic expansion and election-winning budgets—create a strategic matrix of preferences. The analysis details outcomes like the end-game scenario where monetary policy remains contractionary while fiscal policy is expansionary, and evaluates the conditions required to reach a stable Nash Equilibrium, offering insights into macroeconomic policy coordination challenges.

[Read the knowledge note](posts/post-845c202050c5.md) · [Original](https://stockviz.biz/2014/07/02/game-theory-rbi-vs-government/) [2014]

## Building Winning Portfolios with SPDR Sector ETFs

This article evaluates quantitative rotation strategies for SPDR sector ETFs, comparing selection criteria (highest Omega, Sharpe, lowest drawdown) and rebalancing frequencies. It assumes familiarity with momentum, rolling windows, performance metrics like Omega Ratio, and backtesting concepts. The analysis includes transaction costs, tax implications for Indian investors, and walk-forward testing, requiring understanding of statistical measures and portfolio construction. The content is applied research with code references, suitable for readers with advanced quantitative finance knowledge.

[Read the knowledge note](posts/post-f42e1551cf1e.md) · [Original](https://stockviz.biz/2026/08/05/building-winning-portfolios-with-spdr-sector-etfs/) [2026]

## The MNC Fund Gravy Train, Part II

This document provides an advanced financial analysis of multinational corporation (MNC) stocks and mutual funds listed in India. It examines the performance of the NIFTY MNC index against broader market benchmarks like the NIFTY 50 and NIFTY Midcap indices, highlighting structural limitations such as shrinking free-float market capitalization from buy-backs and uncaptured dividend yields. The text then compares actively managed MNC funds from Birla Sun Life and UTI against midcap funds, incorporating metrics like cumulative returns, internal rate of return, and drawdowns. Finally, it addresses qualitative risks facing minority shareholders, including parent companies establishing wholly owned subsidiaries, restructuring manufacturing plants, and substantial royalty payouts that potentially hollow out listed entities.

[Read the knowledge note](posts/post-f3f2bef9737a.md) · [Original](https://stockviz.biz/2016/09/07/mnc-fund-gravy-train-part-ii/) [2016]

## Quarterly Results Announcement Day Returns

This document analyzes quarterly results announcement day returns, finding that aggregate return distributions resemble ordinary days, skews between positive and negative surprises fluctuate, and a mechanical overnight strangle selling strategy remains unprofitable. The supporting data includes ticker-wise closing prices in zipped CSV format and a PDF chart book. Additionally, the linked sources discuss whether stocks serve as an inflation hedge in India using historical monthly data from 1994 to 2014, and provide a comprehensive monthly recap of various asset classes including equities, commodities, energy, metals, currencies, agricultural products, ETF performance, yield curves, and investment theme performance metrics for March.

[Read the knowledge note](posts/post-1958b0a51e57.md) · [Original](https://stockviz.biz/2017/03/08/announcement-day-returns/) [2017]

## Factor Holding Periods for Excess Returns

The article analyzes the minimum holding periods required for various equity factor and strategy indices on the NSE, such as low-volatility, quality, momentum, value, and alpha, to consistently yield positive excess returns over the NIFTY 50 TR index. While these factor strategies generally outperform the benchmark index since inception, their excess returns are unevenly distributed over time. The analysis reveals that low-volatility and quality factors have the shortest required holding period of at least five years, whereas alpha and value indices demand roughly ten years of patience. The piece also highlights structural challenges for retail investors implementing DIY strategies, including transaction costs like the securities transaction tax, capital-gains tax, and the current lack of low-cost, liquid ETFs and index funds tracking these specific factors, emphasizing that statistical equity edges require long-term commitment.

[Read the knowledge note](posts/post-d8b5812da4ad.md) · [Original](https://stockviz.biz/2019/05/08/factor-holding-periods-for-excess-returns/) [2019]

## Macro Volatility and the NIFTY 50

This document explores the use of macro market volatility indicators to time the NIFTY 50 index. The author examines whether trading strategies based on the median of 10-day volatility of major world indices can effectively time market entries and exits. The analysis concludes that going long when volatility is below the median and short otherwise only successfully avoids the 2008 crash, but otherwise fails as a reliable timing mechanism. Consequently, observed volatility trading strategies are rejected as ineffective tools for consistent NIFTY trading, adding to a series of exploratory attempts at macro market timing.

[Read the knowledge note](posts/post-c455fe62833e.md) · [Original](https://stockviz.biz/2016/02/10/macro-volatility-and-the-nifty-50/) [2016]

## Mutual Fund Performance in Bear Markets

This article analyzes the performance of 200 equity mutual funds with over 90% allocation during recent bear markets, benchmarking them against the CNX Midcap Index. It evaluates various risk-adjusted metrics, including Information Ratio, Sharpe Ratio, Beta, Bear Beta, drawdown depth, and drawdown length, to identify both the best and worst performing funds. The analysis highlights funds like Birla MNC, Axis Long-term equity, and Mirae Asset Emerging Bluechip as standout performers with low drawdowns and superior returns, while funds such as HSBC Progressive Themes, JM Basic, and Sundram SMILE underperformed significantly. The piece provides an advanced quantitative evaluation of active fund management during market downturns, illustrating how most investment strategies struggle to beat benchmarks in adverse conditions.

[Read the knowledge note](posts/post-76c32b578872.md) · [Original](https://stockviz.biz/2015/06/10/mutual-fund-performance-in-bear-markets/) [2015]

## The Pair Trading Tip-Sheet

This document discusses back-testing results for pair trading strategies, specifically focusing on going long the spread when it is below its historical average for pairs within the same index. It introduces an easy-to-use pair trading tip-sheet that displays end-of-day and live status data side-by-side, including p-values, beta, and spread metrics. The tool is currently preliminary and lacks portfolio tracking capabilities, but it is planned to be made available exclusively to trading and demat customers. The text highlights that trading equity futures carries high risk and requires significant capital, but can yield commensurate returns when executed correctly.

[Read the knowledge note](posts/post-221ae7f06017.md) · [Original](https://stockviz.biz/2014/06/11/pair-trading-tip-sheet/) [2014]

## Can NIFTY be modeled using ARIMA?

This document discusses an empirical analysis investigating whether the NIFTY stock index can be effectively modeled as an ARIMA(1,1,1) process, as asserted by a prior research paper. The author evaluates the best-fit models across rolling windows of different sizes and finds that the majority of the time, the optimal fit is actually ARIMA(0,0,0), which corresponds to white noise. Furthermore, the author tests the forecasting ability of the ARIMA(1,1,1) model by comparing buy-and-hold annualized returns against long and short NIFTY strategies using various look-back periods, revealing a slight return advantage without transaction costs included. Ultimately, the author notes a discrepancy between the best-fit model results and the forecasting performance, concluding that the observed advantages might be random.

[Read the knowledge note](posts/post-aac0264e51f4.md) · [Original](https://stockviz.biz/2017/06/14/can-nifty-modeled-using-arima/) [2017]

## You are always right in some universe

This document explores the fascinating intersection of human decision-making and the mathematical principles of quantum physics through the emergent field of quantum cognition. Researchers propose that human brains do not necessarily act as quantum computers, but rather that the mathematical models governing physical quantum processes can accurately describe cognitive processes and behavioral phenomena. The text highlights how quantum math successfully accounts for anomalies such as the violation of the statistical 'sure thing principle' in human choices, akin to particle interference in a double-slit experiment, as well as context effects where question order alters polling outcomes due to non-commutative matrices. These insights challenge traditional views of human irrationality by providing a rigorous mathematical framework for understanding complex psychological judgments and decision-making behaviors.

[Read the knowledge note](posts/post-bb5a45a57806.md) · [Original](https://stockviz.biz/2014/07/16/always-right-universe/) [2014]

## Benchmarking against a Momentum Index

This document discusses the benchmarking of systematic momentum investment strategies in India. Originally, the lack of dedicated thematic indices forced strategies to use inappropriate market-cap weighted benchmarks. The introduction of the S&P BSE Momentum Index provides a better comparative baseline, despite having a six-month rebalance frequency instead of the monthly rebalance common in academic research. The article evaluates the author's Momo Relative Momentum strategy against this new index, noting that the risk-managed momentum strategy successfully outperforms the momentum index even after accounting for transaction costs.

[Read the knowledge note](posts/post-e83d3c1963ad.md) · [Original](https://stockviz.biz/2018/05/16/benchmarking-momentum-index/) [2018]

## pluto: Your Research Velocity

This document outlines the practical application of pluto, a compute cloud designed for exploratory financial data analysis, aiming to increase research velocity by abstracting away data acquisition and maintenance drudge work. It demonstrates the replication and extension of a VIX-adjusted momentum trading strategy originally designed for the S&P 500 index. Using pluto's Indices datasets and R notebooks, researchers quickly tested whether the strategy could be successfully applied to the NIFTY 50 index across various look-back periods and compared its performance against a simple moving average system. The analysis evaluates annual returns, handles data manipulation using tidyverse and quantitative finance libraries, and explores the impact of market regimes and volatility adjustments on trading signals.

[Read the knowledge note](posts/post-9e24786412d7.md) · [Original](https://stockviz.biz/2019/07/17/pluto-your-research-velocity/) [2019]

## Principal Component Analysis, Part II

This article continues the previous exploration of applying Principal Component Analysis to NASDAQOMX India Total Return indices by testing whether rolling returns could reduce daily noise and reveal clear regime-specific portfolios based on the 200-day simple moving average. The author evaluates factor loadings when the index is both above and below the SMA-200 threshold using rolling returns and lagged periods. Unfortunately, the analysis reveals that neither daily returns nor rolling returns successfully yield a useful, stable regime-specific portfolio for constructing actionable investment strategies. Additional code, charts, and data are provided via the referenced GitHub repository for further advanced financial data analysis and investigation.

[Read the knowledge note](posts/post-6ce1887af4fa.md) · [Original](https://stockviz.biz/2018/10/17/principal-component-analysis-part-ii/) [2018]

## Sector Momentum

This article evaluates a sector momentum rotation strategy on Indian sector indices, using a 6-month look-back and monthly rebalancing. It reports higher absolute returns but lower Sharpe ratios compared to the NIFTY 100, with outperformance concentrated post-2020. The author discusses implementation via index funds/ETFs, caveats about index construction changes, and tests alternative weighting schemes (equal-weight, inverse-volatility, trend-filtered) that do not justify costs. The analysis requires understanding of momentum strategies, portfolio performance metrics (Sharpe ratio), backtesting, and market microstructure (costs, index rules). It also references prior work on S&P 500 sector rotation, indicating a need for familiarity with that context. The level is 4 because it involves applied backtesting and performance evaluation, but not novel research or advanced statistical methods.

[Read the knowledge note](posts/post-8ceb3b6477ff.md) · [Original](https://stockviz.biz/2026/02/18/sector-momentum/) [2026]

## MAD – Moving Average Distance

This article explores the Moving Average Distance (MAD) crossover strategy, applying academic research by Avramov et al. to Indian equities. It demonstrates that the simple moving average crossover signal remains robust against momentum and profitability anomalies. The analysis highlights how the COVID-19 pandemic turbo-charged strategy returns, while pre-COVID equity curves appeared more stable. A critical evaluation reveals significant drawdowns exceeding 25%, prompting the application of a volatility filter. This filter successfully reduces maximum drawdown below 20% at the cost of roughly two percentage points of return. Furthermore, the post conducts an exhaustive parameter search to assess whether the standard 21/200 lookback periods are data-mined, finding they were legitimate pre-COVID but less dominant post-COVID. It also touches upon the utility of rolling Sharpe ratios to avoid misinterpreting strategy performance during bull markets.

[Read the knowledge note](posts/post-aa2dbb13f300.md) · [Original](https://stockviz.biz/2023/12/20/mad-moving-average-distance/) [2023]

## Practical Momentum Part III – Hedging

This document explores the practical implementation of hedging strategies in long-only momentum portfolios to mitigate steep drawdowns. It evaluates two primary hedging approaches: purchasing individual put options and shorting NIFTY futures based on portfolio beta. Through simulation analysis, the text demonstrates that hedging a long-only futures portfolio with single-name put options performs poorly due to tracking differences and the negative impact of time decay. Additionally, the article highlights that increasing portfolio diversification—such as expanding from 10 to 20 stocks—successfully reduces volatility and enhances returns without relying on ineffective derivative hedges, concluding that leveraged momentum presents significant risks of wipe-out.

[Read the knowledge note](posts/post-392de9ae01c7.md) · [Original](https://stockviz.biz/2015/05/20/practical-momentum-part-iii-hedging/) [2015]

## Macro: Using Currencies to Predict NIFTY, Part V

This document series explores advanced quantitative finance techniques, specifically applying Support Vector Machines (SVM) with polynomial kernels to currency indices like DTWEXM, DTWEXB, and DEXINUS (USDINR) to predict future NIFTY 50 returns. Moving across five parts, the analysis progressively refines feature selection, dataset timeframes (comparing 2000-2018 versus 2005-2018 datasets), and model architectures, culminating in an ensemble approach. The final stages incorporate a 50-day Simple Moving Average (SMA) as a regime signifier filtering mechanism to enhance decision matrices, effectively balancing cumulative returns against drawdowns, particularly around major market corrections in 2016 and 2018.

[Read the knowledge note](posts/post-ec7bb34f1409.md) · [Original](https://stockviz.biz/2018/11/21/macro-using-currencies-to-predict-nifty-part-v/) [2018]

## Internal Bar Strength

This document explores the Internal Bar Strength (IBS) trading strategy, which relies on mean-reversion by evaluating the position of a day's closing price relative to its high and low range. It outlines the theoretical framework where low IBS values theoretically correlate with positive future returns and high IBS values with negative returns. The article details a back-test conducted across 16 NSE indices, where buy and sell signals were executed at the close based on specific IBS thresholds. The findings reveal that the strategy underperformed compared to buy-and-hold approaches when tested on Indian market indices, leading to the conclusion that the IBS effect does not reliably hold for the tested equities or markets.

[Read the knowledge note](posts/post-b8cf924cc20e.md) · [Original](https://stockviz.biz/2016/06/22/internal-bar-strength/) [2016]

## SMA Distance, Part III – Backtest

This document outlines the backtest results of a market-timing strategy based on Simple Moving Average (SMA) Distance quintiles for the S&P 500. Two long-only portfolios, designated L1 and L2, were established. L1 goes long if either the 50-day or 100-day SMA Distance falls within the first quintile. L2 adds the condition that the 200-day SMA Distance must not be in the first quintile to prevent over-extended market exposure. The findings reveal that using SMA Distance is generally a poor long-term standalone trading strategy. However, it proves effective at helping avoid deep drawdowns and can successfully serve as a confirming indicator to support other primary trading signals.

[Read the knowledge note](posts/post-536f71cd2ed6.md) · [Original](https://stockviz.biz/2018/12/26/sma-distance-part-iii-backtest/) [2018]

## Analyzing the Analysts

This article analyzes the price targets and ratings given by research analysts for the year 2014, evaluating 718 total ratings. The findings show that most BUY ratings were issued on stocks that had already experienced significant upward movement prior to the announcement, suggesting momentum-chasing behavior by analysts. Conversely, SELL ratings underperformed the BUY pool of stocks in subsequent returns, though they still posted positive short-term returns. The analysis compares previous and next 100-day returns for both categories, along with short-term performance metrics, providing a quantitative evaluation of analyst recommendation efficacy against the CNX 100 index.

[Read the knowledge note](posts/post-2ea4e3629eb7.md) · [Original](https://stockviz.biz/2015/04/29/analyzing-the-analysts/) [2015]

## Volatility and Equity Index Returns

This article evaluates the Omega Ratio as a downside risk measure for equity index exposure, comparing it to traditional volatility metrics like VIX and standard deviation. It details a quantitative strategy that buckets indices into quintiles based on rolling Omega, assigning discrete exposure levels (25% to 100%) to reduce drawdowns. The analysis includes backtests across five Indian indices, sensitivity tests on transaction costs (drag), and lookback selection. Results show significant drawdown reduction and improved Sharpe ratios, but at the cost of lower returns and sensitivity to cost assumptions. The article requires understanding of performance metrics, backtesting, and risk management, making it suitable for advanced practitioners.

[Read the knowledge note](posts/post-02141339b5ab.md) · [Original](https://stockviz.biz/2026/07/29/volatility-and-equity-index-returns/) [2026]

## The Bank Nifty – ICICI Bank Pair

This article examines the pairs trading relationship and spread dynamics between the Bank Nifty index and ICICI Bank nearest to expiry futures using a 50-day look-back period. It defines the spread as the difference between the price of asset A and the hedge ratio beta multiplied by the price of asset B, noting that neither beta nor the relationship is guaranteed to be stable over time. The text highlights that the spread exhibits alternating phases of stability and adjustment, where stability itself can occasionally be viewed as an anomaly. By analyzing these statistical properties and tracking p-values alongside regression coefficients, quantitative traders can better evaluate cointegration and mean reversion for strategy execution.

[Read the knowledge note](posts/post-a84ddd582b67.md) · [Original](https://stockviz.biz/2014/04/30/bank-nifty-icici-bank-pair/) [2014]

## The futility of market timing?

This document examines the concept of market timing by replicating an Albert Bridge Capital study on the Indian NIFTY 50 index, comparing it with findings on the S&P 500. The author evaluates consecutive 10- and 20-year rolling periods starting from 1991, analyzing the terminal wealth generated by investing fixed amounts annually at the yearly high, the yearly low, or on a random day. The analysis highlights how the extreme volatility of the NIFTY 50 creates significant wealth gaps between these timing scenarios, unlike the long-term futility observed in the S&P 500, thereby sustaining investor temptation to time the market in volatile emerging indexes.

[Read the knowledge note](posts/post-762ecf28a08a.md) · [Original](https://stockviz.biz/2019/01/30/the-futility-of-market-timing/) [2019]

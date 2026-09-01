# Trend Following Mindset: Test Plan

Source: Michael W. Covel, *Trend Following Mindset: The Genius of Legendary Trader Tom Basso* (2021 EPUB).

This is a research backlog, not a claim that the book's results will replicate. The book mixes concrete rule descriptions, portfolio-management proposals, and psychology/operations advice. Only the first two groups are directly testable as return hypotheses; the last group should be tested as execution and robustness controls.

The source references below use line numbers from the extracted EPUB text. They are navigation aids, not printed-page numbers.

## Research conventions

- Use simple portfolio P&L, not log P&L.
- Use causal signals: information observed at the close of day t is applied no earlier than day t+1. Test next-day-open and next-day-close implementations separately where the data permits.
- Use the standard StockViz windows: pre through 2019-12-31, post from 2020-05-01, and full history. Select parameters only on the pre window or by a specified walk-forward protocol.
- Charge realistic turnover, roll, slippage and financing costs. Report gross and net results.
- Compare every strategy with buy-and-hold, flat/cash, and a volatility-matched benchmark where relevant.
- Report CAGR, annualized volatility, Sharpe, Sortino, MaxDD, longest drawdown, average drawdown, time in market, turnover, number of trades, win rate, payoff ratio, and the share of total P&L from the best 1/3/5 trades.
- Keep the hypothesis, signal, sizing, exit, and portfolio construction as separate components. Do not let an improved sizing rule be described as an improved entry signal.
- Use at least two instruments/markets and, where possible, a broad cross-asset futures panel. A result on one index is evidence about that index, not about trend following generally.

## Most efficient path with the available data and costs

The efficient approach is staged validation, not implementing all 20 ideas at once. We already have the main daily data and mechanics needed for the first pass: rolled NIFTY futures, the validated synthetic SELECT futures series, TR index closes for MIDCAP 150 and SMALLCAP 250, daily MCX commodity futures in `bhav_com_mcx`, and shared return/futures/chart/metric helpers. The house cost assumption is 25 bps per position turnover, which makes high-turnover variants expensive and makes causal daily implementation more important than adding many indicators.

### Stage 0: freeze the common test harness

Build one reusable daily backtest harness before testing individual ideas. It should accept a price series, a signal, an exit/stop, a sizing rule, and a portfolio-combination rule, and return returns, positions, turnover, and diagnostics. Keep all variants on the same dates and use the same cost accounting.

Use these initial instruments:

- NIFTY rolled futures: the primary futures test with the established roll calendar.
- Synthetic SELECT futures: the second futures test, with its synthetic-series caveat kept visible.
- MIDCAP 150 TR and SMALLCAP 250 TR: cheap close-only robustness checks, not substitutes for futures results.
- Liquid MCX futures: a separate commodity sleeve sourced from `bhav_com_mcx`, initially screening GOLD, SILVER, CRUDEOIL, NATURALGAS, and COPPER for usable history, contract continuity, and liquidity.

Do not start with manager indices or a new broad cross-asset loader. Currency tests are out of scope because currency data is unavailable. The MCX sleeve can be added with the existing SQL source after a focused contract and liquidity audit.

### Stage 1: cheapest high-information tests

Run items 1, 2, 4, 5, and 6 in one producer script, using the exact book-inspired EMA rule as the frozen entry signal. This gives the highest information per run because it tests:

1. Whether the basic timing effect exists in our data.
2. Whether the effect is mainly avoided downside and shorter drawdowns.
3. Whether stop-risk sizing and volatility caps improve the same signal.
4. Whether true range adds value over high-low range.

Use a small pre-declared grid only: the book's exact EMA smoothing constants, 10/50 EMA, one 20-day volatility estimator, and risk budgets of 0.50% and 1.00%. Add 0.25% and 2.00% only if the initial sizing results are informative. Do not run a large EMA/lookback sweep at this stage.

For every candidate, calculate net results immediately at 25 bps. Also retain gross and 10/50 bps sensitivity as diagnostics. A high-turnover result that works only below 25 bps should be rejected before further research.

### Stage 2: isolate the source of any edge

Run item 3 only after the harness and risk controls are trusted. Use the same trailing exit and sizing for random-entry, EMA-entry, and breakout-entry variants. Start with NIFTY and SELECT; run many random seeds but keep the exit and sizing fixed. This separates entry information from the book's stronger claim that a few large trends plus disciplined exits drive the result.

Then run item 7 with only a small, pre-declared comparison: EMA crossover, Donchian breakout, and the existing reference signal. Avoid stacked filters. At 25 bps, the most useful outputs are trade count, average holding period, turnover, and the fraction of P&L from the largest winners—not just Sharpe.

### Stage 3: expand breadth only if Stage 1 survives

If at least one low-turnover, risk-controlled signal shows a stable post-window improvement in drawdown or risk-adjusted return on both NIFTY and SELECT, extend items 8-10 to a broader panel containing the screened MCX commodities. If not, stop expanding the universe and document the negative result; broader data will not repair a core signal that is already cost-sensitive or non-causal.

The broad-panel run should be one shared data build followed by cheap replays of the signal/sizing variants. Do not fetch and rebuild data separately for each hypothesis. First test equal-weight diversification, then volatility-balanced diversification. Only after those are understood should market selection or trend-speed analysis be added. Keep MCX contracts in their own data-loading and roll-validation block so a commodity data defect cannot contaminate the equity-futures conclusions.

For the MCX screen, use the existing `bhav_com_mcx` fields and retain only contracts/series that pass all of these checks: sufficient observations in each evaluation window, no long stale-price runs, a reproducible front/continuous construction, positive and plausible prices, and enough activity to support the intended position size. Start with the most liquid surviving contracts rather than selecting commodities by backtested return. Report the selected universe and exclusions in the findings file.

### Stage 4: portfolio overlays after component behavior is known

Run items 11-13 using the saved return streams from the best Stage 1-3 components. This avoids recomputing prices and signals. The efficient order is:

1. Equal weight with and without monthly rebalancing.
2. Inverse 20-day extreme-volatility allocation.
3. The combination of inverse volatility and monthly rebalancing.
4. Low-Sharpe/low-correlation sleeve allocations.

Use fixed 10% as the primary low-Sharpe sleeve test and a small 5%-15% sensitivity band, rather than scanning every possible allocation. Charge 25 bps on actual weight changes; otherwise the allocation conclusion will be overstated.

### Stage 5: no-new-data diagnostics

Implement item 14, the ETR Comfort Ratio, and item 15, the abandonment simulation, directly on the existing strategy equity curves. These are inexpensive and useful even when return alpha is absent because they test whether the lower-drawdown interpretation is meaningful for adherence.

Run item 9's volatility/trend-environment attribution alongside the first results rather than as a separate expensive project. It uses the same daily returns and ranges and can explain why a signal works or fails without changing the signal.

### Stage 6: defer data-heavy and operational extensions

Leave items 16, 18, and 19 until a candidate strategy survives the preceding stages:

- The MCX sleeve needs a separate contract-continuity, lot-size, margin, and roll-cost audit. Currency overlays are not planned because currency data is unavailable.
- Disaster tests are most useful on a stable candidate portfolio, not on every discarded signal.
- Missed-signal and outage tests should be applied to the final execution design.

Item 17, flat-to-cash versus short hedges, can be run earlier as a cheap replay once Stage 1 produces a trusted signal. It should remain a separate overlay test, since the existing evidence base favors removing exposure over automatically adding a short leg.

### Stop/go gates

After each stage, use these gates:

- Stop a variant if it relies on look-ahead, stale/frozen prices, or an unverified roll.
- Stop a high-turnover variant if its post-window result disappears at 25 bps and there is no compensating risk benefit.
- Do not expand to the broad panel if the two established futures series disagree because of a data or execution problem that has not been resolved.
- Advance a component only if the result is directionally consistent across the pre/post/full windows or its regime dependence is explicit.
- Keep all shortlisted variants for the next stage; select final parameters only on the train window or by walk-forward testing.

This ordering gives the fastest answer to the practical question: does Basso's combination of simple trend signals, exits, and conservative sizing improve the risk/comfort profile of the NIFTY and SELECT books after our 25 bps cost structure? It postpones new data engineering until the existing data has either supported or ruled out the core mechanism.

## Priority 0: establish the core claims

### 1. Reproduce Basso's simple EMA timing rule

Source: "Time Stocks Spent in Up, Down and Sideways Markets (2018 Update)" and "Timing the Market Revisited" (lines 1854-1907 and 1919-2045).

Hypothesis: a fast/slow EMA timing rule gives up some upside during persistent bull markets but materially reduces drawdown depth and drawdown duration.

Test variants:

- Exact book rule: fast EMA smoothing constant 0.30, slow constant 0.05; long when fast > slow, otherwise cash/T-Bills.
- The later explanatory rule: 10-day EMA versus 50-day EMA.
- The earlier 9-day versus 41-day equivalent described in the up/down/sideways study.
- Buy-and-hold and a cash benchmark.
- Long/flat first; add long/short only as a separate follow-up, since the book's main timing example is long/flat.

Diagnostics:

- Classify each completed signal episode as up, down, or sideways using the book's +/-5% outcome threshold.
- Measure the fraction of time and trades in each class, whipsaw loss, missed upside, avoided downside, and recovery time.
- Test whether the stated qualitative result survives costs and a one-day execution lag.

Acceptance criterion: preserve the result even when the exact parameter choice, execution convention, and sample window change. Do not optimize the EMA pair before this reproduction is complete.

### 2. Test whether the value comes from avoiding the worst days

Source: "Timing the Market Revisited," especially the best/worst-day analysis (lines 1993-2039).

Hypothesis: timing's practical benefit is concentrated in reducing exposure during a small number of extreme losses, rather than in producing higher average returns every month.

For each strategy, compare:

- Total return and compounded return.
- The 10 best and 10 worst daily returns.
- Best/worst rolling 250-day periods.
- Maximum drawdown, longest drawdown, and recovery time.
- Results with the best and worst days removed only as a descriptive decomposition, never as a trading rule.

Required control: use the same dates and return engine for timing and buy-and-hold. The decomposition must not be used to claim tradable hindsight.

### 3. Is entry direction less important than exits and sizing?

Source: interviews and the Chat With Traders interview (lines 896-908 and 2287-2301).

Hypothesis: random entries can be weakly profitable when paired with sufficiently disciplined trailing exits and risk control, because a few large trends dominate total P&L.

Design:

- Across a broad futures panel, generate reproducible random long/short entries when flat.
- Compare random entry, EMA/breakout entry, and always-long/always-short controls.
- Use identical trailing exits, position sizing, and turnover assumptions across entries.
- Run many random seeds; report the distribution, not one favorable seed.
- Attribute P&L to small losses, ordinary winners, and the largest winners.

Guardrails:

- No reusing the same random seed as a result selector.
- No optimization of the exit after seeing the random-entry results.
- Separate the test of entry information from the test of trend persistence.

A positive median result across markets and seeds would support the claim. A result driven by one or two seeds or one market would not.

## Priority 0: risk control and position sizing

### 4. Stop-distance risk sizing

Source: "Risk Control System — New position risk" (lines 2047-2058).

Hypothesis: sizing each new position so the distance from entry to stop represents a fixed fraction of equity produces more stable risk than fixed-contract sizing.

Implement:

- Entry supplied by a fixed signal, initially the exact EMA rule and then a simple breakout.
- Stop supplied by a fixed, causal rule; begin with a 10-day closing-price trailing stop as in the example.
- Position size = floor(account equity x risk budget / dollar risk per contract).
- Test risk budgets such as 0.25%, 0.50%, 1.00%, and 2.00% per new position.
- Include contract multipliers, minimum tradable units, gaps through stops, and cash for unallocated capital.

Compare fixed-contract, volatility-sized, and stop-risk-sized portfolios. Report realized loss at stop, portfolio heat, concentration, and the frequency of size reductions.

### 5. Volatility and margin caps on ongoing positions

Source: interviews and "Ongoing risk exposure" (lines 304-310, 366-369, 1112-1120, and 2059-2079).

Hypothesis: the smaller of stop-risk size, volatility-to-equity size, and margin-to-equity size prevents one market from dominating portfolio risk, especially during volatility shocks.

Implement three independent caps:

- Stop-risk cap: current distance to stop as a percentage of equity.
- Volatility cap: 20-day EMA of dollar high-low or true range per contract divided by equity.
- Margin cap: face-value or required-margin exposure divided by equity.

Test:

- Each cap alone.
- The minimum of all three, the book's conservative rule.
- Initial-position limits versus more permissive existing-position limits.
- Static size versus daily re-sizing, with explicit turnover and a rule for rounding down.

Stress cases must include overnight gaps and limit moves. The main outcome is not just CAGR; it is whether the portfolio survives and keeps trading after the shock.

### 6. True range versus high-low volatility

Source: "Measuring Futures Volatility — Daily volatility" (lines 2068-2079) and the interview's 20-day high-low description (lines 2175-2185).

Hypothesis: including yesterday's close in true range gives better protection against overnight gaps than a high-low-only estimator, at an acceptable cost in turnover and foregone exposure.

Compare:

- High-low dollar range.
- True range: max(today high, yesterday close) minus min(today low, yesterday close).
- 10-, 20-, and 40-day causal averages, with the 20-day version as the book-inspired baseline.

Evaluate gap days separately. Do not select the lookback on the post window.

## Priority 1: trend construction and market breadth

### 7. Simple trend following versus many trend implementations

Source: interviews: price as the core variable, simple rules, and multiple trend portfolios (lines 292-302, 1006-1016, 1134-1154, and 2205-2259).

Hypothesis: simple price-based rules are robust across markets, while additional filters increase degrees of freedom and may remove the few large winners that pay for the strategy.

Compare, with identical sizing and exits:

- EMA crossover.
- Donchian/range breakout.
- Moving-average breakout or price-versus-average.
- The existing Bézier/turbulence signal as an unrelated technical comparator.
- Versions with no filter, one volatility filter, and many stacked filters.

Use parameter neighborhoods rather than a single optimized setting. Count how often filters suppress the top 1% of profitable trades and how many trades they eliminate.

### 8. Trend speed and the compression of market cycles

Source: interviews on faster information flow and potentially faster trends (lines 2261-2269).

Hypothesis: shorter signal horizons perform relatively better in more recent, faster markets, but the relationship should be visible in turnover, holding time, and whipsaw statistics rather than assumed from calendar time.

Compare fixed EMA/breakout horizons over rolling eras. Use a walk-forward design and report:

- Median holding period.
- Trade frequency.
- Trend episode length.
- Gross and net return after costs.
- Performance by volatility and trend-strength bucket.

Do not infer that faster is better merely because a recent period has higher returns.

### 9. Volatility and trend-following returns

Source: interviews and "Study of Time Spent in Trending and Sideways Markets" (lines 404-422, 1144-1150, 1616-1663, and 1894-1907).

Hypothesis: trend-following returns are positively related to market movement/volatility and are weak or negative in low-direction, sideways markets.

For each market-month:

- Compute realized range/true-range volatility.
- Measure directional movement or trend efficiency.
- Join those variables to the strategy's next-month return.
- Test rank correlations, bucketed averages, and regressions using only information known before the return period.

The key distinction is volatility alone versus volatility with direction. High volatility with no direction should not be labeled a favorable trend environment.

### 10. Broad diversification versus concentrated market selection

Source: interviews on trading 80 markets to catch the few large winners (lines 402-410, 446-450, and 2207-2217).

Hypothesis: broad market coverage improves the probability of catching a small number of outsized trends, but indiscriminate markets can dilute returns and add costs.

Construct portfolios with:

- One market.
- A small hand-defined diversified panel.
- Equal-weight broad cross-asset panel.
- Volatility-balanced broad panel.
- A restricted panel selected before the test period.

Measure the proportion of total P&L contributed by the best trades and markets, the probability of missing the major winners, concentration, correlation, turnover, and capacity proxies. Selection must be fixed or walk-forward; do not choose markets by full-sample winner contribution.

## Priority 1: portfolio construction

### 11. Monthly rebalancing and inverse extreme-volatility allocation

Source: "The Value Added of Asset Allocation Combined with Rebalancing" (lines 1773-1834).

Hypothesis: monthly rebalancing to target weights improves risk-adjusted performance, and combining rebalancing with inverse extreme-volatility allocation improves comfort/risk more than either alone.

Use a multi-strategy or multi-market return panel and compare:

1. Equal weights, no rebalancing.
2. Equal weights, monthly rebalancing.
3. Initial inverse 20-day extreme-volatility weights, no rebalancing.
4. Inverse-volatility target weights, monthly rebalancing.
5. Optional volatility-balanced weights as an external benchmark.

Define extreme volatility explicitly before running the test: maximum or high quantile of absolute daily returns/ranges over a trailing 20-day window. Test reasonable alternatives as robustness variants, not as post-hoc winners.

Charge trading costs on weight changes and show whether the benefit survives net costs. Preserve the distinction between allocation alpha and exposure reduction.

### 12. Adding a low-Sharpe, low-correlation sleeve

Source: "Adding Low Sharpe Ratio Investments Can Increase Your Sharpe Ratio" (lines 1436-1460).

Hypothesis: a lower-Sharpe trend-following sleeve can improve the total portfolio if its correlation and tail behavior are sufficiently different; the candidate allocation may be around 5%-15%, with 10% as the book-inspired test point.

Test allocations from 0% to 100% in fixed increments, with 10% pre-specified as the primary hypothesis. Use actual strategy sleeves or independently constructed return streams, not smoothed manager indices where possible.

Report:

- Portfolio Sharpe, Sortino, CAGR, MaxDD, drawdown duration, and tail loss.
- Correlation instability across rolling windows.
- Marginal contribution to risk and return.
- Whether the allocation benefit survives costs and out-of-sample testing.

Do not call a sleeve diversifying merely because its full-sample correlation is low.

### 13. Fixed allocation versus adaptive allocation

Source: the same allocation study and the interviews' discussion of strategy personalities (lines 1006-1016).

Hypothesis: strategies with different trend horizons and drawdown personalities can be combined more effectively than several near-duplicates, especially when allocations are rebalanced.

Build a controlled set of short-, medium-, and long-horizon trend sleeves. Compare:

- Equal-weight static allocation.
- Monthly rebalanced equal weights.
- Volatility-target allocation.
- Correlation-aware allocation, only as a secondary benchmark.

Check whether diversification comes from genuinely different return timing or simply different leverage levels.

## Priority 1: comfort, drawdowns, and investor behavior

### 14. Implement and evaluate the ETR Comfort Ratio

Source: "The ETR Comfort Ratio" (lines 1682-1771).

Hypothesis: a metric combining drawdown magnitude and time spent in drawdown ranks strategies more consistently with likely investor adherence than Sharpe or MAR alone.

Implement exactly the book's two-threshold concept:

- Drawdown return threshold, primary value 10% and robustness values 5% and 20%.
- Drawdown time threshold, primary value six months/approximately 126 trading days and robustness values 63 and 252 days.
- Accumulate discomfort while either threshold is exceeded.
- Accumulate comfort during new-high/surge periods.
- Ratio = accumulated comfort / accumulated discomfort.

Compare strategies by ETR, MAR, average drawdown, longest drawdown, and actual subsequent abandonment proxies where data exists. Check sensitivity to daily versus monthly sampling. Treat the ratio as a comparative ranking, not an absolute probability of investor success.

### 15. Does lower drawdown improve persistence at the cost of CAGR?

Source: timing studies and the psychology discussion (lines 1634-1663, 1905-1909, and 2031-2039).

Hypothesis: investors may prefer a lower-return, lower-drawdown timing portfolio, and that preference can make realized investor outcomes better even when buy-and-hold has higher backtested CAGR.

This is partly a behavioral simulation:

- Define abandonment rules based on drawdown depth, duration, or both.
- Apply them to buy-and-hold, timing, and trend portfolios.
- Measure the return actually realized under each abandonment rule versus the uninterrupted backtest.
- Vary the abandonment rule without choosing it from the results.

This does not prove human behavior, but it tests whether the book's proposed mechanism is plausible.

## Priority 2: commodities, hedges, and regime overlays

### 16. Commodity diversification and a speculative trend-following sleeve

Source: interviews on trading many markets and catching a small number of large trends (lines 402-410, 446-450, and 2207-2217), together with the book's discussion of risk-balanced portfolios (lines 304-310 and 2175-2185).

Hypothesis: adding a small, liquid MCX commodity trend-following sleeve to the equity-futures portfolio improves diversification and return-to-risk because commodity trends are not perfectly synchronized with Indian equity trends.

Use the screened MCX contracts from Stage 3, initially testing:

- Equal-weight commodity sleeve.
- Volatility-balanced commodity sleeve.
- Combined NIFTY/SELECT/MCX portfolio with no allocation overlay.
- The same combined portfolio with monthly rebalancing and with inverse 20-day extreme-volatility allocation.

Use fixed commodity sleeve allocations of 5%, 10%, and 20%, with 10% as the primary test. Size individual contracts using the same stop-risk and volatility caps as the equity futures. Include MCX lot sizes, contract multipliers, expiry rolls, roll costs, margin, overnight gaps, and the 25 bps turnover drag. Report the marginal contribution of commodities to CAGR, Sharpe, MaxDD, drawdown duration, and tail losses. Do not call the sleeve diversifying based only on correlation; verify whether it improves the combined net portfolio.

### 17. Flat-to-cash versus short hedges during down regimes

Source: timing sections and the interviews' discussion of hedges (lines 1638-1646, 1380-1392, and 2381-2393).

Hypothesis: reducing or removing long exposure may be more reliable than adding a short leg, particularly after costs and gap risk.

Compare:

- Long/flat timing.
- Long/short reversal of the same signal.
- Long plus a separately sized trend-following hedge.
- Buy-and-hold.

Use the same signal dates and classify short-leg P&L on the position in effect, not the newly generated target. Report whether the hedge earns positive standalone P&L, reduces drawdown, or merely increases turnover.

## Priority 2: robustness and operational tests

### 18. Disaster and gap-risk stress testing

Source: interviews on planned disaster days and the Iraq/oil gap (lines 1096-1120).

Hypothesis: a strategy that appears safe at daily close can still be vulnerable to overnight gaps, data outages, or order-execution failures; explicit stress tests reveal whether the sizing rules leave enough capital to continue.

Stress the complete portfolio with:

- One-day gaps through stops.
- Multi-market correlated gaps.
- Volatility multiplying by 2x, 4x, and 8x.
- Delayed execution by one or more sessions.
- Missing data and stale prices.
- Failure to transmit one rebalance or stop order.

Measure loss, margin breach, forced liquidation, recovery time, and whether the strategy resumes with valid signals. This is a survivability test, not a forecast.

### 19. Execution reliability and missed-signal sensitivity

Source: the silver missed-trade story and TrendStat's backup procedures (lines 346-356, 910-932, and 1096-1108).

Hypothesis: operational failures can dominate small improvements in signal quality, so a robust strategy should degrade gracefully when a trade is delayed or missed.

Simulate:

- Randomly missing a small percentage of signals.
- Missing the largest winner, largest loser, or an entire high-volatility week.
- One-day and three-day order delays.
- Stop execution at next open versus next close.
- Data outage with last-known signal held until recovery.

Report the distribution over many missing-event draws. The important output is sensitivity to rare missed winners and whether diversification reduces that sensitivity.

### 20. Process adherence versus outcome evaluation

Source: the psychology sections and the repeated distinction between following the strategy and making money on one day (lines 330-342, 1120-1124, 2303-2327).

Hypothesis: evaluating daily process adherence separately from daily P&L reduces destructive strategy changes during normal drawdowns.

Create a paper-trading audit table containing:

- Signal generated.
- Order required.
- Order executed.
- Position and stop after execution.
- Whether the rule was overridden.
- Daily P&L and whether the day was inside the expected simulation range.

Test decision rules for when a strategy may be changed: only after a pre-specified deviation from expected behavior, not after an arbitrary losing streak. The backtest should compare uninterrupted execution with simulated human overrides such as taking profits early, skipping uncomfortable entries, and adding filters after drawdowns.

## Suggested implementation order

1. Exact EMA timing reproduction with causal execution.
2. Stop-risk sizing and true-range/volatility caps.
3. Random-entry versus rule-entry experiment.
4. Broad-market diversification and P&L concentration.
5. Monthly rebalancing and inverse extreme-volatility allocation.
6. ETR Comfort Ratio and drawdown-adherence simulation.
7. Volatility/trend-environment attribution.
8. Flat-to-cash versus short hedge.
9. MCX commodity sleeve and cross-asset allocation.
10. Operational and disaster stress tests.

## What would count as a convincing result

A book-inspired idea is worth carrying forward only if:

- The rule is specified before looking at the test result.
- The result survives causal execution and realistic costs.
- The direction is stable across instruments and time windows, or the failure is clearly explained by an identified market condition.
- The result is not dependent on one or two trades unless the strategy explicitly claims convexity; in that case, report the concentration honestly.
- Parameter neighborhoods behave reasonably, rather than one isolated optimum winning.
- The result survives a walk-forward or untouched holdout period.
- Risk and comfort metrics agree with the claimed mechanism.

Do not treat the book's historical numbers as benchmarks to match mechanically. Several examples use old S&P 500/T-Bill data, smoothed CTA or fund-of-funds indices, and assumptions that differ from the current StockViz futures data. The strongest reusable contribution is the separation of signal, position sizing, portfolio construction, execution reliability, and trader psychology into distinct testable layers.

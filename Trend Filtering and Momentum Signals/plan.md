# Trend filtering and momentum signals: India and US backtest plan

Status: plan only. No backtest code or results are produced by this document.

Paper reviewed: Shenhao Zhang, “A Low-Frequency Quantitative Trading Strategy Based on Trend Filtering and Momentum Signals: Empirical Evidence from A-Share Banking Stocks,” ICDEIT 2025, ACM DOI 10.1145/3788763.3788797.

Primary local source:

```text
/mnt/ssd1/stockviz/R2/backtests/Trend Filtering and Momentum Signals/3788763.3788797.pdf
```

Publisher source: https://dl.acm.org/doi/full/10.1145/3788763.3788797

## 1. What the paper actually does

The paper proposes a long-only, low-frequency, event-driven strategy for individual Chinese bank stocks. It combines four ideas:

1. A long-term trend filter using a 120-day simple moving average.
2. A shorter-term EMA momentum condition using a 12-day EMA and a 30-day EMA.
3. RSI-based pullback confirmation using a 7-day RSI.
4. ATR-based risk control, including initial and trailing stops, risk-based position sizing, and target-volatility scaling.

The paper does not present a conventional cross-sectional momentum portfolio. It backtests the strategy separately on two A-share bank stocks:

- China Merchants Bank, 600036.SSE
- Ping An Bank, 000001.SZ

The reported sample is January 2021 through April 2023, with RMB 1,000,000 initial capital. It reports seven trades for Ping An Bank and nine for China Merchants Bank. The paper reports total returns of about 15.5%, annualized returns of 6.61%, and maximum drawdowns of 6.82% and 9.94%, respectively. The reported Sharpe ratios are approximately 0.82 and 0.66.

The paper describes the strategy as low frequency because it generates few trades. This is different from the monthly cross-sectional momentum experiment currently in this folder: the paper evaluates each stock every trading day, enters when its event conditions are met, and exits through stops or a trend reversal. It does not select the top N stocks by a cross-sectional momentum score.

## 2. Signal construction in the paper

### 2.1 Long-term trend filter

A long entry is permitted only when the closing price is above the 120-day simple moving average:

```text
close[t] > SMA(close, 120)[t]
```

This is intended to keep the strategy on the long side of the dominant trend and suppress signals during sideways or structurally weak periods.

The paper varies this window in a robustness check:

- MA100
- MA120, described as the baseline in the robustness table
- MA150

The reported interpretation is that a shorter moving average reacts faster but produces more noise, while a longer moving average delays confirmation and generates fewer signals.

### 2.2 EMA momentum condition

Within the long-term uptrend regime, the paper uses:

```text
EMA12[t] > EMA30[t]
```

The 12-day EMA is treated as the fast momentum measure and the 30-day EMA as the medium-term reference. The paper calls this a momentum breakout or momentum structure. The published methodology does not give a separate price-breakout level such as a 20-day high; the explicit condition shown is the fast EMA being above the slow EMA.

This distinction matters. The paper's “breakout” language should not be silently implemented as a Donchian breakout. The primary replication must use EMA12 > EMA30. A separate price-breakout variant may be useful later, but it is not the paper's stated rule.

### 2.3 RSI pullback confirmation

The paper adds a 7-day RSI as a pullback or reversal confirmation. Its description says that only momentum signals accompanied by adequate momentum amplitude should be accepted. The paper's tables refer to an RSI pullback threshold, but the exact state-transition rule is not fully specified in the accessible text. In particular, it does not unambiguously state whether entry means:

- RSI[t] below a threshold;
- RSI crossing upward through a threshold;
- RSI having been below the threshold recently and recovering above it; or
- a different pullback condition combined with EMA12 > EMA30.

The paper reports two conflicting baseline references:

- The parameter table lists `rsi_threshold = 45`.
- The robustness discussion calls 35 the baseline and varies the threshold over 25, 35, and 45.

The paper reports that the tested threshold results were all profitable and that 35 was described as the most stable setting in that robustness table. This is an internal specification inconsistency, not a reason to choose one value silently.

The implementation plan therefore treats the RSI rule as an explicit replication decision:

Primary paper-parameter arm:

```text
trend_ok[t] = close[t] > SMA120[t]
ema_ok[t]   = EMA12[t] > EMA30[t]
entry[t]    = trend_ok[t] and ema_ok[t]
              and RSI7[t-1] <= RSI_threshold
              and RSI7[t] > RSI_threshold
```

The crossing formulation is the least ambiguous causal representation of “pullback confirmation”: RSI was at or below the threshold and then recovered above it while the longer trend and EMA conditions were positive. It is an operationalization, not a claim that the paper explicitly wrote this exact Boolean expression.

A second sensitivity arm should implement a threshold-state version:

```text
entry[t] = trend_ok[t] and ema_ok[t] and RSI7[t] <= RSI_threshold
```

The plan must report both if the paper's exact source code cannot be obtained. The findings must label them “RSI recovery-cross interpretation” and “RSI low-state interpretation,” rather than merging them into one result.

The first implementation should use RSI threshold 45 because it is the value in the paper's parameter table. Thresholds 25, 35, and 45 should then be tested as a declared robustness grid, with the 35-vs-45 discrepancy documented.

### 2.4 Entry timing

The paper describes daily OHLCV data and an event-driven vn.py implementation, but the accessible article does not fully specify whether a signal observed at the close is executed at that same close, the next open, or another order event.

The StockViz replication must use the conservative causal convention:

```text
indicators through close t
signal decided after close t
order executed at the next observed session open
```

A same-close execution must not be used in the primary result. If open prices are unavailable for a source or a particular history, the study must stop or use an explicitly labeled next-session close proxy. It must not silently use close t to earn return t.

### 2.5 ATR risk control

The paper uses a 14-day Average True Range:

```text
ATR14 = average true range over the previous 14 trading days
```

The parameter table gives:

```text
atr_len        = 14
stop_atr_mult  = 1.2
trail_atr_mult = 1.0
```

The stated initial stop is:

```text
initial_stop = entry_price - 1.2 * ATR14_at_entry
```

The trailing stop is described as:

```text
trailing_stop = highest_price_since_entry - 1.0 * ATR14
```

The article does not fully specify whether the trailing ATR is frozen at entry or recomputed daily. The replication should use a recomputed causal ATR, with the stop for day t set using information through t-1. A frozen-entry-ATR variant should be retained as a sensitivity check.

The paper also exits when the price falls below the long-term MA120. The primary exit priority should be:

1. Apply a gap-aware stop at the next session open if the open is below the effective stop.
2. Apply the intraday stop using that day's low, without using the same day's high to improve the stop before checking the low.
3. Apply the MA120 reversal exit at the next executable price after a close below MA120.
4. Keep the position otherwise.

For a daily OHLC backtest, stop ordering is not fully observable inside a bar when both the stop and another event occur. Use a conservative rule and record it. If the day's low breaches the stop, execute at the stop price unless the opening price is below the stop, in which case execute at the opening price. Do not use the day's close as an artificially favorable stop fill.

### 2.6 Position sizing

The paper reports `lot_size = 100 shares` for its Chinese implementation. That constraint is not needed for this StockViz study. Use whole-share positions with no 100-share-lot restriction in both India and the US, subject only to any actual instrument-specific exchange constraint discovered during data validation.

The stated logic is to cap monetary loss per trade as a fixed percentage of current portfolio capital and divide that risk budget by the ATR-based per-share stop distance:

```text
risk_budget    = equity[t] * 0.015
per_share_risk = 1.2 * ATR14_at_entry
raw_shares     = risk_budget / per_share_risk
shares         = floor(raw_shares)
```

The paper also applies target-volatility scaling:

```text
vol_len    = 60 trading days
target_vol = 0.12 annualized
```

The article does not spell out the exact order of operations between risk-per-trade sizing and volatility scaling. The replication should make it explicit:

```text
vol60 = annualized volatility of daily close-to-close returns through t-1
vol_scale = target_vol / vol60
vol_scale = clipped to a declared range
shares = floor(raw_shares * vol_scale)
```

The primary range should be `0 <= vol_scale <= 1.0`, so target-volatility control only reduces the ATR-risk position and does not introduce leverage. A separate uncapped or leverage-permitted variant must not be mixed into the primary result.

The final position must also respect a portfolio-level gross exposure cap. For a single-stock replication the cap is 100% of equity. For a multi-stock portfolio, use an explicit cap and report any risk-budget residual that cannot be invested. Position sizing must be based on equity and indicators known before execution; it must not use the next day's return or next day's realized volatility.

### 2.7 Warm-up

The paper says that rolling indicators use a warm-up equal to the longest indicator window plus a 20-day buffer. The longest stated window is the 120-day trend filter, while volatility uses 60 days and ATR uses 14 days.

Use this as the minimum warm-up:

```text
120 + 20 = 140 prior trading observations
```

The actual first tradable date must also satisfy all data requirements for the stock, including the 365-calendar-day momentum benchmark described below. Warm-up must be calculated separately for each symbol. No indicator may be backfilled across a listing start, suspension gap, or missing-price interval.

## 3. What the paper does not establish

The paper is useful as a transparent technical-strategy specification, but its evidence is narrow:

- It tests two Chinese bank stocks, not a broad stock universe.
- The sample is only January 2021 to April 2023 for the main results.
- The reported number of trades is very small: 7 to 9 per stock.
- It reports no broad-market stock-selection comparison.
- It does not demonstrate that the strategy beats a properly matched buy-and-hold or basic momentum benchmark.
- The exact RSI entry state, signal-to-order timing, trailing ATR update convention, and intraday stop ordering are not fully specified.
- The accessible text contains inconsistent RSI baseline references: 45 in the parameter table and 35 in the robustness narrative.
- The out-of-sample description says 2021–2023 in-sample and 2023–2024 out-of-sample, which overlaps at 2023 and is not a clean non-overlapping split as written.
- Some reported table values contain apparent formatting or transcription errors, including the China Merchants Bank maximum-drawdown row.

The StockViz study must preserve these caveats. It should test whether the mechanism survives a larger universe and longer history, not present the paper's two-stock evidence as established generality.

## 4. Cross-market backtest scope

Create the following study under:

```text
/mnt/ssd1/stockviz/R2/backtests/momentum_cidce_eff/trend_filter_paper/
```

Use separate subdirectories:

```text
trend_filter_paper/india/
trend_filter_paper/us/
```

Run the same causal engine and the same signal definitions in both markets. Keep market-specific data loading, trading calendars, lot conventions, and transaction-cost assumptions visible in each market's build script.

### 4.1 India universe

Primary universe:

- Indian cash equities with valid adjusted OHLC history.
- Historical point-in-time eligibility from the available StockViz universe data.
- Free-float market-cap screens at the decision date, using the latest record on or before that date.
- Test both top 60% and top 70% free-float market-cap universes, matching the prior `momentum_cidce_eff` work.
- Hold the top 20 eligible stocks by the declared paper signal score only if a cross-sectional ranking is needed for portfolio construction.

The paper itself is not a top-20 cross-sectional model. Therefore implement two India portfolio forms:

1. Stock-level paper replication: every eligible stock can hold an independent long position subject to portfolio exposure and position limits.
2. Portfolio adaptation: rank stocks by the paper's current signal strength and hold the top 20 within FF60 and FF70.

The stock-level form is the primary paper replication. The top-20 form is the apples-to-apples portfolio adaptation alongside the current momentum/CID-CE study.

Use adjusted NSE OHLC prices for returns and indicators. Verify whether the adjusted source has internally consistent adjusted O/H/L/C. If only adjusted close is reliable, do not calculate ATR from a mixture of adjusted and unadjusted fields; either use a fully adjusted OHLC source or mark the ATR arm unavailable for that source.

### 4.2 US universe

Primary universe:

- Historical S&P 500 constituents only.
- Point-in-time constituent membership from `SP500_CONSTITUENTS`.
- Use the latest constituent snapshot on or before each decision date.
- No current-membership backfill into earlier dates.

Implement the same two portfolio forms:

1. Stock-level paper replication across all eligible historical S&P 500 constituents.
2. Top-20 paper-signal adaptation within the point-in-time S&P 500 universe.

Use US daily OHLC data from the existing StockViz US source. Confirm split/dividend adjustment conventions before using the series for both momentum and ATR. Do not mix adjusted close for returns with unadjusted high/low for stops without documenting and testing that choice.

## 5. Benchmarks and comparison arms

Every market must include the following benchmark arms:

### 5.1 Basic 12-month momentum benchmark

This is the benchmark already used in `momentum_cidce_eff`:

```text
formation = close[t] / latest close on or before t-365 calendar days - 1
rebalance = monthly
holding   = one month
skip      = none
```

For India, report `Momentum_FF60` and `Momentum_FF70`. For the US, report `Momentum_SP500`.

Use the same top-20 construction, trading-cost conventions, and date windows as the corresponding paper-adaptation portfolio wherever comparison is made. The benchmark is not risk-sized by ATR and does not use stops.

### 5.2 Paper signal without risk controls

Run:

- MA120 filter only
- MA120 + EMA12/EMA30
- MA120 + EMA12/EMA30 + RSI

All three should use fixed equal weight or a clearly stated fixed-risk portfolio. This isolates the incremental effect of the paper's signal components before adding stops and volatility scaling.

### 5.3 Full paper strategy

Run the complete strategy:

```text
MA120 trend filter
+ EMA12 > EMA30
+ RSI pullback confirmation
+ ATR14 initial stop
+ ATR trailing stop
+ MA120 reversal exit
+ 1.5% ATR risk sizing
+ 60-day volatility scaling toward 12%
```

Retain separate variants for the two RSI interpretations if the paper's exact trigger remains unavailable.

### 5.4 Risk-control ablations

At minimum compare:

- Full signal, no stops, no volatility scaling
- Full signal + ATR stops, fixed position sizing
- Full signal + ATR-risk sizing, no target-volatility scaling
- Full signal + target-volatility scaling, no ATR stops
- Full strategy

This prevents a low drawdown result from being attributed to the signal when it is primarily caused by reduced exposure.

## 6. Causal timing and no-look-forward rules

These rules are mandatory:

1. Compute MA, EMA, RSI, ATR, volatility, momentum, free-float ranks, and constituent membership using information available through the decision close only.
2. A close-t signal cannot earn the return from close t to close t+1 unless the execution model explicitly buys at the next session open and the open-to-close return is used.
3. The primary execution is next-session open.
4. A stop used during session t must be frozen from information known before session t. Do not update a trailing stop using the day's high and then test the same day's low.
5. A gap through a stop executes at the open, not at the stale stop level.
6. A signal is not forward-filled across a missing decision date unless the strategy explicitly holds an existing position; new entries require a fresh valid signal.
7. Free-float and constituent records must be selected with `record_date <= decision_date`.
8. Cross-sectional scores must be ranked only within the point-in-time eligible universe.
9. The portfolio must use actual observed trading dates. Do not create synthetic returns for missing sessions.
10. Stop, position, and turnover state must be carried separately by symbol and must be auditable in an event ledger.

The build must include automated assertions for these conditions, including checking that every entry timestamp is after the signal timestamp and that no feature row uses a price dated after its decision date.

## 7. Sample windows and selection protocol

Use the StockViz house reporting convention:

```text
pre:  through 2019-12-31
post: from 2020-05-01
full: entire valid sample
```

Do not use 2020-01-01 as the post start. The 2020-01 through 2020-04 interval is excluded from both pre and post to keep the comparison convention consistent with the existing study.

For parameter selection:

- The primary paper parameter set is fixed before inspecting Indian or US results.
- Do not select RSI, MA, ATR, trail, or volatility parameters using post-2020 results.
- Use a declared pre-2020 training/validation design for any selected adaptation.
- A simple fixed-parameter replication does not need a selector, but its post period remains a holdout.
- If a parameter is selected, save a selection log with the cutoff, candidate grid, eligible data, selected arm, and application interval.
- Do not call the overlapping 2023/2024 split in the paper a valid out-of-sample split. Replace it with non-overlapping StockViz windows.

Recommended parameter grid for robustness, not unrestricted optimization:

```text
RSI threshold:       25, 35, 45
MA trend window:     100, 120, 150
ATR stop multiplier: 1.0, 1.2, 1.5
ATR trail multiplier: 0.75, 1.0, 1.5
Volatility target:    0.08, 0.12, 0.16
Volatility window:    40, 60, 90
```

Select at most one primary robustness axis at a time in the first pass. A full Cartesian sweep would create a large multiple-testing burden and should not be presented as confirmation.

## 8. Costs and market-specific execution

The paper reports 0.1% commission and 0.0005 slippage, and its Chinese implementation uses 100-share lots. Those assumptions cannot be transplanted unchanged into both markets; the StockViz adaptation uses whole-share positions without imposing a 100-share lot constraint.

India:

- Use the existing StockViz India equity drag convention as the primary portfolio cost.
- Add explicit entry and exit cost components where the event-driven engine supports them.
- Report brokerage, taxes, exchange charges, and slippage separately if available; otherwise state the bundled assumption.
- Do not force 100-share lots for Indian equities. Use one-share lots unless a security-specific lot constraint is present.

US:

- Use the existing US equity drag convention as the primary cost.
- Model whole-share positions.
- Report a separate sensitivity with 10, 25, and 50 bps round-trip-equivalent assumptions or the project's established per-turnover convention.

For both markets, save gross returns, cost returns, net returns, turnover, number of entries, exits, stop exits, MA exits, and average exposure. The paper's low trade count makes cost assumptions look less important; the broad-universe adaptation may trade much more frequently, so this must be measured rather than assumed.

## 9. Required outputs

Each market directory should contain:

```text
build.R
render.R or a shared render consumer
checkpoint.rds
positions_daily.csv
trades.csv
daily_returns.csv
event_ledger.csv
metrics.csv
metrics_pre.png
metrics_post.png
metrics_full.png
cum_dd_pre.png
cum_dd_post.png
cum_dd_full.png
parameter_sensitivity.csv
coverage_audit.csv
```

The charts must use the shared cumulative-plus-drawdown helper. Every chart must show the benchmark and strategy variants with end labels. Produce separate pre, post, and full charts. Metric tables should color-code CAGR and Sharpe so that higher values are greener and MaxDD so that less-negative values are greener.

The event ledger must include at least:

```text
SYMBOL
SIGNAL_DATE
EXECUTION_DATE
ENTRY_PRICE
EXIT_DATE
EXIT_PRICE
EXIT_REASON
ATR_AT_ENTRY
INITIAL_STOP
TRAIL_STOP
MA120_AT_SIGNAL
EMA12_AT_SIGNAL
EMA30_AT_SIGNAL
RSI7_AT_SIGNAL
VOL60_AT_SIGNAL
SHARES
GROSS_EXPOSURE
TURNOVER
COST
```

The coverage audit must report, by market and period:

- symbols and constituent observations;
- valid OHLC observations;
- indicator warm-up loss;
- missing or stale prices;
- number of eligible signals;
- number of entries and exits;
- stop exits versus MA exits;
- average and maximum holding duration;
- average position size and exposure;
- percentage of dates with an active position;
- number of stocks with no valid history.

## 10. Findings report requirements

Create a final `findings.md` next to the market folders. It must first restate the paper's actual design and the ambiguities identified above. Then report:

1. Whether the paper signal improves on the basic 12-month momentum benchmark.
2. Whether improvement survives pre and post windows.
3. Whether ATR stops reduce MaxDD without destroying CAGR or Sharpe.
4. Whether target-volatility sizing changes the result or only reduces exposure.
5. Whether India and US results agree.
6. Whether FF60 and FF70 change the India conclusion.
7. Whether stock-level and top-20 adaptations tell the same story.
8. How many trades and independent signal episodes support the conclusion.
9. The sensitivity to RSI interpretation, MA window, ATR multipliers, and costs.
10. Any data coverage or execution assumptions that could change the result.

Use “positive but underpowered” when results are directionally favorable but trade or regime counts are too small for a strong conclusion. Reserve “failed” for a result that is negative, unstable, or clearly dominated after the declared comparison.

## 11. Recommended implementation sequence

1. Probe and document India and US OHLC schemas, adjusted-price conventions, constituent history, and free-float history.
2. Build a synthetic unit-test fixture covering an entry, a gap through the stop, an intraday stop, a trailing-stop update, an MA exit, a missing-price date, and a constituent change.
3. Implement causal indicator calculation and verify representative rows independently.
4. Implement the stock-level event-driven paper strategy with fixed paper parameters.
5. Implement the basic momentum and signal-ablation benchmarks.
6. Implement the India FF60/FF70 top-20 adaptation and the US S&P 500 top-20 adaptation.
7. Add ATR-risk sizing and target-volatility scaling as separate, auditable layers.
8. Run pre/post/full metrics and produce cumulative-plus-drawdown charts.
9. Run the declared robustness grid without selecting on the holdout.
10. Reconcile charts, metrics, trades, and event ledgers; inspect rendered charts and tables.
11. Write findings from the final CSVs, not from console output or an earlier run.

## 12. Interpretation standard

The intended test is not “does a technical indicator make money?” The useful questions are narrower:

- Does the MA120 + EMA12/EMA30 + RSI structure select better entry episodes than basic momentum in a broad, point-in-time stock universe?
- Do ATR exits reduce drawdown after accounting for gap execution and costs?
- Does volatility targeting improve risk-adjusted returns, or merely reduce participation?
- Are any improvements stable across India and the US, across FF60/FF70, and across pre/post periods?

The paper's evidence supports treating this as an interpretable baseline to test, not as a validated production strategy. The original sample is short, concentrated in two bank stocks, and contains underspecified execution and RSI details. The StockViz extension should preserve the paper's stated parameters, make every ambiguity explicit, and judge the result against the already-built basic momentum benchmark.

## References

- Zhang, S. (2025/2026 publication record), “A Low-Frequency Quantitative Trading Strategy Based on Trend Filtering and Momentum Signals: Empirical Evidence from A-Share Banking Stocks,” DOI: 10.1145/3788763.3788797. https://dl.acm.org/doi/full/10.1145/3788763.3788797
- Existing comparison study: `/mnt/ssd1/stockviz/R2/backtests/momentum_cidce_eff/findings.md`
- Existing India/US build conventions: `/mnt/ssd1/stockviz/R2/backtests/momentum_cidce_eff/india/build.R` and `/mnt/ssd1/stockviz/R2/backtests/momentum_cidce_eff/us/build.R`

## Appendix A. Allocating 1.5% risk per trade

The paper's `risk_per_trade_pct = 0.015` is a loss budget, not a 1.5% capital allocation. For each new position, calculate the intended loss at the initial ATR stop and size the position so that this loss is approximately 1.5% of current portfolio equity.

### A.1 Risk budget and share count

```text
risk_budget = current_equity × 0.015
```

The initial stop uses the paper's ATR multiplier:

```text
per_share_risk = ATR14_at_entry × 1.2
```

The unrounded position size is:

```text
raw_shares = risk_budget / per_share_risk
```

Round down to a whole share:

```text
shares = floor(raw_shares)
```

Do not impose the paper's 100-share lot size on the India or US adaptations. Record whole-share rounding and any genuine instrument-specific constraint separately.

### A.2 Worked example

Suppose:

```text
portfolio equity = 1,000,000
ATR14            = 4.00
stop multiplier   = 1.2
entry price       = 100.00
```

Then:

```text
risk budget       = 1,000,000 × 0.015 = 15,000
risk per share    = 4.00 × 1.2 = 4.80
raw shares        = 15,000 / 4.80 = 3,125
position notional = 3,125 × 100.00 = 312,500
```

The initial stop is:

```text
initial_stop = entry_price - 1.2 × ATR14
             = 100.00 - 4.80
             = 95.20
```

If the stop is reached exactly, the intended gross loss is approximately 15,000, or 1.5% of equity, before commissions, slippage, taxes, and any rounding effect.

### A.3 Target-volatility adjustment

The paper also specifies 60-day volatility and a 12% target volatility. Apply this as a separate scaling layer after the ATR-risk position has been calculated:

```text
vol60     = annualized volatility of daily returns through t-1
vol_scale = target_vol / vol60
```

For the primary no-leverage implementation:

```text
vol_scale = min(1, vol_scale)
final_shares = floor(raw_shares × vol_scale)
```

This allows volatility targeting to reduce the ATR-sized position but does not allow it to increase the position above the 1.5%-risk size. Any leverage-permitted variant must be a separate, explicitly labeled experiment.

The volatility estimate must use only observations available before execution. A close-t signal executes at the next session open, so the estimate must be frozen using data through close t, not recalculated using the execution day or any future return.

### A.4 Portfolio-level limits

A 1.5% risk budget per position can exceed total portfolio capacity when many positions are open. The implementation must therefore define and report:

- maximum gross exposure;
- maximum number of simultaneous positions;
- whether new entries are rejected, reduced, or queued when the portfolio is full;
- how risk budgets are scaled when several signals arrive together;
- unused risk budget caused by whole-share rounding or exposure limits.

The primary multi-stock adaptation should cap gross exposure at 100% of equity unless a separate leveraged variant is declared. It should not silently allocate 1.5% risk to every signal and allow the resulting notional exposure to exceed the cap.

### A.5 Stops, gaps, and realized loss

The 1.5% calculation is an intended loss at the initial stop, not a guarantee of realized loss. A gap below the stop can produce a larger loss. The event-driven engine should use:

```text
if next_open < effective_stop:
    exit at next_open
else if intraday_low <= effective_stop:
    exit at effective_stop
```

The stop for a session must be determined from information available before that session begins. Do not use the current day's high to improve a trailing stop and then use the same day's low to trigger it; that introduces intraday ordering lookahead.

The event ledger should report both:

```text
intended_stop_risk = shares × per_share_risk
actual_gross_loss   = shares × (entry_price - exit_price)
```

It should also report commissions, slippage, gap exits, and the difference between intended and realized loss. This distinction is especially important for volatile Indian and US stocks.


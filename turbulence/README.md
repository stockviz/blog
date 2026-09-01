# Market Turbulence Study

## Overview

This folder contains exploratory tests of the market-turbulence strategy described in:

- `mathematics-12-01416.pdf` — Zheng and Dong, *Quantum Temporal Winds: Turbulence in Financial Markets*.
- `strategy.md` — the local interpretation of the paper.

The work tests the strategy on NIFTY futures, the synthetic SELECT futures series, and the NIFTY MIDCAP 150 TR and NIFTY SMALLCAP 250 TR indices.

The paper does not fully specify the Bézier fitting procedure, turning-point detector, turbulence threshold, position sizing, exits, or trading costs. Those choices are therefore explicit in the scripts and should be treated as research assumptions rather than as a precise reproduction of the paper.

All reported trading results use simple daily returns and 25 bps of drag per unit traded.

## Folder structure

- `simple/` — the original close-based reversal implementation.
- `regime-aware/` — fixed-parameter comparison of reversal, breakout, and trend-filtered variants on NIFTY and synthetic SELECT futures.
- `walk-forward-ohlc/` — combined annual walk-forward re-selection with causal OHLC turning points.
- `midcap-indices/` — trend-filtered reversal long/flat applied to MIDCAP 150 TR and SMALLCAP 250 TR.
- `strategy.md` — local interpretation of the source strategy.
- `mathematics-12-01416.pdf` — source paper.

Outputs are written beside the script that produced them. Each study produces pre, post, and full metrics where sufficient history exists, matching cumulative-plus-drawdown charts, daily output files, and parameter sweeps.

## Common conventions

The standard reporting windows are:

- `pre`: through 2019-12-31;
- `post`: from 2020-05-01;
- `full`: the complete available sample.

The period from 2020-01-01 through 2020-04-30 is excluded from the named pre/post comparison.

The close-based implementations use this causal procedure:

1. Take the prior rolling channel history.
2. Split it into three equal segments.
3. Use the maximum of each segment as upper control points and the minimum of each segment as lower control points.
4. Evaluate a quadratic Bézier curve at `t = 0.75` to form the upper and lower boundaries.
5. Define turbulence as `abs(MA20 - MA50) / close >= threshold`.
6. Use only information available at the previous close to generate the next day’s position.
7. Charge 25 bps for each unit of position change.

The trend-filtered reversal long/flat variant goes long after a turbulent lower-boundary event. A turbulent upper-boundary event exits to flat only when the lagged 50-day moving average is not rising. Upper-boundary events during a rising MA50 are ignored, preventing new short exposure during persistent advances.

NIFTY futures use the canonical futures roll calendar. SELECT futures use the validated synthetic MIDCAP SELECT front-month close series. The index study uses the TR index close series directly.

## Experiment 1: simple reversal

Script: `simple/turbulence_futures.R`

This is the direct mean-reversion reading of the paper:

- upper boundary: short signal;
- lower boundary: long signal.

It is reported as both long/flat and long/short. The strategy assumes that an upper-boundary touch represents resistance and a lower-boundary touch represents support.

This interpretation performed poorly after 2020, particularly on SELECT. Sustained advances caused repeated upper-boundary events to be interpreted as short signals, which is the opposite of what was needed in the post-period bull-market sample.

## Experiment 2: regime-aware comparison

Script: `regime-aware/turbulence_futures_regime_aware.R`

This experiment keeps the same close-derived channel and turbulence filter but compares different interpretations of the boundary events:

- Reversal Long/Flat — lower boundary enters long; upper boundary goes flat.
- Reversal Long/Short — lower boundary enters long; upper boundary enters short.
- Breakout Long/Flat — upper boundary enters long; lower boundary goes flat.
- Breakout Long/Short — upper boundary enters long; lower boundary enters short.
- Trend-Filtered Reversal Long/Flat — reversal logic with rising-MA50 short suppression.
- Trend-Filtered Reversal Long/Short — the same filter while retaining the short arm.

### Fixed-parameter post-period results

Values are CAGR / Sharpe / MaxDD, with 25 bps drag.

| Instrument | System | CAGR | Sharpe | MaxDD |
|---|---|---:|---:|---:|
| NIFTY | B&H | 15.7% | 1.05 | 17.2% |
| NIFTY | Reversal Long/Flat | 4.9% | 0.54 | 16.5% |
| NIFTY | Reversal Long/Short | -6.2% | -0.35 | 52.0% |
| NIFTY | Breakout Long/Flat | 9.6% | 0.86 | 17.2% |
| NIFTY | Breakout Long/Short | 2.9% | 0.27 | 28.3% |
| NIFTY | Trend-Filtered Reversal Long/Flat | 15.7% | 1.05 | 17.2% |
| NIFTY | Trend-Filtered Reversal Long/Short | 15.7% | 1.05 | 17.2% |
| SELECT | B&H | 27.6% | 1.33 | 24.7% |
| SELECT | Reversal Long/Flat | 3.5% | 0.33 | 23.0% |
| SELECT | Reversal Long/Short | -17.9% | -0.89 | 72.2% |
| SELECT | Breakout Long/Flat | 22.2% | 1.42 | 17.2% |
| SELECT | Breakout Long/Short | 15.0% | 0.80 | 24.5% |
| SELECT | Trend-Filtered Reversal Long/Flat | 24.5% | 1.25 | 24.7% |
| SELECT | Trend-Filtered Reversal Long/Short | 21.1% | 1.07 | 33.9% |

The strongest fixed-parameter post-period candidate is SELECT Breakout Long/Flat. It slightly exceeds B&H on Sharpe and reduces drawdown, but it does not exceed B&H on CAGR.

## Experiment 3: combined walk-forward and OHLC test

Script: `walk-forward-ohlc/turbulence_walk_forward_ohlc.R`

This is the more demanding validation. For each instrument, the script:

1. trains on the preceding five years;
2. selects both the system interpretation and the channel/threshold;
3. applies that choice unchanged to the next one-year test period;
4. repeats this through the available history;
5. stitches the non-overlapping test years into one out-of-sample series.

The candidate set includes all reversal, breakout, and trend-filtered variants. Turning points use daily highs and lows with a two-bar confirmation delay, and signals remain lagged by one day.

### Data treatment

NIFTY uses observed rolled futures OHLC from `BHAV_EQ_FUT`.

SELECT retains the validated synthetic futures close. MIDCAP SELECT PR OHLC is scaled to that close where available. Before MIDCAP SELECT PR OHLC coverage begins, NIFTY 50 OHLC is used as an explicit range proxy and scaled to the synthetic SELECT close. Consequently, the SELECT turning-point logic is OHLC-based, but its pre-real-OHLC range is synthetic rather than observed SELECT futures OHLC.

### Walk-forward results

Values are CAGR / Sharpe / MaxDD, with 25 bps drag.

| Window | Instrument | Walk-forward selected | B&H |
|---|---|---:|---:|
| Pre | NIFTY | 5.7% / 0.43 / 37.5% | 12.9% / 0.64 / 60.1% |
| Pre | SELECT | -4.6% / -0.27 / 25.9% | 9.9% / 0.58 / 21.7% |
| Post | NIFTY | 9.4% / 0.74 / 17.2% | 15.7% / 1.05 / 17.2% |
| Post | SELECT | 12.8% / 0.87 / 20.9% | 27.6% / 1.33 / 24.7% |
| Full | NIFTY | 5.2% / 0.40 / 38.7% | 12.3% / 0.64 / 60.1% |
| Full | SELECT | 5.6% / 0.45 / 26.9% | 16.1% / 0.82 / 45.1% |

Walk-forward re-selection improves risk control relative to the original fixed reversal specification, especially on SELECT, but it does not beat buy-and-hold on CAGR or Sharpe. The year-by-year choices are in `walk_forward_selection.csv`; Breakout Long/Flat is selected most often.

## Experiment 4: MIDCAP 150 TR and SMALLCAP 250 TR

Script: `midcap-indices/turbulence_midcap_trend_filtered.R`

This applies only Trend-Filtered Reversal Long/Flat to each TR index. Parameters are selected independently through 2019-12-31:

- MIDCAP 150 TR: 42-day channel, 5% turbulence threshold.
- SMALLCAP 250 TR: 42-day channel, 5% turbulence threshold.

The TR close is used for both P&L and the close-derived channel, matching the existing regime-aware implementation.

### Results

Values are CAGR / Sharpe / MaxDD, with 25 bps drag.

| Window | Instrument | Strategy | B&H |
|---|---|---:|---:|
| Pre | MIDCAP 150 TR | 13.4% / 0.71 / 68.1% | 15.1% / 0.76 / 72.9% |
| Pre | SMALLCAP 250 TR | 13.0% / 0.68 / 65.2% | 13.0% / 0.67 / 75.6% |
| Post | MIDCAP 150 TR | 4.1% / 0.54 / 13.9% | 28.5% / 1.56 / 21.1% |
| Post | SMALLCAP 250 TR | 30.9% / 1.54 / 26.6% | 30.9% / 1.54 / 26.6% |
| Full | MIDCAP 150 TR | 9.3% / 0.58 / 68.1% | 17.5% / 0.88 / 72.9% |
| Full | SMALLCAP 250 TR | 16.2% / 0.82 / 65.2% | 16.2% / 0.81 / 75.6% |

The MIDCAP 150 result is very different from SELECT because these are different instruments and different data series. SELECT refers to the narrower synthetic SELECT futures series, while MIDCAP 150 TR is a broad 150-stock total-return index.

Post-period exposure makes the difference clear:

- MIDCAP 150 TR was long only 24.0% of the time and switched position once. It missed much of the post-2020 advance.
- SMALLCAP 250 TR was long 100% of the time, so its strategy result is effectively buy-and-hold.
- The earlier SELECT trend-filtered result was long roughly 89% of the time and had two position changes.

The strategy is therefore highly path-dependent. A favorable result on SELECT cannot be generalized to MIDCAP 150. The same rule can be mostly flat on one index, permanently long on another, and close to buy-and-hold on a third.

## Short-leg and hedge assessment

The short leg is not effective as a general return engine.

Unfiltered reversal shorting was particularly damaging in the post period:

- NIFTY: -6.2% CAGR, -0.35 Sharpe, 52.0% MaxDD.
- SELECT: -17.9% CAGR, -0.89 Sharpe, 72.2% MaxDD.

Breakout long/short also underperformed breakout long/flat. Post-period Sharpe was 0.27 versus 0.86 for NIFTY and 0.80 versus 1.42 for SELECT.

Trend-filtered shorting reduced the damage but did not add reliable value. For SELECT post, trend-filtered long/short had 1.07 Sharpe and 33.9% MaxDD versus 1.25 Sharpe and 24.7% MaxDD for long/flat. On NIFTY, the two trend-filtered variants were identical post-period because the filter effectively prevented short exposure.

The walk-forward selector chose a long/short system only once for NIFTY, in 2019, and never for SELECT. From 2020 onward, NIFTY selections were also long/flat.

The phrase “occasional hedge” therefore applies only weakly to NIFTY. A trend-filtered short could be tested separately as a defensive overlay in selected regimes, but it was not useful in the post-2020 test. For SELECT, the evidence does not support using the short leg.

## Overall conclusions

1. The direct mean-reversion interpretation is not robust. In persistent advances, upper-boundary touches often behave as continuation rather than reversal signals.
2. Breakout Long/Flat is the strongest fixed-parameter candidate for SELECT’s post-period risk-adjusted performance.
3. Trend filtering can prevent some of the worst reversal errors, but its benefit varies sharply by instrument and sample.
4. Walk-forward re-selection and OHLC turning points make the test more realistic, but they do not produce outperformance versus buy-and-hold after 25 bps drag.
5. The short leg should not be part of the core strategy. Long/flat exposure is the preferred default for both futures studies and the index application.
6. The results are research findings, not verified trading edges. Further work would need realistic execution modelling, instrument-specific data validation, and an independent out-of-sample validation design.

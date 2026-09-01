01 EMA timing, risk sizing, and walk-forward selection

Scope
- EMA lookbacks 20, 50, and 100 trading days.
- Unit exposure versus 15% annualized volatility-capped exposure.
- NIFTY and validated synthetic SELECT daily series.
- Signal is lagged one session and 25 bps is charged on exposure changes.
- Windows use pre <= 2019-12-31, post >= 2020-05-01, and full history.

Surface fixed-parameter screen

The strongest surface result was SELECT EMA(100) unit exposure: post CAGR
20.1%, Sharpe 1.23, MaxDD 18.3%, with 3.3% mean daily turnover. However, the
full-period result was CAGR 9.5%, Sharpe 0.67, MaxDD 31.2%, and the pre-period
Sharpe was only 0.33. It was not accepted without a walk-forward test.

Walk-forward design

`walk_forward.R` selects among EMA(20/50/100) once per calendar year. Each
selection uses only the immediately preceding 756 trading days (approximately
three years), with the selected EMA applied to the next calendar year. The final
walk-forward position is charged 25 bps on every position change, including a
change caused by switching selected EMAs. No post-period result is used in
selection.

Walk-forward results

| Instrument | Window | CAGR | Sharpe | MaxDD | Mean exposure | Mean turnover |
|---|---|---:|---:|---:|---:|---:|
| NIFTY | pre | 9.8% | 0.68 | 33.6% | 65.8% | 4.9% |
| NIFTY | post | 4.6% | 0.46 | 22.0% | 67.7% | 6.9% |
| NIFTY | full | 7.9% | 0.60 | 33.6% | 65.7% | 5.5% |
| SELECT | pre | 4.4% | 0.36 | 27.0% | 66.1% | 8.4% |
| SELECT | post | 18.4% | 1.17 | 21.0% | 71.0% | 4.7% |
| SELECT | full | 11.1% | 0.78 | 27.0% | 68.0% | 6.4% |

The fixed SELECT EMA(100) screen was better in the post sample (20.1% CAGR,
1.23 Sharpe) than the walk-forward selector (18.4%, 1.17), but that is the
expected cost of refusing to use future information. The walk-forward selector
still produced a positive SELECT result with lower full-period drawdown than
buy-and-hold, but did not improve full-period Sharpe enough to establish a
robust edge. NIFTY was not compelling in the post period.

Selected lookbacks

- NIFTY: EMA(50) was selected most often early and late; EMA(100) dominated
  much of 2009-2024; EMA(20) was selected for 2008 and 2025.
- SELECT: EMA(50) dominated 2014-2022; EMA(100) was selected from 2023 onward.

Verdict

SELECT EMA(100) remains a reasonable research candidate and the walk-forward
selector survives as a positive, low-turnover post-period test. It is not a
validated deployable strategy: the edge is concentrated in the post sample,
full-period Sharpe is only 0.78 for the selector, and the instrument is a
synthetic futures series. Close this study at the research-candidate stage.

Files
- `run.R`: fixed-parameter EMA and risk-sizing screen.
- `walk_forward.R`: expanding three-year train / one-year test selector.
- `metrics.csv`, `daily_outputs.csv`: fixed-parameter outputs.
- `walk_forward_metrics.csv`, `walk_forward_daily.csv`,
  `walk_forward_selections.csv`: walk-forward evidence.

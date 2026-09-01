# Trend Following Mindset: Executable Test Program

Source: Michael W. Covel, *Trend Following Mindset: The Genius of Legendary Trader Tom Basso* (2021 EPUB).

This project converts the book-derived backlog in `plan.md` into bounded,
reproducible experiments. The implementation follows the available-data and
cost gates: simple daily P&L, causal one-session signal lag, 25 bps turnover
drag, train/pre <= 2019-12-31, post >= 2020-05-01, and no currency data.

## Folder structure

- `01-ema-risk/` — EMA timing, drawdown-sensitive exposure, and volatility cap.
- `02-entry-exits/` — rule-based versus controlled random-entry tests.
- `03-mcx-diversification/` — MCX contract liquidity and continuity gate.
- `04-comfort/` — ETR comfort ratio and drawdown-adherence diagnostics.
- `plan.md` — full 20-experiment backlog and staged execution order.

Each folder contains its own executable script, README, findings, and generated
CSV evidence. The scripts do not depend on an untracked shared checkpoint.

## Findings

### 1. EMA timing and risk sizing

The post-window unit-exposure results from `01-ema-risk/metrics.csv` show that
longer EMA lookbacks were stronger in this sample and had lower turnover:

| Instrument | EMA | CAGR | Sharpe | MaxDD | Mean turnover |
|---|---:|---:|---:|---:|---:|
| NIFTY | 20 | 4.4% | 0.46 | 19.7% | 10.2% |
| NIFTY | 50 | 4.9% | 0.49 | 20.3% | 7.2% |
| NIFTY | 100 | 7.4% | 0.68 | 21.1% | 4.5% |
| SELECT | 20 | 10.0% | 0.75 | 17.8% | 10.4% |
| SELECT | 50 | 17.1% | 1.13 | 21.0% | 5.6% |
| SELECT | 100 | 20.1% | 1.23 | 18.3% | 3.3% |

The fixed SELECT EMA(100) result is only a surface candidate. Its full-period
Sharpe is 0.67 and pre-period Sharpe is 0.33, so it was tested with a rolling
walk-forward selector before being retained.

### Walk-forward closure

`01-ema-risk/walk_forward.R` selects EMA(20/50/100) once per calendar year from
the immediately preceding 756 trading days and applies it to the next year.
Selection is strictly prior to the test year; the assembled position is charged
25 bps on every exposure change, including selector changes.

| Instrument | Window | CAGR | Sharpe | MaxDD | Mean exposure |
|---|---|---:|---:|---:|---:|
| NIFTY | pre | 9.8% | 0.68 | 33.6% | 65.8% |
| NIFTY | post | 4.6% | 0.46 | 22.0% | 67.7% |
| NIFTY | full | 7.9% | 0.60 | 33.6% | 65.7% |
| SELECT | pre | 4.4% | 0.36 | 27.0% | 66.1% |
| SELECT | post | 18.4% | 1.17 | 21.0% | 71.0% |
| SELECT | full | 11.1% | 0.78 | 27.0% | 68.0% |

The walk-forward SELECT result remains positive but is slightly weaker than the
fixed EMA(100) post result (18.4% CAGR / 1.17 Sharpe versus 20.1% / 1.23).
NIFTY is not compelling post-period. SELECT EMA(100) is therefore retained as
a research candidate, not a validated deployable strategy: the result is
post-period concentrated, the full-period Sharpe is moderate, and the futures
series is synthetic.

### 2. Rule entry versus random entry

The `02-entry-exits` five-seed control rejects random entry in the post window.
Random entries had mean CAGR/Sharpe of -21.1%/-2.15 on NIFTY and -18.9%/-1.41
on SELECT, versus 4.9%/0.49 and 17.1%/1.13 for EMA(50). The tested breakout
variant was also not a candidate: -6.7% CAGR on NIFTY and -0.3% on SELECT.

This isolates a useful result from the book’s broader claims: in this test,
entry information matters, and the result is not reproduced by random timing.

### 3. MCX breadth gate

The `03-mcx-diversification` audit found long histories for all five candidates,
but the current EOD front-series fields are not yet safe for portfolio P&L:

- GOLD: 4,577 pre observations; 1,632 post; maximum stale run 2 days.
- SILVER: 4,566 pre; 1,632 post; 4 moves above 15%.
- CRUDEOIL: 4,240 pre; 1,632 post; an implausible 1323x maximum absolute move.
- NATURALGAS: 3,821 pre; 1,632 post; 18 moves above 15%.
- COPPER: 4,278 pre; 1,632 post; 3 moves above 15%.

The explicit go/no-go gate stops before equal-weight or volatility-balanced
commodity allocation. The next required implementation is contract-safe
roll construction using expiry and multiplier/lot metadata, then a rerun of
the audit. Treating the current CRUDEOIL series as investable would fabricate
returns.

### 4. ETR and adherence

The `04-comfort` diagnostic computes ETR under 5/10/20% depth and 63/126/252
day duration thresholds, at daily and monthly sampling. At the primary 10% /
126-day setting:

- NIFTY B&H: CAGR 11.62%, MaxDD 60.14%, daily ETR 0.15, monthly ETR 0.48.
- NIFTY Timing Long/Flat: CAGR 5.43%, MaxDD 54.27%, daily ETR 0.09, monthly ETR 1.48.
- SELECT B&H: CAGR 14.32%, MaxDD 45.09%, daily ETR 0.17, monthly ETR 0.64.
- SELECT Timing Long/Flat: CAGR 8.54%, MaxDD 26.65%, daily ETR 0.24, monthly ETR 0.87.
- SMALLCAP trend-filtered long/flat: CAGR 16.19%, Sharpe 0.82, MaxDD 65.20%.

Sampling changes ETR materially, so it is a behavioral diagnostic rather than
an alpha-ranking metric. The abandonment simulation shows that simple depth or
duration redemption rules can lock in losses and miss recoveries.

## Plan status and gates

Implemented and run:

- Exact EMA timing rule and lookback sensitivity.
- Rolling three-year train / one-year test EMA walk-forward selection.
- Drawdown/risk exposure decomposition scaffold.
- Controlled random-entry test.
- Rule-entry comparison.
- MCX liquidity/continuity audit.
- ETR comfort ratio.
- Drawdown-adherence/abandonment simulation.

Deferred by explicit gates rather than simulated with unsafe assumptions:

- MCX equal-weight/volatility-balanced allocation until continuous contract
  construction passes the audit.
- Broad futures diversification until commodity continuity is fixed.
- Pyramiding, margin-based sizing, and operational stress tests until a clean
  candidate return stream exists.
- Monthly rebalancing and inverse-volatility portfolio variants until saved
  component return streams are admissible.
- Process-override experiments, which require a pre-registered override log.

This is the efficient staged implementation of the 20-item plan: experiments
that can run on validated streams were executed first; downstream allocation and
operational experiments remain visibly gated instead of being presented as
completed evidence. The project is closed at the research-candidate stage.

## Reproduction

From each experiment folder:

    Rscript run.R

For ETR:

    python3 etr_comfort.py

The MCX script reads `STOCKVIZ_CONFIG` when set, otherwise the standard
`/mnt/hollandC/StockViz/R/config.r`. It never prints credentials. All generated
CSV files in the experiment folders are the evidence used by this README.

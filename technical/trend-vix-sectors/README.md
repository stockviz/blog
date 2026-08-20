# India VIX-Adaptive Trend Following — Sectoral Indices

This project is the sectoral-index variant of
`/mnt/data/blog/technical/trend-vix`. It replaces the three market-capitalisation
indices (NIFTY 50 TR, NIFTY MIDCAP 150 TR, NIFTY SMALLCAP 250 TR) with the
sectoral total-return indices and extends the cross-asset momentum test from
"top 1 / top 2" to "top 1 / top 2 / top 3 / top 4".

## What is tested

The same India VIX-adaptive momentum framework as the market-cap study:

- At every completed month-end, India VIX selects a momentum lookback
  (Green 10-month, Yellow 3-month, Red 1-month; see `PLAN.md` in the parent
  project for the exact thresholds).
- Every available sector index plus the cash proxy is ranked by that lookback.
- The portfolio goes long the top 1, 2, 3, or 4 sectors at equal weight.
- Any selected sector with negative momentum is replaced by cash.
- The fixed 10-month control ranks the same universe but always uses 10 months.

The signal computed at the close of month `t` is applied only during month
`t+1`. Costs are charged on the first trading day of a new holding month at 0,
10, 25, and 50 bps per unit of one-way turnover; the primary comparison uses
25 bps. Daily P&L uses simple arithmetic returns; compounding is used only for
monthly, annual, and cumulative statistics.

## Universe

34 sectoral total-return indices (`classification == "sectoral-indices"` from
the niftyindices.com catalogue), plus Quantum Liquid Fund-Growth Option
(scheme `103734`) as the investable cash proxy:

```text
NIFTY AUTO TR, NIFTY BANK TR, NIFTY CAPITAL GOODS TR, NIFTY CEMENT TR,
NIFTY CHEMICALS TR, NIFTY COMMERCIAL & TRANSPORT SERVICES TR,
NIFTY CONSTRUCTION TR, NIFTY CONSUMER DURABLES TR, NIFTY CONSUMER SERVICES TR,
NIFTY FINANCIAL SERVICES 25/50 TR, NIFTY FINANCIAL SERVICES EX-BANK TR,
NIFTY FINANCIAL SERVICES TR, NIFTY FMCG TR, NIFTY HEALTHCARE TR,
NIFTY HOSPITALS TR, NIFTY HOUSING FINANCE TR, NIFTY INSURANCE TR,
NIFTY IT TR, NIFTY MEDIA TR, NIFTY METAL TR,
NIFTY MIDSMALL FINANCIAL SERVICES TR, NIFTY MIDSMALL HEALTHCARE TR,
NIFTY MIDSMALL IT & TELECOM TR, NIFTY NBFC TR, NIFTY OIL & GAS TR,
NIFTY PHARMA TR, NIFTY POWER TR, NIFTY PRIVATE BANK TR, NIFTY PSU BANK TR,
NIFTY REALTY TR, NIFTY REITS & REALTY TR, NIFTY RETAIL TR,
NIFTY TELECOMMUNICATIONS TR, NIFTY500 HEALTHCARE TR
```

## Staggered inclusion

Sector indices do not all start on the same date. Rather than forcing a common
intersection (which would trim the universe to the latest index's inception,
May 2026), each index enters the investable universe **as and when it becomes
available**:

- Index levels are outer-joined, so each index keeps its own inception date.
- An index is only eligible for ranking once it has a finite momentum score for
  the selected lookback — i.e. once it has `lookback` months of history.
- `top_n` is capped to the number of available assets, so "top 4" degrades
  gracefully to fewer sectors early on, with cash filling the remaining slots.

Start date, train/test split, and warm-up are identical to the market-cap study
(VIX SMA40 warm-up and the 10-month momentum warm-up are the binding
constraints, since almost all sector indices predate India VIX):

- Train/reporting period: through 2019-12-31
- Test period: from 2020-05-01
- Full period: first valid holding month (2007-02 signal) through the latest
  completed month

## Data-quality corrections

The `bhav_index` table carries a few single-day artifacts for some sectoral TR
series. `build.R` neutralises them before any returns are computed and records
each correction in `data-quality-report.txt`:

- Eight spike-and-revert bad ticks (a level that jumps more than ±30% and
  reverts the next day) are carried forward to the prior level — zero return on
  the spike day, correct continuation after it. Affected: NIFTY HEALTHCARE TR,
  NIFTY MIDSMALL FINANCIAL SERVICES TR, NIFTY OIL & GAS TR (five ticks),
  NIFTY PSU BANK TR.
- One re-basing (NIFTY REALTY TR, 2006-12-29, 1684 → 1000) is back-adjusted by
  rescaling the pre-event levels by the jump ratio so the level path is
  continuous.

No whole index is dropped, and no value is fabricated beyond carrying the last
good level across a single bad tick or rescaling a re-based series.

## Coverage

- 30 of 34 sectors have a month-end level at the first test month-end
  (2020-05-29).
- 29 to 33 of 34 sectors are available with 10-month momentum across the test
  period.
- Late starters enter mid-test: NIFTY CONSUMER SERVICES TR (2020-07),
  NIFTY HOSPITALS TR (2021-03), NIFTY REITS & REALTY TR (2021-07),
  NIFTY COMMERCIAL & TRANSPORT SERVICES TR (2026-05).

Top 1 through top 4 are therefore fully populated from the start of the test
period, and the staggered machinery only matters for the handful of late
starters.

## Test-period results (25 bps, from 2020-05-01)

```text
Strategy     CAGR     Sharpe   MaxDD
VIX Top 1   24.17%    0.93    33.99%
VIX Top 2   30.12%    1.29    28.23%
VIX Top 3   30.49%    1.39    27.08%
VIX Top 4   27.80%    1.34    25.86%
10M Top 1   18.99%    0.83    43.83%
10M Top 2   26.52%    1.21    28.23%
10M Top 3   27.57%    1.30    27.08%
10M Top 4   26.85%    1.31    25.86%
```

These are economic backtest results, not statistical conclusions. The VIX
lookback beats the fixed 10-month lookback at every portfolio size in this
sample, and maximum drawdown falls as the number of held sectors rises. The
full metrics tables (CAGR, volatility, Sharpe, MaxDD, Calmar, recovery days,
worst month, positive months) are in `metrics-*.html` for train, test, and full
periods.

## Files

- `build.R` — database extraction, bad-tick cleaning, staggered alignment, and
  fingerprinted cache
- `backtest.R` — regime, momentum, ranking, staggered availability, P&L, and
  cost functions
- `tests.R` — deterministic synthetic tests
- `run.R` — canonical runner: test, build cache, run all 8 strategies
- `analysis.R` — metrics tables, cumulative/annual charts, availability report
- `data-quality-report.txt` — the exact data corrections applied

## Reproduce

From this directory:

```bash
Rscript tests.R
Rscript build.R
Rscript run.R
Rscript analysis.R
```

`run.R` also runs `tests.R` and rebuilds the live cache before running all
strategies. Generated artifacts include `cache.rds`, `backtest-results.rds`,
`audit-monthly.csv`, `daily-returns.csv` / `.rds`, and the chart/table files.
All ggplot outputs carry the `@StockViz` caption and all gt tables use
`tab_source_note("@StockViz")`.

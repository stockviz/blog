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

## Benchmark

A monthly-rebalanced **Equal Weight B&H** portfolio of the full sectoral
universe is included as a buy-and-hold benchmark (no momentum, no VIX
regime, no cash substitution, no transaction costs). Each month it holds
`1 / N_available` in every sector with a finite return on the first trading
day of the month and drifts within the month — the sectoral analogue of the
parent project's equal-weight benchmark. Late starters enter only at the
next monthly rebalance. It is the only benchmark in `daily-returns.csv` /
`primary_daily`; metrics tables compare all 8 trading formulations against it.

## Results (25 bps cost for trading formulations; benchmark is cost-free)

Train period is the in-sample window used to validate that the code runs — no
parameter was tuned on it — but it is the only place to test whether trend
had any unconditional edge over naive diversification before the 2020 regime
shift.

| Strategy         | Train CAGR | Train Sharpe | Train MaxDD | Test CAGR | Test Sharpe | Test MaxDD | Full CAGR |
|------------------|-----------:|-------------:|------------:|----------:|------------:|-----------:|----------:|
| VIX Top 1        |      3.79% |         0.28 |      33.25% |    24.17% |        0.93 |     34.00% |     9.01% |
| VIX Top 2        |      6.71% |         0.43 |      40.56% |    30.12% |        1.29 |     28.23% |    12.09% |
| VIX Top 3        |      8.24% |         0.52 |      34.15% |    30.49% |        1.39 |     27.08% |    13.49% |
| VIX Top 4        |      9.99% |         0.63 |      33.11% |    27.80% |        1.34 |     25.86% |    13.22% |
| 10M Top 1        |      5.32% |         0.35 |      31.94% |    18.99% |        0.83 |     43.83% |     8.30% |
| 10M Top 2        |      8.78% |         0.53 |      31.25% |    26.52% |        1.21 |     28.23% |    12.87% |
| 10M Top 3        |      8.06% |         0.52 |      29.63% |    27.57% |        1.30 |     27.08% |    12.85% |
| 10M Top 4        |     11.84% |         0.73 |      28.18% |    26.85% |        1.31 |     25.86% |    14.39% |
| Equal Weight B&H |     12.93% |         0.82 |      34.25% |    25.11% |        1.49 |     21.14% |    15.36% |

Train: 2009-06-01 to 2019-12-31 (2,625 trading days). Test: 2020-05-04 to
2026-07-31 (1,553 trading days). Full: 2009-06-01 to 2026-07-31 (4,260
trading days).

### Do any formulations beat the benchmark in train?

No — not on the metrics that matter for an unconditional rule.

On CAGR the benchmark wins outright at **12.93%**, ahead of the best
trend formulation (10M Top 4, 11.84%) and well ahead of the best
VIX-adaptive portfolio (VIX Top 4, 9.99%). On risk-adjusted return
the gap is the same: benchmark Sharpe **0.82** exceeds every trend
variant (best is 10M Top 4 at 0.73). Drawdowns do not rescue the story
— trend MaxDDs of 28–40% sit in the same band as the benchmark's 34%.

Annual attribution shows why. Two broad-based rallies account for most
of the shortfall:

* **2012** — Equal Weight +43.7% (NBFC +85.5%, Private Bank +69.0%,
  Midsmall Financial Services +68.6% — almost every sector paid).
  VIX Top 1 managed +7.6% and VIX Top 2 +7.9%; the concentrated picks
  captured only a slice of a breadth rally.
* **2014** — Equal Weight +49.0% (Capital Goods +75.8%, Chemicals +75.7%,
  PSU Bank +69.9%). Trend finished materially negative (VIX Top 1
  −8.7%, 10M Top variants −5% to −11%) — momentum held the wrong
  leaders heading into the year and the negative-momentum-to-cash rule
  did not help because the prior leaders were not negative, just no
  longer winners.

Those two years alone more than offset VIX wins in 2011 (+18pp vs
benchmark), 2013 (+25pp), and 2017 (+4pp). In a universe where 30-odd
sectors can all rally 30–70% in the same year, diversification is the
free lunch and concentration is the cost — independent of whether the
lookback is 1, 3, or 10 months. The daily correlation of VIX Top 1 with
the benchmark is 0.67 in train, so the two series are not independent
bets; trend simply held a subset of what the benchmark already owned.

### What explains the test set?

The ranking flips in test, but only narrowly and for a specific reason.

VIX Top 2 and Top 3 beat the benchmark on absolute return (30.12% and
30.49% vs 25.11%, about +5pp of CAGR), and VIX Top 1/Top 4 also clear it
(24.17% is a whisker below, 27.80% above). The fixed 10M controls beat
the benchmark only at Top 2 and above and by a smaller margin. On
Sharpe, however, the benchmark still leads: its test Sharpe of **1.49**
(at 15.9% annualised volatility) exceeds the best trend Sharpe of 1.39
(VIX Top 3, 20.7% vol). The benchmark also has the smallest MaxDD
(21.1% vs 25.9–34.0% for VIX variants and 27.1–43.8% for fixed).

Year-by-year, the test result is not a uniform trend edge. The
benchmark actually won the single most extreme year:

* **2020** — benchmark +53.4% vs VIX Top 1 +34.0% / 10M Top 1 +33.1%.
  The Covid rebound lifted almost every sector (Midsmall IT & Telecom
  +72%, Pharma +62%, IT +58%); holding 34 names beat holding 1–4.
* **2021** — trend recoups: VIX Top 1 +46.2% vs benchmark +40.6% /
  10M Top 1 +34.0%. Leader: Power +114%, Metals +73%.
* **2022** — the VIX-adaptive edge in isolation: VIX Top 1 +8.0%,
  benchmark +2.1%, 10M Top 1 −5.0%. The Red regime (1-month lookback)
  avoided the sustained losers that the fixed 10-month rule kept
  holding. This single year is the cleanest demonstration of the VIX
  switch.
* **2023** — VIX Top 1 +41.8% (nearly identical to fixed) vs benchmark
  +34.6%. Breadth again, but this time trend held the right tail
  (Realty +82%, Construction +75%).
* **2024–2025** — benchmark reasserts (+20.9% and +6.6% vs VIX Top 1
  +18.5% and +1.7%). Concentration gives back part of the lead.

In other words, test outperformance is **concentrated in 2021–2023 and
within that in 2022's regime switch**, not a persistent premium. The
equal-weight benchmark remains the best risk-adjusted portfolio in both
train and test; trend's test win is absolute-return only, higher-vol
and drawdown-heavier, and depends on a small number of years. Over the
full 2009–2026 window the train shortfall dominates, leaving the
benchmark ahead at 15.36% full-period CAGR versus 13.49% for the best
trend variant (VIX Top 3).

These are economic backtest results, not statistical conclusions. The
full metrics tables (CAGR, volatility, Sharpe, MaxDD, Calmar, recovery
days, worst month, positive months) are in `metrics-*.html` for train,
test, and full periods.

## Files

- `build.R` — database extraction, bad-tick cleaning, staggered alignment, and
  fingerprinted cache
- `backtest.R` — regime, momentum, ranking, staggered availability, P&L, and
  cost functions
- `tests.R` — deterministic synthetic tests
- `run.R` — canonical runner: test, build cache, run all 8 trading formulations plus the Equal Weight B&H benchmark
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

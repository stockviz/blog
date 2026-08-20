# Trend-VIX — Lookback Sensitivity (Train Only)

Blog: [VIX and Trend-following](https://stockviz.biz/2026/08/18/vix-and-trend-following/)

This is a companion experiment to `../trend-vix`. It keeps the asset universe
unchanged — the same three NIFTY total-return indices plus the Quantum Liquid Fund
as cash — and asks a single question on the **training set only**:

> Does changing the momentum lookback window meaningfully change the strategy's
> metrics?

## Plain-English explanation

The parent project ranks four assets (NIFTY 50 TR, NIFTY MIDCAP 150 TR,
NIFTY SMALLCAP 250 TR, and a liquid fund) by how much they have gained over a
recent period, then holds the winner(s) for the next month. The "lookback" is how
many months of past performance are used for that ranking.

The VIX-adaptive strategy changes this lookback between 10, 3, and 1 month
depending on volatility. That raises a natural question: how much does the
lookback choice matter on its own, before volatility even enters the picture?

This experiment answers that by running the strategy with a **fixed** lookback of
1, 2, ..., 12 months, holding everything else identical, and comparing the
train-period metrics across lookbacks. It looks only at data through 2019-12-31,
so nothing here is an out-of-sample result — it is a sensitivity surface.

## Method

- Universe: unchanged from the parent project (`NIFTY 50 TR`, `NIFTY MIDCAP 150 TR`,
  `NIFTY SMALLCAP 250 TR`, Quantum Liquid Fund-Growth, scheme `103734`).
- Strategy: at each month-end, rank the four assets by trailing `L`-month return,
  hold the top 1 (or top 2, equally weighted). If a chosen equity index has a
  negative `L`-month return, that slot goes to cash. No India VIX is used.
- Lookbacks swept: `1:12` months (integers). The 10-month case reproduces the
  parent project's fixed-10M control exactly (verified by assertion).
- Cost: 25 bps per unit of one-way turnover, primary comparison (0 bps also run).
- Period: training set only, through 2019-12-31.
- Alignment: all lookbacks are compared over a **common window** starting at the
  latest first holding date (2007-05-03), so no lookback gets credit for an extra
  stretch of early history that shorter lookbacks could not access.

The cached aligned data has 5,036 common daily rows and 244 completed month-ends.
The common train window is 2007-05-03 through 2019-12-31 (about 152 holding months
per lookback).

## Results (train, 25 bps)

### Top 1

| Lookback (months) | CAGR | Sharpe | Max drawdown | Volatility | Calmar | Monthly turnover | Identical to L10 |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 5.70% | 0.42 | 51.54% | 16.58% | 0.11 | 71.1% | 42% |
| 2 | 9.65% | 0.63 | 54.89% | 16.77% | 0.18 | 55.9% | 46% |
| 3 | 15.04% | 0.94 | 29.48% | 16.26% | 0.51 | 36.2% | 53% |
| 4 | 15.80% | 0.97 | 31.96% | 16.49% | 0.49 | 40.1% | 55% |
| 5 | 11.83% | 0.74 | 44.50% | 17.20% | 0.27 | 38.8% | 55% |
| 6 | 14.19% | 0.86 | 44.50% | 17.10% | 0.32 | 29.6% | 62% |
| 7 | 11.93% | 0.77 | 44.50% | 16.31% | 0.27 | 32.2% | 66% |
| 8 | 11.61% | 0.76 | 44.50% | 16.29% | 0.26 | 29.6% | 71% |
| 9 | 12.24% | 0.79 | 48.01% | 16.35% | 0.26 | 27.6% | 79% |
| 10 | 13.07% | 0.84 | 46.00% | 16.08% | 0.28 | 24.3% | 100% |
| 11 | 11.25% | 0.73 | 47.11% | 16.39% | 0.24 | 23.7% | 84% |
| 12 | 8.34% | 0.57 | 45.61% | 16.49% | 0.18 | 25.0% | 76% |

Spread: Sharpe 0.42–0.97 (0.55), CAGR 5.70–15.80% (10.11 pp), MaxDD 29.48–54.89%
(25.41 pp). The 10-month control ranks 4th of 12 by Sharpe.

### Top 2

| Lookback (months) | CAGR | Sharpe | Max drawdown | Volatility | Calmar | Monthly turnover | Identical to L10 |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 8.85% | 0.61 | 47.78% | 15.85% | 0.19 | 55.3% | 37% |
| 2 | 9.59% | 0.64 | 50.42% | 16.30% | 0.19 | 44.4% | 43% |
| 3 | 15.75% | 1.02 | 30.18% | 15.44% | 0.52 | 29.6% | 53% |
| 4 | 16.86% | 1.06 | 32.28% | 15.82% | 0.52 | 28.3% | 53% |
| 5 | 10.99% | 0.71 | 44.48% | 16.58% | 0.25 | 28.6% | 58% |
| 6 | 13.55% | 0.85 | 44.48% | 16.59% | 0.30 | 20.4% | 63% |
| 7 | 12.91% | 0.85 | 44.48% | 15.78% | 0.29 | 18.4% | 68% |
| 8 | 12.99% | 0.85 | 44.48% | 15.82% | 0.29 | 17.4% | 72% |
| 9 | 12.04% | 0.80 | 48.56% | 15.68% | 0.25 | 15.8% | 84% |
| 10 | 10.70% | 0.73 | 46.89% | 15.58% | 0.23 | 14.1% | 100% |
| 11 | 12.00% | 0.80 | 46.89% | 15.80% | 0.26 | 14.5% | 82% |
| 12 | 10.48% | 0.71 | 46.89% | 15.82% | 0.22 | 14.1% | 73% |

Spread: Sharpe 0.61–1.06 (0.45), CAGR 8.85–16.86% (8.01 pp), MaxDD 30.18–50.42%
(20.25 pp). The 10-month control ranks 8th of 12 by Sharpe.

## Answer

**Yes — the lookback window meaningfully affects the metrics.** On the training
set the differences are large and systematic, not noise:

- The annualized Sharpe ratio varies by **0.45 (Top 2) to 0.55 (Top 1)** across
  the 1–12 month range. That is the difference between a mediocre trend rule and
  a strong one.
- CAGR varies by **8–10 percentage points**, and maximum drawdown by **20–25
  percentage points**, across lookbacks.
- The pattern is a hump, not a monotone line. Very short lookbacks (1–2 months)
  are the worst: they whipsaw, turn over ~55–71% a month, and suffer the deepest
  drawdowns (51–55%). The peak is at **3–4 months** (Sharpe 0.94–1.06, max
  drawdown ~29–32%), after which results decay gradually out to 12 months.
- The 10-month lookback used by the parent's fixed control is **not optimal**:
  it sits mid-pack for Top 1 (4th of 12) and below mid-pack for Top 2 (8th of 12).
  The 3-month lookback — the "Yellow" regime in the VIX rule — is one of the two
  best fixed lookbacks on train.
- Holdings genuinely change with the lookback: a 1-month lookback holds the same
  asset as a 10-month lookback only ~42% of the time, rising to ~76% for a
  12-month lookback.

Caveat: this is a single train sample, and the peak at 3–4 months is an
in-sample observation that would need out-of-sample confirmation before being
treated as a tuned parameter. The point here is the size of the surface, not the
location of its peak.

## Ideal lookback by India VIX regime

"Ideal" here means the 1–12 month lookback with the **highest annualized Sharpe**
within that regime, using 25 bps costs and train-period returns only. The regime is
measured at the signal month-end; the following holding month's return is assigned
to that regime. The original fixed thresholds are unchanged:

- Green: VIX SMA40 `<= 18`
- Yellow: VIX SMA40 `> 18` and VIX SMA20 `< 32`
- Red: VIX SMA40 `> 18` and VIX SMA20 `>= 32`

India VIX begins after the overall strategy history. Consequently, this conditional
analysis has 127 train holding months with an observable signal regime: 70 Green,
53 Yellow, and only 4 Red. (The parent project's count of 71 Green signals includes
the December 2019 signal whose holding return occurs in January 2020, outside the
train return period.)

| Portfolio | Regime | Train months | Best lookback | Sharpe | CAGR | MaxDD | Runner-up | Original rule | Original Sharpe (rank) |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Top 1 | Green | 70 | **10M** | 1.13 | 15.40% | 14.37% | 11M (1.03) | 10M | 1.13 (1/12) |
| Top 1 | Yellow | 53 | **6M** | 0.72 | 11.27% | 12.05% | 9M (0.72) | 3M | 0.57 (7/12) |
| Top 1 | Red | 4 | 9M* | 5.24 | 100.50%* | 0.00%* | 6M (4.57) | 1M | 1.62 (11/12) |
| Top 2 | Green | 70 | **8M** | 1.06 | 14.18% | 15.62% | 11M (1.04) | 10M | 0.92 (4/12) |
| Top 2 | Yellow | 53 | **9M** | 0.81 | 12.02% | 16.96% | 12M (0.79) | 3M | 0.74 (5/12) |
| Top 2 | Red | 4 | 12M* | 4.11 | 62.67%* | 0.00%* | 9M (4.10) | 1M | 3.67 (10/12) |

The regime-conditional CAGR and MaxDD stitch together only months belonging to the
same regime; they are descriptive conditional statistics, not the return path of a
continuously investable standalone portfolio. Sharpe is the selection objective.

### Interpretation

For the primary **Top 1** strategy, the train data supports:

- **Green: 10 months.** The original Green lookback is exactly the train winner.
- **Yellow: about 6–9 months, not 3 months.** Six months has the highest Sharpe,
  but 9 months is essentially tied (0.725 versus 0.721). The original 3-month rule
  ranks only 7th of 12. This does not support speeding all the way up to 3 months
  when VIX is merely elevated.
- **Red: unknowable.** Four observations cannot identify an ideal lookback. The
  apparent 9-month winner and its extreme statistics are sampling noise, not a
  usable parameter estimate. There is no empirical train-set basis here for either
  9 months or the original 1 month.

For **Top 2**, the train winners are 8 months in Green and 9 months in Yellow.
The runner-ups (11 and 12 months) are close, so the evidence favors a slower
8–12 month range rather than a precise single optimum.

Overall, the regime-conditioned result is more conservative than the unconditional
3–4 month peak: calm markets favor a slow 8–10 month signal, and elevated-VIX months
favor 6–9 months. Only the severe Red regime would justify testing a very fast
lookback in theory, but this dataset has nowhere near enough Red observations to
calibrate it.

Full results for all 72 Top-N × regime × lookback cells are in
`regime-lookback-metrics.csv`; the summary is in
`ideal-lookbacks-by-regime.html/.png`.

## Held-out test: Green 10M / Yellow 6M / Red 1M

The Top 1 train sweep selected 10 months in Green and 6 months in Yellow. Red had
only four train observations, so it was **not retuned** and remains at the original
1-month lookback. These choices were frozen before evaluating the test set:

| Regime | Test rule | Reason |
|---|---:|---|
| Green | 10M | Highest Top 1 train Sharpe |
| Yellow | 6M | Highest Top 1 train Sharpe |
| Red | 1M | Original rule retained; insufficient train coverage to optimize |

The held-out comparison runs from 2020-05-01 onward, uses 25 bps per unit of
one-way turnover, and compares the train-tuned 10/6/1 rule with the original
10/3/1 VIX rule and fixed 10M momentum. All three strategies use the same indices,
cash proxy, dates, ranking rule, and transaction-cost treatment.

| Portfolio | Strategy | CAGR | Sharpe | MaxDD | Volatility | Calmar | Avg. monthly turnover |
|---|---|---:|---:|---:|---:|---:|---:|
| Top 1 | Train-tuned 10/6/1 | 15.28% | 1.00 | 32.95% | 15.50% | 0.46 | 25.33% |
| Top 1 | Original 10/3/1 | **19.72%** | **1.19** | 33.79% | 16.23% | **0.58** | 32.00% |
| Top 1 | Fixed 10M | 14.87% | 0.96 | 34.98% | 15.69% | 0.43 | 25.33% |
| Top 2 | Train-tuned 10/6/1 | 17.05% | 1.14 | 25.19% | 14.72% | 0.68 | 19.33% |
| Top 2 | Original 10/3/1 | **19.93%** | **1.27** | 25.19% | 15.21% | **0.79** | 22.00% |
| Top 2 | Fixed 10M | 17.26% | 1.17 | 25.19% | 14.55% | 0.68 | 16.00% |

There are 75 test holding months: 50 Green, 23 Yellow, and only 2 Red. Changing
Yellow from 3M to 6M changes the selected allocation in 9 Top 1 months and 13 Top
2 months.

### Test conclusion

The train-selected 6M Yellow lookback **does not generalize better than the
original 3M Yellow lookback**:

- Top 1 loses 4.43 percentage points of CAGR and 0.19 Sharpe versus original
  10/3/1. It reduces turnover by 6.67 percentage points per month and improves
  MaxDD by 0.84 percentage points, but that does not compensate for the lower
  return.
- Top 2 loses 2.88 percentage points of CAGR and 0.13 Sharpe versus original
  10/3/1, with the same 25.19% MaxDD.
- Against fixed 10M, tuned Top 1 is only marginally better (+0.41 pp CAGR, +0.03
  Sharpe, and 2.03 pp lower MaxDD). Tuned Top 2 is marginally worse (-0.21 pp CAGR
  and -0.02 Sharpe) while producing the same MaxDD.

The defensible result is therefore **not** to replace 10/3/1 with 10/6/1. The 6M
Yellow optimum was an in-sample fit; the faster original 3M signal performed much
better in the held-out period. Red remains unresolved because both train and test
coverage are inadequate.

Generated test outputs:

- `metrics-test.html` / `metrics-test.png` / `metrics-test.csv`
- `cumulative-returns-test.png`

## Files

- `build.R` — unchanged database extraction and fingerprinted cache (same indices
  and cash proxy as the parent project; only the report path and query tag differ)
- `backtest.R` — parent strategy functions plus `run_fixed_lookback_portfolio`
- `tests.R` — deterministic synthetic tests, including the fixed-lookback runner
- `run.R` — builds the cache, runs the lookback sweep, verifies L10 == fixed 10M,
  and carries the frozen 10/6/1 rule into the test comparison
- `analysis.R` — train-only lookback/regime analysis plus held-out test metrics and
  cumulative-return charts
- `README.md` — this file

## Reproduce

```bash
cd /mnt/data/blog/technical/trend-vix-lookback
Rscript tests.R
Rscript run.R
Rscript analysis.R
```

Generated artifacts: `cache.rds`, `lookback-results.rds`, `metrics-top1.html/.png`,
`metrics-top2.html/.png`, `lookback-sharpe.png`, `lookback-cagr.png`,
`lookback-maxdd.png`, `lookback-cumulative.png`, `regime-lookback-metrics.csv`,
`ideal-lookbacks-by-regime.html/.png`, `metrics-test.html/.png/.csv`, and
`cumulative-returns-test.png`.

All ggplot charts carry the `@StockViz` caption; all gt tables use
`tab_source_note("@StockViz")`.

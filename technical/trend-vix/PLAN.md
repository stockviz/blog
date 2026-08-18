# India VIX-Adaptive Trend Framework Implementation Plan

> **For Hermes:** Use the `subagent-driven-development` skill to implement this plan task-by-task.

**Goal:** Test whether an India VIX-dependent momentum lookback improves trend-following results across the NIFTY 50 TR, NIFTY MIDCAP 150 TR, and NIFTY SMALLCAP 250 TR indices, using Quantum Liquid Fund-Growth Option as the investable cash proxy.

**Architecture:** Build a point-in-time daily data cache from StockViz SQL Server, then run a monthly signal/next-month holding backtest. Keep data construction, strategy logic, tests, and analysis separate. The primary experiment is a fixed-parameter replication of the Alpha Architect 10/3/1-month framework; any India-calibrated variation is a separately labelled train-only robustness test.

**Tech stack:** R, RODBC, xts, zoo/TTR, PerformanceAnalytics, ggplot2, viridis, gt, webshot2, digest, and StockViz `Common.R` chart helpers.

---

## 1. Research question and pre-registered hypotheses

### Primary question

Does changing the momentum lookback according to India VIX improve performance relative to a fixed 10-month momentum strategy?

### Risk assets

- `NIFTY 50 TR`
- `NIFTY MIDCAP 150 TR`
- `NIFTY SMALLCAP 250 TR`

### Cash proxy

- Table: `mf_nav_history`
- Scheme code: `103734`
- Scheme: `Quantum Liquid Fund-Growth Option`
- Columns: `SCHEME_CODE`, `SCHEME_NAME`, `NAV`, `AS_OF`
- Verified coverage: 2006-04-09 through 2026-08-17
- Verified observations: 5,695 unique dates

The Growth Option NAV is treated as a total-return cash series. It is both eligible for ranking and the fallback for a selected risk asset with negative absolute momentum, matching the original framework's use of 30-day Treasury bills.

### Primary hypotheses

1. VIX-adaptive Top 1 improves Sharpe and/or maximum drawdown from 2020-05-01 relative to fixed 10M Top 1 after transaction costs.
2. Any benefit is stronger for Top 1 than Top 2 because Top 2 diversification dilutes changed ranking decisions while retaining additional turnover.
3. Most excess return comes from Yellow regimes, not the sparsely observed Red regime.
4. India VIX may have greater timing value for NIFTY 50 than for midcap/smallcap indices because it is derived from NIFTY option prices.
5. A valid result must survive removal of 2020-2021 and must not be explained by only a few outlier months.

---

## 2. Verified data coverage and sufficiency

| Series | First date | Last verified date | Rows |
|---|---:|---:|---:|
| NIFTY 50 TR | 1999-06-30 | 2026-08-17 | 6,749 |
| NIFTY MIDCAP 150 TR | 2005-04-01 | 2026-08-17 | 5,302 |
| NIFTY SMALLCAP 250 TR | 2005-04-01 | 2026-08-17 | 5,302 |
| India VIX | 2009-03-03 | 2026-08-17 | 4,296 |
| Quantum Liquid Fund-Growth | 2006-04-09 | 2026-08-17 | 5,695 |

The common experiment begins after India VIX starts and after the 40-trading-day VIX warm-up.

Using the original fixed thresholds, verified month-end regime coverage is:

| Split | Green | Yellow | Red |
|---|---:|---:|---:|
| Train, through 2019-12-31 | 71 | 53 | 4 |
| Test, from 2020-05-01 | 50 | 23 | 2 |

Green and Yellow have enough coverage for comparison. Red does not: four train and two test months cannot support a strong statistical conclusion. All Red results must display the observation count and be labelled exploratory.

---

## 3. Primary strategy specification

### 3.1 VIX regimes

At each signal month-end, using India VIX data available through that date:

- **Green:** 40-trading-day SMA of India VIX is `<= 18`
- **Yellow:** 40-day SMA is `> 18` and 20-day SMA is `< 32`
- **Red:** 40-day SMA is `> 18` and 20-day SMA is `>= 32`

India VIX is stored in percentage units, e.g. `20` means 20%, not `0.20`.

### 3.2 Momentum lookback

- Green: trailing 10-month return
- Yellow: trailing 3-month return
- Red: trailing 1-month return

Use month-end NAV/index levels and simple holding-period returns:

```r
momentum <- currentLevel / laggedLevel - 1
```

### 3.3 Portfolio variants

Run these cross-asset portfolios over the same four-asset universe: three equity indices plus Quantum Liquid Fund.

1. `VIX Top 1`
2. `10M Top 1`
3. `VIX Top 2`
4. `10M Top 2`

Rules:

- Top 1 holds the highest-ranked asset at 100%.
- Top 2 holds the two highest-ranked assets at 50% each.
- VIX variants rank on the regime-selected 10/3/1-month lookback.
- Fixed variants always rank on trailing 10-month return.
- If a selected risk asset has negative momentum, replace that slot with cash.
- Combine duplicate cash slots into one cash weight.
- Weights must sum to exactly one before costs.

### 3.4 Independent index-timing tests

For each equity index separately, compare:

1. Buy-and-hold index
2. Fixed 10-month absolute momentum: index if positive, otherwise cash
3. VIX-adaptive absolute momentum: use 10/3/1-month lookback, index if positive, otherwise cash

These tests isolate timing value from cross-index rotation value.

### 3.5 Additional benchmark

Include an equal-weight buy-and-hold portfolio of the three equity indices, rebalanced monthly, as a broad risk-asset benchmark. It is not a substitute for the fixed-10M controls.

---

## 4. Timing and look-ahead controls

1. Define the signal date as the last common equity-index trading date in each calendar month.
2. Use only VIX, index, and cash observations available on or before the signal date.
3. Determine the regime and holdings at the close of month `t`.
4. Apply those holdings only from the first index trading day of month `t+1` through its final trading day.
5. Do not apply the month `t` signal to month `t` returns.
6. The first investable month must follow a complete 10-month momentum history and 40-day VIX history.
7. The final incomplete month must not be included in completed monthly performance.

Required audit table columns:

- `signal_date`
- `holding_month`
- `vix_sma20`
- `vix_sma40`
- `regime`
- `lookback_months`
- momentum score for every asset
- selected assets
- final weights
- turnover
- gross return
- cost
- net return

Required assertions:

```r
stopifnot(all(signalDate < holdingStartDate))
stopifnot(all(abs(rowSums(weights) - 1) < 1e-12))
stopifnot(!anyDuplicated(signalDate))
stopifnot(all(is.finite(portfolioReturns)))
```

---

## 5. Return alignment and the cash series

### Index data

Read total-return index levels from `bhav_index`:

```sql
SELECT index_name, time_stamp, px_close
FROM bhav_index
WHERE index_name IN (
  'NIFTY 50 TR',
  'NIFTY MIDCAP 150 TR',
  'NIFTY SMALLCAP 250 TR'
)
ORDER BY time_stamp, index_name;
```

### India VIX

```sql
SELECT time_stamp, px_close
FROM vix_history
ORDER BY time_stamp;
```

### Quantum Liquid Fund

```sql
SELECT AS_OF, NAV
FROM mf_nav_history
WHERE SCHEME_CODE = 103734
ORDER BY AS_OF;
```

### Cash alignment

The fund NAV includes weekends and some non-equity dates. Sample the latest available NAV on each equity trading date. The cash return between consecutive equity trading dates is:

```r
cashRet[t] <- cashNavOnEquityDate[t] / cashNavOnEquityDate[t - 1] - 1
```

This captures weekend/holiday accrual between equity trading dates without inventing equity returns on non-trading days.

Do not merge all level series and call `na.omit()` before alignment. Build each series independently, map cash NAV point-in-time to the equity calendar, then intersect the three equity calendars.

Data-quality assertions:

- Exactly one fund NAV per `AS_OF` date.
- All NAVs and index levels are positive.
- Dates are strictly increasing after deduplication.
- No future NAV is carried backward.
- Daily simple equity return is inside a documented plausible range, e.g. `[-0.30, 0.30]`; stop and inspect violations.
- Daily liquid-fund return is inside a conservative range, e.g. `[-0.01, 0.01]`; stop and inspect violations.
- Report missing-date counts before any fill or intersection.

---

## 6. Train/test design

### Primary fixed-parameter replication

- **Train/reporting period:** through 2019-12-31
- **Test period:** from 2020-05-01 through the last completed month
- **Full period:** first valid investment month through the last completed month

The original thresholds and 10/3/1 lookbacks are pre-registered and require no optimization. The result from 2020-05-01 is the primary Indian out-of-sample result.

### Secondary India-calibrated robustness test

Run only after the primary fixed specification is complete.

- Derive any VIX percentile thresholds using data through 2019 only.
- Freeze thresholds before evaluating 2020 onward.
- Use one common threshold specification across all three indices; do not optimize separately by index.
- Show the full threshold sensitivity surface, not only the winner.
- Do not replace or blend this result with the primary replication.

A suitable robustness specification is:

- Green: lower 50% of the trailing/expanding 40-day VIX-SMA distribution
- Red: upper 10% of the trailing/expanding 20-day VIX-SMA distribution
- Yellow: all remaining observations

Expanding percentiles must use only observations available through each signal date.

---

## 7. Transaction costs

Calculate one-way turnover at each rebalance:

```r
turnover <- 0.5 * sum(abs(newWeights - oldWeights))
cost <- turnover * costRate
netReturn <- grossReturn - cost
```

Run these cost levels:

- 0 bps
- 10 bps
- 25 bps
- 50 bps

The primary net comparison uses 25 bps per unit of turnover. Do not divide drag by the number of holdings.

Report:

- average monthly turnover
- annualized turnover
- total cost drag
- CAGR at each cost level
- break-even cost at which VIX-adaptive performance equals fixed 10M performance

---

## 8. Metrics and diagnostics

Use simple daily portfolio P&L returns. Compound only for cumulative wealth, monthly aggregation, annual returns, and CAGR.

### Core metrics

For train, test, and full periods:

- CAGR
- annualized volatility
- annualized Sharpe ratio
- maximum drawdown
- Calmar ratio
- average drawdown recovery days
- worst month
- percentage positive months
- annualized turnover

### Incremental metrics versus fixed 10M

- CAGR difference
- Sharpe difference
- maximum-drawdown difference
- tracking error
- information ratio
- percentage of identical-allocation months
- percentage of divergent-allocation months
- percentage of months adaptive beats fixed
- arithmetic sum of monthly return differences

### Required decompositions

- Green/Yellow/Red performance with observation counts
- contribution to excess return by regime
- annual returns
- rolling 12-month adaptive-minus-fixed return
- rolling 36-month Sharpe
- five largest positive and negative relative months
- holdings and signal values for those ten months
- share of total excess return produced by the best 1, 3, and 5 months
- index-specific timing results

The report must explicitly answer whether the result is broad or concentrated.

---

## 9. Statistical and robustness tests

For monthly adaptive-minus-fixed returns, report:

- paired t-test
- Wilcoxon signed-rank test
- moving/block bootstrap confidence interval for mean monthly excess return
- block-bootstrap confidence interval for Sharpe improvement

Use approximately 3-6 month blocks because regimes and allocations are persistent. Do not treat overlapping/serially dependent observations as IID.

Run these adversarial checks:

1. Exclude March-December 2020.
2. Exclude all of 2020-2021.
3. Exclude calendar year 2022.
4. Leave one calendar year out at a time.
5. Remove the best relative month.
6. Remove the best three relative months.
7. Compare original absolute VIX thresholds with train-derived percentiles.
8. Compare 0/10/25/50 bps costs.
9. Confirm that Red-regime conclusions remain labelled inconclusive because of insufficient coverage.

---

## 10. Output files

Create the following implementation files in this directory:

- `build.R` — database extraction, alignment, validation, and checkpoint construction
- `backtest.R` — pure strategy functions and portfolio return engine
- `tests.R` — deterministic assertions and small synthetic regression tests
- `analysis.R` — metrics, robustness tests, tables, and charts
- `run.R` — canonical runner: build/load cache, test, backtest, analyze
- `README.md` — methodology, commands, data coverage, and output index

Generated artifacts:

- `cache.rds`
- `backtest-results.rds`
- `audit-monthly.csv`
- `daily-returns.csv`
- `daily-returns.rds`
- `metrics-train.html` / `.png`
- `metrics-test.html` / `.png`
- `metrics-full.html` / `.png`
- `regime-metrics.html` / `.png`
- `cost-sensitivity.html` / `.png`
- `annual-returns-*.png`
- `cumulative-returns-*.png`
- `rolling-relative-returns.png`
- `rolling-sharpe.png`
- `allocation-history.png`
- `largest-relative-months.html` / `.png`

All ggplot charts must use `labs(caption = "@StockViz")`. All gt tables must use `tab_source_note("@StockViz")`. Do not add `@StockViz` to the `Common.PlotCumReturns` subtitle.

---

## 11. Checkpoint contract

`build.R` must save a checkpoint containing:

```r
list(
  fingerprint = fingerprint,
  index_levels = indexLevels,
  index_returns = indexReturns,
  cash_nav = cashNav,
  cash_returns = cashReturns,
  vix = vixXts,
  month_ends = monthEnds,
  coverage = coverageTable
)
```

Fingerprint inputs must include:

- exact index names
- cash scheme code `103734`
- SQL query text/version
- first and last source dates
- regime windows `20/40`
- momentum lookbacks `1/3/10`
- train/test cutoffs

On cache load, recompute the fingerprint and require `identical(saved$fingerprint, currentFingerprint)`. Stop on a mismatch rather than silently reusing stale data.

---

## 12. Implementation tasks

### Task 1: Build and validate the source cache

**Files:** Create `build.R` and `tests.R`.

1. Put all `library()` and `source()` calls at the top.
2. Query the three index series, India VIX, and scheme `103734`.
3. Validate names, date ranges, duplicate dates, positive levels, and return ranges.
4. Map liquid-fund NAV point-in-time to the common equity trading calendar.
5. Build daily simple returns and month-end levels.
6. Save `cache.rds` with a parameter fingerprint.
7. Add synthetic tests for point-in-time cash mapping and month-end selection.
8. Run `Rscript tests.R`; expected result: all cache/alignment tests pass.
9. Run `Rscript build.R`; expected result: coverage table is printed and `cache.rds` is created.

### Task 2: Implement regime and momentum functions

**Files:** Create `backtest.R`; modify `tests.R`.

1. Write failing synthetic tests for Green, Yellow, and Red boundaries.
2. Test exact equality at VIX thresholds 18 and 32.
3. Implement right-aligned 20/40-trading-day VIX SMAs.
4. Implement 1/3/10-month trailing returns from month-end levels.
5. Verify the signal uses no date after the signal month-end.
6. Run `Rscript tests.R`; expected result: all regime and momentum tests pass.

### Task 3: Implement monthly holdings

**Files:** Modify `backtest.R` and `tests.R`.

1. Write failing tests for Top 1, Top 2, ties, negative momentum, and duplicate cash slots.
2. Implement deterministic ranking with a documented tie-break rule based on fixed asset order.
3. Implement cash substitution and weight aggregation.
4. Assert weights sum to one.
5. Verify a month `t` signal creates holdings only for month `t+1`.
6. Run `Rscript tests.R`; expected result: all holdings and timing tests pass.

### Task 4: Implement daily portfolio returns and costs

**Files:** Modify `backtest.R` and `tests.R`.

1. Write synthetic return tests with manually calculated expected P&L.
2. Implement daily weighted simple returns.
3. Apply turnover cost on the first trading day of each new holding month.
4. Test 0, 10, 25, and 50 bps costs.
5. Assert higher costs cannot improve a strategy with positive turnover.
6. Run `Rscript tests.R`; expected result: all P&L and cost tests pass.

### Task 5: Run the canonical backtests

**Files:** Create `run.R`.

1. Load and validate the checkpoint fingerprint.
2. Run VIX Top 1/2 and fixed 10M Top 1/2.
3. Run the equal-weight benchmark.
4. Run three independent index-timing comparisons.
5. Save `backtest-results.rds`, `audit-monthly.csv`, and daily return exports.
6. Verify train, test, and full series have non-zero observations and common comparison dates.
7. Run `Rscript run.R`; expected result: all strategies complete without NA returns or look-ahead assertion failures.

### Task 6: Produce the primary analysis

**Files:** Create `analysis.R`.

1. Generate train/test/full CAGR, Sharpe, and MaxDD tables.
2. Generate cumulative-return and annual-return charts.
3. Generate allocation and regime diagnostics.
4. Generate turnover and cost-sensitivity tables.
5. Generate largest relative-month tables and concentration statistics.
6. Generate rolling relative-return and Sharpe charts.
7. Run `Rscript analysis.R`; expected result: all declared output files exist and contain non-empty tables/charts.

### Task 7: Run robustness and statistical tests

**Files:** Modify `analysis.R`.

1. Add paired t-test and Wilcoxon outputs.
2. Add 3-6 month block bootstrap intervals.
3. Add 2020, 2020-2021, 2022, leave-one-year-out, and best-month exclusions.
4. Add the train-derived percentile-regime sensitivity as a separately labelled section.
5. Verify no post-2019 observation affects calibrated thresholds.
6. Re-run `Rscript analysis.R`; expected result: robustness tables are generated for all strategy pairs.

### Task 8: Document and review

**Files:** Create `README.md`; review all `.R` files.

1. Document exact strategy rules and cash treatment.
2. Include verified coverage and regime counts.
3. State clearly that Red coverage is insufficient.
4. Include commands to rebuild and reproduce every artifact.
5. Confirm all imports are at the top of each file.
6. Confirm daily P&L uses simple returns.
7. Confirm train/test comparisons use common dates.
8. Run the entire pipeline from a clean state.
9. If the directory is tracked by git, commit the plan and each completed implementation task separately.

---

## 13. Acceptance criteria

The project is complete only when:

- The cash proxy is scheme `103734`, not a zero-return placeholder or stale bond index.
- Cash NAV is sampled point-in-time on the equity trading calendar.
- Every holding month uses the prior month-end signal.
- The original 18/32 and 10/3/1 specification is reported before any calibrated variant.
- Train is no later than 2019-12-31 and test begins on 2020-05-01.
- CAGR, Sharpe, and MaxDD are reported for train, test, and full periods.
- Results are shown at 0, 10, 25, and 50 bps.
- Regime tables include observation counts.
- Red is not interpreted as statistically established.
- The Top 1 edge, if any, is tested after removing 2020-2021 and the best relative months.
- Independent timing results are shown for all three indices.
- All synthetic tests, data assertions, and a clean end-to-end run pass.

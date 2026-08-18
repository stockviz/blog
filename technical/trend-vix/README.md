# India VIX-Adaptive Trend Following

This project tests whether an India VIX-dependent momentum lookback improves trend following across:

- NIFTY 50 TR
- NIFTY MIDCAP 150 TR
- NIFTY SMALLCAP 250 TR
- Quantum Liquid Fund-Growth Option as cash

The complete research specification is in `PLAN.md`.

## Data

All data comes from the StockViz SQL Server database.

| Series | Source | First date | Last verified date | Rows |
|---|---|---:|---:|---:|
| NIFTY 50 TR | `bhav_index` | 1999-06-30 | 2026-08-17 | 6,749 |
| NIFTY MIDCAP 150 TR | `bhav_index` | 2005-04-01 | 2026-08-17 | 5,302 |
| NIFTY SMALLCAP 250 TR | `bhav_index` | 2005-04-01 | 2026-08-17 | 5,302 |
| India VIX | `vix_history` | 2009-03-03 | 2026-08-17 | 4,296 |
| Quantum Liquid Fund-Growth | `mf_nav_history`, scheme `103734` | 2006-04-09 | 2026-08-17 | 5,695 |

The liquid-fund Growth NAV is an investable total-return cash proxy. Its latest NAV available on or before each equity trading date is sampled point-in-time. A future NAV is never carried backward. Weekend and holiday accrual therefore appears in the return on the next equity trading date.

The cached aligned data currently has 5,036 common daily rows and 244 completed month-ends. The last incomplete calendar month is excluded.

## Strategies tested

The basic idea is simple. At the end of every month, the backtest looks at how the three indices and the liquid fund have performed over a recent period. It ranks them from strongest to weakest, chooses what to hold, and keeps that allocation for the next month.

"Momentum" here just means recent performance. A 10-month momentum score, for example, is the percentage gain or loss over the last 10 months.

### VIX Top 1

This is the main adaptive strategy. India VIX decides how much recent history to use:

- When volatility is calm, rank the assets by their 10-month performance. This changes slowly and avoids reacting to short-term noise.
- When volatility is elevated, use 3-month performance so the strategy can respond faster.
- When volatility is very high, use only the latest month's performance.

After choosing the lookback, the strategy ranks the NIFTY 50, Midcap 150, Smallcap 250, and the liquid fund. It puts the entire portfolio in the highest-ranked asset for the following month. If the winning equity index has lost money over the selected lookback, the portfolio holds the liquid fund instead.

### Fixed 10M Top 1

This is the control strategy for VIX Top 1. It follows the same ranking and cash rules but always uses 10-month performance, regardless of India VIX. Comparing these two strategies tells us whether changing the lookback using VIX adds anything beyond a standard momentum rule.

### VIX Top 2

This uses the same VIX-dependent 10-, 3-, or 1-month lookback as VIX Top 1, but holds the two highest-ranked assets with 50% in each. If either selected equity index has negative momentum, that half goes to the liquid fund.

### Fixed 10M Top 2

This is the control for VIX Top 2. It always ranks assets using 10-month performance and splits the portfolio equally between the top two choices. The comparison shows whether the adaptive VIX rule still helps after the portfolio is diversified across two assets.

### Equal-weight buy-and-hold

This benchmark keeps one-third each in the NIFTY 50, Midcap 150, and Smallcap 250. It rebalances back to equal weights every month. It does not use momentum, India VIX, or the liquid fund.

### Timing each index separately

The cross-index strategies can benefit either by choosing the right size segment or by moving into cash at the right time. To separate those effects, the backtest also tests each index on its own:

- **Buy-and-hold:** stay fully invested in the index.
- **Fixed 10M timing:** hold the index when its 10-month return is positive; otherwise hold the liquid fund.
- **VIX-adaptive timing:** use the VIX-selected 10-, 3-, or 1-month return; hold the index when that return is positive and the liquid fund when it is negative.

These individual tests show whether India VIX improves the timing of the NIFTY 50, Midcap 150, or Smallcap 250 independently of the relative ranking between them.

## Exact VIX rules

At each completed month-end, India VIX determines the momentum lookback:

| Regime | Plain-English interpretation | Rule | Lookback |
|---|---|---|---:|
| Green | Calm volatility | VIX SMA40 <= 18 | 10 months |
| Yellow | Elevated volatility | VIX SMA40 > 18 and VIX SMA20 < 32 | 3 months |
| Red | Very high volatility | VIX SMA40 > 18 and VIX SMA20 >= 32 | 1 month |

India VIX is stored in percentage units: `20` means 20%, not `0.20`.

The signal calculated at the close of month `t` is applied only during month `t+1`. In other words, the strategy never uses a month-end signal to claim returns that occurred before the signal was known.

## Returns and costs

Daily portfolio P&L uses simple arithmetic returns. Returns are compounded only for monthly, annual, and cumulative statistics.

One-way turnover is:

```r
0.5 * sum(abs(new_weights - old_weights))
```

Cost is charged on the first trading day of a new holding month. Results are generated at 0, 10, 25, and 50 basis points per unit of turnover. The primary comparison uses 25 bps.

## Train/test split and coverage

- Train/reporting period: through 2019-12-31
- Test period: from 2020-05-01
- Full period: first valid holding month through the latest completed month

The live run produced 4,260 common primary daily rows:

- Train: 2,625 rows
- Test: 1,553 rows
- Full: 4,260 rows

Original-threshold regime coverage is:

| Period | Green | Yellow | Red |
|---|---:|---:|---:|
| Train | 71 | 53 | 4 |
| Test | 50 | 23 | 2 |

The Red regime is not adequately covered. Its four train and two test observations are exploratory and must not be interpreted as statistically established.

## Current primary results

At 25 bps, from 2020-05-01 onward:

| Strategy | CAGR | Sharpe | Max drawdown |
|---|---:|---:|---:|
| VIX Top 1 | 19.72% | 1.19 | 33.79% |
| Fixed 10M Top 1 | 14.87% | 0.96 | 34.98% |
| VIX Top 2 | 19.93% | 1.27 | 25.19% |
| Fixed 10M Top 2 | 17.26% | 1.17 | 25.19% |

The Top 1 edge survives the tested cost range. At 50 bps, test-period CAGR is 18.55% for VIX Top 1 versus 13.99% for fixed 10M Top 1.

The result is concentrated rather than broad:

- VIX Top 1 beats fixed 10M Top 1 in 13.33% of test months.
- Paired t-test p-value: 0.1915.
- Wilcoxon p-value: 0.5546.
- Excluding March-December 2020 reduces arithmetic monthly excess from 25.60 to 10.09 percentage points.
- Excluding all of 2020-2021 leaves 13.89 percentage points of arithmetic monthly excess.

These figures are economic backtest results, not statistically conclusive evidence. See the robustness, bootstrap, concentration, and largest-relative-month output tables before drawing conclusions.

## Files

- `build.R` — database extraction, point-in-time alignment, validation, and fingerprinted cache
- `backtest.R` — pure regime, momentum, ranking, timing, P&L, and cost functions
- `tests.R` — deterministic synthetic tests
- `run.R` — canonical live backtest runner
- `analysis.R` — metrics, charts, regime/cost diagnostics, statistical tests, robustness tests, and train-only percentile sensitivity
- `PLAN.md` — pre-registered design and acceptance criteria

## Reproduce

From this directory:

```bash
Rscript tests.R
Rscript build.R
Rscript run.R
Rscript analysis.R
```

`run.R` also runs `tests.R` and rebuilds the live cache before running all strategies.

For a clean rebuild:

```bash
rm -f cache.rds backtest-results.rds audit-monthly.csv daily-returns.csv daily-returns.rds
rm -f metrics-*.html metrics-*.png regime-metrics.* cost-sensitivity.*
rm -f annual-returns-*.png cumulative-returns-*.png rolling-*.png allocation-history.png
rm -f largest-relative-months.* statistical-tests.* robustness-tests.*
rm -f percentile-sensitivity.* excess-return-concentration.*
Rscript run.R
Rscript analysis.R
```

## Generated data artifacts

- `cache.rds`
- `backtest-results.rds`
- `audit-monthly.csv`
- `daily-returns.csv`
- `daily-returns.rds`

The monthly audit includes signal date, holding dates, VIX averages, regime, lookback, all momentum scores, selected assets, final weights, turnover, gross return, cost, and net return.

## Generated reports

- `metrics-train.html` / `.png`
- `metrics-test.html` / `.png`
- `metrics-full.html` / `.png`
- `regime-metrics.html` / `.png`
- `cost-sensitivity.html` / `.png`
- `largest-relative-months.html` / `.png`
- `statistical-tests.html` / `.png`
- `robustness-tests.html` / `.png`
- `percentile-sensitivity.html` / `.png`
- `excess-return-concentration.html` / `.png`
- `annual-returns-train.png`, `annual-returns-test.png`, `annual-returns-full.png`
- `cumulative-returns-train.png`, `cumulative-returns-test.png`, `cumulative-returns-full.png`
- `common-cumulative-returns-train.png`, `common-cumulative-returns-test.png`
- `rolling-relative-returns.png`
- `rolling-sharpe.png`
- `allocation-history.png`

The percentile sensitivity thresholds are estimated only from observations through 2019 and are evaluated separately from the primary fixed 18/32 specification.

All ggplot outputs carry the `@StockViz` caption, and all gt tables use `tab_source_note("@StockViz")`.

## Appendix: What is wrong with VIX Top 2?

There is no sign of a calculation or implementation bug in VIX Top 2. The problem is that its apparent advantage over Fixed 10M Top 2 is weak and concentrated in a few months.

### Headline comparison

At 25 bps, from 2020-05-01 onward:

| Metric | VIX Top 2 | Fixed 10M Top 2 |
|---|---:|---:|
| CAGR | 19.93% | 17.26% |
| Sharpe | 1.27 | 1.17 |
| Maximum drawdown | 25.19% | 25.19% |
| Average monthly turnover | 22% | 16% |
| Annualized turnover | 2.64x | 1.92x |

The adaptive strategy has a higher CAGR, but it does not improve maximum drawdown and it trades about 38% more often.

### Most of the advantage comes from three months

VIX Top 2 produced 14.31 percentage points of cumulative arithmetic excess return over Fixed 10M Top 2. The three largest relative gains were:

| Month | VIX Top 2 advantage |
|---|---:|
| August 2020 | 9.99 percentage points |
| May 2022 | 7.10 percentage points |
| July 2020 | 4.89 percentage points |

The best month accounts for 69.8% of the total measured advantage. The best three months account for 153.6%, which is possible because several other divergent months lost money. If the best three relative months are removed, VIX Top 2 underperforms by 7.67 percentage points.

### It rarely differs from the fixed strategy

Across the 75 test months:

- The two strategies had identical monthly returns in 54 months.
- VIX Top 2 won in 11 months.
- VIX Top 2 lost in 10 months.
- Their holdings differed in only 16 months.

The VIX rule therefore has relatively few opportunities to add value. A small number of those decisions determine the final result.

### The edge largely disappears outside 2020 and 2021

After removing 2020 and 2021, the cumulative arithmetic advantage falls from 14.31 to 1.71 percentage points over the remaining 55 months. This suggests that the result is tied mainly to a particular market episode rather than a persistent improvement.

### The statistical evidence is weak

The paired tests do not reject the possibility that the monthly differences occurred by chance:

- Paired t-test: `p = 0.3494`
- Wilcoxon signed-rank test: `p = 0.6021`

### Higher costs remove the drawdown advantage

At 25 bps, both Top 2 strategies have the same 25.19% maximum drawdown. At 50 bps, VIX Top 2 has a 26.07% drawdown versus 25.49% for Fixed 10M Top 2. Its additional turnover eventually makes the drawdown slightly worse.

### Top 2 can include cash

"Top 2" does not always mean two equity indices. The liquid fund is part of the ranked universe, and any selected equity index with negative momentum is replaced by the liquid fund. The portfolio can therefore hold:

- Two equity indices
- One equity index and 50% cash
- 100% cash

This is intentional under the tested rules.

### Conclusion

VIX Top 2 finishes with a better CAGR, but the evidence does not show a reliable improvement over Fixed 10M Top 2. Its advantage depends on three months, largely disappears outside 2020-2021, does not reduce maximum drawdown, and requires more turnover. The defensible conclusion is that VIX Top 2 had a better ending value in this sample, not that it is consistently superior.

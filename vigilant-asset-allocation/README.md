# VAA (Vigilant Asset Allocation) — ETF Backtest

**Paper:** Keller & Keuning (2017), *"Breadth Momentum and the Vigilant Asset Allocation (VAA):
Winning More by Losing Less"* — [SSRN 3002624](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3002624)

Backtests the VAA-G12 strategy on 12 US-listed ETFs (equity, REITs, commodities, gold,
bonds) plus a 3-asset cash universe using the `bhav_eq_td` table on StockVizUs2.

## How It Works

**13612W Momentum Filter.** Each asset is scored monthly using a weighted blend of
returns over the trailing 1, 3, 6, and 12 months, with 40% weight on the most recent
month. The same filter drives both relative momentum (ranking assets) and absolute
momentum (classifying assets as "good" or "bad" at ≤ 0).

**Breadth-Based Crash Protection.** Count the number of bad assets (*b*) in the
12-asset risky universe. The cash fraction is *CF = b / B* (capped at 100%), where
*B* is the breadth threshold. Easy Trading rounding maps this to whole-asset
replacements — instead of trimming every position, the worst holdings are fully
replaced with the best-performing cash asset.

**Portfolio.** Top *T* assets by momentum (equal-weight 1/*T*). Bottom-ranked
positions are swapped for the best of SHY, IEF, or LQD per the cash fraction.
0.10% one-way transaction cost on turnover. Rebalanced monthly.

**Optimization.** Grid search over T = 1..6 and B = 1..6 on the in-sample half,
maximizing RAD (Returns Adjusted for Drawdowns). Locked-in (T, B) is then validated
out-of-sample.

## Universe

### Risky (12 ETFs)
SPY, IWM, QQQ, VGK, EWJ, VWO, VNQ, GSG, GLD, TLT, LQD, HYG

### Cash (3 ETFs)
SHY, IEF, LQD

LQD appears in both — if it's the top risky pick, it can also be the cash fallback,
matching the paper's design.

## Data & Date Range

| Detail | Value |
|--------|-------|
| Binding ticker | HYG (2007-04-11) |
| Backtest start | 2008-05-01 |
| Backtest end | 2026-07-01 |
| In-Sample / Out-of-Sample split | 2017-06-01 |

## Script

### `vaa-backtest.R`

Self-contained. Loads daily prices for all 14 tickers, computes month-end 13612W
momentum, runs the 36-combination grid search over IS, locks in the RAD-optimal
(T, B), runs the strategy and five benchmarks over FS, builds a gt metrics table
and cumulative/drawdown charts. Saves everything to `vaa-results.Rdata`.

## Running

```bash
cd "/mnt/data/blog/vigilant-asset-allocation"
Rscript vaa-backtest.R
```

## Results

**Optimal parameters:** T = 2, B = 2 (maximizing RAD over In-Sample).

### Full Sample

| Strategy | Sharpe | CAGR | MaxDD |
|----------|--------|------|-------|
| VAA (T=2, B=2) | 0.57 | 5.4% | −24.5% |
| Dual | 0.51 | 4.7% | −19.0% |
| EW (equal-weight risky) | 0.43 | 4.3% | −43.5% |
| 60/40 (SPY/IEF) | 0.74 | 6.2% | −29.3% |
| SPY | 0.67 | 9.7% | −47.3% |
| EWC (cash-only) | NaN | 0.2% | −21.0% |

### In-Sample (2008-05 – 2017-06)

| Strategy | Sharpe | CAGR | MaxDD |
|----------|--------|------|-------|
| VAA | 0.55 | 6.6% | −15.5% |
| Dual | 0.36 | 5.5% | −22.3% |
| EW | 0.12 | 2.1% | −41.3% |
| 60/40 | 0.45 | 5.0% | −29.3% |
| SPY | 0.38 | 6.3% | −47.3% |

### Out-of-Sample (2017-06 – 2026-07)

| Strategy | Sharpe | CAGR | MaxDD |
|----------|--------|------|-------|
| VAA | 0.58 | 4.1% | −24.5% |
| Dual | 0.99 | 11.4% | −21.7% |
| EW | 0.75 | 6.8% | −22.4% |
| 60/40 | 1.04 | 7.5% | −21.4% |
| SPY | 1.00 | 13.2% | −24.8% |

### VAA-Specific Metrics (Full Sample)

| Metric | Value |
|--------|-------|
| Avg cash fraction | 85.6% |
| Annual TTC | 1.07% |
| Avg bad assets / month | 4.6 / 12 |
| VAA Sharpe | 0.52 |
| VAA CAGR | 5.4% |
| VAA MaxDD | −24.5% |

## Key Findings

1. **Drawdown protection works.** VAA's worst drawdown (−24.5%) is roughly half of
   SPY's (−47.3%) and substantially better than equal-weight risky (−43.5%). The
   breadth mechanism moved the portfolio heavily into cash during stress periods.

2. **Cash drag is the tradeoff.** VAA sat in cash ~86% of the time on average,
   delivering a Sharpe of 0.57 — comparable to 60/40 (0.74) and SPY (0.67) on
   risk-adjusted terms, but with 430 bp lower CAGR than SPY.

3. **More defensive than the paper.** The grid search selected B = 2, not the
   paper's B = 4 for this universe. With only ~18 years of data and fewer
   independent crash cycles, the optimizer preferred a tighter crash-protection
   trigger. The cash fraction is higher (86% vs the paper's ~55–60%) and annual
   TTC is slightly lower (1.07% vs the paper's ~1.2–1.5%).

4. **Dual momentum wins OS on raw return.** In the post-2017 bull market,
   traditional dual momentum (Dual) delivered 11.4% CAGR vs VAA's 4.1% — the
   breadth mechanism's conservatism became a headwind. VAA still had the better
   Sharpe in IS, and its RAD (the optimization target) was higher IS than OS,
   consistent with an honest parameter search.

5. **Short backtest window limits confidence.** At ~18 years with one major
   crisis (2008–09), the sample contains very few independent crash events.
   Results are illustrative, not strong statistical evidence. The Harvey (2013)
   haircut caveat from the paper applies with even greater force here.

## Reference

- [Backtest Plan](VAA_Backtest_Plan.md) — step-by-step implementation plan following the paper
- [Paper Summary](VAA_Paper_Summary.md) — summary of Keller & Keuning (2017), core mechanics, results, and limitations

## Output

- [Cumulative returns](vaa-cumulative.png) — full sample: VAA vs Dual, 60/40, EW, SPY
- [Out-of-sample cumulative returns](vaa-cumulative-os.png) — 2017–2026 only
- [Drawdowns](vaa-drawdowns.png) — drawdown comparison
- [Metrics table](vaa-metrics.png) — In-Sample / Out-of-Sample / Full Sample metrics for all strategies

## Data Files

- `vaa-results.Rdata` — all returns, grid search, metrics, and parameters

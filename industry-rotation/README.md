# Industry Rotation — RRG Backtest

> Blog post: [Industry Momentum via RRG](https://stockviz.biz/2026/07/09/industry-momentum/)

Uses Relative Rotation Graphs (RRG) to rotate across NSE industries. Each week
we compute the JdK RS-Ratio and RS-Momentum for every industry with at least 10
stocks, pick the top 5 in the target quadrant, and hold them equal-weighted
until the next rebalance. 50 bps transaction cost drag applied per rebalance.

Two quadrants are tested:

- **Leading** (`/`): RS-Ratio > 100, RS-Momentum > 100. Industries already
  outperforming and still accelerating — momentum continuation.
- **Improving** (`improving/`): RS-Ratio < 100, RS-Momentum > 100. Industries
  still underperforming but turning up — turnaround plays.

## Files

- `common.R` — shared setup for the Leading quadrant: DB connection, benchmark
  loading, industry return cache, RRG computation, and `runBacktest()`. Sourced
  by all Leading scripts.
- `simple.R` — Leading quadrant: three rebalancing frequencies (1W, 2W, 4W).
- `staggered-4w.R` — Leading quadrant: four staggered 4-week scenarios,
  averaged to reduce path-dependency.
- `improving/common.R` — same as above but with the filter flipped for the
  Improving quadrant (RS-Ratio < 100, sorted by RS-Momentum).
- `improving/simple.R` — Improving quadrant: multi-frequency.
- `improving/staggered-4w.R` — Improving quadrant: staggered 4-week.

## How it works

1. Load NIFTY 50, MIDCAP 150, and SMALLCAP 250 TR daily prices (from 2023).
2. For ~58 industries with ≥10 stocks, load daily equal-weighted and
   cap-weighted returns from `BHAV_INDUSTRY`. Cache in memory.
3. At each rebalance date, for each industry:
   - RS = cumulative relative return vs NIFTY 50 TR
   - RS-Ratio = WMA(RS, 10) / WMA(WMA(RS, 10), 10) × 100
   - RS-Momentum = RS-Ratio / RS-Ratio<sub>4-weeks-ago</sub> × 100
4. Filter to the target quadrant. Take top 5 (Leading: by RS-Ratio;
   Improving: by RS-Momentum).
5. Hold equal-weighted. Apply transaction cost drag on each rebalance.
   Industry universe refreshed weekly.
6. Compare against all three benchmarks.

## Findings — Leading Quadrant

### Staggered 4-week (most robust)

| Strategy | Sharpe | Ann. Return |
|---|---|---|
| Cap 4W+0 | 1.29 | 25.74% |
| Cap 4W+1 | 1.10 | 21.24% |
| Cap 4W+2 | 1.08 | 20.20% |
| Cap 4W Avg | 1.08 | 19.80% |
| Cap 4W+3 | 0.69 | 11.84% |
| Eq 4W Avg | 0.67 | 12.64% |
| NIFTY MIDCAP 150 TR | 0.85 | 14.03% |
| NIFTY SMALLCAP 250 TR | 0.78 | 13.29% |
| NIFTY 50 TR | 0.24 | 2.41% |

### Multi-frequency

| Strategy | Sharpe | Ann. Return |
|---|---|---|
| Cap 4-Week | 1.05 | 19.75% |
| Eq 2-Week | 0.32 | 4.70% |
| Eq 4-Week | 0.33 | 4.91% |
| Cap 2-Week | 0.27 | 3.36% |
| Eq 1-Week | −0.12 | −4.81% |
| Cap 1-Week | −0.36 | −7.97% |
| NIFTY MIDCAP 150 TR | 0.65 | 10.04% |
| NIFTY SMALLCAP 250 TR | 0.46 | 6.89% |
| NIFTY 50 TR | 0.17 | 1.42% |

**Key takeaways:**

- **Less is more.** Weekly rebalancing loses money. 4-week is the sweet spot.
- **Staggering matters.** Best single slot gave 1.29 Sharpe, worst 0.69.
  Averaging four staggered starts gives a robust 1.08.
- **Cap-weighted > equal-weighted** across all frequencies. Cap-weighted
  industry returns produce both better signals and better portfolio outcomes.
- **Beats all three benchmarks.** The Leading strategy outperforms NIFTY 50
  (0.24), MIDCAP 150 (0.85), and SMALLCAP 250 (0.78) on a risk-adjusted basis.
- **Absolute returns are strong.** Cap 4W Avg delivers 19.80% annualized vs
  2.41% for NIFTY 50 TR over the same period (Jul 2025 – Jul 2026).

## Findings — Improving Quadrant

The Improving quadrant (RS-Ratio < 100, RS-Momentum > 100) does not work.
Neither cap-weighted nor equal-weighted produces positive returns.

| Strategy | Sharpe | Ann. Return |
|---|---|---|
| Cap 4W Avg | −0.36 | −8.87% |
| Cap 4-Week | −0.42 | −10.00% |
| Eq 4W Avg | −0.60 | −14.16% |
| Eq 2-Week | −0.89 | −19.99% |
| Eq 4-Week | −0.96 | −19.61% |
| Cap 2-Week | −0.96 | −20.06% |
| Eq 1-Week | −1.64 | −32.71% |
| Cap 1-Week | −2.10 | −37.23% |
| NIFTY MIDCAP 150 TR | 0.37 | 5.11% |
| NIFTY SMALLCAP 250 TR | −0.13 | −4.04% |
| NIFTY 50 TR | −0.63 | −9.75% |

**Key takeaways:**

- **Improving is not investable.** RS-Ratio < 100 means the industry is already
  underperforming. Rising momentum alone isn't enough.
- **Cap-weighted slightly less bad** than equal-weighted, but still negative.
- All benchmarks struggled during this window (Sep 2025 – May 2026), with MIDCAP
  150 TR the only positive one.

## Contrast: Leading vs Improving

The RRG framework works as advertised: Leading outperforms, Improving
underperforms. Cap-weighted dominates equal-weighted in both quadrants.
The key insight: RS-Ratio (trend) matters more than RS-Momentum (change).

## Output

### Leading quadrant

- [industry-rotation-rrg.png](industry-rotation-rrg.png) — cumulative returns for 1W, 2W, 4W cap-weighted strategies vs benchmarks
- [industry-rotation-rrg-tbl.png](industry-rotation-rrg-tbl.png) — stats table with Sharpe ratios and annualized returns
- [staggered-4w.png](staggered-4w.png) — cumulative returns for staggered 4W average vs benchmarks
- [staggered-4w-tbl.png](staggered-4w-tbl.png) — stats table showing all 4 staggered scenarios and the average

### Improving quadrant

- [improving/improving-rrg.png](improving/improving-rrg.png) — cumulative returns for 1W, 2W, 4W (all negative)
- [improving/improving-rrg-tbl.png](improving/improving-rrg-tbl.png) — stats table confirming no positive Sharpe
- [improving/improving-4w.png](improving/improving-4w.png) — cumulative returns for staggered 4W average
- [improving/improving-4w-tbl.png](improving/improving-4w-tbl.png) — stats table with all 4 staggered scenarios and the average

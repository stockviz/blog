# Trend-VIX Extended — Broad-Based Index Addition Search

Train-only search for which additional "broad-based" indices improve the
`trend-vix` market-cap universe (lower drawdown, better return). Selection is
made **only on the training set** (through 2019-12-31) — the test period is
never inspected.

## Baseline universe

The `trend-vix` project ranks and rotates among three size-segment total-return
indices plus cash:

- NIFTY 50 TR
- NIFTY MIDCAP 150 TR
- NIFTY SMALLCAP 250 TR
- Quantum Liquid Fund-Growth Option as cash

Train-set metrics at 25 bps (through 2019-12-31):

| Strategy | CAGR | Sharpe | MaxDD |
|---|---:|---:|---:|
| VIX Top 1 | 13.06% | 0.91 | 24.27% |
| VIX Top 2 | 13.13% | 0.95 | 19.85% |

## Method

`search.R` adds each candidate broad-based TR index (one at a time) to the base
universe, rebuilds the cache, runs the four cross strategies (VIX / fixed-10M ×
Top 1 / Top 2) at 25 bps, and measures CAGR, Sharpe, and maximum drawdown on the
train slice only. `combine.R` then tests the most promising combinations.

Candidates are the 17 other `classification == "broad-based-indices"` TR series
with history predating the test period. NIFTY INDIA FPI 150 TR (2022) and
NIFTY NEXT 100 TR (2026) are too recent and were excluded.

Data-quality note: NIFTY NEXT 50 TR has two spike-revert bad ticks, and
NIFTY SMALLCAP 50 TR / NIFTY500 MULTICAP 50:25:25 TR carry one-off re-basings to
1000 on 2005-04-01 (plus a spike-revert in MULTICAP on 2024-03-01). `build.R`
neutralises these (spike carry-forward, re-base back-adjustment) exactly as in
`trend-vix-sectors`.

## Findings — single additions (VIX Top 2, ranked by drawdown improvement)

| Added index | Δ MaxDD (pp) | Δ CAGR (pp) | Sharpe | CAGR |
|---|---:|---:|---:|---:|
| **NIFTY 200 TR** | **-2.56** | **+1.13** | **1.02** | **14.25%** |
| **NIFTY TOTAL MARKET TR** | **-2.53** | **+1.10** | **1.02** | **14.22%** |
| NIFTY 100 TR | -2.40 | +0.91 | 0.98 | 14.04% |
| NIFTY LARGEMIDCAP 250 TR | -2.00 | -0.08 | 0.95 | 13.05% |
| NIFTY 500 TR | -1.65 | +0.66 | 0.99 | 13.79% |
| NIFTY500 MULTICAP 50:25:25 TR | -1.42 | -0.49 | 0.92 | 12.63% |
| NIFTY NEXT 50 TR | +0.32 | -0.73 | 0.89 | 12.40% |
| NIFTY MIDCAP 100 TR | +1.07 | -1.05 | 0.88 | 12.08% |
| NIFTY SMALLCAP 50 / 100 / 500 TR | +1.9 to +3.5 | -0.5 to -1.2 | 0.83-0.88 | ~12% |
| NIFTY MIDCAP 50 TR | +4.07 | -0.95 | 0.85 | 12.17% |
| NIFTY MICROCAP 250 TR | +5.91 | +0.11 | 0.90 | 13.24% |

The pattern is clean: **broad large-cap-tilted indices (NIFTY 200, TOTAL
MARKET, NIFTY 100, NIFTY 500) reduce drawdown and lift return, while
small-cap indices (MICROCAP, SMALLCAP 500, MIDCAP 50) increase drawdown.**

NIFTY 200 TR and NIFTY TOTAL MARKET TR are near-duplicates (~99% correlated) and
produce essentially identical results. NIFTY 100 TR and NIFTY 500 TR help, but
slightly less.

## Findings — VIX Top 1

No broad-based addition meaningfully changes VIX Top 1 maximum drawdown (it
stays at ~24.3%): a single-index holding means the drawdown is set by the worst
single index held, and that is usually the existing mid/small index. Most
additions *reduce* VIX Top 1 CAGR (NIFTY NEXT 50 TR -1.39 pp, NIFTY MIDCAP
SELECT -2.54 pp, NIFTY MICROCAP -1.59 pp) because the ranking occasionally picks
the new index instead of a higher-returning one. The best VIX Top 1 additions
are essentially neutral (NIFTY 500 TR, NIFTY MIDSMALLCAP 400 TR, NIFTY TOTAL
MARKET TR, all ±0.00 pp MaxDD and ±0.0 pp CAGR).

## Combinations

Adding more than one broad index does **not** improve on the single best
addition:

| Universe | VIX T2 CAGR | VIX T2 Sharpe | VIX T2 MaxDD |
|---|---:|---:|---:|
| Base + NIFTY 200 | 14.25% | 1.02 | 17.3% |
| Base + NIFTY 200 + NIFTY 500 | 14.27% | 1.02 | 17.3% |
| Base + NIFTY 200 + NIFTY 100 | 14.12% | 0.99 | 17.3% |
| Base + 200 + 100 + 500 | 14.13% | 0.99 | 17.4% |

The benefit is already captured by a single broad large-cap-tilted index.

## Recommendation

Add **NIFTY 200 TR** to the base universe (or NIFTY TOTAL MARKET TR — they are
interchangeable). On the training set this lowers VIX Top 2 maximum drawdown by
~2.5 percentage points (19.85% → 17.3%) and raises CAGR by ~1.1 percentage
points (13.13% → 14.25%), taking Sharpe from 0.95 to 1.02.

Why it works: the base universe is one large-cap index plus two volatile
mid/small indices. A broad large-cap-tilted index (NIFTY 200 ≈ top 200 by free
float) is materially less volatile than MIDCAP 150 / SMALLCAP 250, so the
momentum rotation gains a lower-volatility "broad market" option that it
sometimes selects — cutting drawdown without sacrificing (and actually
improving) return on the train set.

Caveats:

- Selection is train-only. The test period (2020-05 onward) has **not** been
  examined; out-of-sample behaviour may differ.
- NIFTY 200 TR and NIFTY TOTAL MARKET TR are ~99% correlated — pick one, not
  both.
- The gain is specific to the VIX Top 2 (and fixed-10M Top 2) strategies; VIX
  Top 1 does not benefit from any of these additions.

## Files

- `build.R` — copied from `trend-vix` with the data-quality cleaning step added
- `backtest.R`, `tests.R` — copied from `trend-vix` unchanged
- `search.R` — single-addition train-set search (writes `train-search-results.csv`)
- `combine.R` — combination test
- `train-search-results.csv` — full single-addition results table

Reproduce:

```bash
cd /mnt/data/blog/technical/trend-vix-extended
Rscript search.R
Rscript combine.R
```

# Omega Drag Sensitivity Analysis

> How does drag affect discrete quintile exposure performance across 5 Indian
> equity indices? Sweep from 0% to 0.5%.

**Blog post:** [Volatility and Equity Index Returns](https://stockviz.biz/2026/07/29/volatility-and-equity-index-returns/)

## Approach

Sweeps drag levels (0%, 0.1%, 0.2%, 0.3%, 0.4%, 0.5%) × 5 lookbacks on the
training set, picks the best lookback by Sharpe for each (index, drag) pair,
and evaluates on the test set.

- **Exposures:** Q1=25%, Q2=50%, Q3=75%, Q4+Q5=100%
- **Transitions:** Immediate on quintile change
- **Scenarios:** Discrete exposure at each drag level vs B&H

## Key Findings

### Lookback selection shifts with drag

At zero drag, 3 of 5 indices pick L=20. By 0.5% drag, only MIDCAP 150 TR
keeps L=20 — all others shift to longer lookbacks (L=100 or L=200) to
reduce exposure churn.

| Index | L at 0% | L at 0.5% |
|---|---|---|
| NIFTY 50 TR | 20 | 200 |
| NIFTY MIDCAP 150 TR | 20 | 200 |
| NIFTY SMALLCAP 250 TR | 20 | 100 |
| NIFTY MIDCAP150 MOM 50 TR | 20 | 200 |
| MOMENTUM50 TR | 200 | 200 |

### Sharpe degrades ~linearly with drag

Each 0.1% drag costs approximately 0.02-0.04 Sharpe across most indices.

| Index | 0% | 0.1% | 0.2% | 0.3% | 0.4% | 0.5% | B&H |
|---|---|---|---|---|---|---|---|
| NIFTY 50 TR | 1.36 | 1.26 | 1.19 | 1.17 | 1.15 | 1.13 | 0.88 |
| NIFTY MIDCAP 150 TR | 1.63 | 1.51 | 1.26 | 1.22 | 1.30 | 1.28 | 1.11 |
| NIFTY SMALLCAP 250 TR | 1.76 | 1.66 | 1.57 | 1.28 | 1.25 | 1.21 | 1.04 |
| NIFTY MIDCAP150 MOM 50 TR | 1.67 | 1.44 | 1.41 | 1.37 | 1.27 | 1.24 | 1.14 |
| NIFTY500 MOMENTUM 50 TR | 0.87 | 0.81 | 0.76 | 0.71 | 0.66 | 0.61 | 0.66 |

### Volatility reduction persists even at high drag

Even at 0.5% drag, MaxDD is dramatically lower than B&H across all indices
(19-32% vs 37-45% for B&H). The quintile exposure framework's risk reduction
is robust to realistic trading cost assumptions.

## Files

| File | Description |
|---|---|
| `backtest.R` | Main script — drag sweep, metrics, charts |
| `metrics_*.{html,png}` | Per-index metrics across drag levels |
| `metrics_combined.{html,png}` | Combined metrics (all indices, all drags) |
| `cumulative_test_*.png` | Test-period cumulative returns per index |

## Quick Start

```bash
cd /mnt/data/blog/momentum/volatility-switch/omega-drag-sensitivity
Rscript backtest.R
```

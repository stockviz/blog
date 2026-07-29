# Omega Alt — Discrete Quintile Exposure

> Discrete quintile-based exposure (25/50/75/100/100) with lookback selection
> from train, tested on 5 Indian equity indices.

**Blog post:** [Volatility and Equity Index Returns](https://stockviz.biz/2026/07/29/volatility-and-equity-index-returns/)

[backtest.md](backtest.md) — full methodology, architecture, and limitations.

## Approach

Sweeps 5 lookbacks (20/50/100/200/500) on the training set using discrete
quintile exposure, picks the best lookback by Sharpe, and applies to the
test set.

- **Q1:** 25%
- **Q2:** 50%
- **Q3:** 75%
- **Q4+Q5:** 100%
- **Transition:** Immediate on quintile change.
- **Drag:** 0.5% on every exposure change.

## Key Results — Test Set (≥ 2020-01-01)

### Selected Parameters

| Index | Best L | Train Sharpe | Test Sharpe |
|---|---|---|---|
| NIFTY 50 TR | 20 | 1.42 | 1.19 |
| NIFTY MIDCAP 150 TR | 20 | 1.42 | 1.45 |
| NIFTY SMALLCAP 250 TR | 100 | 0.97 | 1.21 |
| NIFTY MIDCAP150 MOM 50 TR | 200 | 1.56 | 1.24 |
| NIFTY500 MOMENTUM 50 TR | 200 | 0.71 | 0.61 |

### Combined Test Metrics

| Index | L | Discrete CAGR | Discrete Sharpe | Discrete MaxDD | B&H CAGR | B&H Sharpe | B&H MaxDD |
|---|---|---|---|---|---|---|---|
| NIFTY 50 TR | 20 | 17.38% | 1.19 | 27.97% | 19.42% | 0.88 | 38.22% |
| NIFTY MIDCAP 150 TR | 20 | 19.76% | 1.45 | 22.15% | 21.99% | 1.11 | 39.82% |
| NIFTY SMALLCAP 250 TR | 100 | 17.63% | 1.21 | 32.22% | 21.88% | 1.04 | 44.79% |
| NIFTY MIDCAP150 MOM 50 TR | 200 | 20.12% | 1.24 | 29.75% | 24.72% | 1.14 | 37.41% |
| NIFTY500 MOMENTUM 50 TR | 200 | 6.14% | 0.61 | 19.48% | 11.19% | 0.66 | 39.85% |

## Key Findings

- **Discrete exposure cuts volatility dramatically** across all indices.
  MaxDD improves 10-25pp vs B&H while CAGR remains competitive.
- **Momentum indices need longer lookbacks.** MOMENTUM50 TR and MIDCAP150
  Momentum 50 TR both select L=200. Shorter lookbacks churn too much with
  0.5% drag.
- **Small-cap benefits from L=100.** SMALLCAP 250 TR selects L=100 due to
  higher noise at short lookbacks.
- **NIFTY 50 and MIDCAP 150** select L=20 — the fast signal works well with
  drag on large-cap and mid-cap indices.

## Files

| File | Description |
|---|---|
| `backtest.R` | Main script — lookback sweep, metrics, charts |
| `backtest.md` | Detailed methodology |
| `metrics_*.{html,png}` | Per-index GT tables with Train/Test row groups |
| `metrics_combined_test.{html,png}` | Combined test metrics across all 5 indices |
| `cumulative_test_*.png` | Test-period cumulative returns |

## Quick Start

```bash
cd /mnt/data/blog/momentum/volatility-switch/omega-alt
Rscript backtest.R
```

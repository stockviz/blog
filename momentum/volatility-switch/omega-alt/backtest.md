# Omega Alt — Discrete Quintile Exposure

Lookback selection from training set for discrete quintile-based exposure
across 5 Indian equity indices. No circuit breaker or threshold logic —
purely mechanical quintile exposure.

## What This Backtest Does

For each of 5 indices, sweeps 5 lookbacks (20/50/100/200/500) on the
training set using discrete exposure, picks the lookback with the highest
annualized Sharpe, and applies it to the test set.

### Exposure Rules

- **Q1 (lowest omega):** 25% exposure
- **Q2:** 50%
- **Q3:** 75%
- **Q4+Q5 (highest omega):** 100%
- **Transitions:** Immediate on quintile change.
- **Drag:** 0.5% on every exposure change.

### Selected Parameters

| Index | Best L | Train Sharpe |
|---|---|---|
| NIFTY 50 TR | 20 | 1.42 |
| NIFTY MIDCAP 150 TR | 20 | 1.42 |
| NIFTY SMALLCAP 250 TR | 100 | 0.97 |
| NIFTY MIDCAP150 MOM 50 TR | 200 | 1.56 |
| NIFTY500 MOMENTUM 50 TR | 200 | 0.71 |

## Architecture

### Data Pipeline

Uses shared caches from omega-basic:

```
common/cache.rds          — daily prices for 5 indices + risk-free
common/omega_cache.rds    — rolling omega for 5 indices × 5 lookbacks
```

### Training Sweep

For each lookback per index:
1. Compute quintile classifications on full history
2. Apply discrete exposure (25/50/75/100/100) with 0.5% drag
3. Slice to training period (≤ 2019-12-31)
4. Record annualized Sharpe
5. Select lookback with highest train Sharpe

### Comparison Baseline

**B&H:** Full buy-and-hold of the index.

## Exposure Logic (Per Day)

```
1. Compute quintile from rolling omega (same as omega-basic)
2. Determine base exposure: Q1=25%, Q2=50%, Q3=75%, Q4=100%, Q5=100%
3. exposure = baseExposure (immediate, no delay)
4. Strategy return[t] = exposure[t] × indexRet[t] − 0.5% × |Δexposure|
```

## Files

| File | Description |
|---|---|
| `backtest.R` | Main script — lookback sweep, metrics, charts |
| `metrics_*.{html,png}` | Per-index GT tables (Train/Test row groups) |
| `metrics_combined_test.{html,png}` | Combined test metrics across all indices |
| `cumulative_test_*.png` | Test-period cumulative returns |

## How to Run

```bash
cd /mnt/data/blog/momentum/volatility-switch/omega-alt
Rscript backtest.R
```

Requires: R (quantmod, PerformanceAnalytics, xts, gt, webshot2, viridis,
ggthemes). Data: StockViz SQL Server.

## Configuration

```r
LOOKBACKS <- c(20L, 50L, 100L, 200L, 500L)
EXPOSURES <- c(0.25, 0.50, 0.75, 1.0, 1.0)
DRAG      <- 0.5 / 100
```

## Limitations

- **Fixed lookback:** Best lookback is static after training.
- **Fixed drag:** 0.5% drag may not match actual costs.
- **Long-only, no leverage.**

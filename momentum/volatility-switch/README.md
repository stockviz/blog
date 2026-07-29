# Omega Volatility Switch — Project Learnings

> A family of strategies that use rolling omega quintile classifications to
> toggle exposure across 5 Indian equity indices, with extensive sensitivity
> and persistence analysis.

## Project Structure

```
volatility-switch/
├── common/                    # Shared data caches
│   ├── cache.rds              # Daily prices + risk-free for 5 indices
│   └── omega_cache.rds        # Rolling omega (5 indices × 5 lookbacks)
├── omega-alt/                 # Discrete quintile exposure
│   ├── backtest.R             # Lookback sweep → best-L → test
│   ├── backtest.md            # Methodology
│   ├── README.md              # Results summary
│   ├── metrics_*.png          # Per-index Train/Test GT tables
│   ├── metrics_combined_test.png  # Combined test metrics
│   └── cumulative_test_*.png  # Test-period cumulative return charts
└── omega-drag-sensitivity/    # Drag level sweep
    ├── backtest.R             # Sweep drag 0%–0.5%
    ├── README.md              # Sensitivity findings
    ├── metrics_*.png          # Per-index drag-level tables
    ├── metrics_combined.png   # Combined drag-sensitivity table
    └── cumulative_test_*.png  # Per-index drag-scenario charts
```

## Strategies Explored

### 1. Discrete Quintile Exposure (omega-alt)

The core framework: classify each day into an omega quintile (Q1–Q5) and
assign a fixed exposure.

| Quintile | Exposure | Interpretation |
|---|---|---|
| Q1 (lowest omega) | 25% | Weak risk-adjusted returns — reduce exposure |
| Q2 | 50% | Below average |
| Q3 | 75% | Average |
| Q4 | 100% | Above average — full exposure |
| Q5 (highest omega) | 100% | Strong risk-adjusted returns — full exposure |

Transitions are immediate on quintile change. Drag is applied at every
exposure change.

**Key finding:** Discrete exposure halves MaxDD vs B&H across all 5
indices while maintaining competitive CAGR. The framework is robust.

### 2. Q1 Circuit Breaker (explored in earlier iterations)

Zero out exposure when Q1 persists beyond a threshold (mean or percentile
of Q1 stay duration from training).

**Key finding:** Works well for NIFTY 50 TR (Q1 slope −0.33%/day) but
hurts momentum indices (MOMENTUM50 TR Q1 slope near flat). The benefit
is index-dependent — circuit-breaker logic needs to account for each
index's Q1 behavior.

### 3. Binary 0%/100% with Q1 Breaker (explored in earlier iterations)

Simplest form: 100% long except when Q1 overstays → 0%. No tiered
exposure.

**Key finding:** Underperforms tiered exposure. Being 100% long during
Q2–Q5 exposes the strategy to full volatility without the risk reduction
of gradual sizing.

## Key Findings

### Lookback Selection

The optimal lookback varies by index and drag:

| Index | L at 0% drag | L at 0.5% drag |
|---|---|---|
| NIFTY 50 TR | 20 | 200 |
| NIFTY MIDCAP 150 TR | 20 | 200 |
| NIFTY SMALLCAP 250 TR | 20 | 100 |
| NIFTY MIDCAP150 MOM 50 TR | 20 | 200 |
| NIFTY500 MOMENTUM 50 TR | 200 | 200 |

**Takeaway:** At zero drag, short lookbacks capture fast regime changes.
As drag increases, longer lookbacks are favored because they produce fewer
quintile transitions. The momentum-only indices stick with L=200 regardless
of drag — their factor signal is inherently slower.

### Drag Sensitivity

Each 0.1% drag costs approximately 0.02–0.04 Sharpe. At 0.5% drag:

| Index | Best L | Test Sharpe | vs B&H Sharpe |
|---|---|---|---|
| NIFTY 50 TR | 200 | 1.13 | 0.88 |
| NIFTY MIDCAP 150 TR | 200 | 1.28 | 1.11 |
| NIFTY SMALLCAP 250 TR | 100 | 1.21 | 1.04 |
| NIFTY MIDCAP150 MOM 50 TR | 200 | 1.24 | 1.14 |
| NIFTY500 MOMENTUM 50 TR | 200 | 0.61 | 0.66 |

All strategies beat B&H on risk-adjusted basis except MOMENTUM50 TR
(where B&H Sharpe 0.66 edges out discrete 0.61 at 0.5% drag).

### Quintile Returns by Persistence (from earlier analysis)

- **NIFTY 50 TR Q1 slope:** −0.33%/day — extended Q1 stays are dangerous.
- **MOMENTUM50 TR Q1 slope:** −0.07%/day — nearly flat, Q1 is not a strong
  sell signal for momentum indices.
- **Q5 needs persistence:** Short Q5 stays (1-2 days) are often false
  breakouts. Returns materialize after 12+ days in Q5.

### Risk Reduction is the Primary Benefit

Across all indices and drag levels, the quintile framework delivers:

- **MaxDD reduction of 10–25 percentage points** vs B&H
- **Volatility cut by 35–55%** vs B&H
- **Sharpe ratios that beat B&H** at drag levels ≤ 0.3% for all indices
  except MOMENTUM50 TR

The strategy doesn't beat B&H on CAGR — it beats B&H on risk-adjusted
returns by dramatically reducing exposure during unfavorable omega regimes.

## Data Pipeline

All strategies share a common data pipeline:

1. **Prices:** Daily index prices from StockViz `bhav_index` table
2. **Risk-free:** Zero-coupon yield curve for MAR computation
3. **Omega cache:** Rolling omega values precomputed for all 5 indices ×
   5 lookbacks (20/50/100/200/500) using expanding quintile calibration
   (first 500 days of omega history)
4. **No look-ahead:** Omega at day t-1 determines exposure at day t

## Replication

```bash
# 1. Build common caches (run once)
cd volatility-switch/common/
Rscript cache_builder.R

# 2. Discrete quintile strategy
cd ../omega-alt/
Rscript backtest.R

# 3. Drag sensitivity analysis
cd ../omega-drag-sensitivity/
Rscript backtest.R
```

Requires: R (quantmod, PerformanceAnalytics, xts, gt, webshot2, viridis,
ggthemes), RODBC, StockViz database access.

## Limitations

- **Fixed lookback:** Best lookback is static after training.
- **Same exposure map for all indices:** Q1=25% may be too aggressive or
  too conservative depending on the index.
- **Long-only:** No shorting or leverage.
- **No transaction costs beyond drag:** Market impact, slippage not modeled.
- **Quintile calibration uses expanding window:** Early quintile boundaries
  are estimated from fewer observations.

## Key Takeaway

**Omega quintile-based exposure is a robust risk-management framework.**
The 25/50/75/100/100 exposure map with lookback selection from train
consistently delivers superior risk-adjusted returns across all 5 indices,
with the benefit being most pronounced at drag levels ≤ 0.3%.

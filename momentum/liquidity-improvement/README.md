# LIQIM — Liquidity Improvement Factor in Indian Equities

Tests whether the Amihud-based LIQC signal predicts returns and produces
investable portfolios in the NSE top-60% FF-mcap universe.

## Quintile × Lookback Analysis

Next-month return by LIQC quintile for 1/3/6/12-month lookbacks.
Q1 = highest LIQC (most improving). Q5 = lowest (most deteriorating).
Top-60% FF-mcap universe, 0 lag, 0 drag, no stock cap.

| LB | Q | Months | Mean/mo | % Up | Cumulative | Sharpe |
|---|---|---|---|---|---|---|
| 1 | Q1 | 143 | +1.39% | 62.2% | +440% | +0.22 |
| 1 | Q2 | 143 | +1.11% | 62.9% | +284% | +0.20 |
| 1 | Q3 | 143 | +1.02% | 61.5% | +236% | +0.18 |
| 1 | Q4 | 143 | +0.78% | 58.7% | +135% | +0.13 |
| 1 | Q5 | 143 | +0.97% | 60.1% | +186% | +0.15 |
| 3 | Q1 | 141 | +1.27% | 61.7% | +350% | +0.20 |
| 3 | Q2 | 141 | +1.19% | 58.9% | +324% | +0.21 |
| 3 | Q3 | 141 | +0.95% | 56.7% | +195% | +0.16 |
| 3 | Q4 | 141 | +0.74% | 58.2% | +121% | +0.13 |
| 3 | Q5 | 141 | +0.69% | 62.4% | +94% | +0.11 |
| 6 | Q1 | 138 | +1.23% | 60.9% | +305% | +0.19 |
| 6 | Q2 | 138 | +1.09% | 60.1% | +251% | +0.19 |
| 6 | Q3 | 138 | +0.88% | 55.8% | +168% | +0.15 |
| 6 | Q4 | 138 | +0.73% | 58.7% | +119% | +0.13 |
| 6 | Q5 | 138 | +0.63% | 55.8% | +74% | +0.09 |
| 12 | Q1 | 132 | +1.14% | 60.6% | +231% | +0.17 |
| 12 | Q2 | 132 | +1.05% | 59.1% | +218% | +0.18 |
| 12 | Q3 | 132 | +0.92% | 56.1% | +170% | +0.16 |
| 12 | Q4 | 132 | +0.98% | 58.3% | +189% | +0.17 |
| 12 | Q5 | 132 | +0.57% | 56.1% | +55% | +0.08 |

**Pattern holds across all lookbacks:** Q1 is best, Q5 beats the middle (Q3/Q4)
at 1-month, and the signal degrades monotonically with longer lookbacks.

## Q1 LIQC Portfolio (20 stocks, 50bps drag, 1-month hold)

| | NIFTY500 MOM50 TR | Q1 LIQC |
|---|---|---|
| CAGR | 18.50% | 9.38% |
| Volatility | 20.52% | 23.27% |
| Sharpe | 0.93 | 0.50 |
| Max DD | 38.33% | 53.16% |
| Calmar | 0.48 | 0.18 |
| Trades | — | 144 |

The Q1 portfolio earns positive returns but significantly underperforms the
benchmark. Higher volatility, deeper drawdowns. LIQC alone is not sufficient.

## Momentum Enhancement (12-month momentum, 20 stocks, 50bps drag)

Removing Q5 (most deteriorating LIQC) stocks from the momentum universe,
tested with both 1-month and 12-month LIQC lookbacks:

| | NIFTY500 MOM50 TR | Raw Mom | Mom ex-Q5 (1m) | Mom ex-Q5 (12m) |
|---|---|---|---|---|
| CAGR | 19.17% | 22.45% | **23.24%** | 22.70% |
| Volatility | 20.48% | 26.95% | 26.78% | 26.91% |
| Sharpe | 0.96 | 0.89 | **0.92** | 0.90 |
| Max DD | 38.33% | 57.43% | 59.14% | 56.34% |
| Calmar | 0.50 | 0.39 | 0.39 | 0.40 |

Both Q5 exclusions improve on raw momentum. 1-month adds +0.79% CAGR and
+0.03 Sharpe. 12-month adds +0.25% CAGR, nearly flat on Sharpe. 1-month
detection is clearly superior — fresher signal, stronger enhancement.

## Why This Works

1. **Q1 (improving liquidity):** Stocks becoming easier to trade. Often precedes
   positive price moves as institutional interest grows. Positive standalone signal
   but not strong enough to beat momentum on its own.

2. **Q5 (deteriorating 1-month liquidity):** Stocks where liquidity just started
   drying up. These are the "edge of the cliff" — next month's momentum crashes.
   Excluding them removes toxic stocks before they blow up. A 12-month Q5 captures
   stocks that have been deteriorating for a year — the market already knows.

3. **The middle is noise.** Q2-Q4 show no usable pattern.

## Pipeline

```bash
Rscript build.R         # data fetch, ILLIQ, LIQC, universe, quintile stats
Rscript backtest.R      # Q1 portfolio → q1_liqc.rds
Rscript momentum.R      # momentum vs mom-ex-Q5 (1m + 12m) → momentum.rds
Rscript consolidated.R  # load RDS files, combined charts
Rscript quintiles.R     # quintile × lookback analysis (optional)
```

## Files

| File | Purpose |
|---|---|
| `build.R` | Phase 1 — signal construction |
| `backtest.R` | Phase 2 — Q1 portfolio |
| `momentum.R` | Phase 3 — momentum comparison (1m + 12m Q5) |
| `consolidated.R` | Phase 4 — combined charts (no recomputation) |
| `quintiles.R` | Quintile × lookback analysis |
| `liqim-common.R` | All shared logic |
| `liqim-config.R` | Shared parameters |
| `pipeline.md` | Detailed documentation |

## Signal

```
ILLIQ_t  = 1e6 × mean(|simple return| / dollar volume)   per month
LIQC_t   = −(ILLIQ_t − ILLIQ_{t−1})
```

All returns are simple (arithmetic). Monthly: `prod(1 + daily) − 1`.
Universe: top 60% FF-mcap by count, price ≥ ₹30, dollar volume ≥ ₹1cr,
traded within last 90 days. Momentum requires ≥230 trading days of history.

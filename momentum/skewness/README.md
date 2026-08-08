# Skewness-Managed Momentum — NSE Backtest

Long-only Indian momentum strategy with expected-skewness overlay, adapted from Gong, Lynch & Ogden (2026) *"Skewness Managed Portfolios."*

**Universe:** NSE ordinary equities, top 60% free-float market cap  
**Momentum:** 12-month lookback, no skip month  
**Skewness forecast:** monthly cross-sectional regression (RS ~ RV + RS + MOM + PRIOR + SIZE + INDUSTRY)  
**Rebalance:** monthly, next-day execution, 50bps drag

---

## Files

| File | Purpose |
|---|---|
| `skew-config.R` | All parameters (universe, momentum, LIQC, skewness) |
| `backtest-common.R` | Portfolio construction, momentum cache, chart/table helpers |
| `liq-common.R` | LIQC computation (`computeLIQC`, `buildQ5Exclude`, `winsorize`) |
| `skew-common.R` | Skewness computation, industry cache, expected-skewness forecast, sequential picker |
| `build.R` | Self-contained data fetch + ILLIQ/LIQC + checkpoint |
| `momentum.R` | Raw momentum backtest — Momentum vs Mom+Skew vs Mom+Skew+LIQC |
| `omega-momentum.R` | Omega ratio backtest — Momentum vs OmegaMom vs Omega+Skew vs Omega+Skew+LIQC |
| `consolidated.R` | **All scenarios in one run** — 7 portfolios, 3 periods, unified charts |
| `rank-persistence.R` | Portfolio membership persistence by market regime |
| `drop-analysis.R` | Next-month returns of dropped/kept/added stocks |
| `backtest-plan.md` | Full strategy design document |
| `summary.md` | Paper summary (Gong, Lynch & Ogden 2026) |
| `checkpoint.rds` | Cached price/mcap/LIQC data |

---

## 1. Consolidated Performance — All Scenarios

Top 20, equal-weight, monthly rebalance. All scenarios in one table per period.

### Full Period — 2014-08-28 → 2026-07-31 (12 years)

| | NIFTY500 | Mom | M+Skew | M+S+L | ΩMom | Ω+Skew | Ω+S+L |
|---|---|---|---|---|---|---|---|
| CAGR | 18.5% | 21.8% | **26.6%** | 26.5% | 23.9% | 26.2% | 26.3% |
| Vol | 20.5% | 27.0% | 26.2% | 25.8% | 22.4% | 24.0% | 23.8% |
| Sharpe | 0.93 | 0.87 | **1.03** | 1.04 | 1.07 | **1.09** | **1.10** |
| MaxDD | 38% | 57% | 57% | 54% | 45% | 44% | 47% |
| Calmar | 0.48 | 0.38 | **0.46** | 0.49 | 0.53 | **0.59** | 0.57 |

- Best Sharpe: **Omega+Skew+LIQC** (1.10) and **Omega+Skew** (1.09)
- Best CAGR: **Mom+Skew** (26.6%), but with the worst risk profile
- Omega-based strategies dramatically reduce drawdowns: 44-47% vs 57% for raw momentum

### Pre-2019-12-31 — 2014-08 → 2019-12 (65 months)

| | NIFTY500 | Mom | M+Skew | M+S+L | ΩMom | Ω+Skew | Ω+S+L |
|---|---|---|---|---|---|---|---|
| CAGR | 16.0% | 11.1% | 20.6% | 22.5% | 12.4% | **24.8%** | 24.0% |
| Vol | 18.2% | 26.8% | 25.5% | 25.1% | 22.5% | 23.6% | 23.3% |
| Sharpe | 0.91 | 0.53 | 0.87 | 0.93 | 0.63 | **1.06** | 1.04 |
| MaxDD | 23% | 45% | 51% | 49% | 39% | **33%** | 38% |
| Calmar | 0.71 | 0.25 | 0.41 | 0.46 | 0.32 | **0.75** | 0.62 |

- Raw Momentum was **terrible** pre-2020: Sharpe 0.53, MaxDD 45%, CAGR barely 11%
- **Skewness overlay saved it**: Mom+Skew Sharpe 0.87 (+0.34), CAGR 20.6% (+9.5pp)
- **Omega+Skew was the best**: Sharpe 1.06, CAGR 24.8%, MaxDD only 33%
- The skewness signal was most valuable in the tough pre-COVID market

### Post-2020-05-01 — 2020-05 → 2026-07 (75 months)

| | NIFTY500 | Mom | M+Skew | M+S+L | ΩMom | Ω+Skew | Ω+S+L |
|---|---|---|---|---|---|---|---|
| CAGR | 24.0% | 36.2% | 38.2% | 35.5% | **38.7%** | 34.2% | 34.4% |
| Vol | 20.3% | 26.2% | 25.6% | 25.3% | **20.9%** | 22.9% | 22.9% |
| Sharpe | 1.16 | 1.31 | 1.39 | 1.33 | **1.67** | 1.40 | 1.41 |
| MaxDD | 32% | 42% | 50% | 51% | **36%** | 43% | 42% |
| Calmar | 0.76 | 0.85 | 0.76 | 0.69 | **1.08** | 0.80 | 0.82 |

- **OmegaMom alone dominates post-2020**: Sharpe 1.67, lowest vol (20.9%), smallest drawdown
- **Skewness overlay detracts**: every skewness variant has lower Sharpe than its parent
- The expected-skewness signal has not added value during the post-COVID bull market
- Raw momentum works much better post-2020 (Sharpe 1.31 vs 0.53 pre-2019) — regime matters enormously

---

## 2. Rank & Portfolio Persistence — 2014-07 → 2026-06

### Momentum Score Persistence (Spearman ρ)

| Regime | N Full | Mean ρ | N Post-2020 | Mean ρ |
|---|---|---|---|---|
| Normal | 55 | 0.925 | 29 | 0.921 |
| Drawdown | 52 | 0.913 | 29 | 0.907 |
| Recovery | 21 | 0.911 | 10 | 0.907 |

Momentum scores are highly sticky across all periods and regimes — ~0.91–0.93 correlation.

### Portfolio Membership Persistence (% of top-20 retained t→t+1)

| Regime | Mom Full | Mom Post-2020 | Skew Full | Skew Post-2020 |
|---|---|---|---|---|
| Normal | 68% | 66% | 28% | 21% |
| Drawdown | 63% | 61% | 18% | 14% |
| Recovery | 66% | 67% | 20% | 20% |

The high-churn pattern **worsens post-2020**: Skew overlap drops to just 14% during drawdowns. The skewness overlay's turnover problem is not improving with time.

---

## 3. Drop & Add Analysis — What Happens to Traded Stocks?

### Mean Next-Month Return

| | Mom Drop | Mom Keep | Mom New | Skew Drop | Skew Keep | Skew New |
|---|---|---|---|---|---|---|
| Full | +2.12% | +2.09% | **+3.13%** | +2.07% | +2.09% | **+2.44%** |
| Post-2020 | +3.00% | +3.35% | **+4.01%** | +2.76% | +2.94% | **+3.33%** |

### Hit Rates (% positive next-month returns)

| | Mom Drop | Mom Keep | Mom New | Skew Drop | Skew Keep | Skew New |
|---|---|---|---|---|---|---|
| Full | 56% | 65% | 60% | 66% | 59% | 65% |
| Post-2020 | 56% | 66% | 58% | 67% | 57% | 63% |

### Key Findings

1. **New stocks outperform kept stocks** in all periods — the rebalance captures fresh momentum
2. **Drop quality is similar** between strategies and across periods
3. **Mom+Skew drops more winners** than Momentum (66-67% still go up vs 56%) but replacements have higher hit rate (63-65% vs 58-60%)
4. **Post-2020 returns are higher across all groups** (bull market) but the relative patterns are unchanged
5. **The cost is volume, not quality** — 3× turnover with similar per-stock quality

---

## How to Run

```bash
cd /mnt/data/blog/momentum/skewness

# First time: symlink or build the checkpoint
ln -s ../liquidity-improvement/checkpoint.rds checkpoint.rds   # fast
# Rscript build.R                                               # full rebuild (~15 min)

Rscript consolidated.R        # all scenarios, 3 periods (~5 min)
Rscript momentum.R            # raw momentum backtest (~3 min)
Rscript omega-momentum.R      # omega ratio backtest (~3 min)
Rscript rank-persistence.R    # persistence analysis (~3 min)
Rscript drop-analysis.R       # drop/add analysis (~3 min)
```

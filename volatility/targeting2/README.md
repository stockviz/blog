# VIX Regime Timing — Long-Short + Fast Overlay Backtest

> Tests whether tilting equity exposure based on India VIX regime signals
> adds value over plain inverse-vol weighting. The long-short version can
> go net short; a fast VIX-spike overlay de-risks during crash windows.
> Result: the regime tilt and overlay both **underperform** the simple
> long-only inverse-vol baseline.

## What's Here

| File | What It Is |
|---|---|
| `backtest_ls.R` | Full R backtest script — all 8 phases (load data → signal → portfolio → stats → sweep → OS → reporting) |
| `vix_regime_long_short_backtest_plan.md` | Pre-registered backtest plan with all phase definitions |
| `vix_regime_long_short_outline.md` | Shorter summary of the claims and test approach |
| `metrics_ls.html` | Performance table (gt) — full-sample metrics for all 4 portfolios |
| `cumulative_full.png` | Cumulative return chart — full sample |
| `cumulative_os.png` | Cumulative return chart — out-of-sample (2019–2026) |
| `sweep_slow.csv` | Slow-signal parameter sweep (window × tilt, 16 combos) |
| `sweep_overlay.csv` | Overlay parameter sweep (sigma × cooldown, 15 combos) |

Long-only plans (`vix_regime_robust_backtest_plan.md`, `vix_regime_testing_outline.md`)
are also present for reference — the long-only version ran separately and reached
the same conclusion (inverse-vol baseline dominates).

## What the Script Does

**Data:** India VIX from `vix_history` (2009–2026), NIFTY 50 TR + NIFTY MIDCAP SELECT TR
from `bhav_index`. Transaction cost 0.5%, 1-day execution lag.

**Slow regime signal (Phase 2):**
- Level score: expanding-history percentile of 6-month average VIX, scaled to [-1, +1]
- Trend score: rolling z-score of 20-day VIX slope, tanh-squashed to [-1, +1]
- Regime score: bullish (high VIX + falling) minus bearish (low VIX + rising), clipped [-1, +1]

**Fast overlay (Phase 2b):**
- VIX spike trigger: VIX > 3σ above 20-day average
- Response: linear decay from flat (0%) back to slow-signal weight over 10 days
- Re-entry: 5 quiet days before handing back to slow signal

**Four portfolios built (Phase 3):**
1. **B&H** — 100% NIFTY 50 TR
2. **InvVol-LO** — `clip(target_vol / VIX, 0, 1.5)`, split by inverse-vol between N50 + Midcap
3. **InvVol-LS** — same base × `(1 + 1.5 × regime_score)`, clipped [-1.5, +1.5]
4. **InvVol-LS+OV** — LS with overlay override during VIX spikes

**Robustness checks:** block bootstrap (p-values, CIs), subperiod decomposition,
grinding-bull stress test, parameter sensitivity sweep (window × tilt), overlay
sensitivity sweep (sigma × cooldown), placebo randomization on overlay timing,
borrow cost, weight bounds, and out-of-sample split (train ≤ 2018).

## Results Summary

### Full Sample (2009-10-07 → 2026-07-10, 4,122 trading days)

| Portfolio | CAGR | Vol | Sharpe | MaxDD |
|---|---|---|---|---|
| B&H (NIFTY 50 TR) | 9.9% | 16.6% | 0.650 | 39.6% |
| InvVol-LO | 9.4% | 13.9% | **0.684** | 30.7% |
| InvVol-LS | -0.8% | 16.2% | -0.004 | 49.9% |
| InvVol-LS+OV | 0.5% | 11.7% | 0.103 | 32.8% |

### Key Findings

1. **Inverse-vol LO is the winner.** Best Sharpe (0.684), lower vol than B&H,
   good subperiod consistency. No regime timing needed.

2. **Regime tilt destroys value.** The LS portfolio has negative CAGR (-0.8%),
   zero Sharpe, and 50% MaxDD. The short leg fires 14% of the time with a **-3.14
   conditional Sharpe** — the short thesis is actively wrong.

3. **Fast overlay helps in crashes but hurts everywhere else.** On COVID (Feb–Apr 2020),
   LS+OV reduced MaxDD from 36.4% to 24.8%. But overlay active days have Sharpe -2.23,
   and 96% of triggers are false positives (outside crash windows). The placebo test
   shows the DD reduction is worse than random — so the overlay effect is noise.

4. **All 16 slow-signal sweep combinations are negative** (Sharpe difference LS − LO
   ranges from -0.22 to -0.36). No parameter combination saves the regime tilt.

5. **Grinding-bull stress test confirms the failure mode.** In 2017, a low-VIX
   slowly-rising year, LS lost -11.1% while LO gained 41.6%. This is exactly
   the adversarial case the plan warned about.

6. **Out-of-sample confirms.** IS Sharpe diff (LS − LO) = -0.85. OS diff = -0.54.
   Both periods negative. No overfitting concern — it's uniformly bad everywhere.

7. **Short leg is UNTESTED** in the sense that when it fires, it loses money so
   decisively (Sharpe -3.14) that the question isn't "is the short thesis right"
   but "why does the short leg exist at all."

### Overlay Sweep Summary

15 combinations of sigma (2.0–4.0) × cooldown (5–20 days):
- Max DD reduction in crash window: 30.8% (σ=2, cool=20)
- False-positive rate: 95–100% across all combinations
- Placebo p-value: 0.000 (random timing beats the overlay's timing)

### Bottom Line

> **The inverse-vol baseline is all you need.** Adding VIX regime timing — whether
> slow (6-month level/trend), short-side (going net short on low+rising VIX),
> or fast (VIX-spike overlay) — uniformly reduces risk-adjusted returns. The
> evidence is consistent across all subperiods, all parameter combinations, and
> both in-sample and out-of-sample windows.

## How to Run

```bash
cd /mnt/data/blog/volatility/targeting2
Rscript backtest_ls.R
```

Requires: R with RODBC, quantmod, PerformanceAnalytics, xts, tidyverse, gt, webshot2.
Database: StockViz SQL Server on `norway` (vix_history, bhav_index tables).

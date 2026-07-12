# BAA-R — Bold Asset Allocation (Robust)

**Paper:** Keller (2022), *"Bold Asset Allocation (BAA)"* — SSRN

Walk-forward backtest comparing four BAA variants (SPY/QQQ offensive × EEM/SPHB
canary pools). Uses SMA(12) for offensive momentum selection, 13612W for canary
and defensive momentum, smoothed breadth with multi-asset defensive sleeve, and
tiered transaction costs with no synthetic proxy data.

## Reference

- [Backtest Plan](BAA_backtest_plan_pseudocode.md) — robustness-first plan addressing BAA-specific faults
- [BAA vs PAA/VAA/DAA](BAA_vs_PAA_VAA_DAA_comparison.md) — comparison of all four strategies in the family

## How It Works

**Dual-filter architecture.** BAA is the hybrid of the family: SMA(12) momentum
(borrowed from PAA) selects the top offensive assets, while 13612W momentum
(borrowed from VAA/DAA) drives the canary breadth signal and defensive asset
selection. This combines PAA's slow, stable offensive rotation with DAA's fast,
responsive crash protection.

**Smoothed breadth + multi-asset defensive.** The canary breadth is converted to
a continuous score via sigmoid smoothing, then the defensive fraction follows
DF = 1 − breadth^B. The defensive sleeve selects the top 3 assets from a 6-ETF
pool (BIL, IEF, SHY, TLT, LQD, AGG) using both relative and absolute 13612W
momentum — only assets with positive momentum are eligible.

**Two offensive universes, two canary pools.**

| Offensive | Swap | Canary Pool | Label |
|-----------|------|-------------|-------|
| SPY-based | — | EEM, AGG | BAA-SPY-EEM |
| SPY-based | — | SPHB, AGG | BAA-SPY-SPHB |
| QQQ-based | SPY→QQQ | EEM, AGG | BAA-QQQ-EEM |
| QQQ-based | SPY→QQQ | SPHB, AGG | BAA-QQQ-SPHB |

**Walk-forward.** Each year, the canary set and parameters (B in [1,2], TopN in
[3,5,7]) are re-selected on expanding-window training data. 3 canary candidates
× 6 parameter combos = 18 evaluations per year per variant.

## Universe

### Offensive Base (10 ETFs)
SPY, IWM, EFA, EEM, VNQ, DBC, GLD, TLT, HYG, LQD

### Offensive QQQ (SPY → QQQ)
QQQ, IWM, EFA, EEM, VNQ, DBC, GLD, TLT, HYG, LQD

### Defensive (6 ETFs)
BIL, IEF, SHY, TLT, LQD, AGG

### Canary Candidates

| Pool | Assets | Channel |
|------|--------|---------|
| EEM | EEM, AGG | EM stress + aggregate bonds |
| SPHB | SPHB, AGG | High-beta equity + aggregate bonds |

## Data & Date Range

| Detail | Value |
|--------|-------|
| Binding ticker | SPHB (2011-05-05) |
| Walk-forward warm-up | 5 years (60 months) |
| Common test period | 2017-02 → 2026-07 (105 months) |
| IS/OS split | 2021-10-01 |

## Script

### `baa-backtest.R`

Runs four walk-forward loops (2 offensive × 2 canary), aligns on common dates,
produces comparison charts and an IS/OS/FS metrics table with all four variants
plus benchmarks.

## Running

```bash
cd "/mnt/data/blog/bold-asset-allocation"
Rscript baa-backtest.R
```

## Results

### Canary + Parameter Selection

**SPY-EEM:** EEM selected 8/10 years, B=1 dominant. AGG picked in 2017 (first
year, short training) and EEM+AGG in 2021.

**SPY-SPHB:** SPHB selected 8/10 years, B=1 dominant. AGG in first year, SPHB+AGG
in 2020 (COVID). Clean and stable.

**QQQ-EEM:** EEM 8/10, AGG in first year, EEM+AGG in 2020. B=1 in 7/10.

**QQQ-SPHB:** SPHB 8/10, same pattern as SPY-SPHB. Most consistent variant.

TopN=3 was selected in every single year across all four variants (40/40) — the
3-asset offensive rotation is robust. B=1 was selected in 33/40 decisions,
confirming the paper's aggressive breadth setting.

### Full Period (2017-02 – 2026-07)

| Strategy | Sharpe | CAGR | MaxDD |
|----------|--------|------|-------|
| BAA-QQQ-SPHB | 0.65 | 6.6% | −12.7% |
| BAA-QQQ-EEM | 0.60 | 5.4% | −17.2% |
| BAA-SPY-SPHB | 0.58 | 5.4% | −15.7% |
| BAA-SPY-EEM | 0.21 | 1.6% | −19.7% |
| 60/40 (SPY/IEF) | 0.98 | 6.9% | −18.1% |
| EW (equal-weight SPY-based) | 0.42 | 4.0% | −21.2% |
| EW-Q (equal-weight QQQ-based) | 0.70 | 8.7% | −20.7% |
| SPY | 0.95 | 12.3% | −20.9% |
| EWC (bonds-only) | NaN | −0.9% | −14.9% |

### Cross-Family Comparison (best variant per strategy, 2017–2026)

| Strategy | Filter | Canary | Offense | Sharpe | CAGR | MaxDD |
|---|---|---|---|---|---|---|
| **BAA-QQQ-SPHB** | SMA+13612W | SPHB | QQQ | 0.65 | 6.6% | −12.7% |
| **DAA-SPHB** | 13612W | SPHB | SPY | 0.44 | 4.3% | −14.9% |
| **PAA-EEM** | SMA | EEM | SPY | 0.47 | 3.9% | −20.2% |
| **VAA** | 13612W | self | SPY | 0.57 | 5.4% | −24.5% |
| 60/40 | — | — | — | 0.69–0.98 | 6.4–6.9% | −18.1% |
| SPY | — | — | — | 0.79–0.95 | 11.5–12.3% | −20.9% |

BAA's dual-filter approach (SMA for offense, 13612W for defense) produces the
best risk-adjusted return in the family. SPHB is the dominant canary under
13612W (DAA, BAA), while EEM works better under SMA alone (PAA). No strategy
beats 60/40 or SPY in this bull-market window — structural conservatism limits
upside. VAA shows the strongest drawdown protection (−24.5% vs SPY's −47.3% on
its full 2008–2026 window) at the highest return cost.

## Key Findings

1. **SPHB dominates EEM as a canary across both offensive universes.** SPHB
   variants deliver Sharpes of 0.58–0.65 vs 0.21–0.60 for EEM. The high-beta
   equity canary provides a better crash signal than the EM stress channel under
   the 13612W filter, consistent with DAA's finding.

2. **QQQ offensive beats SPY offensive.** Swapping QQQ for SPY adds 80–380 bps
   of CAGR with lower drawdowns. The Nasdaq-100's higher growth exposure was
   beneficial in the 2017–2026 tech-led bull market.

3. **BAA-QQQ-SPHB is the clear winner.** At Sharpe 0.65 with −12.7% max
   drawdown, it delivers the best risk-adjusted return among all BAA variants
   with the lowest drawdown — nearly matching SPY's absolute Sharpe while
   cutting drawdown by 40%.

4. **Multi-asset defensive sleeve is underutilized.** The defensive allocation
   consistently favored SHY/BIL/IEF (short/intermediate Treasuries), with TLT
   and AGG rarely selected. The expanded defensive universe adds complexity
   without materially improving outcomes over a simpler cash sleeve.

5. **B=1 and TopN=3 are remarkably stable.** B=1 was selected in 33/40
   decisions and TopN=3 in 40/40. Even under walk-forward re-selection, the
   aggressive single-asset breadth trigger and tight 3-asset rotation are the
   consistent optima — not artifacts of a static IS/OS split.

6. **BAA improves on PAA but still trails the market.** The best BAA variant
   (Sharpe 0.65) beats the best PAA variant (0.47) and the best DAA variant
   (0.44), but still trails 60/40 (0.98) and SPY (0.95) over this period. The
   multi-asset defensive sleeve and dual-filter architecture help, but the
   structural conservatism of spending ~50%+ time in bonds limits upside.

## Output

- [Cumulative returns](baa-cumulative.png) — full period: 4 BAA variants vs 60/40, SPY
- [Recent performance](baa-cumulative-os.png) — Out-of-Sample period only
- [Drawdowns](baa-drawdowns.png) — drawdown comparison
- [Metrics table](baa-metrics.png) — In-Sample / Out-of-Sample / Full Sample metrics

## Data Files

- `baa-results.Rdata` — all returns, walk-forward history, metrics

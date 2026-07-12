# PAA-R — Protective Asset Allocation (Robust)

**Paper:** Keller & Keuning (2016), *"Protective Asset Allocation (PAA): A Simple Momentum-Based Alternative for Term Deposits"* — [SSRN 2759734](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2759734)

Walk-forward backtest comparing two canary-pool variants (EEM/AGG and SPHB/AGG)
using SMA momentum, smoothed bond-fraction blending, and tiered transaction costs
with no synthetic proxy data.

## Reference

- [Backtest Plan](PAA_backtest_plan_pseudocode.md) — unified robustness-first plan for PAA/VAA/DAA
- [PAA vs VAA vs DAA Comparison](PAA_vs_VAA_DAA_comparison.md) — comparison of the three strategies

## How It Works

**SMA momentum filter.** Unlike VAA/DAA's fast 13612W filter, PAA uses a slower,
smoothed SMA(L) filter (L=12 in most years). The SMA momentum score is
log(price / SMA) — positive when above trend.

**Smoothed bond fraction.** The breadth of the canary universe is converted to a
continuous score via sigmoid smoothing, then the bond fraction follows
BF = 1 − breadth^(1+a), where *a* is the protection factor. This avoids the
discrete threshold jumps of VAA/DAA in favor of gradual de-risking.

**Portfolio.** Top N risky assets (equal-weight) blended with the best-performing
bond from the bond candidate pool (IEF, SHY, BIL, AGG). The risky sleeve gets
(1−BF)/N each, the bond sleeve gets BF.

**Walk-forward selection.** Each year, the canary set and parameters (L, TopN, a)
are re-selected using only data strictly prior to the test year. Two canary
pools are tested:

| Variant | Canary Pool | Bond Candidates |
|---------|-------------|-----------------|
| PAA-EEM | EEM, AGG | IEF, SHY, BIL, AGG |
| PAA-SPHB | SPHB, AGG | IEF, SHY, BIL, AGG |

**Parameter grid.** L = [6, 12], TopN = [3, 5], a = [0, 1] — 8 combos × 3
canary candidates = 24 evaluations per year.

**Tiered costs.** 5 bps for liquid ETFs (SPY, IWM, EFA, EEM, GLD, TLT, IEF,
SHY, BIL, AGG), 15 bps for moderate liquidity (HYG, LQD, VNQ, DBC, SPHB).

## Universe

### Risky (10 ETFs)
SPY, IWM, EFA, EEM, VNQ, DBC, GLD, TLT, HYG, LQD

### Bond Candidates (4 ETFs)
IEF, SHY, BIL, AGG

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

### `paa-backtest.R`

Loads daily prices for all 15 tickers, computes SMA momentum inline for each
parameter combination, runs two walk-forward loops (EEM and SPHB canary pools),
aligns on their common date range, and produces comparison charts and an
IS/OS/FS metrics table.

## Running

```bash
cd "/mnt/data/blog/protective-asset-allocation"
Rscript paa-backtest.R
```

## Results

### Canary Selection

**EEM pool:** EEM was selected as the sole canary in all 10 years, with L=12,
TopN=3, a=0 — perfectly stable. No bond-fraction boost needed; the base SMA(12)
signal with 3-asset rotation was consistently optimal.

**SPHB pool:** SPHB was selected every year, but with a=1 in 7 of 10 years
(more protective). The optimizer consistently chose a higher protection factor,
suggesting SPHB's signal was noisier and needed stronger de-risking. In the
final two years (2025–2026) it flipped back to a=0 as late-cycle momentum
dominated.

### Full Period (2017-02 – 2026-07)

| Strategy | Sharpe | CAGR | MaxDD |
|----------|--------|------|-------|
| PAA-EEM | 0.47 | 3.9% | −20.2% |
| PAA-SPHB | 0.31 | 2.1% | −21.3% |
| EW (equal-weight risky) | 0.42 | 4.0% | −21.2% |
| 60/40 (SPY/IEF) | 0.69 | 6.9% | −18.1% |
| SPY | 0.81 | 12.3% | −20.9% |
| EWC (bonds-only) | NaN | −0.9% | −14.9% |

## Key Findings

1. **EEM canary outperforms SPHB.** PAA-EEM delivers Sharpe 0.47 vs PAA-SPHB's
   0.31. This is the opposite of DAA, where SPHB dominated. The slower SMA
   filter appears to work better with the emerging-market stress channel than
   with high-beta equity — a regime-dependent result.

2. **PAA roughly matches equal-weight on raw return.** PAA-EEM's CAGR of 3.9%
   is nearly identical to EW's 4.0%, with marginally better drawdown protection
   (−20.2% vs −21.2%). The bond blending provides only a modest edge.

3. **SPHB pool defaults to higher protection.** The optimizer selected a=1 in
   7/10 years for the SPHB variant vs never for EEM. SPHB's signal was noisier,
   requiring a stronger protection factor — and still underperformed.

4. **Both variants trail 60/40 and SPY.** Over this 2017–2026 window, a simple
   60/40 portfolio (Sharpe 0.69) handily beats either PAA variant. The bond
   fraction's conservatism was a headwind in a predominantly bull market.

5. **Parameter stability is excellent.** L=12 and TopN=3 were selected in every
   single year for both pools — 20/20 decisions. The SMA(12) lookback and
   3-asset rotation appear robust to regime changes, even if the strategy's
   absolute performance is modest.

6. **Smoothed bond fraction avoids whipsaw.** Unlike VAA/DAA's discrete CF =
   b/B trigger, PAA's continuous BF = 1 − breadth^(1+a) produces gradual
   allocation shifts. This likely contributes to the strategy's stability but
   also limits its ability to go aggressively defensive during crashes.

## Output

- [Cumulative returns](paa-cumulative.png) — full period: PAA-EEM vs PAA-SPHB vs 60/40, EW, SPY
- [Recent performance](paa-cumulative-os.png) — 2020-07 onward
- [Drawdowns](paa-drawdowns.png) — drawdown comparison
- [Metrics table](paa-metrics.png) — In-Sample / Out-of-Sample / Full Sample metrics

## Data Files

- `paa-results.Rdata` — all returns, walk-forward history, metrics

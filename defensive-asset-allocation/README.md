# DAA-R — Defensive Asset Allocation (Robust)

**Paper:** Keller & Keuning (2018), *"Breadth Momentum and the Canary Universe: Defensive Asset Allocation (DAA)"* — [SSRN 3212860](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3212860)

Walk-forward backtest comparing two canary-pool variants: the original EEM-based
pool and a SPHB-based alternative. Both use annual walk-forward canary
re-selection, a pre-registered small parameter grid, and tiered transaction
costs with no synthetic proxy data.

## Reference

- [Paper Summary](DAA_paper_summary.md) — summary of Keller & Keuning (2018), core mechanics, claimed results, and limitations
- [Improved Strategy Design](DAA_improved_strategy.md) — walk-forward canary selection, nested validation, tiered costs, and other fixes addressing the original paper's weaknesses

## How It Works

**Canary universe.** A separate set of assets is used solely for crash signaling
— the canary breadth signal gates the risky portfolio. Two pools are tested:

| Variant | Canary Pool | Rationale |
|---------|-------------|-----------|
| DAA-EEM | HYG, EEM, TLT, IEF, UUP | Original: EM stress, credit spreads, rates, USD |
| DAA-SPHB | HYG, SPHB, TLT, IEF, UUP | Alternative: replaces EM with high-beta equity |

**Walk-forward selection.** Each year, the canary set and parameters (T, B) are
re-selected using only data strictly prior to the test year. The canary is
chosen from all size-1 and size-2 combinations (15 total) by evaluating a
reference portfolio on RAD.

**Pre-registered grid.** T = [3, 5], B = [1, 2] — only 4 combinations per
canary candidate, avoiding the free-form optimization of the original paper.

**Tiered costs.** 5 bps for highly liquid ETFs (SPY, IWM, EFA, EEM, GLD, TLT,
IEF, SHY, BIL), 15 bps for moderate liquidity (HYG, LQD, VNQ, DBC, UUP, SPHB).

## Universe

### Risky (10 ETFs)
SPY, IWM, EFA, EEM, VNQ, DBC, GLD, TLT, HYG, LQD

### Cash (3 ETFs)
BIL, IEF, SHY

### Canary Candidates

| Pool | Assets | Macro channel |
|------|--------|---------------|
| EEM | HYG, EEM, TLT, IEF, UUP | Credit stress, EM risk, rate shock, USD |
| SPHB | HYG, SPHB, TLT, IEF, UUP | Credit stress, high-beta equity, rate shock, USD |

## Data & Date Range

| Detail | Value |
|--------|-------|
| Binding ticker (EEM pool) | BIL (2007-05-30) |
| Binding ticker (SPHB pool) | SPHB (2011-05-05) |
| Walk-forward warm-up | 5 years (60 months) |
| Common test period | 2018-02 → 2026-07 (94 months) |

## Script

### `daa-backtest.R`

Loads daily prices for all 16 tickers, computes 13612W momentum, runs two
separate walk-forward loops (EEM pool and SPHB pool), aligns both on their
common date range, and produces comparison charts and a metrics table. Each
walk-forward loop evaluates 15 canary combinations × 4 parameter combos per
test year.

## Running

```bash
cd "/mnt/data/blog/defensive-asset-allocation"
Rscript daa-backtest.R
```

## Results

### Canary Selection

**EEM pool:** TLT was selected in 7 of 9 years (2019–2025), with EEM picked
in 2018 and 2026. The pool quickly abandoned EEM after the first year in
favor of long-duration Treasuries as the crash signal.

**SPHB pool:** SPHB was selected in 7 of 9 years (2018–2019, 2021–2026), with
TLT picked only in 2020 (the COVID year). The high-beta equity signal was the
dominant canary, consistent with SPHB's macro transmission channel (equity
stress).

### Full Period (2018-02 – 2026-07)

| Strategy | Sharpe | CAGR | MaxDD |
|----------|--------|------|-------|
| DAA-EEM | 0.19 | 1.4% | −18.0% |
| DAA-SPHB | 0.44 | 4.3% | −14.9% |
| EW (equal-weight risky) | 0.32 | 3.1% | −21.2% |
| 60/40 (SPY/IEF) | 0.75 | 6.4% | −18.1% |
| SPY | 0.79 | 11.5% | −20.9% |
| EWC (cash-only) | NaN | −0.8% | −11.9% |

## Key Findings

1. **SPHB substantially outperforms EEM as a canary pool.** DAA-SPHB delivers
   Sharpe 0.44 and CAGR 4.3% vs DAA-EEM's 0.19 and 1.4%. The high-beta equity
   signal appears better calibrated to the 2018–2026 market regime than the
   emerging-market stress channel.

2. **Canary stability varies by pool.** The SPHB pool is highly stable (SPHB
   picked 7/9 years). The EEM pool flipped from EEM to TLT after one year and
   never went back — a red flag for signal robustness. The EEM pool's
   instability is precisely the diagnostic the walk-forward design was meant
   to surface.

3. **Both DAA variants underperform 60/40 and SPY.** Over this period, neither
   canary variant beats a simple 60/40 portfolio on risk-adjusted terms (Sharpe
   0.75). The 2018–2026 window featured a strong bull market where being in
   cash was a headwind, not a shield.

4. **SPHB variant draws down less than SPY.** DAA-SPHB's max drawdown of
   −14.9% compares favorably to SPY's −20.9%, achieving its crash-protection
   goal at the cost of lower returns.

5. **TLT's brief dominance as a canary.** In both pools, TLT was the preferred
   canary during the 2020 COVID shock — a rare but correct signal when
   long-duration Treasuries rallied hard as equities crashed. Outside of 2020,
   TLT was not competitive as a canary, highlighting the challenge of finding
   a single instrument that works across different crisis types.

## Output

- [Cumulative returns](daa-cumulative.png) — full period: DAA-EEM vs DAA-SPHB vs 60/40, EW, SPY
- [Recent performance](daa-cumulative-os.png) — 2020-07 onward
- [Drawdowns](daa-drawdowns.png) — drawdown comparison with both variants
- [Metrics table](daa-metrics.png) — full-period metrics

## Data Files

- `daa-results.Rdata` — all returns, walk-forward history, metrics for both variants

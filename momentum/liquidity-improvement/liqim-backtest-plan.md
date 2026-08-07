# Backtest Plan: LIQIM Long-Short vs. Long-Only

**Objective:** Test whether the paper's liquidity improvement factor (LIQIM) delivers a real, investable return premium — both as an academic long-short factor and as a practical long-only strategy — and whether it holds up once realistic frictions are applied.

---

## 1. Research Questions

1. Does a long-short LIQIM portfolio earn a statistically and economically significant return, net of common risk factors?
2. Does a long-only version (just the "improving liquidity" leg, no shorting) still earn a meaningful premium over a passive benchmark?
3. How much of the paper's headline result survives realistic transaction costs, shorting costs, and out-of-sample data?
4. Is the long-only version's performance concentrated in the "long leg gains" or does it depend on the short leg's losses (per Table 9's turnover/cap differences)?

---

## 2. Data Requirements

| Data | Source | Notes |
|---|---|---|
| Daily prices, returns, volume | CRSP (or a proxy like WRDS, Compustat, or a paid vendor like Sharadar/Polygon if no CRSP access) | NYSE, AMEX, NASDAQ common stocks (share codes 10/11) |
| Market cap | CRSP | For NYSE median size breakpoint |
| Fama-French factors (MKT, SMB, HML, RMW, CMA) | Ken French data library | Monthly |
| Momentum factor (UMD/WML) | Ken French data library | For benchmark comparison |
| Risk-free rate | Ken French data library | 1-month T-bill |
| NASDAQ volume adjustment | Gao & Ritter (2010) scalars | 2.0 / 1.8 / 1.6 / 1.0 by period, as in the paper |
| Borrow costs / short-availability | Markit Securities Finance or a proxy (e.g., flag stocks with low market cap as "hard to borrow") | Needed for realistic long-short cost estimate |
| Bid-ask spread or effective spread data (optional) | TAQ, or use Amihud as a proxy | For transaction-cost robustness check |

**Sample period suggestion:** Full replication window 1963–2018 (matches paper) plus an out-of-sample extension 2019–2025 to test whether the effect has persisted or decayed (the paper's own Figure 2 shows a declining alpha trend into the 2010s — the out-of-sample period is the real test).

---

## 3. Signal Construction (Replicating the Paper)

**Step 1 — Illiquidity (ILLIQ):**
Monthly Amihud measure per stock:
```
ILLIQ_i,t = mean over days( 1,000,000 × |return| / dollar volume )
```

**Step 2 — Liquidity Change (LIQC):**
```
LIQC_i,t = -( ILLIQ_i,t - mean(ILLIQ_i, t-11 to t-1) )
```
Positive LIQC = stock got more liquid. Requires a full 12 months of ILLIQ history (impose a min-history filter).

**Step 3 — Sort:**
- 2×3 sort at the start of month *t*: split by NYSE median market cap (Big/Small), then by LIQC within each size group using 30th/70th percentile breakpoints (Deteriorated / Neutral / Improved).
- Rebalance monthly.

**Step 4 — Portfolios:**
- **Long-short LIQIM** = ½(Big Improved + Small Improved) − ½(Big Deteriorated + Small Deteriorated)
- **Long-only LIQIM (LO)** = ½(Big Improved + Small Improved) only — value- or equal-weighted within the leg, benchmarked against the market (MKT) or a matched universe.

Build both value-weighted and equal-weighted versions of each, since the paper shows the pattern is sensitive to weighting (Figure 1, Panels B vs. D).

---

## 4. Portfolio Construction Variants to Test

| Variant | Description | Purpose |
|---|---|---|
| LIQIM (long-short, VW) | As in paper | Baseline replication |
| LIQIM (long-short, EW) | Equal-weighted | Check size-driven results |
| LIQIM-LO (long-only) | Improved leg only, held vs. cash | Investable, no-shorting version |
| LIQIM-LO excess | Improved leg only, held vs. matched market/benchmark | Long-only "alpha" version |
| LIQIM orthogonalized (LIQIM^O) | Residual after regressing on SMB | Matches paper's confound-check |
| LIQIM 20%-of-market (tighter sort) | Footnote 6 variant | Robustness on overlap with momentum |

---

## 5. Benchmark & Comparison Strategies

- Market portfolio (MKT)
- Standard momentum (UMD/WML), long-short and long-only (winners-only)
- Carhart 4-factor and FF5 model portfolios
- Pástor-Stambaugh liquidity factor (LIQV) and Sadka factor (LIQλ), long-short and long-only, as competing liquidity explanations

---

## 6. Performance Metrics

**Return-based:**
- Annualized return, volatility, Sharpe ratio
- Skewness, kurtosis, max drawdown, worst month (paper flags momentum's negative skew/crash risk — check if LIQIM long-only shares this)
- Cumulative return charts by sub-period (pre-2000, 2000–2018, out-of-sample 2019+)

**Risk-adjusted (time-series regressions):**
- Alpha and factor loadings vs. CAPM, FF3, FF5, Carhart
- Run UMD ~ LIQIM (and reverse) spanning tests exactly as in the paper's Tables 3–8
- 10-year rolling alpha/beta plot (replicate Figure 2) — critical given the paper's own evidence of a declining LIQIM beta advantage into 2010s

**Cross-sectional:**
- Fama-MacBeth regression on 25 size/momentum portfolios (Table 10/11 replication) — check if the risk premium sign and significance hold out-of-sample

**Long-only specific:**
- Information ratio vs. market and vs. cap-weighted benchmark matched on size (since the improved leg skews small-cap per Table 9)
- Active share / turnover vs. benchmark
- Up-capture / down-capture ratios

---

## 7. Transaction Cost & Implementation Realism

The paper's own cost estimate (Table 9) uses simplified basis-point assumptions. This should be stress-tested:

1. **Turnover measurement:** Compute actual monthly turnover from the backtest, not assumed values.
2. **Cost model:** Apply a market-impact model scaled by Amihud illiquidity and trade size (e.g., Novy-Marx & Velikov 2016 method cited in the paper, or a square-root impact model) rather than flat bps.
3. **Short-side frictions (long-short only):** Add borrow fees, especially for the deteriorating/illiquid short leg — these are likely to be far more expensive than the paper's flat 150bps assumption for genuinely hard-to-borrow small/illiquid losers.
4. **Capacity check:** Estimate strategy capacity given the long leg tilts small-cap (avg $6.14B in the paper) — test whether the effect survives at realistic AUM levels via a market-impact decay curve.
5. **Rebalance lag:** Add a 1-day (or longer) implementation lag between signal date and trade date to remove look-ahead bias.

---

## 8. Robustness & Falsification Checks

- **Causality check (paper's LIQIM^LAG):** Re-run the strictly pre-formation lagged version to confirm predictive (not just contemporaneous) power.
- **Sub-period stability:** Split 1965–1999 vs. 2000–2018 vs. out-of-sample, as the paper does in Table 12 — confirm whether the long-only leg's premium is also fading, not just the short leg.
- **Volume vs. price-impact decomposition:** Replicate the paper's LIQIM^Δ|R| test to check whether the long-only effect is driven by genuine liquidity/volume dynamics or just mechanical price continuation.
- **Alternative liquidity measures:** Re-run using bid-ask spread or Kyle's lambda (if TAQ data available) instead of Amihud, to check measure-dependence.
- **Placebo/random sort:** Compare against random long-only portfolios matched on size and sector to confirm the improved-liquidity leg isn't just a size or momentum proxy in disguise.

---

## 9. Suggested Workflow / Deliverables

1. Build ILLIQ, LIQC, and LIQIM signals from raw CRSP data; validate against paper's Table 1–2 summary stats as a sanity check.
2. Construct long-short and long-only portfolios (VW and EW).
3. Run full-sample and sub-period performance and regression tests (Sections 6–7 above).
4. Apply transaction cost and capacity overlays.
5. Produce an out-of-sample extension (2019–2025) as the key "does this still work" test.
6. Summarize findings in a comparison table: gross vs. net returns, long-short vs. long-only, in-sample vs. out-of-sample.

---

## 10. Key Open Questions to Flag in Results

- Does the long-only leg's outperformance depend heavily on small-cap exposure that could be captured more cheaply by a simple size tilt?
- Is the alpha concentrated pre-2000 (paper's Figure 2/Table 12 already hints at this), meaning the strategy may be crowded or arbitraged away?
- How sensitive are results to the SMB confound noted in the paper (LIQIM correlates 0.29 with SMB, same as MKT does) — does the long-only version need orthogonalizing too, or does that only matter for the short leg?

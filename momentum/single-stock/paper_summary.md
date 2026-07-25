# Feasible Momentum Strategies in the US Stock Market
**Ammann, Moellenbeck & Schmid (2010)** — [SSRN: Feasible Momentum Strategies](https://ssrn.com/abstract=1694700)

## Overview

The paper tests whether momentum strategies — buying past winners and selling past losers — can be implemented profitably once trading costs are taken seriously. Instead of the classic Jegadeesh & Titman (1993) approach of trading decile portfolios across the whole market (which requires shorting small, illiquid, expensive-to-borrow stocks), the authors restrict the universe to large-cap, liquid **S&P 100** constituents and trade **single stocks rather than portfolios**.

**Data:** Total-return series (dividends reinvested) for the historical S&P 100 constituents, January 1982 – December 2009, with strategies starting in January 1984 (26-year live sample). Historical index membership is replicated to avoid survivorship bias.

## Methodology

- Each month, stocks are ranked by past return over a **formation period J** (3, 6, or 12 months).
- The best-performing stock(s) are "winners" (bought), the worst-performing are "losers."
- Two ways to take the short leg are tested: (a) short the **S&P 100 index**, or (b) short the **individual loser stock(s)**.
- Positions are held for a **holding period K** (3, 6, or 12 months), using the standard overlapping-portfolio construction (K staggered investment "strands" so only 1/K of the book turns over each month).
- A **one-month lag** is inserted between the formation period and the holding period to avoid short-term reversal effects.
- Portfolios of 1, 3, 5, or 10 stocks per side are tested (buying/selling more than just the single best/worst).
- Risk-adjustment is done via CAPM, Fama-French 3-factor, and a conditional (macro-state-dependent) Fama-French model.

## Key Findings

1. **Long single winner stock + short the S&P 100 index** dominates every other variant tested.
   - Monthly excess returns of 0.73%–1.52% across the nine J/K combinations, mostly significant at the 5–10% level or better.
   - Buying more stocks (3, 5, 10) monotonically **reduces** returns — the momentum effect is concentrated in the single best-performing stock.
   - Shorting individual loser stocks instead of the index produces **substantially lower and mostly insignificant** returns, with much higher volatility — the loser-stock short leg contributes noise, not profit.
2. Returns survive CAPM, Fama-French 3-factor, and conditional Fama-French risk adjustments largely intact (alphas only modestly smaller than raw excess returns).
3. Returns are robust to removing the single best monthly return (not a one-hit-wonder result) and hold up under transaction costs of up to ~1–2% roundtrip, depending on holding period.
4. No strong seasonal concentration, though June and Nov–Jan tend to be the best months and Jul/Aug/Oct the worst.

## Best Strategy Identified in the Paper

**Long the single best-performing winner stock, short the S&P 100 index, formation period J = 6 months, holding period K = 3 months.**

| Metric | Value |
|---|---|
| Monthly mean excess return | **1.52%** (significant at 1%) |
| Annualized volatility | 9.85% |
| Sharpe ratio (annualized) | **0.373** (highest of the 9 combos) |
| CAPM alpha (monthly) | 1.42%*** |
| Fama-French 3-factor alpha (monthly) | 1.35%*** |
| Conditional FF alpha (monthly) | 1.28%** |
| Transaction-cost breakeven | Zero at ~193 bps round-trip cost; remains significant at 10% up to ~75 bps |

Close runners-up are **J = 6 / K = 6** and **J = 6 / K = 12**, which have slightly lower but steadier returns (lower turnover, lower transaction-cost sensitivity, still statistically significant Sharpe ratios of 0.41 and 0.45 respectively). The J = 6 formation period consistently performs best across all holding periods — a 6-month look-back appears to capture momentum more reliably than 3-month or 12-month formation windows in this dataset.

## Robustness Checks

- **No annual rebalancing** (letting cohort NAVs drift instead of resetting evenly each year): returns decline meaningfully (by 0.04–0.91 pp/month) and about a third of previously significant results become insignificant. Annual rebalancing across the K overlapping strands matters.
- **Restricting to one stock per strand** (no duplicate winner across strands): returns decline modestly but remain mostly significant.
- Dropping the single best monthly return: return magnitude drops slightly, results hold qualitatively.

## Bottom Line

The paper's core contribution is showing that a **simple, low-turnover, single-stock-long / index-short momentum strategy** on S&P 100 names — cheap to implement because it requires no shorting of small/illiquid stocks — generates economically and statistically significant abnormal returns that survive standard risk adjustments and realistic transaction costs. The best-performing specific parameterization is **J=6, K=3**, though J=6 with longer holding periods offers a lower-turnover, lower-volatility alternative with only slightly reduced returns.

# Backtest Plan: Single-Stock Winner / Index-Short Momentum Strategy

Based on Ammann, Moellenbeck & Schmid (2010), "Feasible Momentum Strategies in the US Stock Market."
Core idea: long the single best-performing large-cap stock over a formation window, short a broad-market index of equal notional, hold for a fixed period, roll on a staggered (overlapping) schedule.

## 1. Objective & Hypotheses

- **H1:** Buying the single best-performing stock (over a J-month formation period) from a large-cap universe and shorting the market index (equal notional) generates positive, statistically significant excess returns.
- **H2:** Returns are concentrated in the long leg — shorting the single worst-performing stock instead of the index underperforms and adds unrewarded volatility.
- **H3:** Returns are monotonically decreasing in the number of stocks held (1 > 3 > 5 > 10) — momentum concentrates in the extreme winner, not the broad top decile.
- **H4:** A 6-month formation period outperforms 3-month and 12-month formation periods.

## 2. Universe Definition

- Define universe as constituents of a broad large-cap index (e.g., S&P 100, S&P 500, or a regional equivalent).
- **Use point-in-time index membership**, not today's constituent list — reconstruct the historical membership as of each formation date. This is the single most important control against survivorship bias.
- For stocks that were listed prior to index inclusion, permit use of their pre-inclusion return history if available (matches the paper's approach), but document this choice — it's a modeling assumption, not free of controversy.
- Exclude the stock from eligibility if there is insufficient history to compute the full J-month formation return (e.g., recent IPOs).

## 3. Data Requirements

| Data | Notes |
|---|---|
| Total-return price series (dividends reinvested) | Not just price-return — momentum computed on unadjusted price return will misrank dividend-paying vs. non-paying stocks |
| Historical index membership (point-in-time) | Needed to reconstruct the tradeable universe at each rebalance date |
| Corporate actions (mergers, delistings, spin-offs) | Needed to define what happens to a held position if the stock leaves the universe mid-holding-period |
| Index level / total return series (for the short leg) | Same total-return convention as the stocks |
| Risk-free / money-market rate | To model return on uninvested collateral, since the long/short pair is cash-neutral at inception |
| Fama-French factor returns (mkt, SMB, HML) + macro conditioning variables (dividend yield, short rate, term spread, default spread) | For risk-adjustment tests |

## 4. Signal Construction

1. At each monthly rebalance date *t*, compute trailing return over the formation window `[t - J, t - 1]` for every eligible stock (using total-return prices).
2. Rank stocks by formation-period return.
3. Select the winner (top 1, or optionally top-N for a sensitivity/diversification variant).
4. **Insert a 1-month skip/lag** between the end of the formation window and the start of the holding period, to avoid the well-documented short-term reversal effect contaminating the signal.

## 5. Portfolio Construction

- **Overlapping strands:** run K parallel investment "strands," one initiated each month, each held for K months. Only 1/K of the book turns over in any given month — this is what keeps turnover and transaction costs manageable relative to a strategy that fully rebalances monthly.
- Each strand: long the winner stock, short the index, in equal notional (cash-neutral at inception); park the (zero net) proceeds conceptually in the money market and accrue the risk-free rate.
- **Annual rebalancing:** once a year, redistribute capital evenly across the K strands (prevents one strand's drift from dominating total NAV). Flag this as a parameter to test — the paper shows it matters a lot (removing it costs 0.04–0.91 pp/month).
- **Bankruptcy/blow-up rule:** if a strand's NAV goes to zero or negative, close it and re-seed it from an equal split of remaining capital.
- **Corporate action handling rules to pre-specify:**
  - Stock leaves the index but stays listed domestically → hold to end of holding period.
  - Stock delists / moves to a foreign-only listing → close at last available price, park proceeds in money market until holding period ends.
  - Merger with stock consideration → roll into acquirer's stock.
  - Merger with cash consideration → park cash in money market until holding period ends.

## 6. Execution & Cost Assumptions

- Assume rebalancing/execution at month-end close (or next-day open — pick one and apply consistently; note the assumption explicitly).
- Short leg via index futures (cheaper, more liquid, avoids stock-borrow costs entirely) — this is a deliberate design choice of the strategy, not just a cost assumption.
- Model transaction costs as a turnover-based basis-point charge applied to both buy and sell legs; run a **cost sensitivity sweep** (e.g., 0–250 bps round-trip in 25 bps steps) rather than a single assumed cost, since breakeven cost varies a lot by holding period (shorter K = much higher effective turnover).
- No leverage beyond the cash-neutral long/short structure unless explicitly testing a levered variant.

## 7. Parameter Grid to Test

| Parameter | Values |
|---|---|
| Formation period J | 3, 6, 12 months |
| Holding period K | 3, 6, 12 months |
| Number of stocks (winner leg) | 1, 3, 5, 10 |
| Short leg | Index vs. individual loser stock(s) |
| Annual rebalancing | On / off |
| Duplicate-stock restriction across strands | Allowed / restricted to distinct stocks |

Report the full 3×3×4×2 grid (72 combos) at minimum for the primary universe, then narrow to the best cell(s) for deeper robustness work.

## 8. Performance & Risk Metrics

- Monthly arithmetic mean excess return, with t-stat / p-value (Newey-West adjusted for serial correlation from overlapping holding periods).
- Annualized volatility, skewness, kurtosis.
- Sharpe ratio (annualized).
- Correlation and beta to the benchmark index.
- CAPM alpha.
- Fama-French 3-factor alpha.
- Conditional Fama-French alpha (betas/alpha modeled as linear functions of lagged macro state variables — dividend yield, short rate, term spread, default spread).
- Max drawdown, time-to-recovery.
- Turnover (for cost-sensitivity linkage).

## 9. Robustness / Sensitivity Checks

- **Sub-period stability:** split the sample (e.g., in half, or by decade) and confirm the effect isn't driven by one regime (paper found returns concentrated in the second half of its sample for some J/K combos — check whether that holds here too).
- **Drop-the-best-month test:** zero out each strand's single best monthly return and re-run; confirm results aren't driven by one outlier month.
- **No-rebalancing variant:** compare against the annual-rebalancing base case.
- **Duplicate-stock restriction variant.**
- **Transaction cost breakeven:** find the cost level at which each J/K combo's mean return crosses zero and where it loses statistical significance.
- **Seasonality check:** mean return by calendar month, to check for concentration in specific months.
- **Alternative short instruments:** index futures vs. ETF vs. synthetic short, if cost data differs meaningfully.

## 10. Bias Controls (critical — verify explicitly before trusting results)

- [ ] **Survivorship bias:** universe reconstructed using point-in-time membership, not today's constituent list.
- [ ] **Look-ahead bias in signal:** formation-period return uses only data available as of the rebalance date; the 1-month skip is applied before the holding period begins.
- [ ] **Look-ahead bias in universe eligibility:** a stock is only eligible if it was actually a tradeable member of the index (or listed with return history) *as of* the formation date — not filtered using knowledge of what happened afterward.
- [ ] **Corporate action handling doesn't leak information:** e.g., don't use the *actual* future merger/delisting outcome to decide the position mid-holding-period faster than it would have been knowable in real time.
- [ ] **Data-snooping / multiple testing:** with a 72-cell grid, expect some combinations to look significant by chance — report the full grid, not just the best cell, and consider a multiple-testing correction (e.g., Bonferroni or reporting out-of-sample/holdout performance) before declaring the best J/K combination "the" strategy.
- [ ] **Point-in-time factor data:** Fama-French and macro conditioning variables should be lagged appropriately (use *known-at-the-time* values, not revised/finalized data unavailable until later).

## 11. Suggested Backtest Sequence

1. Build point-in-time universe + total-return price panel; validate against a few known historical constituent changes.
2. Implement signal + eligibility logic; spot-check a handful of months by hand.
3. Implement single-strand portfolio construction (J=6, K=3, N=1, index short) — the paper's best cell — get it working end-to-end first.
4. Validate against paper's reported numbers as a sanity check (same universe/period, if feasible) before trusting the pipeline on new data/universes.
5. Extend to the full parameter grid.
6. Run cost sensitivity and bias-control checks.
7. Run out-of-sample or holdout-period validation on the best cell(s) identified in-sample, given the multiple-testing concern above.

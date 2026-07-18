# The Intramonth Momentum Cycle — Summary

[SSRN](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=6426026) &middot; *Nathan, Suominen, and Tasa (June 2026)*

## Summary

The paper argues that most of the equity momentum premium isn't really about winners
winning or losers losing based on fundamentals — it's a **plumbing effect** tied to when
institutions need cash.

### Key findings

- **Concentration in time**: Over 1980–2025, $1 invested in the Winners-Minus-Losers
  (WML) portfolio grows to $18.78 if held only during a six-day pre-month-end window
  ("PreTOM," specifically τ−9 to τ−4), versus $2.37 for the other ~15 trading days in the
  month. That's 78% of WML's cumulative return from 29% of trading days.
- **It's a loser story, not a winner story**: Losers underperform the market by ~8bps/day
  during PreTOM but show no such effect the rest of the month. Winners show no
  comparable timing pattern at all.
- **Mechanism — "dispensability"**: Investors facing month-end cash needs sell the stocks
  that are cheapest to part with — those with embedded tax losses, low foregone dividends,
  and salient recent underperformance. A direct dispensability proxy (distance from
  52-week high) predicts the effect even better than the standard 12-month momentum sort
  ($45.72 vs. $18.78 growth).
- **Causal evidence**: The 2024 US T+2→T+1 settlement shortening and the 2014 European
  T+3→T+2 shortening each shifted the loser-selling window one trading day closer to
  month-end, exactly as the cash-timing story predicts.
- **Reversal**: ~70% of PreTOM loser underperformance reverses within about a week,
  consistent with transitory price pressure rather than new information.
- **Broader reach**: The same asymmetric loser-selling shows up in acute liquidity crises
  (Sept/Oct 2008, March 2020) and in mutual fund return persistence (it explains the
  momentum-loading part of Carhart's loser-fund underperformance finding).
- Momentum crashes, by contrast, cluster at *month-start*, not PreTOM — so the PreTOM
  premium isn't just compensation for crash risk.

## How the paper actually implements the idea

### Data sources (five main inputs)
1. **CRSP daily stock panel** (1980–2025) — all NYSE/AMEX/NASDAQ common stocks, merged
   with bid-ask quotes. This is the backbone: 53.3 million stock-day observations.
2. **WRDS TAQ Intraday Indicators** (2003–2022) — Lee-Ready-classified buy/sell volume,
   used to build a direct daily net selling-pressure measure (~17.9 million stock-days).
3. **Morningstar daily fund flows + CRSP Mutual Fund Holdings** (2010–2023) — used to
   trace flow-driven selling pressure down to the stocks funds actually hold.
4. **CRSP Mutual Fund Database** (monthly 1963–2025; daily NAV-based from Sept 1998) —
   actively managed U.S. equity funds only (CRSP objective code ED, ETFs/index funds
   excluded), used for the Carhart persistence decomposition.
5. **Compustat Global** (20 developed markets, 1990–2025) — for the international
   replication and the European settlement-reform test.

### Defining the calendar window (the core trick)
The paper indexes every trading day relative to τ, the **last trading day of the month**
(not the calendar date). τ−1, τ−2, … count backward from month-end; τ+1, τ+2, … count
forward into the next month. **PreTOM** is defined as the fixed six-day window
**[τ−9, τ−4]**, following Etula et al. (2020). This window was *not* fit to the data by
searching for whichever days looked best — it's taken from prior literature and then
validated with a permutation test (p = 0.012) confirming it is the deepest trough in
loser returns. Everything else in the month is "Rest"; a separate 7-day "Post" window
[τ−3, τ+3] is used only for the reversal analysis.

### Portfolio construction
- **Momentum deciles**: every stock is ranked once a month into 10 deciles by cumulative
  return over months t−12 through t−2 (skipping the most recent month), using **NYSE
  breakpoints** applied across all NYSE/AMEX/NASDAQ stocks. Decile assignment is **held
  fixed for the whole calendar month** — it is a monthly sort, not a rolling daily
  re-sort (they note a daily-rebalanced version, from Ken French's library, gives similar
  but slightly smaller numbers: $15.52 vs $2.07).
- **WML portfolio**: value-weighted (previous-day market cap, normalized within decile)
  long top decile (D10) minus short bottom decile (D1). It is **self-financing**: on days
  the strategy isn't "in the market" (e.g., a Rest-only or PreTOM-only backtest), it's
  assumed to earn zero (hold cash), which is how the $18.78 vs $2.37 decomposition in
  Figure 1 is built — literally splitting the same WML return series by calendar-day
  bucket and compounding each bucket separately from $1.

### The core statistical machinery
1. **Fama-MacBeth two-stage regression** (Table 1): Stage 1, each day, regress
   market-adjusted returns on decile dummies (no intercept, value-weighted) to get a
   daily decile-return time series. Stage 2, regress that daily series on a PreTOM dummy
   with Newey-West standard errors — this cleanly separates "PreTOM average" from
   "Rest average" for losers and winners separately.
2. **Stock-level panel regression with fixed effects** (Table 2, the main workhorse):
   `stock_excess_return = β1·Loser + β2·(Loser × PreTOM) + stock FE + day FE + error`,
   run on all 53.3M stock-days, standard errors double-clustered by stock and day. β2 is
   the headline number (−7.15 bps/day value-weighted).
3. **Liquidity-interaction regression** (Table 3): restricted to D1 stocks only, interacts
   PreTOM with the stock's **prior-month average bid-ask spread** to show the effect
   concentrates in liquid (institutionally-tradeable) losers.
4. **TAQ selling-pressure regression** (Table 5): same fixed-effects structure as #2, but
   the dependent variable is (sell volume − buy volume)/total volume instead of returns —
   directly testing for order-flow evidence rather than just price evidence.
5. **Reversal regression** (Table 6): splits the month into three non-overlapping
   buckets (PreTOM, Post, mid-month reference) and adds a Loser × Post interaction to
   quantify how much of the PreTOM underperformance reverses afterward.

### The settlement-reform natural experiment (the causal identification strategy)
This is the paper's cleanest test of mechanism, not just correlation:
- **U.S. T+2→T+1 (May 28, 2024)**: predicts the marginal cash-raising day should shift
  from τ−4 to τ−3. Tested three ways — (a) portfolio-level difference-in-differences
  comparing τ−4/τ−3 returns pre- vs. post-reform against an early-month control window
  [τ+5, τ+8]; (b) a stock-level triple-difference (Loser × day × post-reform) that nets
  out any generic post-2024 repricing by contrasting D1 against D10; (c) event-time bins
  to check pre-trends are flat before the reform.
- **European T+3→T+2 (Oct 6, 2014)**: same logic applied to 15 countries independently,
  then pooled with inverse-variance weights (treating each country as one replication
  unit) rather than run as one big panel — this avoids relying on panel SEs with only 15
  clusters.
- **U.S. T+3→T+2 (Sept 2017)**: run as a third, weaker confirmatory test (underpowered,
  not relied upon alone).
- Falsification checks: placebo boundary days (τ−6 vs. τ−7) and placebo reform dates
  (May 28 of years with no actual reform) show no effect, supporting that the real
  reforms — not just calendar drift — cause the shift.

### Mechanism decomposition (why losers specifically)
Three "dispensability" channels are tested with separate, targeted analyses rather than
bundled into one regression:
- **Tax-loss selling**: re-run the main regression excluding quarter-end months and
  excluding December (the highest tax-incentive month) — the effect barely changes,
  showing tax motives alone don't explain it.
- **Dividend income**: compare non-payer vs. payer shares by decile (84% vs 68%), and
  split dividend-payer stock-months by position in the payment cycle (Month 1/2/3) to
  show selling pressure builds as the next dividend gets farther away.
- **Institutional/salience selling**: cross-reference with S&P 500-only subsamples
  (where institutional ownership is heaviest) and cite independent evidence that
  institutions (unlike retail) actively cut losers under time pressure.
- **Alternative dispensability sort**: re-run the entire PreTOM analysis using distance
  from the **52-week high** instead of the 12-2 momentum sort, as an independent proxy
  that more directly captures embedded losses — this variant produces an even larger
  PreTOM premium ($45.72).

### Extending beyond the calendar cycle
- **Crisis episodes**: hand-picks three acute institutional outflow events (Sept 2008,
  Oct 2008, March 2020) and compares D1 vs. D10 stock and fund returns during those
  specific months only, computing Carhart four-factor abnormal returns to isolate the
  loser-specific component from generic factor exposure.
- **Fund-level Carhart decomposition** (Section 7): re-runs Carhart's classic regression
  at *daily* frequency, split into PreTOM vs. Rest, and sequentially adds a loser-stock
  return control and then fund expense ratios as covariates — showing the momentum-
  related component of fund underperformance is absorbed once you control for loser-
  stock returns specifically during PreTOM, isolating it from expense drag.
- **International replication**: same monthly-sort methodology applied to Compustat
  Global data for 19 conventional-momentum countries (pooled with country fixed effects,
  clustered by country-month) plus Japan reported separately since it's a known
  reversed-sign momentum market.
- **Crash-day analysis**: separately classifies "crash days" (WML < −200bps) and checks
  their calendar location against PreTOM vs. month-start, using a proportions z-test
  rather than a return regression, to rule out crash-risk compensation as an alternative
  explanation.

## Does it address borrow costs?

**No.** The paper does not model or mention short-selling borrow costs, securities lending
fees, "hard-to-borrow" stocks, or short rebates anywhere. The only cost adjustment made is
a **round-trip transaction cost estimate using quoted bid-ask spreads** (following
Novy-Marx and Velikov 2016), which knocks the gross WML premium (91.3 bps/month) down
to a net 11.5 bps/month — with 65% of even that thin net premium coming from PreTOM.
That's a spread/execution-cost adjustment, not a borrowing-cost adjustment.

This is a meaningful gap for implementation purposes: momentum losers are
disproportionately small, distressed, low-priced, and out-of-favor — exactly the profile
of stocks that tend to carry high or "special" borrow fees, or that can become
hard-to-borrow altogether during stress periods (like the 2008/2020 episodes the paper
highlights as showing the *strongest* effect). Borrow costs could disproportionately erode
returns on the short leg precisely when the paper's mechanism is strongest.

## Real-world execution challenges

1. **Borrow costs / short availability** — unaddressed by the paper. Loser-decile stocks
   are more likely to have elevated or volatile borrow fees, and general collateral vs.
   "special" status can flip quickly, especially during liquidity crunches — the very
   episodes where the strategy's edge is largest.
2. **Thin net margins after costs** — gross premium (91.3bps/month) collapses to net
   11.5bps/month using just spread costs. Any additional friction (market impact beyond
   quoted spread, borrow fees, financing costs) could plausibly erase the edge.
3. **Precise timing requirement** — the entire edge lives in a 6-day window per month.
   This demands accurate, ongoing tracking of month-end and monthly decile reassignment,
   with disciplined entry/exit exactly on schedule.
4. **Crowding/capacity risk** — if many arbitrageurs exploit the same well-documented
   6-day window, price impact from crowded entries/exits could compress or eliminate the
   premium going forward.
5. **Turnover and monthly rebalancing costs** — decile membership is reassigned monthly
   and is "sticky" but not static; funds must repeatedly re-sort, re-establish, and unwind
   short positions.
6. **Settlement/margin mechanics** — under T+1 settlement, cash timing is tighter; funds
   need to manage margin and collateral precisely around the shortened cycle themselves.
7. **Liquidity concentration is a double-edged sword** — the effect concentrates in
   *liquid* losers, good for execution feasibility but also where institutional
   competition for the same trade is likely most intense.
8. **Capital scale vs. dispensability proxy** — the 52-week-high sort produces a larger
   premium than standard momentum, but distance-from-52-week-high is itself a
   well-known, crowded signal (George and Hwang 2004).

**Bottom line**: the paper is a compelling empirical/mechanistic account of *why*
momentum returns look the way they do, but it stops short of being a directly
implementable trading strategy — it doesn't model shorting frictions, and its own
net-of-cost numbers already show how thin the exploitable edge is even before accounting
for borrow costs.

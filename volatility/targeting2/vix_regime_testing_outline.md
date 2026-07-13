# Testing the VIX Regime / Inverse-Vol Equity Weighting Hypothesis — Outline

## The Claim, Split Into Two Testable Parts

**Claim 1 — Regime signal:** Conditional on VIX's trailing 6-month *level* (high/low) and
*trend* (falling/rising), forward equity returns or risk-adjusted returns are better/worse
than unconditional.

**Claim 2 — Sizing rule:** `equity_weight ∝ target_risk / VIX`, and low VIX usually coincides
with high weight. This is nearly definitional given the formula — the interesting test is how
often the *regime signal* (Claim 1) disagrees with what plain inverse-vol weighting alone
would prescribe.

---

## 1. Nail Down Precise, Non-Circular Definitions First

- **"High/low for six months"** — relative to what?
  - Trailing 5-year percentile?
  - Full-sample percentile (⚠ look-ahead bias)?
  - A fixed VIX level (e.g. 20)?
- **"Trending down/up"** — how measured?
  - Slope of a 6-month moving average?
  - Current VIX vs. VIX 6 months ago?
  - Sign of a linear regression over the trailing window?
- Plan to test **several** definitions, not just one — the choice materially changes results.

## 2. Data Needed

- Daily VIX (CBOE or FRED series `VIXCLS`), back to 1990 if possible.
- Daily S&P 500 **total return** series (not price-only — dividends matter over long horizons).

## 3. Backtest Construction

- Build the regime signal using **only trailing data** — strictly no look-ahead.
- Build the naive inverse-vol weight for comparison.
- Simulate three portfolios:
  1. Buy-and-hold (100% equity always)
  2. Pure inverse-vol target (no regime overlay)
  3. Regime-tilted (the strategy as described in the quote)
- Include realistic turnover / transaction costs — this signal can trade a lot near VIX spikes.

## 4. Performance Metrics

- CAGR, annualized vol, Sharpe, Sortino, max drawdown, Calmar, turnover.
- Compare regime-tilted vs. pure inverse-vol — the real claim is that the regime overlay adds
  value *beyond* simple inverse-vol targeting, not just that VIX-scaling beats buy-and-hold.

## 5. Statistical Care

- VIX-based signals have heavy serial correlation (overlapping 6-month windows), so naive
  t-stats will overstate significance. Use **block bootstrap** or Newey-West-adjusted stats.
- **Subperiod analysis** (pre/post 2008, pre/post 2020) — VIX-timing strategies are often
  dominated by a handful of episodes (2008, 2020, 2022).
- **Parameter sensitivity sweep** — vary the window length and percentile thresholds. If
  performance only holds for one narrow parameter choice, that's overfitting, not a real effect.
- **Walk-forward / out-of-sample split**, not just a single in-sample backtest.

## 6. Interpretation Checklist

- Does the regime-tilted strategy beat plain inverse-vol on Sharpe/Calmar?
- Is that difference statistically significant under the block bootstrap?
- Does the effect survive across the parameter sweep, or only in one lucky corner?
- Does the edge survive if 2008, 2020, and 2022 are excluded from the sample?

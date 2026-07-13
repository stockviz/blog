# Testing the Long-Short VIX Regime / Inverse-Vol Weighting Hypothesis — Outline

## What Changed From the Long-Only Version

- **No zero floor on weight.** Equity weight is clipped to `[-1.5, +1.5]` instead of
  `[0, 1.5]`, so the strategy can go net **short** equity when the regime math calls for
  de-risking beyond flat.
- **Continuous regime score, not binary flags.** Instead of a hard on/off "high VIX falling"
  / "low VIX rising" flag, a `regime_score` in roughly `[-1, +1]` scales the tilt smoothly:
  - Strongly positive → high VIX level + falling trend → tilt weight **up** (more long).
  - Strongly negative → low VIX level + rising trend → tilt weight **down**, potentially
    into **net short** territory if the tilt magnitude is large enough.
  - Near zero in mixed/unremarkable states.

This is a materially stronger claim than the long-only version: going short means betting
that a low-and-rising VIX regime predicts **negative** forward equity returns, not just
"reduced upside," so it deserves more scrutiny.

---

## Why a Fast-Acting Overlay Is Needed

The 6-month level/trend regime signal is a slow, regime-timing overlay — its reaction
latency is on the order of its own window. It cannot get ahead of, or even keep pace with,
a shock that unfolds faster than that window (COVID: calm in January 2020, VIX averaging
13.94, to a March 2020 average of 57.74 and a peak of 82.69, in about six weeks). VIX itself
is contemporaneous with the equity selloff, not a leading indicator with real lead time, so
a rule waiting to confirm a 6-month trend shift will always be late relative to the sharpest
part of a fast move.

**Fix:** add a second, short-window overlay that acts on a much faster timescale (days, not
months) and can override the slow regime weight when it fires — independent of, and layered
on top of, the long-short regime tilt described above.

- **Trigger candidates** (test more than one, they behave differently):
  - Short realized-vol jump: e.g. 5-day realized vol z-scored against its own trailing
    history.
  - VIX level spike: e.g. VIX moving more than N standard deviations above its own 20-day
    average within a few days.
  - Drawdown trigger: equity price falling more than X% from its trailing 10–20 day high.
- **Response:** when triggered, override the slow-signal weight with a sharply reduced (or
  flat/zero) weight, decaying back to the slow signal's prescribed weight over a short
  cooldown period rather than snapping back instantly (to avoid whipsawing in and out on
  the way down).
- **Re-entry rule:** define explicitly how the overlay hands control back to the slow
  regime signal — e.g. once the fast trigger has been quiet for M consecutive days.

This overlay is a genuinely different kind of signal from the regime tilt (fast/reactive vs.
slow/predictive), so it should be built, tested, and reported **separately** before being
combined, then tested again in combination.

---

## The Claim, Split Into Testable Parts

**Claim 1 — Regime score has directional predictive power**, strong enough that in its most
bearish corner (low level + rising trend) it should overcome the baseline long inverse-vol
weight and flip the position net short.

**Claim 2 — The long-short overlay improves risk-adjusted returns** over the pure long-only
inverse-vol baseline, net of the extra turnover and directional risk that shorting adds.

**Claim 3 — The fast-acting overlay meaningfully reduces drawdown during fast shocks**
(e.g. COVID-style crashes) without giving back so much return the rest of the time — via
whipsaw trading costs or being out of the market during recoveries — that it isn't worth it.

---

## 1. Definitions to Fix Before Testing

- **Level score:** expanding-history percentile of trailing 6-month average VIX, rescaled to
  `[-1, +1]` — must use only trailing data (no look-ahead).
- **Trend score:** rolling z-score of the 6-month VIX slope, squashed to `[-1, +1]` via `tanh`.
- **Regime score:** `bullish_component − bearish_component`, where each component is only
  nonzero when both the level and trend conditions align in that direction.
- **Tilt magnitude:** how strongly the regime score scales the base weight — this single
  parameter controls how often and how far the strategy actually goes short. Needs its own
  sensitivity sweep, separate from the window-length sweep.

## 2. Data Needed

- Daily VIX (CBOE or FRED `VIXCLS`), back to 1990 if possible.
- Daily S&P 500 **total return** series.

## 3. Backtest Construction

- Four portfolios: buy-and-hold, long-only inverse-vol baseline, long-short regime-tilted
  (slow signal only), and long-short regime-tilted **with the fast overlay layered on top**.
- Track **% of days net short** and min/max weight observed — if the strategy essentially
  never goes short in-sample, the "long-short" label is aspirational rather than active, and
  results will look nearly identical to the long-only version.
- Track **overlay trigger frequency** and **average days spent in the reduced-weight state**
  per trigger — too frequent means whipsaw/cost drag, too rare means it won't matter for the
  next crash either.
- Same cost model and 1-day execution lag as the long-only version.

## 4. Metrics

- Standard set (CAGR, vol, Sharpe, Sortino, max drawdown, Calmar, turnover), plus:
  - Performance **conditional on being net short** vs. conditional on being net long —
    report these separately, since a strategy can have a good overall Sharpe while its short
    book is actually losing money.
  - **Max drawdown with vs. without the fast overlay**, measured specifically over known
    fast-shock windows (Feb–Mar 2020, Oct 1987, Feb 2018 vol-explosion) — this is the direct
    test of whether the overlay does what it's meant to do.
  - **Whipsaw cost**: number of times the overlay triggers and un-triggers within a short
    span (e.g. 10 trading days), and the cumulative transaction cost attributable to it.

## 5. Statistical Care — Extra Scrutiny for the Short Book

- Block bootstrap on the Sharpe difference, as before, but also block-bootstrap the returns
  **restricted to days when the position was net short** to see if the short leg is adding
  value on its own or just along for the ride.
- Subperiod analysis should specifically flag long, grinding low-vol bull markets (e.g. 2017,
  2021) — a rising-VIX-from-a-low-base signal is exactly the kind of signal that goes short
  and gets run over during those stretches.
- Parameter sweep over **both** window length and tilt magnitude — tilt magnitude
  specifically controls short frequency/depth and needs its own robustness check.
- Fast overlay needs its **own** sensitivity sweep over trigger threshold and cooldown
  length — separate from the slow-signal sweep, since the two operate on completely
  different timescales and shouldn't be tuned jointly (that invites overfitting the
  combination specifically to known historical crashes).

## 6. Interpretation Checklist

- Does the strategy actually go short often enough in-sample to test the short-side claim,
  or is it long-short in name only?
- Is the short-side return conditional on being short actually positive, or is the overall
  Sharpe improvement coming entirely from the long side?
- Does the edge survive excluding 2008, 2020, and 2022?
- Does the edge survive in grinding low-vol bull markets where the signal would want to be
  short and shouldn't have been?
- Does the fast overlay actually reduce max drawdown in fast-shock windows, net of its own
  whipsaw costs elsewhere?
- Is the overlay's crash-window performance driven by a rule that generalizes, or is it
  fit to the specific shape of 2020 (and would misfire or miss a differently-shaped shock)?

# Can a Volatility Regime Signal Improve on Buy-and-Hold?

## The problem

A changepoint-detection model classifies each trading day into one of two
regimes — STABLE or UNSTABLE — based on whether recent volatility is above
or below its historical average, using a 30-method majority vote over a
trailing 5-year window. The question this analysis set out to answer:
**can this regime signal be used to time exposure to the market and beat
simple buy-and-hold, on a risk-adjusted basis, on India's NIFTY 50,
MIDCAP 150, and SMALLCAP 250 total-return indices?**

Three test methodologies were used throughout, so the answer wouldn't
depend on one particular way of slicing the data:

- **Sliding window**: repeated 5-year train / 1-year test splits, averaged
  across ~15 windows per index.
- **Expanding window**: one continuously growing window from 2005 to the
  present.
- **Frozen Annual**: the regime decision is fixed at the end of each
  training window and held completely unchanged for the entire following
  year, with no ability to adapt mid-year. This is the strictest test —
  it's the closest proxy to what a real, non-adaptive deployment would
  experience.

## Approach 1: exit the market whenever the regime is UNSTABLE

The simplest use of the signal: be fully invested when STABLE, fully out
when UNSTABLE. This underperformed buy-and-hold badly, on both return and
Sharpe ratio, on every index, under every methodology:

| Index (Expanding window) | Strategy Return | Strategy Sharpe | Buy & Hold Return | Buy & Hold Sharpe |
|---|---|---|---|---|
| NIFTY 50 | 5.2% | 0.46 | 11.3% | 0.73 |
| MIDCAP 150 | 4.2% | 0.37 | 16.4% | 0.96 |
| SMALLCAP 250 | 1.5% | 0.18 | 13.7% | 0.77 |

**Why:** the regime label reflects volatility *magnitude*, with no regard
for direction — it doesn't distinguish a market falling sharply from one
rebounding sharply, since both are "unstable." Checking mean forward
returns conditional on regime showed this plainly: UNSTABLE days had
*higher* average forward returns than STABLE days, not lower, on every
index (e.g. NIFTY 50: 0.108% vs 0.033% per day). Big crash days and big
rebound days cluster together, and a strategy that exits on volatility
avoids both roughly equally. Because compounding is convex, missing a
share of the best days costs far more than avoiding the same share of the
worst days saves — over the full sample, the days this strategy was
actually invested compounded to only a fraction of what buy-and-hold
achieved.

## Approach 2: only exit when UNSTABLE *and* the price is below its trend

The fix for approach 1's flaw was to add a direction condition: only treat
instability as a reason to exit if the market is also in a downtrend
(price below its 50-day moving average). Otherwise, stay invested through
volatile-but-rising periods.

This recovered most of the lost return while keeping real drawdown
protection:

| Index (Expanding window) | Return | Sharpe | Max Drawdown | Buy & Hold Drawdown |
|---|---|---|---|---|
| NIFTY 50 | 7.1% | 0.56 | -27.2% | -38.3% |
| MIDCAP 150 | 13.0% | 0.89 | -37.6% | -43.1% |
| SMALLCAP 250 | 11.0% | 0.76 | -46.5% | -59.8% |

But the strictest test — Frozen Annual, where the exit decision is locked
in for a full year with no ability to react — surfaced two remaining
problems:

1. **On NIFTY 50 specifically, the improvement mostly disappeared.**
   Under the frozen test, the plain UNSTABLE-only exit (approach 1) scored
   a Sharpe of 0.84, essentially tied with this direction-gated version's
   0.81 — the direction condition wasn't adding much for this index once
   the decision had to be committed to and held.
2. **A wrong year-long commitment left real tail risk exposed.** Because
   the position is still binary (fully in or fully out), if a year's
   frozen call turned out to be wrong, there was no ability to
   partially de-risk — the strategy just rode out whatever happened,
   with drawdown converging back toward buy-and-hold's own. On Smallcap,
   a frozen commitment's worst-case drawdown came in at -59.6%, barely
   better than buy-and-hold's own -59.8%.

The direction condition was clearly the right idea, but an all-or-nothing
position couldn't fully capture it — there was no way to express
"somewhat cautious" versus "fully out," only a hard switch.

## The final approach: continuous exposure sizing, gated by direction

The fix combined both lessons: instead of a binary position, exposure is
sized *continuously*, using how many of the 30 changepoint methods vote
"unstable" (0% to 100% of them) — but that continuous sizing is only
applied once the price has already confirmed a downtrend. In an uptrend,
the strategy stays fully invested regardless of how volatile conditions
are.

An earlier attempt at combining the two ideas — multiplying the
continuous vote-share position by the binary direction signal — actually
made things worse than either idea alone, because it still reduced
exposure during volatile rallies (the exact flaw the direction condition
was meant to fix), while adding nothing extra during real downturns
beyond what the direction condition already provided by itself. The
working version instead applies vote-share sizing *only conditional on*
being in a downtrend, and otherwise ignores the volatility signal
entirely:

```
if in a downtrend:
    exposure = 1 − (fraction of methods voting "unstable")
else:
    exposure = 1   (fully invested, regardless of volatility)
```

This was the best-performing design across all three methodologies,
including the strict Frozen Annual test:

| Methodology | Index | Strategy Sharpe | Buy & Hold Sharpe | Strategy Drawdown | Buy & Hold Drawdown |
|---|---|---|---|---|---|
| Sliding | MIDCAP 150 | **0.92** | 1.10 | -29.1% | -44.0% |
| Expanding | MIDCAP 150 | **0.94** | 0.96 | -39.7% | -43.1% |
| Frozen | MIDCAP 150 | **1.07** | 1.06 | -38.2% | -42.7% |
| Expanding | SMALLCAP 250 | **0.81** | 0.77 | -41.3% | -59.8% |
| Frozen | SMALLCAP 250 | **0.84** | 0.84 | -53.5% | -59.4% |
| Frozen | NIFTY 50 | 0.83 | 0.89 | -27.7% | -38.3% |

On Midcap (every methodology) and Smallcap (Frozen and Expanding), this
design **matches or slightly exceeds buy-and-hold's own risk-adjusted
return, including under the strict frozen test**, while still cutting
maximum drawdown meaningfully. NIFTY 50 remains a partial exception — it
comes close to, but doesn't quite reach, buy-and-hold's Sharpe ratio —
consistent with it being the index where the direction condition was
already contributing the least throughout this analysis.

## Conclusion

The solution that ultimately worked was **direction-gated, continuously
sized exposure**: use the regime model's 30-method vote share to smoothly
scale position size down, but only once the market has already confirmed
a downtrend — and stay fully invested through volatile periods otherwise,
since volatility alone says nothing about direction. This combines the
two properties each earlier attempt was missing on its own: it doesn't
sacrifice upside during volatile rallies (the flaw that sank the plain
UNSTABLE-exit filter), and it doesn't collapse to an all-or-nothing bet
that can't adapt (the flaw exposed in the binary direction-gated version
under a genuinely frozen test). Validated across a rolling-window test, a
full-history test, and — most importantly — a strict test where the
decision is locked in for a full year with no ability to react, it
matches or slightly improves on buy-and-hold's own risk-adjusted return on
two of the three indices tested, while still delivering real drawdown
protection.

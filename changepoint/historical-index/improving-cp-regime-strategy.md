# Improving the Changepoint (CP) Regime Strategy

## Context

As established in `why-cp-filter-underperforms.md`, the CP filter
underperforms buy-and-hold because it flags **volatility magnitude**, not
**direction** — it exits the market on high-volatility days regardless of
whether those days are crashes or rebounds, and in this data the
"unstable" periods actually had *higher* average forward returns than the
"stable" ones. The ideas below target that specific flaw, plus some
secondary costs (turnover drag, binary all-or-nothing exposure).

Several of these were tested directly against the cached price and regime
data (`prices_index.Rdata`, `window-class-cache.Rdata`), applied
consistently (same 0.2% per-flip drag, same 50-day SMA) so the comparisons
are like-for-like. These numbers are from a full-history run and won't
exactly match the earlier sliding-window report, but they're internally
consistent for comparing designs against each other.

## 1. Make the regime signal direction-aware (tested — biggest lever)

Instead of exiting the market whenever CP says UNSTABLE (the current
"AND" logic — in market only if uptrend **and** stable), only treat it as
risk-off when UNSTABLE and the price is below its trend (SMA) — i.e.,
require both signals to agree it's a *bad* kind of instability, rather
than requiring both to agree it's *safe*:

```r
# Current: pos <- ifelse(price > SMA & regime == 1, 1, 0)
# Alternative:
pos <- ifelse(regime == 0 & price < SMA, 0, 1)
```

| Index | Buy & Hold | Current (AND) | Alt (exit only if unstable & downtrend) |
|---|---|---|---|
| NIFTY 50 | 11.8% / Sharpe 0.76 / DD -38% | 4.2% / 0.49 / -23% | **7.5% / 0.59 / -27%** |
| MIDCAP 150 | 16.6% / 0.97 / -43% | 8.3% / 0.86 / -21% | **13.1% / 0.90 / -38%** |
| SMALLCAP 250 | 14.0% / 0.79 / -60% | 7.6% / 0.82 / -24% | **11.3% / 0.78 / -46%** |
| Time in market | 100% | ~43–53% | ~83–91% |

This recovers most of the return sacrificed by the current design while
still trimming drawdown versus raw buy-and-hold. It doesn't beat the
current design's Sharpe or drawdown control outright — the current AND
version is more conservative for less return — but it's a much better
return-per-day-out-of-market trade-off, and it directly fixes the "exits
right before the rebound" problem.

## 2. Position sizing instead of binary in/out (tested)

Rather than a hard 0/1 position, scale exposure down (e.g. to 50%) during
UNSTABLE instead of exiting entirely:

```r
pos <- ifelse(regime == 1, 1.0, 0.5)
```

This lands between the current design and full exposure (e.g. NIFTY 50:
8.4% return, Sharpe 0.65) — a smoother compromise. The 50% scaling factor
was arbitrary here; it's worth tuning as a parameter rather than treating
it as fixed, and could reasonably vary by index.

## 3. Add hysteresis / a minimum dwell time (tested)

The regime flips 3.7–6.5 times per year across the three indices, and each
flip costs drag (0.2% per the current assumption). Requiring the UNSTABLE
signal to persist for a minimum number of days (5 was tested) before
acting on it cuts turnover and its associated cost, at the expense of
slower reaction speed:

```r
# pseudocode: only switch out of the market if regime==0 has persisted >= N days
```

Worth tuning the persistence window (`N`) as an explicit parameter and
checking the turnover/return trade-off curve as `N` varies.

## 4. Use the vote share as a continuous signal, not a binary label (untested — promising)

`regime_tbl` already contains `N_Unstable` and `N_Total` from the 30-method
majority vote — a 28/30 vote is a much stronger signal than 16/30, but the
current design collapses both to the same binary UNSTABLE label. Using
`N_Unstable / N_Total` to modulate position size (e.g. scale exposure down
proportionally to vote strength, rather than a step function) would likely
reduce whipsaw around ambiguous, near-50/50 days without losing
responsiveness on high-conviction ones.

## 5. Use regime as a hedge/overlay trigger rather than a full exit (untested)

Instead of moving fully to cash, use "UNSTABLE and downtrend" as a trigger
to buy protective puts, trim (not zero) exposure, or rotate into a
lower-beta sleeve. This keeps some market participation while still
cutting tail risk, and avoids the all-or-nothing cost structure of a
binary position.

## 6. Use changepoint dates as event triggers, not just segment labels (untested)

Currently the whole segment between changepoints carries one label
regardless of how the market behaves afterward. A large detected
changepoint could instead trigger a short, explicit de-risking window
(e.g. N trading days) that decays back to full exposure over time, rather
than remaining "unstable" for the full duration of the segment.

## 7. Cross-index confirmation (untested)

Regime is currently computed independently for NIFTY 50 / Midcap /
Smallcap. Checking whether all three agree on regime simultaneously could
serve as a market-wide risk-off signal (more macro, less single-index
noise), and might reduce false positives driven by single-index
idiosyncratic volatility rather than genuine market-wide stress.

## Summary

| Idea | Status | Effort | Addresses |
|---|---|---|---|
| Direction-aware regime (asymmetric exit) | Tested — clear improvement | Low | Core flaw (avoiding both tails) |
| Position sizing instead of binary | Tested — moderate improvement | Low | Cost of full exit |
| Hysteresis / minimum dwell time | Tested — reduces turnover | Low | Whipsaw / drag |
| Vote share as continuous signal | Not tested | Low-Medium | Whipsaw on ambiguous days |
| Hedge/overlay instead of full exit | Not tested | Medium | Cost of full exit |
| Changepoint dates as event triggers | Not tested | Medium | Segment label rigidity |
| Cross-index confirmation | Not tested | Medium | False positives |

The direction-aware regime redefinition (#1) is the most impactful and
cheapest change to make, since it directly targets the root cause
identified earlier — the filter can't currently tell "volatile and
falling" from "volatile and recovering."

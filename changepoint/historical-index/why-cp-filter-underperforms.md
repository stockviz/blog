# Why the Changepoint Regime (CP) Filter Underperforms Buy-and-Hold

## Summary

Once the date-alignment bug is fixed (see `sliding-window-bug-report.md`),
the CP strategy underperforms buy-and-hold on all three indices. This isn't
a backtest artifact — it's a real, explainable property of the filter
design: it flags **volatility**, not **direction**, and in this market the
biggest up-days and biggest down-days cluster together in the same
high-volatility periods. Avoiding one means avoiding the other.

## The filter has no opinion on direction

`classify_regime()` labels a period "UNSTABLE" when its segment volatility
(mean absolute return) exceeds the window's overall average — regardless of
whether the large moves are up or down. The strategy exits the market on
UNSTABLE days. That only adds value if high-volatility periods are, on
average, bad days to hold the index. Checking that assumption directly
against the data shows it's false here.

## Evidence: UNSTABLE periods actually have higher average returns

| Index | Mean daily fwd. return, STABLE | Mean daily fwd. return, UNSTABLE |
|---|---|---|
| NIFTY 50 TR | 0.033% | **0.108%** |
| NIFTY MIDCAP 150 TR | 0.034% | **0.155%** |
| NIFTY SMALLCAP 250 TR | 0.023% | **0.117%** |

The "unstable" days the model correctly flags as high-volatility are, on
average, **3–5x better** days to hold the index, not worse. This is
consistent with well-documented volatility clustering in equity markets:
big crash days and big relief-rally days tend to occur close together, and
post-selloff rebounds are often outsized. Looking at the 20 best and 20
worst single days historically for each index, the filter is "in the
market" (STABLE) for roughly the same fraction of both tails — it doesn't
separate the good tail from the bad tail, because it isn't designed to; it
just avoids both together.

## Why this is so costly: convexity of compounding

The strategy is out of the market 20–37% of trading days, depending on the
index. Because compounding is convex, missing even a modest share of the
largest up-days destroys far more terminal wealth than avoiding the
largest down-days saves.

| Index | Buy-and-hold cumulative | While-in-market-only cumulative | Missed upside while "out" |
|---|---|---|---|
| NIFTY 50 TR | +451% | +152% | +118% |
| NIFTY MIDCAP 150 TR | +1,032% | +137% | +378% |
| NIFTY SMALLCAP 250 TR | +676% | +56% | +397% |

## Secondary cost: turnover drag

The regime flips 3.7–6.5 times per year on average across the three
indices. At 0.2% cost per flip, that's roughly **12–21% cumulative drag**
over the ~16-year sample — not the main driver of underperformance, but it
compounds the problem on top of the missed-upside effect above.

## Takeaway

The underperformance is a genuine, explainable property of the filter, not
a bug: an absolute-volatility (magnitude-only) regime signal applied to an
index whose worst and best days both originate from the same
high-volatility bursts will tend to duck out right before the rebound as
often as before the drawdown. If the goal is real downside protection, a
more promising design would be **asymmetric** — e.g., conditioning on
signed returns or recent trend direction — so the filter can distinguish
"volatile and falling" from "volatile and recovering," rather than treating
all high-volatility periods as equally worth avoiding.

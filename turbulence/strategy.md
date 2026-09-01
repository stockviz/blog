# Bézier-Curve Market Turbulence Strategy

This document summarizes the trading strategy described in *Quantum Temporal
Winds: Turbulence in Financial Markets* by Haoran Zheng and Bo Dong.

## Core idea

The strategy treats large price movements as possible reversals around dynamic
support and resistance levels. These levels are represented by smooth Bézier
curves fitted using historical price turning points.

A Bézier curve is a smooth curve controlled by a set of points. For example, a
quadratic Bézier curve is:

\[
B(t)=(1-t)^2P_0+2(1-t)tP_1+t^2P_2,\qquad 0\leq t\leq1.
\]

The endpoint points define the start and end of the curve, while the other
control points influence its shape.

## Strategy construction

1. Collect historical market prices, including highs, lows, opens, and closes.
2. Detect important turning points or inflection points.
3. Use those points as control points to construct smooth upper and lower
   Bézier curves.
4. Interpret the upper curve as a dynamic resistance boundary and the lower
   curve as a dynamic support boundary.
5. Monitor the curvature of the curves or a related volatility measure. Greater
   curvature or wider Bollinger bandwidth is interpreted as greater volatility.

## Turbulence confirmation

The paper confirms a possible turbulent market using moving averages:

- Calculate a 20-day short-term moving average.
- Calculate a 50-day long-term moving average.
- Compare the two averages.
- If their absolute difference exceeds a chosen threshold, classify the market
  as turbulent.

## Trading rules

Only trade when a boundary signal and turbulence confirmation occur together:

| Condition | Signal |
|---|---|
| Price reaches or exceeds the upper Bézier boundary and the market is turbulent | Sell or short |
| Price reaches or falls below the lower Bézier boundary and the market is turbulent | Buy |

The upper boundary represents a possible overvaluation or reversal area. The
lower boundary represents a possible undervaluation or rebound area.

## Risk interpretation

- High Bézier curvature suggests unstable, rapidly changing conditions. A
  risk-averse trader might reduce exposure or hedge.
- Low curvature suggests relatively stable price movement, although it is not a
  guarantee of rising prices or low future risk.

## Important limitations

The paper does not specify enough detail to reproduce or validate the strategy
fully. In particular, it does not clearly define:

- the exact Bézier-curve fitting procedure;
- how turning points are detected;
- the moving-average threshold;
- position sizing and leverage;
- stop-loss and take-profit rules;
- transaction costs, slippage, and short-selling constraints; or
- a rigorous out-of-sample performance test and benchmark.

Therefore, this should be treated as an exploratory research framework rather
than a validated or ready-to-trade system. Bézier curves are used here as a
smoothing and boundary-identification tool; they do not independently predict
future prices.

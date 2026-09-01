Bézier-Curve Market Turbulence — exploratory futures test

The paper does not fully specify the implementation. This run uses rolling quadratic
channels from prior closes, MA20/MA50 normalized spread turbulence, one-day signal lag,
unit exposure, reversal positions held until the opposite boundary, and 25 bps per unit traded.
BHAV_EQ_FUT is close-only in this data path; closes are used instead of OHLC pivots.
NIFTY uses the canonical monthly futures calendar; SELECT uses the validated synthetic
MIDCAP SELECT front-month series from midcpnifty-synth-futures.
Train selection: through 2019-12-31. Post evaluation: from 2020-05-01.
Metrics and cumulative+drawdown charts are emitted for pre, post, and full windows.

This is an exploratory research implementation, not a validated trading system.

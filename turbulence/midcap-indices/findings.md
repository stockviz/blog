Trend-filtered reversal long/flat on MIDCAP 150 TR and SMALLCAP 250 TR

The strategy goes long after a turbulent lower-boundary event. An upper-boundary
event exits to flat only when the lagged MA50 is not rising; rising-MA50 upper
events are ignored to avoid shorting persistent advances.

Each index selects its channel length and turbulence threshold independently using
only data through 2019-12-31. Signals are lagged one day and drag is 25 bps per
unit traded. Metrics and charts use pre <= 2019-12-31, post >= 2020-05-01, and full
windows. The intervening 2020-01-01 to 2020-04-30 period is excluded from named
pre/post comparisons.

The TR series is used for P&L and the channel is close-derived, matching the
existing regime-aware turbulence experiment. This is exploratory research, not
a validated trading system.

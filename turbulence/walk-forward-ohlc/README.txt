Combined walk-forward OHLC turbulence study

Each one-year test period selects both the strategy interpretation and its channel/threshold
from the preceding five years. The selected system is then applied unchanged to the next year.
Systems: reversal long/flat, reversal long/short, breakout long/flat, breakout long/short,
and reversal with a rising-MA50 short filter in long/flat and long/short forms.
Turning points use observed OHLC with a two-bar confirmation delay; all signals are lagged one day.
NIFTY uses observed futures OHLC. SELECT close is the validated synthetic front-month close;
SELECT OHLC is scaled from MIDCAP SELECT PR where available and uses NIFTY 50 OHLC as a range
proxy before that, so pre-real-futures SELECT OHLC is synthetic.
Train/test: five years train, one year test; named report windows use pre <= 2019-12-31 and
post >= 2020-05-01. Trading drag is 25 bps per unit traded.
This remains an exploratory implementation and is not a validated trading system.

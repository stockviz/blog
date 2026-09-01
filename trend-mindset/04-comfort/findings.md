04 ETR comfort ratio and drawdown adherence

The Python implementation follows the book-inspired protocol: comfort accrues
outside material drawdowns, discomfort accrues when either drawdown depth or
duration exceeds the threshold, and ETR is cumulative comfort divided by
cumulative discomfort. It evaluates 5%, 10%, and 20% depth thresholds with 63,
126, and 252 trading-day duration thresholds, daily and monthly sampling, and
abandonment rules.

Verified output

The generated etr_summary.csv contains 10 instrument/strategy rows. At the
primary 10% / 126-day setting:

- NIFTY B&H: CAGR 11.62%, Sharpe 0.61, MaxDD 60.14%, ETR daily 0.15, monthly 0.48.
- NIFTY Timing Long/Flat: CAGR 5.43%, Sharpe 0.42, MaxDD 54.27%, ETR daily 0.09, monthly 1.48.
- SELECT B&H: CAGR 14.32%, Sharpe 0.74, MaxDD 45.09%, ETR daily 0.17, monthly 0.64.
- SELECT Timing Long/Flat: CAGR 8.54%, Sharpe 0.62, MaxDD 26.65%, ETR daily 0.24, monthly 0.87.
- SMALLCAP 250 TR trend-filtered long/flat: CAGR 16.19%, Sharpe 0.82, MaxDD 65.20%.

The monthly ETR ranking can differ materially from daily ETR because sampling
changes the count of comfort/discomfort observations. ETR is therefore a
behavioral diagnostic, not an alpha-selection criterion. The abandonment file
shows that simple depth/duration redemption rules can lock in losses and miss
subsequent recoveries; it should not be used as a trading rule without a separate
re-entry design.

Files
- etr_comfort.py: executable implementation.
- etr_summary.csv: summary diagnostics.
- etr_results.csv: threshold/sampling results.
- abandonment_results.csv: adherence/abandonment simulations.

02 Controlled rule-entry versus random-entry

Scope
- EMA(50), 20-day breakout, and reproducible random 50% entries.
- Same causal one-day-lagged position construction and 3% trailing-exit scaffold.
- Five fixed seeds: 11, 29, 71, 101, 211.
- NIFTY and synthetic SELECT; 25 bps per position change.

Measured post-window mean across seeds

| Instrument | Rule | CAGR | Sharpe | MaxDD | Mean turnover |
|---|---|---:|---:|---:|---:|
| NIFTY | Breakout | -6.7% | -1.07 | 32.4% | 17.0% |
| NIFTY | EMA | 4.9% | 0.49 | 20.3% | 7.2% |
| NIFTY | Random | -21.1% | -2.15 | 77.6% | 49.8% |
| SELECT | Breakout | -0.3% | -0.00 | 17.4% | 16.1% |
| SELECT | EMA | 17.1% | 1.13 | 21.0% | 5.6% |
| SELECT | Random | -18.9% | -1.41 | 75.7% | 49.8% |

Interpretation

The random-entry control is decisively worse than the EMA rule in the post
window on both instruments. The breakout implementation used here is not a
positive candidate: it had negative mean post CAGR on NIFTY and approximately
flat CAGR on SELECT with materially higher turnover. This supports retaining
rule-based entry as a candidate and rejecting the tested random-entry substitute.
The trailing-exit scaffold is intentionally held constant across rules; it is
not evidence that a 3% stop is optimal.

Files
- run.R: executable five-seed experiment.
- entry_metrics.csv: 90 rows.
- entry_summary.csv: grouped means by instrument, rule, and window.

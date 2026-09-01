# Combined Walk-Forward OHLC Turbulence Study

## Protocol

This study combines the regime-aware strategy comparison with causal walk-forward parameter and system re-selection.

- Five-year expanding training window.
- One-year non-overlapping test window.
- Each test year re-selects both the strategy interpretation and the channel/threshold from the preceding five years.
- Candidate systems: reversal long/flat, reversal long/short, breakout long/flat, breakout long/short, and trend-filtered reversal long/flat and long/short.
- Turning points use daily OHLC and a two-bar confirmation delay.
- Signals are applied with a one-day lag.
- Trading drag is 25 bps per unit traded.
- Report windows remain pre through 2019-12-31 and post from 2020-05-01.

## Data treatment

NIFTY uses observed rolled futures OHLC from `BHAV_EQ_FUT`.

SELECT uses the validated synthetic front-month close from the existing synthetic futures series. Its OHLC is scaled from MIDCAP SELECT PR OHLC where available. Before MIDCAP SELECT PR OHLC coverage begins, the script uses NIFTY 50 OHLC as an explicit range proxy and scales that range to the synthetic SELECT close. Therefore, the turning-point logic is genuinely OHLC-based, but the pre-real-OHLC SELECT range is synthetic rather than observed SELECT futures OHLC.

## Results

All figures below include the 25 bps transaction drag.

| Window | Instrument | System | CAGR | Volatility | Sharpe | Max Drawdown |
|---|---|---|---:|---:|---:|---:|
| Pre | NIFTY | Walk-forward selected | 5.7% | 15.6% | 0.43 | 37.5% |
| Pre | NIFTY | B&H | 12.9% | 23.1% | 0.64 | 60.1% |
| Pre | SELECT | Walk-forward selected | -4.6% | 14.0% | -0.27 | 25.9% |
| Pre | SELECT | B&H | 9.9% | 19.6% | 0.58 | 21.7% |
| Post | NIFTY | Walk-forward selected | 9.4% | 13.3% | 0.74 | 17.2% |
| Post | NIFTY | B&H | 15.7% | 15.0% | 1.05 | 17.2% |
| Post | SELECT | Walk-forward selected | 12.8% | 15.2% | 0.87 | 20.9% |
| Post | SELECT | B&H | 27.6% | 19.8% | 1.33 | 24.7% |
| Full | NIFTY | Walk-forward selected | 5.2% | 16.0% | 0.40 | 38.7% |
| Full | NIFTY | B&H | 12.3% | 21.7% | 0.64 | 60.1% |
| Full | SELECT | Walk-forward selected | 5.6% | 14.8% | 0.45 | 26.9% |
| Full | SELECT | B&H | 16.1% | 21.0% | 0.82 | 45.1% |

## Interpretation

Walk-forward re-selection improves the post-2020 SELECT result substantially compared with the original fixed reversal specification, but it still does not beat buy-and-hold on CAGR or Sharpe. It reduces post-period SELECT drawdown from 24.7% to 20.9%.

For NIFTY, the selected strategy also reduces volatility and drawdown in the post period, but its CAGR and Sharpe remain below buy-and-hold. The pre-period result is weak, especially for SELECT, showing that annual re-selection does not remove the underlying regime and sample-composition risk.

The selection history is in `walk_forward_selection.csv`. It shows that breakout long/flat is selected most often, with occasional trend-filtered reversal and reversal selections. This is consistent with the earlier regime-aware result: the channel boundary has more value as a continuation signal than as an unconditional mean-reversion signal in the later bull-market sample.

## Verdict

The combined walk-forward/OHLC implementation is a more realistic test than the fixed-parameter close-only experiment, but it does not establish a deployable edge. The best evidence is risk reduction in the post period, not outperformance versus futures buy-and-hold. SELECT conclusions before observed MIDCAP SELECT OHLC coverage begins should be treated as sensitivity results because their daily ranges are synthetic.

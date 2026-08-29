# 08 — Evaluation & Pitfalls (Level 5–6)

> How to know a strategy is real vs overfit. Sources: S3 Ch.6/10, S4 §3–4, S7 §4, S8 §2–5.

## 8.1 Rigorous assessment (Théate's protocol) [CLOSE]

S4 §3 introduces a **more rigorous performance assessment methodology** motivated by broad-market test:
- Train on artificial trajectories from limited history (bootstrap [Ch.04]), not just the one path.
- Test on **many instruments** (Théate: 30 diverse stocks) → reports distribution of Sharpe, not one lucky asset.
- Baseline zoo: buy&hold, sell&hold, random, market index.
- Transaction-cost sweep (c=0 vs realistic) — strategy that only works at c=0 is rejected.
- Statistical test: distribution of strategy returns must beat benchmarks with significance; single-equity backtest is insufficient [S4 §4].

Hilpisch adds (Ch.6 §6.4, Ch.10):
- Walk-forward (rolling train/test) not single split.
- Keep random agent as floor; any RL agent should crush it — if not, bug.

## 8.2 Metrics

| Metric | Formula | Use | Pitfall |
|--------|---------|-----|---------|
| PnL / CAGR | `Π (1+r_t) -1` | Absolute profit [S5] | Ignores risk; intraday needs daily reset |
| Sharpe | `mean(r)/std(r)·√period` | Risk-adjusted [S4, S6] | Non-additive, period-sensitive; thin intraday sample inflates |
| Sortino | `mean(r)/std(r | r<0)` | Downside only | Rare in folder; useful intraday where upside spikes are common |
| Max drawdown | `max peak-to-trough` | Capitulation risk | One episode drawdown ≠ multi-day |
| Turnover | `Σ|Δa| / T` | Cost exposure | Intraday turnover often 10× daily — costs dominate net Sharpe [S7] |
| Hit rate / precision | `P(r·a >0)` | Diagnostic | High hit rate ≠ positive expectancy if losers large |

S6's multi-objective `U = α·mean - β·std` is effectively a tunable Sharpe proxy for intraday where sample per day `k` is fixed [S6 §3.3].

## 8.3 Overfitting anatomy

| Cause | Symptom | Mitigation (source) |
|-------|---------|---------------------|
| One-path training | Trains well, collapses on next year | Artificial trajectories [S4]; MCS/GAN augmentation [S3 Ch.4–5]; KS validation |
| No transaction cost | Sparkling gross, flat net | Always report net; commission per contract [S7 2.5 rub] or bps [S4]; Si includes costs inside `R_t` [S6] |
| Lookahead leakage | Impossible in-sample Sharpe | Z-score fit on train only; ensure `s_t` excludes `r_{t+1}` [S3 §3.1] |
| State too small | POMDP; policy oscillates | Add LSTM/memory [S6, S7] or expand lag window |
| Reward hacking | High reward, poor Sharpe/drawdown | Evaluate on risk-adjusted and drawdown even if reward was PnL [S4] |
| Venue-specific features | RTS futures profit doesn't port to ES | Blame overfit to order-book microstructure [S7 §4]; test on second venue |
| GAN mode collapse | Synthetic looks good, test fails | KS test per Hilpisch §5.3; train synthetic → test real |

## 8.4 Frequency-specific evaluation traps

**[CLOSE] traps:**
- Daily bars smooth noise → apparent predictability that vanishes net of costs [S2 notes costs omitted initially].
- Regime shift (e.g., 2008, 2020) exposes non-stationarity; rolling window evaluation captures this [S8 §2.5].

**[INTRADAY] traps (all intraday papers stress these):**
- **Microstructure noise** dominates at 1-min/60-sec; raw returns near white noise — needs feature learner [S6] or order-book imbalance [S7], not raw OHLC.
- **Overnight gaps** break continuity; eval per-day (episode = session) and reset LSTM [S6].
- **Slippage assumption:** backtest at mid-price overstates; use bid/ask or `mid ± half-spread` [S7 anonymized bids already reflect this].
- **Small sample per day:** 390 one-min bars → Sharpe denominator noisier; Si's daily-aggregated `mean/std(DR)` dampens this [S6].

## 8.5 Baselines to beat (in order)

1. Random agent [S3 §2.4.2, §6.1] — must lose to.
2. Buy&hold / Hold [S5 Table 6, S4 §4] — market beta.
3. Simple rule (MA cross, TWAP for execution) — practitioner sanity.
4. Supervised predictor + rule-based trader [S1 SL vs RL §, S8 §2 contrast].
5. Published RL baseline (TDQN for daily [S4], A3C+LSTM for intraday [S7]).
6. Your prior agent — ablation (e.g., remove LSTM, remove sentiment [S5], switch Sharpe→PnL).

## 8.6 Reproducibility checklist

- Fix `seed`, log `Δt`, `lookback`, `commission`, `reward`, `algorithm`, `network depth`, `replay size`.
- Save artifact trajectories for Théate-style artificial bootstrap audit.
- Report both gross and net, train and test Sharpe with error bars [S4 §4].
- Intraday: report per-day statistics (mean of daily PnL, std across days), not per-bar pooled [S6 §4].
- Execution: report implementation shortfall vs arrival price [S3 §9.2].

## 8.7 The two questions to ask before deploying

Hilpisch Ch.10 "Concluding Remarks" + S8 §7 synthesize:
1. **Did we overfit to a regime?** If trained 2010–2020 bull, test 2022 bear. Data augmentation partially answers; out-of-sample across decades is better.
2. **Does the agent exploit a friction that will vanish?** Latency, stale quotes, illusory arbitrage introduced by bar resampling — intraday profits of 60+% p.a. [S7] must be haircut for market adaptation.

If both answers are "checked and robust," proceed to paper-trade; otherwise loop to Ch.04.

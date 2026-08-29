# Phase 2 — Close-out (executed 2026-08-29)

Full scorecard of the plan's Phase 2 items (research-plan.md §5) and the
M2 exit-gate verdict. Details per run in research/phase1/runs/*.

## Scorecard

| Plan item | Run | Result |
|---|---|---|
| R2.1 Si pipeline, NIFTY 50 1-min, U reward | runs/r21/ | DONE — gate FAILS (per-day SR -0.86); discovery: intraday-long NEGATIVE (SR -1.33); returns live overnight |
| R2.2 algorithm comparison (A3C+LSTM, PPO+LSTM) | runs/r22b/ | DONE (partial): PPO-MLP -89.5, PPO-LSTM (RecurrentPPO) -48.3 — catastrophic per-bar churn; A3C/A2C-LSTM UNAVAILABLE on SB3 2.9 (recurrent A2C removed; sb3_contrib ships only RecurrentPPO) — documented limitation |
| R2.3 risk-aversion (vol-cond alpha/beta, 1-min VIX, TVT p_off) | runs/r22/ (reward redesign), runs/r23c/ (features + vol-cond beta) | DONE — feats_sr -0.86 (features did not cure SR overfit), feats_u21_vc -1.18 (flat-collapse persists); U-family flat-optimality robust to alpha/beta/features |
| R2.4 generalization litmus (cross-instrument + cross-venue) | runs/r24/ | DONE — NIFTY-50 feats_sr policy transfer: BANK -1.40, MIDCAP 50 **+0.31**, IT -0.81, MCXGOLDEX -2.58. ONE weak positive transfer (MIDCAP 50); mostly negative |
| Augmentation (mandatory on MCX) | — | NOT attempted — documented; MCX indices (MCXGOLDEX etc., continuous) are the venue per user directive |
| 2x slippage stress | runs/r24/ | FAILS — feats_sr -1.95 @10bps (vs -0.86 @5bps) |
| Per-day Sharpe discipline, walk-forward folds | all runs | DONE in every run |

## M2 exit-gate verdict: NOT MET

- Net per-day Sharpe > 1 on NIFTY 50: NO (best -0.86, feats_sr).
- ...and >=1 more NSE index: NO positive gate-eligible result anywhere.
- Walk-forward-stable (>=2 post-2020 folds positive): NO (0/7 on BANK,
  1 positive month on gold, none stable).
- Positive transfer to >=1 instrument/venue: WEAK (MIDCAP 50 +0.31 only).
- Survives x2 slippage: NO.
- Turnover sustainable: NO (PPO arms churn to ruin; Si U arms collapse to
  flat).

## Conclusion

Phase 2 is COMPLETE as a documented NEGATIVE, per the plan's honest-
outcome clause (M2: "even if negative — intraday is independent of daily
verdict"). The phase's durable scientific outputs:

1. **Market structure**: NIFTY-family cash-index intraday is negative-carry
   in 2020+ (the return lives in overnight gaps — intraday-long SR -1.33
   on NIFTY 50, -0.55 on BANK); MCX gold (futures and index) is zero-drift;
   neither venue offers learnable intraday alpha to any of the tested
   agents (Si-LSTM U/SR, PPO, PPO-LSTM).
2. **Objective lessons**: the S6 U-family objective makes flat optimal
   (robust to alpha/beta, features, vol-conditioning); the SR-type
   objective trades but overfits the train window OOS; per-bar PnL
   rewards with continuous actions churn to ruin.
3. **Infrastructure**: MCX indices (MCX*DEX in zd_index_bars) are the
   correct MCX venue (no expiry, no duplicate-token traps); A3C/A2C-LSTM
   requires an SB3 < 2.3 stack or a manual recurrent A2C.

Per the plan, the next decision point is M4 (deployment decision) or
Phase 3 (per-track) — with the accumulated evidence, neither daily nor
intraday RL beat classical baselines on this estate's current data.

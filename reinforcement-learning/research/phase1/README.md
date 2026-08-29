# Phase 1 — Daily [CLOSE]: TDQN replication & NIFTY variants

Status: PHASE 2 COMPLETE (2026-08-29) as a documented NEGATIVE — M2 gate
NOT MET. Scorecard in runs/r24/findings.md: R2.1 (r21) FAILS; R2.2
(r22b) PPO -89.5 / PPO-LSTM -48.3 (A3C/A2C-LSTM unavailable on SB3 2.9);
R2.3 (r22, r23c) feats_sr -0.86, feats_u21_vc -1.18; R2.4 transfer litmus
(r24): MIDCAP 50 +0.31 (only positive), BANK -1.40, IT -0.81, MCXGOLDEX
-2.58; 2x slippage FAILS (-1.95). Durable outputs: (1) NIFTY-family
cash-index intraday = negative carry (overnight dominance; intraday-long
SR -1.33/-0.55); MCX gold (futures AND MCX*DEX index) = zero drift; no
learnable intraday alpha for any agent (Si U/SR, PPO, PPO-LSTM); (2) S6
U-family makes flat optimal; SR-type overfits; per-bar PnL churns;
(3) MCX work uses the MCX*DEX indices (no expiry/duplicate tokens).
Next per plan: M4 deployment decision / Phase 3 — with the accumulated
evidence, neither daily nor intraday RL beats classical baselines here.

## R1.1 — Replication & honesty check

TDQN (Double+Dueling, differential-Sharpe reward) on NIFTY 50 PR daily,
trained on S4-style artificial trajectories (block bootstrap), tested
out-of-sample — vs the baseline zoo (random, B&H, MA20/50 net of 25bps) and
the R2 prior agents (ABS@FF60 Stop-Cash book; TVT-HMM index gate A).

| Run | Index | Data | Windows | Status |
|---|---|---|---|---|
| `runs/nifty50/` | NIFTY 50 PR | train 2000-2014 / test 2015+ | pre/post/full | FAILS floor (churn: 177 flips/yr × 25bps) |
| `runs/midcap_select/` | NIFTY MIDCAP SELECT | train 2004-2014 / test 2015+ (closes 2004+, OHLC 2022+ → close-only loader) | pre/post/full | FAILS floor (same churn: 200 flips/yr; art beats real again) |

| File | Role |
|---|---|
| `scripts/train_r11.py` | producer: `--index` / `--tag` / `--start` / `--episodes`; trains `tdqn_art` + `tdqn_real`, saves `runs/<tag>/checkpoints/` |
| `scripts/analyze_r11.py` | consumer: same args; eval + metrics/deltas/turnover/charts + honesty verdicts -> `runs/<tag>/findings.md` |
| `cache/prior_*.csv` | prior-agent daily returns extracted from R2 checkpoints (R) |

Conventions: pre <= 2019-12-31 / post >= 2020-05-01 / full; cost-in-reward
(25bps per flip); stacked cum+drawdown charts with end labels; @StockViz.

## Run

```bash
cd /mnt/data/books/RL/research/phase1
# NIFTY 50 (defaults)
/mnt/ssd1/pyenv/bin/python scripts/train_r11.py --episodes 500      # ~10 min, background
/mnt/ssd1/pyenv/bin/python scripts/analyze_r11.py
# NIFTY MIDCAP SELECT (closes from 2004; OHLC from 2022 — loader falls back to close-only)
/mnt/ssd1/pyenv/bin/python scripts/train_r11.py --index "NIFTY MIDCAP SELECT" --tag midcap_select --start 2004-01-01
/mnt/ssd1/pyenv/bin/python scripts/analyze_r11.py --index "NIFTY MIDCAP SELECT" --tag midcap_select
```

## Next (after R1.1 verdicts)

R1.2 reward sweep (PnL vs Sharpe vs differential-Sharpe vs multi-objective)
— the fix agenda for the R1.1 floor failure; R1.3 state design (+VIX,
+TVT-HMM p_off, +breadth); R1.4 augmentation comparison; R1.5 broad-market.

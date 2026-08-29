# Reinforcement Learning for Finance — Research Story
### A technical narrative of the StockViz RL program (Phases 0–2, M4)

> **TL;DR** — We built a rigorous RL-for-finance research program on the
> StockViz data estate (daily + 1-minute Indian market data), executed it
> phase-by-phase against a written plan with house backtesting conventions,
> and produced a fully documented NEGATIVE: across every reward, cost,
> state, action, algorithm, universe, and venue we tried, no RL agent
> beat classical baselines net of costs out-of-sample. The program's real
> outputs are durable market-structure findings (NIFTY-family intraday is
> negative-carry; returns live in overnight gaps), a taxonomy of RL failure
> modes (drift-betting, flat-optimality, churn), and a reproducible
> pipeline to test the next idea. Deployment verdict: no.

This document tells that story the way engineers like it: as a narrative
with the numbers intact, the plot twists documented, and the receipts in
`research/phase1/runs/*`.

---

## Chapter 0 — The setup

Everything lives under `/mnt/data/books/RL/`:

- `kb/` — an 11-file knowledge base distilled from 8 sources (Hilpisch's
  RL-for-Finance book, Théate & Ernst TDQN, Si et al. intraday LSTM,
  Ponomarev A3C, Hambly et al. survey, ...) with a frequency matrix.
- `research-plan.md` — the master plan: Phase 0 (infrastructure) →
  Phase 1 (daily [CLOSE]) → Phase 2 (intraday [INTRADAY]) → Phase 3
  (frontier), with M0–M4 milestones, exit gates, and an honest-outcome
  clause (negative results are legitimate conclusions).
- `research/phase0/` — the infrastructure: a tested `rl` package
  (39/39 unit tests) with data loaders (daily SQL Server + intraday
  Postgres, per-table tick-epoch decoders), six Gym environments,
  four agent families (DQN/TDQN, Si-LSTM BPTT, SB3 PPO/A2C), house
  metrics/charts, and a run registry mirroring the R2 tree's `run-all.R`.
- `research/phase1/` — the experiments, one `runs/<id>/` directory each,
  every one with checkpoints, params.json, metrics CSVs, house charts
  (stacked cum+drawdown, end-labeled, @StockViz), and a findings.md.

House conventions held throughout: pre ≤ 2019-12-31 / post ≥ 2020-05-01,
costs in-reward, no-lookahead (state at t, reward on r_{t+1}), per-day
(never per-bar pooled) intraday statistics, fixed seeds, and a classical
baseline zoo (random, buy-and-hold, MA20/50) as the floor every agent
must clear.

---

## Chapter 1 — The daily track: every lever, pulled

### R1.1 — The Théate TDQN replication (runs/r11*, r15/runs/r15)

Replicated TDQN (Double+Dueling, differential-Sharpe reward) on NIFTY 50
daily with S4-style artificial-trajectory training (2000–2014), tested
2015+. **Verdict: catastrophic churn** — 177–222 position flips/year at
25bps ≈ 45–55%/year drag; full-window Sharpe -2.08 vs MA +0.76 and B&H
+0.64. Repeated on NIFTY MIDCAP SELECT with identical failure → the
mechanism, not the data, was broken.

### R1.2 — Reward × cost sweep (runs/r12/)

Five arms isolating reward and cost: differential-Sharpe churns at any
cost (confirmed broken as an immediate reward); PnL @5bps and
churn-penalized PnL @25bps learn to HOLD (Sharpe -2.08 → +0.19/+0.30).
The mechanism fix worked; the floor (beat MA net of costs) still failed
by d = -0.55.

### R1.3 — State design (runs/r13/)

VIX z/change + TVT-HMM p_off as state: +0.76 Sharpe ablation vs the
control, and the best post-2020 window result of the daily track — the
regime features deliver exactly the de-risking information the agent
lacked. Still no floor crossing.

### R1.5 — Cross-sectional shared agent (runs/r15/, r15_ff60/)

One shared TDQN over a top-50 FF universe looked POSITIVE (book +1.21 vs
MA +1.14, 94% names positive). **Plot twist**: widening to the FF60
universe (970 names) exposed it as *drift-betting* — the pnl-reward agent
learns the training window's best CONSTANT direction. The 4-attempt
collapse taxonomy (runs/r15_ff60/findings.md): under-coverage
(always-short), drift-betting (always-short in a bear slice,
always-flat in the long-only corner), and finally a stable-but-edge-free
policy after de-drifted trajectories + per-stock vol-scaled rewards +
long-only actions. The top-50 "win" was regime alignment, not skill.

### R1.6 — PPO continuous allocation (runs/r16/)

Weekly simplex allocation over FF60 momentum sleeves (EW/Q5/Q1/cash)
with de-drifted training: +0.89 Sharpe vs Q5-only +1.72, EW +1.47, MA
+1.35. The learned posture — 42% cash + a bottom-quintile tilt — is
"defensive with a bad contrarian bet".

### R1.7 — The rich-state upgrade (runs/r15_ff60_rich/)

Identical config to the stable R1.5 control, state upgraded with VIX,
p_off, cross-sectional momentum rank, and 12m relative strength: book
+0.58 (vs +0.46), MaxDD **8.9%** — the best drawdown of the entire daily
track (MA 15.7%, B&H 25.3%). The state upgrade is real on the risk side;
the floor still stands (d = -1.11). **The one reproducible daily skill
is drawdown avoidance, and it under-earns.**

### Daily track verdict

Closed as a documented negative across reward, cost, state, action,
normalization, universe width, and allocation. RL's only reproducible
daily contribution: drawdown avoidance.

---

## Chapter 2 — The intraday track: where the returns actually live

### R2.1 — Si pipeline on NIFTY 50 1-min (runs/r21/)

Faithful S6 port: FC feature learner → LSTM → tanh position, episode =
one trading day (375 bars), U = α·mean(DR) − β·std(DR), BPTT on −U.
**The discovery of the program**: intraday-long on NIFTY 50 in 2020+ is
a LOSING trade (SR -1.33, -14.8%/yr, MaxDD 63.8%) while daily B&H earns
+15.4% — the index's return is carried entirely by OVERNIGHT gaps; the
intraday session has negative drift. The agent, meanwhile, learned FLAT
(the U objective makes inaction optimal — U=0 beats any noisy strategy).

### R2.2 — Reward redesign on BANK NIFTY (runs/r22/)

U(1,1) -0.60, U(2,1) -0.95 (flat-optimality robust to α/β), SR-type
-1.12 (trades — fixes the collapse — but overfits the 2015-19 intraday
regime and loses -18%/yr OOS). BANK intraday-long is also negative
(-0.55). The overnight-dominance finding generalizes across the NIFTY
family.

### R2.3 — MCX gold: futures → indices (runs/r23/, r23b/)

The futures contract (GOLD26OCTFUT) had 61 train days and a
duplicate-token trap in zd_master (lesson: verify bar counts per token).
**Per user directive, MCX work moved to the MCX*DEX indices** in
zd_index_bars (continuous, no expiry; MCXGOLDEX real coverage 2020-11+).
Result: gold intraday is ZERO-drift (SR +0.17, +1.2%/yr) — no negative
carry, but no harvestable drift either. U(2,1) +0.14 ≈ flat; SR-type
-2.53. The only positive intraday fold in all of Phase 2: gold 2026-07
(+1.28), which broke in August.

### R2.2b/R2.3c/R2.4 — The close-out (runs/r22b/, r23c/, r24/)

- **R2.2 plan-spec (algorithm comparison)**: PPO-MLP -89.5, PPO-LSTM
  (RecurrentPPO via sb3-contrib) -48.3 — per-bar PnL rewards with
  continuous actions churn to ruin. A3C/A2C-LSTM is unavailable on SB3
  2.9 (recurrent A2C removed; documented limitation).
- **R2.3 plan-spec (risk-aversion)**: 1-min INDIA VIX + TVT p_off state
  + vol-conditioned β (new env/agent capabilities, tested): feats_sr
  -0.86, feats_u21_vc -1.18 — features did not cure the SR overfit; the
  U-family collapse persists.
- **R2.4 transfer litmus**: the best NIFTY-50 policy onto other venues:
  MIDCAP 50 +0.31 (the only positive), BANK -1.40, IT -0.81,
  MCXGOLDEX -2.58. 2× slippage: -1.95 (fails).

### Phase 2 verdict

M2 gate NOT MET. Full scorecard: `research/phase1/runs/r24/findings.md`.
Durable market-structure knowledge: NIFTY-family cash-index intraday =
negative carry (returns live overnight); MCX gold = zero drift; no
learnable intraday alpha for any tested agent.

---

## Chapter 3 — M4: the deployment decision

`research/m4/findings.md` answers the KB 08 §8.7 questions:

1. **Did we overfit to a regime?** Yes — extensively documented
   (drift-betting taxonomy, SR-type intraday overfit, same-regime
   train/test in the only "positive" run). Not robust → blocks
   deployment.
2. **Does the agent exploit a vanishing friction?** No — no friction was
   found; the dominant friction is real (per-bar costs).

**Decision: do not deploy RL.** Classical momentum books (ABS 365 @
FF90 / PROB 365 @ FF80 with the TVT-HMM Gate-B overlay) remain
production. A shadow paper-trade (classical vs the R1.7 drawdown-avoider)
monitors the gap monthly, with explicit promotion thresholds and revisit
triggers (MCX history > 2y, new venues, order-flow state).

---

## The numbers (net of costs, out-of-sample)

| Experiment | Best RL result | Classical floor | Delta |
|---|---|---|---|
| R1.1 TDQN daily (NIFTY 50) | -2.08 | MA +0.76 | -2.84 |
| R1.2 reward sweep | +0.30 | MA@5 +0.85 | -0.55 |
| R1.3 state (VIX/p_off) | +0.23 post +0.68 | MA +1.69 | -1.11 |
| R1.5 top-50 book | +1.21 | MA +1.14 | +0.07 (drift-betting) |
| R1.5 FF60 book | +0.46 | MA +1.69 | -1.23 |
| R1.7 rich-state book | +0.58 (MaxDD 8.9%) | MA +1.69 | -1.11 |
| R1.6 PPO allocation | +0.89 | Q5 +1.72 | -0.83 |
| R2.1 Si intraday (N50) | -0.86 | flat 0.0 | -0.86 |
| R2.3b gold index | +0.14 | flat 0.0 | +0.14 (≈0) |
| R2.2 PPO / PPO-LSTM | -89.5 / -48.3 | flat 0.0 | — |
| R2.4 transfer (MIDCAP50) | +0.31 | flat 0.0 | +0.31 (weak) |

## The lessons (durable, reusable)

1. **Drift-betting**: pnl-reward RL learns the training window's best
   constant direction. De-drifted artificial trajectories + per-stock
   vol-scaled rewards are the fix; same-regime train/test windows
   masquerade as edge.
2. **Flat-optimality**: the S6 U-family objective makes inaction optimal
   (robust to α/β, features, vol-conditioning). Sharpe-type rewards trade
   but overfit the train window. Per-bar PnL with continuous actions
   churns to ruin.
3. **Where the alpha lives**: on Indian NIFTY-family data, daily returns
   are overnight; intraday-long is negative-carry; gold is zero-drift.
   Test the venue before the agent.
4. **Drawdown avoidance is the one learned skill** — and it under-earns
   in melt-ups. Regime features (VIX, filtered P(risk-off), cross-
   sectional rank) are what deliver it.
5. **Infrastructure gotchas paid for in blood**: gymnasium
   `action_space.sample()` is not seeded by `env.reset(seed=)`; the
   Si-intraday env's per-step day-mask was a 100x bottleneck; the
   momentum loader's cache key ignored the symbol filter; zd_master
   carries duplicate tokens (verify bar counts per token); MCX work uses
   the MCX*DEX indices.

## Reproduce it

- Phase 0 tests: `cd research/phase0 && /mnt/ssd1/pyenv/bin/python -m pytest tests/`
- Every run: `research/phase1/scripts/*.py` (train/analyze pairs per
  experiment; see each runs/<id>/params.json + findings.md for config and
  verdicts).
- House charts/metrics: `research/phase0/rl/eval/` (strategy_metrics,
  plot_cum_drawdown — @StockViz, stacked cum+dd, end labels).

## Epilogue

A negative result, properly executed, is a deliverable: it kills a
hypothesis family with evidence and sharpens the map of where the alpha
isn't. When the estate changes (more MCX history, new venues, order-flow
data) or a classical-blind spot is identified, the pipeline is standing
and the next hypothesis has a floor to beat.

*Written by the StockViz RL research program — @StockViz*

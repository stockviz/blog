# RL for Finance — Research Plan

> Grounded in the knowledge base (`kb/`, 8 sources) and the **verified** data
> inventory below (probed 2026-08-28 against StockViz NORWAY/SWEDEN). The KB
> is the map (algorithms, envs, pitfalls); this document is the route and the
> evidence trail. Frequency tags [CLOSE]/[INTRADAY] follow kb/README.md.

---

## 0. Goal

Build, validate, and — only if they survive the evaluation protocol — deploy
reinforcement-learning trading agents on the StockViz data estate. Success is
**not** "an RL agent that prints a Sharpe" — it is a documented agent that
beats the baseline hierarchy (KB 08 §8.5) **net of realistic costs, out of
sample, across instruments**, with the same discipline the R2/backtests tree
already enforces (pre/post windows, cost-in-reward, walk-forward, metrics +
charts + findings per phase).

Framing rule (KB 09 §9.8): RL is a numerical solver for sequential decisions
where closed forms and hand-tuned rules fail — not a replacement for the
existing momentum/regime work. The prior agents we have (canonical stop-loss
momentum books, TVT-HMM gated books, full Sharpe ≈ 2.0) are the **high bar**
RL must beat; expect the honest answer to often be "not yet" on daily data,
and the real opportunity in intraday/execution where classical rules are weak.

## 1. Data inventory (verified 2026-08-28)

### 1.1 Daily [CLOSE] — SQL Server `StockViz` (NORWAY)

| Dataset | Coverage | Notes | KB tag |
|---|---|---|---|
| `bhav_index` NIFTY 50 PR | 1990-07-03 → now (8,790 rows) | OHLC effective ~1995+ (memory: full 1995+; TR from 1999-06) | [CLOSE] |
| NIFTY 50 TR | 1999-06-30 → now | total-return variant | [CLOSE] |
| NIFTY MIDCAP 150 PR/TR, SMALLCAP 250 PR/TR | 2005-04-01 → now | PR OHLC effectively ~2018-19 (rows before carry zeros) | [CLOSE] |
| `VIX_HISTORY` (India VIX) | 2009-03-03 → now (4,304 rows) | regime/vol state feature | [CLOSE] |
| `BHAV_EQ_FUT` NIFTY futures | 2000-06-12 → now, 321 expiries | also BANKNIFTY (2005+), FINNIFTY (2021+), MIDCPNIFTY (2022+) | [CLOSE] |
| `PX_HISTORY` SERIES='EQ' | 1994-11-03 → now, 5,144 symbols | `eod_adjusted_nse` (PG) 3,117 tickers from 2001-01 | [CLOSE] |
| `MOMENTUM_ABS` / `MOMENTUM_PROB` | 2015-09 / 2010-01 → now | 50/100/365 lookbacks — state features for equity RL | [CLOSE] |
| `EQUITY_MISC_INFO`, `CORP_ACTION`, `RETURN_SERIES_ALL` | as used in R2 tree | universe filters, adjustment | [CLOSE] |
| `mf_nav_history` (cash proxy), `ZERO_COUPON_CURVE` | 2006+ / n/a | risk-free asset for allocation env | [CLOSE] |
| US: `bhav_yahoo` (^GSPC 1950+), `bhav_cboe_fut` (CBOE VIX fut) | stockvizus2 | second-venue / US replication | [CLOSE] |

### 1.2 Intraday [INTRADAY] — Postgres `StockVizDyn` (SWEDEN)

| Dataset | Coverage | Notes | KB tag |
|---|---|---|---|
| `zd_index_bars` | **2015-01-09 → 2026-08-27 (~11.6y)** | **1-minute index candles**, 95.1M rows total, ~1.08M bars per main index (375 bars/day = NSE cash session 09:15–15:29 IST, median spacing 60s, verified). NIFTY 50, NIFTY BANK, NIFTY IT, NIFTY 100, NIFTY MIDCAP 50, **INDIA VIX (1-min!)** + ~150 sector/theme indices (shorter history for newer ones) | [INTRADAY] |
| `zd_option_bars` | depends on downloader cadence | 1-minute NFO **futures & options** bars (inst_token, o/h/l/c/v/oi), per-contract; `oi` included | [INTRADAY] |
| `zd_bars_mcx` | 2025-09-17 → 2026-08-27 (~11.5 months) | 1-minute OHLCV+OI bars, 64.8M rows, 6,477 MCX commodity-futures contracts (gold, silver, crude, natgas, copper…) | [INTRADAY] |

### 1.3 tick_stamp base-date conventions (CRITICAL — verified in downloaders)

`tick_stamp` is **NOT** the same epoch in every table. Decoding with the wrong
base silently shifts every timestamp by decades.

| Table | tick_stamp base | Downloader | Evidence |
|---|---|---|---|
| `zd_index_bars` | **Unix epoch** (1970-01-01 UTC) | `CandleDownloader.py` (`dup.parse(x[0]).timestamp()`, comment: *"doesn't offset by basedate"*) | probe: 1420775100 → 2015-01-09; 1787830860 → 2026-08-27 |
| `zd_bars_mcx` | **Unix epoch** (1970-01-01 UTC) | (MCX downloader) | probe: 1758129540 → 2025-09-17 |
| `zd_option_bars` | **seconds since 1990-01-01** | `DerivativeBarsDownloader.py` (`baseDate = datetime(1990, 1, 1)`; `tics = int((dt - baseDate).total_seconds())`) | source read — this is the base-date to note |

The data layer (Phase 0) must carry a per-table `tick_epoch` decoder
(1970 vs 1990) and unit-test it against a known bar (e.g., NIFTY 50
2026-08-27 first bar = 2026-08-27 03:45:00 UTC = 1787802300).

### 1.4 Downloader usage (Kite/Zerodha — Admin02/kite)

- Entry: `python main.py` from `/mnt/ssd1/stockviz/Admin02/kite` with
  `ZdkWrapper("SJ0355")` auth (apiKey + access_token; config via
  `common/CommonConfig`, SWEDEN PG conn string).
- `--download-index-candles` → refresh ALL indices in `zd_index_bars`
  incrementally (each index resumes from `max(time_stamp) − 1 day`; first
  backfill 2014-01-01). `-asof YYYY-MM-DD` → single-date backfill
  (`DownloadAllIndicesAof`).
- `--download-deriv-candles` → NFO futures+options minute bars into
  `zd_option_bars` for the latest `zd_master` snapshot (`-asof` for a
  specific master date; `DownloadAllAsof` pulls every instrument with
  `expiry >= today` on that master date, one day at a time, `oi=1`).
- Mechanics: Kite REST `instruments/historical/{tok}/{interval}?oi=1&from=..&to=..`;
  60-day chunks for indices, per-day calls for derivatives; **idempotent**
  (`ON CONFLICT DO NOTHING`) → safe to re-run for backfill/repair.

### 1.5 Gaps that shape the plan

1. **Intraday index FUTURES bars are thinner than index candles.** We have
   11.6y of 1-minute **cash-index** candles (`zd_index_bars`), but the
   NFO **futures/options** minute bars (`zd_option_bars`) only cover what the
   downloader has pulled since it started running — per-contract, expiry-
   bound. Phase 2 can (a) run on index minute candles as the primary signal
   venue (matches S6's index-level setup; cash index is tradeable via
   futures in practice), and (b) use `zd_option_bars` NFO futures bars where
   coverage allows, treating the cash→futures basis as an execution detail.
2. **MCX history is short** (~11 months, and contracts roll) → augmentation
   is mandatory (KB 04: GAN/MCS + noise; S4-style artificial trajectories);
   MCX is the second venue for the cross-venue litmus, not the primary one.
3. **No sentiment feed** → S5 multi-modal is data-gated (external acquisition
   required); not in the default path.
4. **No options surface beyond bars** (no implied-vol/option-chain pricing
   data) → S3 Ch.7 live hedging is data-gated for now (`zd_option_bars`
   holds option BARS, which may later support microstructure/IV studies;
   BSM-simulated hedging remains doable as a method exercise — the env needs
   no market data, only GBM paths).

## 2. Guiding principles (from the KB — non-negotiable)

1. **Cost in reward, never ex-post** (KB 04 §4.5, 08 §8.3). Daily: house
   25 bps per unit exposure change (S3-style `r -= c·|Δa|`); intraday:
   commission per contract + half-spread slippage, inside `R_t` (S6/S7).
2. **No lookahead, anywhere**: z-score/feature scalers fit on train only
   (KB 04 §4.5); state at `t` excludes `r_{t+1}`; house lag convention
   `k = 1` (signal at close `t-1` earns day `t`) — the bug that inflated the
   HMM family must not re-enter via RL envs.
3. **Baseline zoo is mandatory** (KB 08 §8.5): random agent (floor), B&H,
   simple rule (MA-cross / rolling-momentum), supervised predictor + rule
   trader, published RL baseline (TDQN for [CLOSE]; A3C+LSTM for
   [INTRADAY]), and **prior agent = our canonical momentum+stop books**.
4. **Augmentation is infrastructure, not an afterthought** (KB 09 §9.8):
   artificial trajectories (S4 block bootstrap) for daily; GAN/MCS + noise
   for intraday; **KS-validate synthetic vs real before training** (S3 §5.3).
5. **Evaluation splits**: house windows pre ≤ 2019-12-31 / post ≥ 2020-05-01
   for *reporting*; walk-forward rolling train/test for *training* (KB 08
   §8.1). Never train on the test window. Multi-instrument tests wherever
   possible (S4's 30-stock protocol; we have 5,000+).
6. **Report the whole picture**: net AND gross, Sharpe, CAGR, MaxDD,
   turnover, cost sensitivity sweep (0/10/25/50 bps daily; spread ×1/×2
   intraday), per-day stats intraday (S6), pre/post/full.
7. **Reproducibility**: fixed seeds, logged hyperparams (Δt, lookback,
   commission, reward, algo, net depth, replay size — KB 08 §8.6), per-phase
   checkpoint + metrics CSV + gt table + charts (@StockViz caption, stacked
   cum+drawdown, end-labeled series — house chart rules), findings.md.
8. **Timestamp discipline**: `tick_stamp` epochs differ per table (Unix
   epoch for `zd_index_bars`/`zd_bars_mcx`, **1990-01-01 base for
   `zd_option_bars`** — §1.3). All bars are decoded through a single,
   unit-tested loader; no hand-rolled arithmetic in experiment code. Session
   awareness: NSE cash session 09:15–15:29 IST (= 03:45–09:59 UTC); reset
   per-day state at session open (KB 05 §5.6).

## 3. Phase 0 — Infrastructure & environment library (no results yet)

**Goal:** a tested, deterministic RL stack shared by all later phases.

- **Data layer** (`rl/data/`): loaders for bhav_index, VIX_HISTORY,
  BHAV_EQ_FUT (rolled continuous series using the R2 `common/futures.R`
  calendar logic — phantom-expiry filter included), PX_HISTORY /
  eod_adjusted_nse, MOMENTUM_ABS/PROB, zd_index_bars (1-min indices incl.
  1-min INDIA VIX), zd_option_bars (NFO futures/options), zd_bars_mcx.
  **Every loader decodes `tick_stamp` through the per-table epoch decoder**
  (§1.3) and carries a session calendar (NSE 09:15–15:29 IST, MCX
  09:00–23:30 IST). Refresh/backfill path: idempotent re-run of
  `main.py --download-index-candles [-asof]` / `--download-deriv-candles
  [-asof]` (Admin02/kite, §1.4). Output: parquet/CSV aligned frames + a
  `DataInventory` manifest (coverage, gaps, tick-epoch per table) that the
  evaluation layer can assert against.
- **Environments** (Gym contract, KB 05): `FinanceEnv` (z-scored window,
  {−1,0,1}), `TradingEnv` (+position, +costs), `TDQNEnv` (Sharpe/differential
  Sharpe reward, S4), `SiIntradayEnv` (episode = 1 trading day, continuous
  action, multi-objective reward `U = α·mean(DR) − β·std(DR)`, S6),
  `AllocationEnv` (simplex weights, S3 Ch.8), `ExecutionEnv` (Almgren-Chriss,
  S3 Ch.9). Every env ships unit tests: reward accounting, cost charging,
  **no-lookahead assertion** (state at t must not contain t+1 info).
- **Agents** (small, auditable, PyTorch): DQLAgent (Hilpisch baseline),
  TDQN (Double + Dueling, S4), PPO (clip objective, S5/S8), A3C+LSTM
  (Ponomarev), LSTM continuous policy (S6). Off-the-shelf SB3 allowed where
  the recipe is standard (PPO); TDQN and the Si pipeline get bespoke
  implementations because the KB recipes are specific.
- **Verification gates (exit criteria):** CartPole sanity (DQLAgent must
  solve it); random-agent floor test on every env (agent must beat random;
  if not — bug, not signal, KB 08 §8.1); replay of one published number
  (e.g., TDQN-style daily on NIFTY vs B&H must at least match the direction
  of S4's result); `parse`/lint clean; runnable via a `run-all`-style
  registry mirroring the R2 tree.

**Stack decision:** Python (PyTorch) for envs/agents/training (the RL
ecosystem is Python; R has no viable Gym/RL story). Evaluation/reporting may
be R or Python but must emit house-style artifacts (pre/post/full metrics
CSV, gt tables, stacked cum+drawdown PNGs, `@StockViz`).

## 4. Phase 1 — Daily [CLOSE]: TDQN replication & NIFTY variants

**Seed (KB 09 §9.9 ex.1):** replicate Théate's TDQN on NIFTY 50 daily with
realistic costs; sweep Sharpe vs PnL reward; then extend.

- **R1.1 — Replication & honesty check.** TDQN (Double+Dueling, Sharpe
  reward, artificial-trajectory training per S4 §3.3) on NIFTY 50 TR daily
  (1999+; or PR 1990+ with OHLC caveat). Baselines: random, B&H, MA-cross,
  **prior agent = ABS@FF60 Stop-Cash book and TVT-HMM gated book from the R2
  tree**. Exit: does TDQN beat B&H net of 25 bps? Does it get within
  shouting distance of the prior agent? If it cannot beat MA-cross, stop and
  fix before spending anything on variants (KB 08 §8.5 floor).
- **R1.2 — Reward sweep.** PnL vs Sharpe vs differential Sharpe vs
  multi-objective (α·mean−β·std over monthly buckets, KB 06 §6.5 ported to
  [CLOSE]). Expect: Sharpe-family rewards generalize better (S4), profit
  reward over-leverages (S6).
- **R1.3 — State design.** (a) lagged z-scored returns only (S2/S3 baseline);
  (b) + India VIX (2009+ — state feature, not reward); (c) + **house
  TVT-HMM filtered P(risk-off) as a state feature** (cross-pollination: the
  regime signal that already works as an overlay becomes part of the agent's
  observation); (d) + cross-sectional breadth (MOMENTUM_ABS/PROB quantile
  aggregates). Record what each addition does to train/test Sharpe gap.
- **R1.4 — Augmentation.** Artificial trajectories (S4 block bootstrap) vs
  Gaussian noise (S3 §4.1) vs GBM (S3 §4.2). KS-validate synthetic vs real
  (S3 §5.3). Metric: out-of-sample Sharpe and the train→test Sharpe decay —
  the decay is the overfit meter (KB 08 §8.3).
- **R1.5 — Cross-sectional / multi-instrument.** Broad-market test (S4
  protocol): TDQN trained on artificial trajectories over a universe slice
  (e.g., top-FF NSE stocks with MOMENTUM_ABS features) and evaluated across
  many names; plus a futures variant on rolled NIFTY/BANKNIFTY continuous
  series (2005+). Report the *distribution* of net Sharpe across names, not
  one lucky asset.
- **Evaluation:** walk-forward (e.g., 2y train / 1y test rolls), report
  pre/post/full; cost sweep 0/10/25/50 bps; turnover logged; paired
  bootstrap significance of daily strategy−baseline returns (house style
  from the TVT-HMM studies).
- **Success criteria:** post-window net Sharpe > 1 at ≥25 bps; MaxDD ≤ the
  gated prior agent; beats B&H and the simple rule in ≥2 of 3 windows;
  beats the prior agent at least in MaxDD (drawdown management is where RL
  reward shaping should win; raw Sharpe vs the momentum books is a stretch).

## 5. Phase 2 — Intraday [INTRADAY]: 1-minute NIFTY-family index candles (primary) + MCX (second venue)

> **EXECUTED 2026-08-29 — VERDICT: NEGATIVE (M2 gate NOT MET).** Full
> scorecard: `research/phase1/runs/r24/findings.md`. All R2.1-R2.4 items
> run: per-day Sharpe never > 1 on any venue; NIFTY-family cash-index
> intraday is negative-carry (overnight dominance); MCX gold (futures and
> MCX*DEX index) is zero-drift; Si-LSTM (U/SR rewards), PPO and PPO-LSTM
> all fail OOS; A3C/A2C-LSTM unavailable on SB3 2.9; 2x slippage fails.
> MCX venue convention: use the MCX*DEX indices in `zd_index_bars`
> (continuous, no expiry), NOT `zd_bars_mcx` futures.

**Seed (KB 09 §9.9 ex.2):** port Si's pipeline (DNN feature learner → LSTM
policy, continuous action, multi-objective) to 1-min index bars — the KB's
S6 recipe was written for exactly this data shape; add the A3C (Ponomarev)
recipe and a PPO+LSTM uplift (KB 09 §9.5).

- **Data reality (upgraded).** Primary venue: `zd_index_bars` — **11.6 years
  (2015-01-09 → now) of 1-minute candles** for NIFTY 50, NIFTY BANK, NIFTY
  MIDCAP 50, NIFTY IT, NIFTY 100, and **1-minute INDIA VIX** (a state feature
  no intraday paper in the KB had). 375 bars/day (09:15–15:29 IST); episode
  = one trading day (S6); reset LSTM at session open (KB 05 §5.6); per-day
  aggregation for all statistics (S6 §4). Index futures execution: where
  `zd_option_bars` NFO futures coverage allows, run the same policy on
  rolled futures bars (basis = execution detail); otherwise trade the cash
  index candle via futures in practice. Second venue (cross-venue litmus):
  `zd_bars_mcx` 1-min commodity futures (liquid subset: crude, gold, silver,
  natgas, copper families), ~11 months.
- **R2.1 — Si pipeline replication on NIFTY 50 1-min.** FC feature learner on
  raw 1-min OHLCV windows → LSTM policy `a_t = tanh(W h_t + b) ∈ [−1,1]`;
  reward `U = α·mean(DR) − β·std(DR)` with costs inside `DR`. α/β default
  from S6, sweep later. Baselines: random, intraday B&H (index), TWAP-flat,
  and the same strategy sampled at Δt = 1 day (sanity that intraday is
  additive, not a resampling artifact, KB 08 §8.4).
- **R2.2 — A3C+LSTM (Ponomarev recipe) and PPO+LSTM.** 60-second decisions →
  our 1-min bars map directly. A3C replicates S7; PPO+LSTM is the stability
  uplift the KB flags as open (09 §9.5). Compare sample efficiency and net
  per-day Sharpe.
- **R2.3 — Risk-aversion.** Static α/β vs **vol-conditioned** α/β (KB 09
  §9.5 open problem) — state features: rolling realized vol and the
  **1-minute INDIA VIX** (unique to this estate). Also test the house
  TVT-HMM filtered P(risk-off) resampled to 1-min as a conditioning signal.
- **R2.4 — Generalization litmus (now a strong test).** (a) Cross-instrument
  within NSE: train NIFTY 50, test NIFTY BANK / MIDCAP 50 / NIFTY IT
  (11.6y of each); (b) cross-venue: train NIFTY 50, test MCX gold/silver —
  the KB 09 §9.5 test that separates signal from microstructure overfit
  (S7's RTS result likely overfit; a NIFTY-trained policy transferring to
  MCX is the strongest evidence available to us).
- **Evaluation.** Walk-forward across calendar years (train 2015–2019,
  test folds 2020+; then rolling 2y/1y); report pre ≤ 2019-12-31 / post ≥
  2020-05-01 windows intraday too; per-day mean/std of PnL and per-day
  Sharpe (S6 §4 — never per-bar pooled); turnover per day.
- **Cost model.** NSE index: commission per contract + half-spread slippage
  (index spread is tight; sweep ×1/×2); MCX: real commission + half-spread.
  Costs inside `DR`, never ex-post.
- **Augmentation.** 11.6y of index bars makes GAN/MCS optional on the primary
  venue (use noise + block bootstrap per S4); MCX's 11 months makes it
  mandatory there (GAN synthetic 1-min bars, S3 Ch.5, KS-validated; train on
  augmented + real, test on held-out real days, KB 04 §4.7).
- **Success criteria:** net per-day Sharpe > 1 on NIFTY 50 **and** ≥1 more
  NSE index; walk-forward-stable (≥2 post-2020 folds positive); positive
  transfer to ≥1 second instrument or venue; survives ×2 slippage; turnover
  sustainable (not 10× the daily rate with no net gain, KB 08 §8.2). Any
  60%+ p.a.-style headline gets the KB 08 §8.7 haircut — an intraday result
  is a sandbox result until it passes paper-trading.

## 6. Phase 3 — Frontier tracks (one at a time; each gated on Phase 1/2 learnings)

- **3.1 Dynamic allocation [CLOSE]** — S3 Ch.8: simplex-action agent over
  2–3 assets (NIFTY 50 / MIDCAP 150 / SMALLCAP 250 TR, or NIFTY vs
  BANKNIFTY futures) with risk-free cash proxy (`mf_nav_history`). Sanity
  check: under GBM the optimal is Merton/Kelly — RL must recover it (S3
  §8.1). Benchmark: equal weight (surprisingly hard to beat, S3 §8.5).
- **3.2 Optimal execution [INTRADAY]** — S3 Ch.9/S8 §4: Almgren-Chriss env
  on liquid MCX contracts and NIFTY futures (`zd_option_bars`); actor-critic
  vs TWAP/VWAP heuristics; implementation-shortfall evaluation. Natural
  complement to Phase 2 signals (same venues, same data layer).
- **3.3 Hedging (method-only, data-gated for live)** — S3 Ch.7: BSM-world
  delta-hedging env (needs only GBM simulation, no market data); RL hedge
  vs analytic delta with cost-free and costly regimes. Deployable to real
  options only when an options surface enters the estate (gap 1.5.4).
- **3.4 Regime-aware rewards** — dynamic risk-aversion conditioned on the
  house TVT-HMM/VIX state (KB 09 §9.5); bridges the regime-gate research and
  the RL reward design.
- **3.5 Multi-modal [CLOSE] (data-gated)** — S5 price⊕sentiment embedding
  fusion; requires acquiring a daily sentiment feed (gap 1.5.3); do not
  start until the data exists.

## 7. Evaluation & governance (KB 08 as a checklist, enforced per phase)

1. Baseline zoo order fixed: random → B&H → simple rule → supervised
   predictor → published RL baseline → prior agent (our books). An agent
   that doesn't clear the floor doesn't get a findings.md.
2. Théate protocol where data allows: train on artificial trajectories;
   test on many instruments; cost sweep; distribution of Sharpe, not a
   point; significance via paired bootstrap (house implementation).
3. Regime honesty: hold out known regime breaks (2008/2015/2020/2022) in
   walk-forward folds; pre/post windows for reporting; never train on test.
4. Every phase ends with: checkpoint + metrics CSV + gt tables + stacked
   cum/dd charts (@StockViz) + findings.md containing the two deployment
   questions (KB 08 §8.7): *overfit to a regime? exploit a vanishing
   friction?*
5. Artifacts live in `/mnt/data/books/RL/research/` (one folder per phase,
   mirroring R2/backtests conventions); run registry + logs; `git diff
   --check` clean; rerun-and-flag-changes discipline for any shared code.

## 8. Risks & mitigations

| Risk | Mitigation (KB source) |
|---|---|
| One-path overfitting → collapse next year | Artificial trajectories + multi-instrument tests + walk-forward (04, 08 §8.3) |
| Lookahead leakage in envs/features | No-lookahead unit tests per env; scalers fit on train only; house lag k=1 (04 §4.5) |
| RL on daily data fails to beat our strong classical prior agents | Expected; Phase 1 framed as honest benchmark; upside redirected to intraday/execution; report the negative result properly (08 §8.5, 09 §9.8) |
| MCX short history + venue-specific microstructure (second venue) | GAN/MCS augmentation, cross-instrument litmus on 11.6y index bars first, MCX cross-venue as the harder test, sandbox labeling (04, 09 §9.5) |
| Wrong tick_stamp epoch decode (1970 vs 1990 base) | Single unit-tested loader per table (§1.3); known-bar regression test; no hand-rolled timestamp arithmetic in experiment code |
| Cost realism (daily vs intraday) | Cost-in-reward, cost sweeps, turnover reporting (06 §6.8, 08 §8.2) |
| Non-stationarity / regime shift | Walk-forward + regime holdouts; regime features (VIX/TVT-HMM) in state (08 §8.4) |
| GAN mode collapse → fake-looking synthetic | KS validation before training; train synthetic → test real (03, 04 §4.7) |
| Reward hacking (high reward, bad drawdown) | Always evaluate Sharpe/MaxDD/turnover regardless of reward choice (08 §8.3) |
| Python-vs-R friction with house conventions | Fixed artifact contracts (CSV metrics, gt tables, PNG charts); evaluation can stay in either language |
| Compute creep | Small nets first (KB: 100–200 hidden units sufficed); GPU optional; budget per phase before starting |

## 9. Milestones & exit gates

| Milestone | Entry criteria | Exit gate |
|---|---|---|
| M0 (Phase 0) | — | Env suite + agent zoo + data layer tested; random-floor passes; run registry live |
| M1 (Phase 1) | M0 | TDQN beats B&H net 25bps; reward/state/augmentation sweep documented; verdict vs prior agent written |
| M2 (Phase 2) | M1 (even if negative — intraday is independent of daily verdict) | Net per-day Sharpe > 1 on NIFTY 50 + ≥1 more NSE index; walk-forward-stable; positive cross-instrument/venue transfer; slippage-survivor |
| M3 (Phase 3, per track) | M2 or explicit decision to branch | Track-specific success criteria in §6 |
| M4 (deployment decision) | M2/M3 | Both KB 08 §8.7 questions answered in findings.md; paper-trade design included |

> **M4 EXECUTED 2026-08-29 — DECISION: DO NOT DEPLOY RL.** KB 08 §8.7
> answered in `research/m4/findings.md` with the full-program evidence:
> (1) regime overfit — YES, documented (drift-betting taxonomy, SR-type
> intraday overfit); (2) vanishing friction — N/A, no friction found.
> Shadow paper-trade (classical momentum books vs the R1.7
> drawdown-avoider) with monthly review + promotion thresholds. Program
> narrative: `README.md` (engineers), `eli5.md` (lay story).

Sequencing rule: don't escalate a failed mechanism (if TDQN can't beat
MA-cross, no new reward function will fix it — fix the env/data first). The
intraday track may start in parallel with Phase 1 (different data, different
questions) but only after M0.

## 10. Deliverables & house conventions (recap)

- `research/phase<N>/` — `build`/`train` scripts, `checkpoint` artifacts,
  `metrics_{pre,post,full}.csv`, `cumulative_{pre,post,full}.png` (stacked
  cum+drawdown, end-labeled series), `gt_{pre,post,full}.png`, `findings.md`.
- Windows: pre ≤ 2019-12-31, post ≥ 2020-05-01 (house); full = everything.
- Caption/credit: `@StockViz` only (never subtitle — user rule).
- All imports at file top; deterministic seeds; per-phase run logs; a
  `research/README.md` index updated as phases land.
- Every claim in findings.md cites the KB source (S1–S8 + section) it
  derives from, so the KB stays the single source of truth for the
  literature side.

---

*Next action: M0 — scaffold `research/`, data layer with the per-table
tick_stamp epoch decoder (1970 vs 1990, §1.3) and NSE/MCX session calendars,
and the first two envs (FinanceEnv + TDQNEnv) with their no-lookahead tests;
then run the random-agent floor on NIFTY 50 daily.*

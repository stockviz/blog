# Phase 0 — Infrastructure & environment library

Status: **BUILD COMPLETE — verify gates in Findings below.**

Goal (research-plan §3): a tested, deterministic RL stack shared by all
later phases. No results are claimed yet — this phase produces machinery
and its verification gates.

## Layout

```
rl/
  config.py        DB creds (reuses R2/backtests/config.json; SWEDEN pw resolved)
  data/
    epochs.py      per-table tick_stamp epoch decoder (1970 vs 1990 — §1.3)
    sessions.py    NSE (09:15-15:29 IST) / MCX session calendars
    loaders.py     daily + intraday loaders, futures roll calendar (futures.R port)
    inventory.py   DataInventory manifest -> artifacts/inventory.{json,md}
  envs/
    finance_env.py     KB 05: z-window, {-1,0,1}, PnL - costs
    trading_env.py     KB 05 (S3 Ch.6): window + position + vol
    tdqn_env.py        S4: differential-Sharpe reward (Moody & Saffell)
    si_intraday_env.py S6: episode = trading day, continuous action, U = a*mean - b*std
    allocation_env.py  S3 Ch.8: simplex weights, turnover cost
    execution_env.py   S3 Ch.9: Almgren-Chriss, impact + risk penalty
  agents/
    dqn.py         DQLAgent (Hilpisch): replay + target net, eps-greedy
    tdqn.py        TDQN: Double-DQN + Dueling heads (S4)
    ppo.py         SB3 PPO / A2C+LSTM wrappers (off-the-shelf)
    si_policy.py   S6: FC learner -> LSTM -> tanh policy, BPTT on U
  eval/
    metrics.py     CAGR/Vol/Sharpe/MaxDD pre/post/full (returns.R port)
    baselines.py   random / buy&hold / flat / MA-cross (KB 08 §8.5 zoo)
    charts.py      stacked cum+drawdown, end labels, @StockViz (charts.R port)
tests/             pytest suite (epochs, sessions, loaders, metrics, envs,
                   agents, random floor)
scripts/
  smoke_charts.py  house-convention artifacts on real NIFTY 50 data
run_all.py         run registry (mirrors common/run-all.R); logs/ per step
```

## Key conventions enforced (house + KB)

- **No lookahead (house lag k=1):** state at step t = window *ending* at t
  (data ≤ t); reward = `a_t * r_{t+1} - c*|a_t - a_{t-1}|`. Unit-tested per
  env via oracle recomputation (test_envs.py).
- **Cost-in-reward:** 25 bps per unit position change daily; commission +
  half-spread intraday (Si env); turnover cost in allocation; impact in
  execution. Never ex-post.
- **Windows:** pre ≤ 2019-12-31 / post ≥ 2020-05-01 / full — metrics AND
  charts always split all three.
- **Charts:** stacked cum+drawdown, every series end-labeled
  (name + CAGR + Sharpe), `@StockViz` caption only.
- **tick_stamp discipline:** all intraday loaders decode through
  `epochs.decode_*` with the per-table base (zd_index_bars/zd_bars_mcx =
  1970; zd_option_bars = 1990 — DerivativeBarsDownloader baseDate).
- All imports at file top; deterministic seeds; parquet cache under
  `cache/` (idempotent, re-fetch on demand).

## Usage

```bash
# full registry (inventory -> pytest -> smoke charts), logs in logs/
python run_all.py
python run_all.py --only=loaders      # grepl-style substring filter

# ad-hoc
python -m pytest tests -q             # whole suite
python scripts/smoke_charts.py        # real-data artifact smoke
```

## Data refresh (research-plan §1.4)

Intraday tables are fed by the Kite downloaders in
`/mnt/ssd1/stockviz/Admin02/kite`:
`python main.py --download-index-candles [-asof YYYY-MM-DD]` refreshes
`zd_index_bars`; `--download-deriv-candles` refreshes `zd_option_bars`
(1990 base!). Both idempotent (ON CONFLICT DO NOTHING).

## Findings (Phase 0)

1. **Epoch trap confirmed & handled:** `zd_option_bars` ticks are seconds
   since 1990-01-01 while `zd_index_bars`/`zd_bars_mcx` are Unix — decoding
   the former as Unix shifts every timestamp 20 years. Loaders carry the
   per-table base; known-bar regression test in tests/test_epochs.py.
2. **Env timing bug found by tests:** the first draft aligned rewards one
   bar early (reward r[t] was already inside the state window). The
   no-lookahead oracle tests caught it; all envs now use window-ending-at-t
   states and r[t+1] rewards. This is exactly the class of bug the KB
   warns about (KB 04 §4.5).
3. **Futures roll calendar ported** from `common/futures.R` (phantom-expiry
   filter: contracts whose last trade is >7d before expiry are dropped
   before the same-weekday narrowing; roll 5 trading days before expiry;
   returns never cross an expiry). Sanity: rolled NIFTY futures returns
   correlate >0.9 with the cash index.
4. **Baseline zoo ready:** random / buy&hold / flat / MA-cross over the
   house windows; smoke charts on real NIFTY 50 data prove the artifact
   pipeline (metrics CSV + stacked cum/dd PNGs) end-to-end.
5. **Agent zoo ready:** DQLAgent + TDQN (Double+Dueling) pass CartPole /
   loss-decrease sanity; SB3 PPO and A2C-LSTM wrappers run; SiAgent (S6
   LSTM policy, BPTT on U) trains on 1-min synthetic days.
6. **gymnasium seeding gotcha (root cause of the CartPole flake):**
   `env.reset(seed=)` seeds the env's transition RNG but NOT
   `action_space.sample()` — the space keeps an unseeded internal
   RandomState, so epsilon-greedy action sequences differed across
   processes with identical seeds (init weights identical, ep-0 rewards
   different: 292 vs 72 vs 121 across runs). Fix: seed the action space
   explicitly per episode (`env.action_space.seed(seed+ep)` in
   `agents/dqn.py` / `tdqn.py`, `baselines.evaluate_policy`). Verified
   fully deterministic after the fix. Also: torch CPU needs
   `torch.set_num_threads(1)` for reproducibility; DQN target hard-sync
   every 100 steps + 50k replay beats tiny-tau soft updates on CartPole.
7. **Huge intraday tables need bounded queries:** zd_option_bars is
   ~2.36 BILLION rows — full-table scans hang (inventory, tests).
   Loaders push date bounds into SQL (per-table epoch-encoded); tests and
   inventory discover tokens via zd_master / per-token indexed probes.

## Exit gates (research-plan §9, M0)

- [x] Env suite + agent zoo + data layer tested (pytest: **39 passed**)
- [x] Random-floor passes (random finite everywhere; B&H beats random on
      trends; DQN beats random floor)
- [x] CartPole sanity (DQLAgent solves CartPole-v1, deterministic)
- [x] Run registry live: `python run_all.py` — inventory / pytest /
      smoke charts ALL PASSED (EXIT=0)
- [x] House-convention artifacts on real data: `artifacts/smoke_metrics.csv`
      + `smoke_cumulative_{pre,post,full}.png` (stacked cum+dd, end labels,
      @StockViz) — visual-verified
- [ ] (deferred to Phase 1) TDQN-vs-B&H published-number replay — that IS
      R1.1's first line, not an infrastructure gate

Next: Phase 1 (R1.1) — TDQN on NIFTY 50 daily vs the baseline zoo above,
with the artificial-trajectory augmentation (S4 §3.3) and the cost sweep.

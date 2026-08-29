# RL for Finance — Research workspace

Execution roadmap: see `../research-plan.md` (grounded in `../kb/`).

## Layout

| Folder | Contents |
|---|---|
| `phase0/` | Infrastructure: data layer, Gym envs, agent zoo, evaluation tooling, tests, run registry |
| `phase0/rl/` | Python package (`data`, `envs`, `agents`, `eval`) |
| `phase0/tests/` | pytest suite (epochs, sessions, loaders, envs, metrics, agents, random floor) |
| `phase0/logs/` | run-registry logs (mirrors `R2/backtests/momentum-roundup/run-logs/`) |
| `phase0/cache/` | parquet cache of DB tables (idempotent re-fetch) |
| `phase0/artifacts/` | inventory manifest + charts |

## House conventions honored

- Windows: pre ≤ 2019-12-31 / post ≥ 2020-05-01 / full (metrics + charts all three).
- Cost-in-reward (25 bps per unit exposure change on daily; commission + half-spread intraday), never ex-post.
- No lookahead: state at `t` built from data ≤ `t`; reward uses `t+1` bar (house lag k=1).
- Charts: stacked cumulative + drawdown panels, every series end-labeled (name + CAGR + Sharpe), `@StockViz` caption only.
- All imports at file top; deterministic seeds; per-step run logs; `git diff --check` clean.

# 10 — Frequency Matrix: What Works on CLOSE vs INTRADAY

> Every idea in the KB cross-referenced. [CLOSE] = validated daily; [INTRADAY] = validated intraday; [BOTH] = structure-agnostic.

## 10.1 Core mechanics (always [BOTH] — frequency lives in env, not update)

| Concept | Tag | Evidence | Note |
|---------|-----|----------|------|
| MDP tuple `(S,A,P,R,γ)` | [BOTH] | S2, S3 §3, S8 §2 | Markov by embedding lags; intraday needs longer history or LSTM |
| Bellman / Q-learning update | [BOTH] | S1, S3 §2.3 | Update rule identical; reward scale differs |
| Experience replay & target nets | [BOTH] | S3, S4 | Essential both frequencies |
| Gym `reset/step` contract | [BOTH] | S3 | All envs obey it regardless of Δt |

## 10.2 Representations & features

| Idea | Tag | Source | Comment |
|------|-----|--------|---------|
| Lagged returns `s_t=(e_{t-4}..e_t)` | [CLOSE] demo / [BOTH] structurally | S2, S3 | Daily 5-lag baseline; intraday needs longer lookback or deep features |
| Z-scored window (returns) | [BOTH] | S3 §3.1 | Reset per session intraday |
| OHLC + MA/RSI | [CLOSE] | S5 | Overkill intraday; hand-crafted indicators underperform DNN learner there |
| Multi-modal price⊕sentiment embedding | [CLOSE] validation | S5 | Daily sentiment aggregation; intraday sentiment sparse — not validated |
| Aggregated bid/order-book vector | **[INTRADAY]** | S7 | 60-sec bid aggregation; venue-specific |
| DNN feature learner (FC stack) | **[INTRADAY]** | S6 §3.1 | Learns from raw intraday bars end-to-end |
| Volatility / regime feature | [BOTH] | S3 | Daily hist vol; intraday hourly realized vol |
| Include position in state | [BOTH] | S3, S6 | Required whenever reward is PnL |

## 10.3 Rewards

| Reward | Tag | Source |
|--------|-----|--------|
| Period PnL `a·r - c|Δa|` | [BOTH] | S3 Ch.6, S2 |
| Cumulative / terminal wealth | [BOTH] | S3 Ch.8 |
| Sharpe / differential Sharpe | [CLOSE] | S4 (TDQN), S6 Eq.4 |
| Multi-objective `α·mean(DR) - β·std(DR)` (`DR` daily cum) | **[INTRADAY]** only validated | S6 Eq.5 |
| Hedging replication error | [BOTH] | S3 Ch.7 |
| Execution cost + risk penalty | **[INTRADAY]** conceptual | S3 Ch.9, S8 §4 |

**Takeaway:** profit-only is fine for [CLOSE]; intraday demands variance penalty (Sharpe or multi-objective).

## 10.4 Augmentation

| Method | Tag | Source |
|--------|-----|--------|
| Gaussian noise on history | [BOTH] | S3 §4.1 |
| Stochastic process (GBM/Heston) simulation | [BOTH] | S3 §4.2 |
| Artificial trajectories (block bootstrap) | [CLOSE] validation | S4 §3.3 |
| GAN synthetic bars + KS test | [BOTH] | S3 Ch.5 |

Intraday benefits more from GAN/MCS because history is shorter per venue.

## 10.5 Algorithms

| Algorithm | Discrete/Continuous | Tag in folder | Recommendation by frequency |
|-----------|---------------------|---------------|------------------------------|
| Tabular Q-learning | D | [CLOSE] demo | teaching only |
| DQN | D | [CLOSE] validation | daily discrete trading |
| Double-DQN / Dueling (TDQN) | D | [CLOSE] | best daily close baseline |
| PPO | C/D | [CLOSE] validation (Avramelou, S8 cite Liang) | daily multi-modal / portfolio |
| A3C + LSTM | C/D | **[INTRADAY]** | Si/S7 intraday specialty |
| LSTM continuous policy `tanh(W h + b)` | C | **[INTRADAY]** | futures intraday position scaling |
| Actor-critic (execution) | C | **[INTRADAY]** conceptual | optimal execution |

## 10.6 Environments (see 05/07)

| Env | Tag | Primary source |
|-----|-----|----------------|
| Finance / Trading (daily) | [CLOSE] (S3, S4) | S3 Ch.3/6, S4 |
| Multi-modal (crypto daily) | [CLOSE] | S5 |
| Intraday futures (DNN+LSTM) | **[INTRADAY]** | S6 |
| RTS futures 60-sec A3C | **[INTRADAY]** | S7 |
| Hedging | [BOTH] | S3 Ch.7 |
| Allocation 2–3 assets | [CLOSE] | S3 Ch.8 |
| Execution (Almgren-Chriss) | **[INTRADAY]** | S3 Ch.9 |

## 10.7 Evaluation

| Practice | Tag | Source |
|----------|-----|--------|
| Broad-market test (many assets) | [CLOSE] | S4 |
| Per-day episode aggregation | **[INTRADAY]** | S6 |
| Cost-in-reward (not ex-post) | **[INTRADAY]** critical, [CLOSE] recommended | S6, S7 |
| Turnover / drawdown reporting | [BOTH] but turnover 10× intraday | S7, S4 |
| KS synthetic vs real validation | [BOTH] | S3 Ch.5 |

## 10.8 Quick selector

- **You have daily OHLC only:** use Finance/Trading [CLOSE] + DDQN + artificial trajectories [S4] + Sharpe reward. Expect net Sharpe 1–2 if lucky; costs decide fate.
- **You have minute bars (futures/ETF):** switch to Si pipeline [INTRADAY] — DNN features + LSTM + multi-objective; plan 10× more tuning.
- **You have order-book/bids:** Ponomarev recipe [INTRADAY] — aggregate to 60-sec + LSTM + A3C; paper-trade venue.
- **You have sentiment feed:** only daily so far [CLOSE] — fuse via embeddings [S5], not concatenation.
- **You want execution/market-making:** Ch.9/S8 territory — [INTRADAY] with impact model, not price-taker.

## 10.9 One-line summary

> RL update rules are [BOTH]; daily vs intraday is an **environment and evaluation** distinction — pick the right state, cost, reward, and aggregation, and pair discrete DQN for [CLOSE] with recurrent actor-critic for [INTRADAY].

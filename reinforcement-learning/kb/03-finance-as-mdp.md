# 03 — Finance as MDP (Level 3: Core Finance Formulation)

> How to cast price series as states/actions/rewards, and where the casting cracks. Sources: S2 §2, S3 Ch.3, S4 §2, S6 §2-3, S7 §2, S8 §4-6.

## 3.1 The canonical Financial Trading System (FTS)

Bertoluzzo [S2 §2] gives the minimal tradable MDP still used by S4/S6:

```
State   s_t = (e_{t-4},...,e_t)      where e_t = (p_t - p_{t-1})/p_{t-1}   [CLOSE] daily returns window
Action  a_t ∈ {-1, 0, +1}            sell / flat / buy
Reward  r_{t+1} = a_t · e_{t+1}  (- costs if included)
Transition  price evolves exogenously, largely independent of a_t
```

Hilpisch's FinanceEnvironment [S3 §3.1] adds:
- `features` = z-scored log returns over lookback (e.g., 20–50 bars)
- `position` in info, leverage, trading costs
- `Gym API`: `reset()` picks a random start index, `step(a)` advances one bar.

Théate [S4 §2.2] formalizes discretization: choose bar resolution Δt (e.g., day [CLOSE], minute [INTRADAY]); max trading frequency = 1/Δt. Same MDP works if you change Δt — but costs and noise regime change.

## 3.2 States: what the agent actually sees

| Construction | Example | Tag | Trade-off |
|-------------|---------|-----|-----------|
| Lagged returns | `s_t = [r_{t-4}…r_t]` [S2] | [CLOSE] (S2's daily) but [BOTH] structurally | Simple, Markov-approx by embedding history; loses volatility regime |
| Normalized window | `z = (r - μ)/σ` over 25-bar window [S3 §3.1] | [BOTH] | Stationarity; `μ,σ` estimation leaks if fit on future |
| OHLC + indicators | OHLC + MA, RSI, volatility [S5 §3] | [CLOSE] validation | More signal but feature selection risk |
| Multi-modal embedding | price embedding ⊕ sentiment embedding via learned fusion [S5] | [CLOSE] crypto-sentiment | Captures public mood; needs sentiment pipeline |
| Order-book / futures features | anonymized bids aggregated into feature vector [S7 §3] | **[INTRADAY]** | Richest but venue-specific, non-portable |
| Deep feature learner | Conv/DNN on raw prices → abstract features [S6 §3.1] | **[INTRADAY]** | End-to-end; no hand-crafting |

**Key principle:** if state omits relevant history, MDP → POMDP. LSTM/GRU (S6, S7) compensates by carrying hidden state `h_t`.

## 3.3 Actions

- **Discrete 3-way:** `{-1,0,1}` — most papers, position is fully invested or flat [S2, S3-Ch6, S4].
- **Continuous position:** `a_t ∈ [-1,1]` via `tanh(w·h + b)` [S6 Eq.8] — scales long/short exposure, needs policy-gradient (A3C/DDPG).
- **Allocation vector:** `w_t ∈ Simplex` for n assets, `Σ w_i =1, w_i ≥0` (long-only) [S3 Ch.8, S8 §5].
- **Execution speed / shares per step:** `a_t = shares to trade ∈ [0, remaining]` [S3 Ch.9, S8 §4].

Choice dictates algorithm: discrete → DQN family; continuous → actor-critic / PPO [S8 §3].

## 3.4 Rewards — the most consequential design decision

| Reward | Formula | Pros | Cons | Tag |
|--------|---------|------|------|-----|
| **Period PnL / log-return** `r_t = a_{t-1}·r_t` | Immediate, unbiased | Myopic; high variance | [BOTH] |
| **Cumulative / terminal wealth** `G_T` | Matches investor objective | Sparse, delayed credit assignment | [BOTH] |
| **Sharpe ratio** `(mean(r)/std(r))` [S4, S6 Eq.4-5] | Risk-adjusted, industry standard | Non-additive (needs online estimator), can be gamed | **[CLOSE] in S4, [INTRADAY] multi-objective in S6** |
| **Differential Sharpe** [S8 §4.1] | Incremental Sharpe contribution | Additive, online | Requires careful `η` parameter |
| **Multi-objective** `U = α·mean(DR) - β·std(DR)` [S6 Eq.5] where `DR = Σ_{t=1}^{k} r_t` daily cum | Balances profit vs intraday risk | Two hyperparams `α,β` | **[INTRADAY]** explicitly |
| **Hedging error** `-(V_T - payoff)^2` or replication loss [S3 Ch.7] | Directly matches hedging goal | Only for derivatives | [BOTH] |
| **Execution cost** `- Σ cost(a_t)` + mean-variance penalty [S3 Ch.9, S8 §4] | Microstructure-aware | Model of market impact needed | [INTRADAY] intraday; [CLOSE] for large-cap close |

**Lessons from sources:**
- Théate [S4]: Sharpe reward beats pure profit on broad daily market test (improves risk-adjusted generalization).
- Si [S6]: single-objective profit led to risky intraday leverage; multi-objective stabilized. *Always* penalize variance intraday.
- Avramelou [S5]: reward is still PnL even with sentiment; sentiment enters via state, not reward.

## 3.5 Transition & market impact assumption

All papers in folder assume **price is exogenous**: `P(s'|s,a) ≈ P(s'|s)` — the agent is price-taker [S3 §3.3.2]. S8 §2.5 warns this fails for large orders (execution) or illiquid intraday — needs Almgren-Chriss impact model [S3 Ch.9]. S2 notes omission of transaction costs as a limitation; S6/S7 include costs explicitly (critical intraday).

## 3.6 Where the analogy fails (Hilpisch §3.3, Hambly §2.5)

| Game (CartPole) | Market |
|-----------------|--------|
| Infinite resets, perfect simulator | **One history** per asset; can't re-roll 2020. Patch: simulation/GAN (Ch.04) |
| Counterfactual known (what if I'd moved left?) | **No counterfactual**: unchosen action's reward unobserved |
| Stationary dynamics `P` | **Non-stationary**: regimes shift, adversary adapts |
| Dense reward, clear terminal | Sparse/noisy reward, choice of horizon is arbitrary |
| State fully observed | **Partially observed** → POMDP, need memory (LSTM) |
| No transaction cost | Costs dominate intraday; must model |

S8 frames this as the gap between stochastic control (knows model) and RL (model-free) — RL's advantage is fewer assumptions, but its price is sample inefficiency and non-stationarity [S8 §1].

## 3.7 Minimal MDP checklist (use before coding Chapter 05)

1. Choose Δt (determines tag): day [CLOSE] vs minute/second [INTRADAY] [S4 §2.2].
2. Define `s_t` embedding (lags vs OHLC vs embeddings) and normalization window — no future leakage.
3. Pick discrete vs continuous action → determines algorithm family.
4. Pick reward (start with PnL, graduate to Sharpe/multi-objective).
5. Add costs: `r ← r - c·|a_t - a_{t-1}|` per trade/volume.
6. Decide episode horizon (e.g., 500 bars or 1 trading day intraday [S6]).
7. Document POMDP risk → plan recurrent layer if intraday.

Next: data scarcity and augmentation (Ch.04), then Gym env implementation (Ch.05).

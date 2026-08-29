# 06 — Algorithms (Level 4–5)

> Value-based vs policy-based vs actor-critic. Sources: S1 Q-learning, S3 Ch.2/9, S4, S5, S6, S7, S8 §3.

## 6.1 Decision tree

```
Action space discrete & small (-1,0,1)?
  ├─ YES → Value-based: Q-learning / DQN / DDQN / Dueling  [CLOSE] most tested
  │         Examples: S2 (tabular Q), S3 Finance/Trading, S4 TDQN, S5 baselines
  └─ NO (continuous: position ∈ [-1,1], weights, execution speed)
          → Policy / Actor-Critic: A2C/A3C, DDPC, PPO
            Examples: S6 (LSTM policy), S7 (A3C+LSTM), S3 Ch.9 (actor-critic for execution)
```

## 6.2 Value-based family

### Tabular Q-learning [S1, S2]
```python
Q[s,a] += α * (r + γ*max_a' Q[s',a'] - Q[s,a])
```
Works for FTS with binned returns (S2). **Tag:** [CLOSE] tiny demo only; fails beyond low-d states.

### DQN [S3 §2.4, S4]
- `Q(s,a;θ)` = MLP; experience replay + target network.
- Loss: `L = E[(r + γ max_a' Q(s',a';θ⁻) - Q(s,a;θ))²]`
- Hilpisch uses this for all Finance/Trading/Allocation demos [S3 Ch.3,6,8].
- Avramelou baselines include DQN on price+sentiment [S5 Table 2].
- **When to use:** daily close discrete trading; fastest to implement.
- **Limitations:** overestimates Q (max bias) on noisy financial rewards — Théate reports Double-DQN mitigates.

### Double-DQN (DDQN) [S4 §2.4.1, S8 §3.3.2]
Decouple selection and evaluation:
```
a* = argmax_a Q(s',a;θ)
y  = r + γ Q(s',a*;θ⁻)
```
Théate's TDQN builds on DDQN; extends to trading-specific Bellman backup with Sharpe-aware reward shaping.

### Dueling DQN [S4 §2.4.1 cites Wang et al.]
`Q(s,a)=V(s)+A(s,a)-mean_A`. Théate ablates it — marginal but helps when "hold vs trade" advantage is small (costly trading).

**Intraday note:** None of S6/S7 use DQN for intraday; they prefer policy methods because intraday position is naturally continuous and variance-sensitive.

## 6.3 Policy & actor-critic family

### REINFORCE / vanilla policy gradient [S1 Policy Gradient, S8 §3.2]
```
∇J ≈ E[ G_t ∇ log π(a_t|s_t;θ) ]
```
Baseline b(s) subtracted to reduce variance: `∇J ≈ E[(G_t - b(s_t)) ∇ log π]`.

### Advantage Actor-Critic (A2C/A3C) [S3 Ch.9, S7]
- **Actor** `π(a|s;θ)` proposes action, **Critic** `V(s;w)` or `Q(s,a)` evaluates advantage `A = r+γV(s')-V(s)`.
- **A3C** [S7]: asynchronous workers (parallel envs) update global network; stabilizes training and speeds up.
- Ponomarev [S7 §2]: `h_t = LSTM(features_t, h_{t-1})`, policy head `π = softmax(W h_t)` (discrete) or `tanh` (continuous), trained by RMSProp async.

### Deterministic PG (DDPG) / continuous control [S8 §3.2.3]
Actor outputs deterministic action `a=μ(s;θ)`; critic `Q(s,a;w)` guides update. Mentioned in S3 Ch.9 and S8 §5.2 (allocation/execution baselines).

### PPO (Proximal Policy Optimization) [S5 §3, S6 discussion, S8 §3.2.4]
Clipped objective prevents destructively large policy steps:
```
L_CLIP = E[ min( ρ_t A_t, clip(ρ_t,1-ε,1+ε) A_t ) ],  ρ_t = π_new/π_old
```
- Avramelou [S5]: PPO among tested DRL agents for multi-modal crypto trading.
- Liang et al. [S8 §5.2 cites] PPO + adversarial noise beats DDPG on 5-stock portfolio (daily [CLOSE]).
- **Tag:** [BOTH] but most reliable PPO results in folder are daily; intraday S6/S7 stuck with A3C/LSTM for simplicity.

## 6.4 Recurrent policy (intraday specialty)

Both intraday papers insert memory:

```
features_t ──► LSTM (h_t, c_t) ──► tanh(W h_t + b) → a_t ∈ [-1,1]   [S6 Eq.8]
            ──► advantage / value head also from h_t
```

- Si [S6 §3.2]: LSTM makes trading decisions `A_t`; loss balances `mean(DR)` and `std(DR)`.
- Ponomarev [S7 §3.2]: LSTM layer *between* FC stack and policy/value heads; ablation shows it adds ~10pp annual return vs feed-forward alone.

**Rule:** Use recurrence when state is POMDP or has intraday seasonality (open/close effects).

## 6.5 Multi-objective RL (Si's contribution) [S6 §3.3, S8 §3]

Single profit objective → leveraged risk intraday. Si formulates:

```
DR_d = Σ_{t∈day d} R_t          (daily cumulative PnL)
U = α · mean(DR_d) - β · std(DR_d)   [S6 Eq.5]
max_Θ U
```

- `α,β` trade profit vs risk (β > α dampens intraday volatility).
- Alternative is pure Sharpe `SR = mean(R)/std(R)` [S6 Eq.4] — but Si shows multi-objective better because it separates **mean daily** from **volatility** explicitly.
- Generalization: Bertsekas' multi-objective DP [S8 §3.4]; recent scalarization tricks.

**Tag: [INTRADAY] only validated here**, but idea ports to [CLOSE] if you redefine DR as monthly.

## 6.6 Multi-modal fusion (Avramelou [S5]) [CLOSE]

```
e_price = Encoder_price(OHLC)      # MLP/autoencoder
e_sent  = Encoder_sentiment(text)   # sentiment scores → embedding
e_joint = [e_price; e_sent]  (or attention-weighted)
reward: PnL using e_joint as state
```

- Trains embedding jointly; can reweight `w_price·e_price + w_sent·e_sent` without retraining — quantifies modality impact.
- Validated on daily crypto OHLC + tweet/news sentiment.
- Tag **[CLOSE]**: daily sentiment pipeline; intraday sentiment is sparse and was not tested.

## 6.7 Which algorithm for which frequency?

| Task | Frequency | Recommended algo | Why |
|------|-----------|------------------|-----|
| Single-asset trend / mean-reversion | [CLOSE] daily | DDQN/Dueling DQN (TDQN recipe) | discrete, sample-efficient, Sharpe-aware |
| Price + sentiment daily | [CLOSE] crypto | PPO or DDQN over multi-modal embedding [S5] | embedding needs stable policy gradient |
| Intraday index futures (bars) | **[INTRADAY]** | LSTM actor-critic with multi-objective [S6] | continuous position + risk control |
| LOB / RTS futures (60-sec) | **[INTRADAY]** | A3C + LSTM [S7] | async stabilizes high-noise intraday |
| Option hedging | [BOTH] | DQN / FBSDE-like (S8 §6) — Hilpisch uses DQN with hedge ratio action | replication loss is smooth, DQN works |
| Portfolio allocation (n assets) | [CLOSE] n=2–3, [BOTH] intraday if high freq | A2C/DDPG over simplex [S3 Ch.8, S8 §5] | continuous simplex needs policy |
| Large-order execution | **[INTRADAY]** conceptual | Actor-critic [S3 Ch.9] (Almgren-Chriss env) | action = shares/window, path-dependent |

## 6.8 Implementation hygiene (all algos)

- Replay buffer 10k–100k; batch 32–128; Adam lr 1e-4–1e-3 [S3].
- ε decay over 10k steps or entropy bonus for actor [S7].
- Target update τ=1e-3 (soft) or every 100 steps (hard) [S4].
- Gradient clipping (norm 1–5) essential when reward is PnL (Heavy tails).
- Always log: episode return, Sharpe, turnover, max drawdown — not just loss.

Next: applications (Ch.07) end-to-ends each env+algo pair.

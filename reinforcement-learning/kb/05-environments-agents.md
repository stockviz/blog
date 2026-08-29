# 05 — Environments & Agents (Level 4)

> Practical Gym construction. Sources: S3 §2.4.1, §3.1, Ch.6–9 appendices; S6 §3–4; S7 §3–4.

## 5.1 The Gym contract (all envs obey this)

```python
class FinanceEnv(gym.Env):
    def __init__(self, data, lookback=25, commission=0.0):
        self.data = data  # DataFrame with OHLC/returns
        self.action_space = Discrete(3)  # or Box(-1,1)
        self.observation_space = Box(low=-inf, high=inf, shape=(lookback,))
    def reset(self, seed=None):
        self.t = random_start()    # random episode start [S3 §3.1]
        self.position = 0
        return self._get_state(), {}
    def step(self, action):
        self.position = action     # or update gradually
        r = self._reward(action)   # see Ch.03
        self.t += 1
        done = (self.t >= self.end)
        return self._get_state(), r, done, False, {}
```

Hilpisch's FinanceEnvironment [S3 §3.1 p112], TradingEnvironment [Ch.6 p112], HedgingEnvironment [Ch.7 p129], ExecutionEnvironment [Ch.9 p190] all follow this skeleton — differing only in `_get_state` and `_reward`.

## 5.2 Catalogue of environments in folder

| Env | State | Action | Reward | Tag | Primary source |
|-----|-------|--------|--------|-----|----------------|
| **Finance (prediction game)** | z-scored log-return window (25) [S3 §3.1] | {-1,0,1} | accuracy or PnL | [BOTH] starter | S3 Ch.3 |
| **Trading (algo trading)** | same window + position + vol [S3 §6.3] | {-1,0,1} or position | next-bar PnL minus costs | **[CLOSE] tested**, [INTRADAY*] by changing Δt | S3 Ch.6 |
| **TDQN trading** | window + holding + market features, Sharpe-based [S4 §2.3] | discrete position | differential Sharpe | [CLOSE] | S4 |
| **Multi-modal trading** | concatenated `embed(price) ⊕ embed(sentiment)` [S5 §4] | discrete | PnL | [CLOSE] crypto | S5 |
| **Multi-objective intraday** | DNN-learned features from intraday bars [S6 §3.1] | continuous `a∈[-1,1]` (LSTM policy) | `U=α·mean - β·std` [S6 Eq.5] | **[INTRADAY]** | S6 |
| **A3C RTS futures** | 60-sec aggregated bids + history buffer + current position [S7 §3] | {-1,0,1} | log PnL with commission | **[INTRADAY]** | S7 |
| **Hedging** | underlying price, time-to-maturity, moneyness, current hedge [S3 Ch.7] | hedge ratio `h ∈ [0,1]` | negative replication error `-(payoff - V_T)^2` | [BOTH] but daily in demos | S3 Ch.7 |
| **Allocation (2–3 assets)** | price levels / returns of n assets [S3 Ch.8] | simplex weight `w` | Sharpe or terminal wealth | [CLOSE] | S3 Ch.8 |
| **Optimal execution** | remaining shares, time, mid-price [S3 Ch.9] | shares to trade | negative cost = `- (execution price - arrival price)·shares - risk_penalty` | **[INTRADAY]** conceptual | S3 Ch.9, S8 §4 |

## 5.3 Design decisions checklist

1. **Lookback** — daily: 20–50 bars; intraday: 30–100 minutes but shorter if using LSTM (LSTM keeps memory) [S6].
2. **Normalization** — `StandardScaler` per episode/window; intraday resets at session open [S7].
3. **Costs** — always model. Daily: `c` ≈ 0.05–0.10% per notional; futures intraday: commission per contract (S7: 2.5 rubles) + slippage. Include as `r -= c·|Δa|` directly in `step()` so agent learns to avoid churning [S6 used costs in reward; S8 stresses cost-sensitivity].
4. **Episode definition** — daily: random rolling window of 500 bars; intraday: **one trading day = one episode** (`k` bars per day [S6 Eq.5]) — allows daily `mean/std` objective.
5. **Action persistence** — if action is target position, environment must execute `Δa = a_t - a_{t-1}` and charge costs on the delta.

## 5.4 DQLAgent anatomy (Hilpisch baseline, reusable)

```python
# S3 DQLAgent class p.78, p.114 — simplified
class DQLAgent:
    def __init__(self, env, hidden=128, layers=2, lr=1e-3, gamma=0.99, epsilon=1.0):
        self.q_net = MLP(env.obs_dim, env.n_actions, hidden, layers)
        self.target = deepcopy(self.q_net)
        self.replay = ReplayBuffer(10000)
        self.epsilon = epsilon
    def act(self, s):
        if random() < self.epsilon: return env.action_space.sample()
        return argmax(self.q_net(s))
    def learn(self, s,a,r,s2,done):
        self.replay.add((s,a,r,s2,done))
        batch = self.replay.sample(32)
        # TD target with target network
        y = r + gamma * max(self.target(s2)) * (1-done)
        loss = mse(self.q_net(s)[a], y)
        loss.backward(); optimizer.step()
        # soft update target periodically
```

Extensions used elsewhere:
- **Double-DQN** (Théate, Avramelou): decouple action selection and evaluation to reduce bias.
- **Dueling** (Théate ablates): `Q(s,a)=V(s)+A(s,a)` helps when many actions have similar value.
- **Recurrent** (Si, Ponomarev): insert LSTM between features and `tanh` head [S6 Fig.1, S7 Fig.1].

## 5.5 Random agent baseline (mandatory before RL)

```python
# S3 §2.4.2, §6.1; S6 benchmarks; S7 baselines
random_rewards = [run_episode(env, policy=lambda s: env.action_space.sample()) for _ in range(100)]
# Also: buy&hold = always a=1 [S5 compares to buy&hold]
```

Hilpisch shows random CartPole solves by luck; random finance baseline is *not* trivial — it defines the floor Sharpe you must beat. Théate [S4 §4] argues rigorous assessment must include buy&hold and market benchmarks, not just random.

## 5.6 Intraday-specific env notes

- Si et al. [S6 Fig.1]: split **feature learning** (FC stack on raw bars) from **decision making** (LSTM). Feed `FC_features` into LSTM each step.
- Ponomarev [S7 Fig.1–2]: several ANN architectures tested — adding LSTM + deeper FC consistently improved intraday Sharpe; best net had ~100–200 hidden units per layer.
- Session handling: reset hidden state at day open; don't carry LSTM across days (overnight gap is a discontinuity).

## 5.7 When to use which env

- Learning: start with Hilpisch Finance → Trading [CLOSE] on daily SPY synthetic data.
- Daily live: TDQN-style Sharpe env [S4] or multi-modal [S5] if you have sentiment.
- Intraday: clone S6 (if you have OHLC bars) or S7 (if you have order-book/bid stream).
- Derivatives: Hedging env [S3 Ch.7]; Large orders: Execution env [S3 Ch.9].

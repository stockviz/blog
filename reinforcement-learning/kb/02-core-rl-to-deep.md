# 02 — Core RL → Deep RL (Level 2: Intermediate Foundations)

> From dynamic programming to Q-learning to DQN. Sources: S1 (DP, MC, TD), S3 Ch.2, S8 §3.

## 2.1 Dynamic programming (the oracle)

If you *knew* `P` and `R`, Bellman optimality gives the optimal value:

```
V*(s) = max_a [ R(s,a) + γ Σ_s' P(s'|s,a) V*(s') ]
Q*(s,a) = R(s,a) + γ Σ_s' P(s'|s,a) max_a' Q*(s',a')
```

Policy iteration / value iteration solve this exactly [S3 §2.2; S1 DP]. Finance never grants this luxury (unknown market dynamics) — so we estimate from samples.

## 2.2 From samples: Monte Carlo → TD → Q-learning

- **Monte Carlo:** estimate returns from full episodes `G_t`; unbiased but high variance [S1 MC].
- **TD(0):** bootstrap: `V(s) ← V(s) + α[ r + γV(s') - V(s) ]` — lower variance, works online.
- **Q-learning (off-policy TD):** 
  ```
  Q(s,a) ← Q(s,a) + α[ r + γ max_a' Q(s',a') - Q(s,a) ]
  ```
  No model of `P` needed; learns `Q*` while following an exploratory policy [S1 Q-learning; S3 §2.3].

S2's FTS is tabular Q-learning with discrete state bins and 3 actions — works for tiny state spaces only.

## 2.3 Why deep? Function approximation

CartPole state is continuous (4-d float); trading state is 10–100-d. Table `Q(s,a)` is impossible. **Deep Q-Learning (DQN)** replaces table by `Q(s,a; θ)` with a neural net [S3 §2.4; S8 §3.3].

Hilpisch's minimal DQLAgent [S3 §2.4.3, DQLAgent class p.78]:
- Feed-forward MLP (2–4 hidden layers, ReLU, 64–256 units)
- ε-greedy exploration (`ε` decays 1.0 → 0.05)
- Experience replay buffer (store `(s,a,r,s',done)`, sample mini-batches) — breaks correlation, reuse data [S8 §3.3.1]
- Target network `θ⁻` updated with soft copy `θ⁻ ← (1-τ)θ⁻ + τθ` for stability (Double-DQN optional)
- MSE loss `L = E[(r + γ max Q(s',a';θ⁻) - Q(s,a;θ))²]` optimized by Adam

**Notable:** Avramelou [S5] and Théate [S4] use Double-DQN / Dueling variants to reduce overestimation bias on noisy financial rewards.

## 2.4 Training loop details that matter for finance

| Detail | Game default | Finance adaptation | Tag |
|--------|--------------|--------------------|-----|
| Episodes | terminate on failure (pole falls) | fixed horizon (e.g., 500 bars) or rolling windows [S3 §3.1] | [BOTH] |
| Reward scale | 0/1 or score | returns are ~1e-3; must scale/normalize or Sharpe explodes [S4] | [BOTH] |
| Replay buffer | 10k–1M transitions | same, but watch leakage: don't sample across non-overlapping regimes naively [S3 §4] | [BOTH] |
| Exploration | ε-greedy | often higher initial ε; some papers use stochastic policy (A3C) for continuous actions [S7] | [INTRADAY] benefits from stochastic |
| Normalization | pixel/state norm | z-score returns/features; Hilpisch uses `StandardScaler` per window [S3 §3.1] | [BOTH] |

## 2.5 Q-learning vs supervised learning (Hilpisch §2.5)

|  | Supervised | Q-learning |
|---|---|---|
| Label | `y` given | `r` observed, delayed, noisy |
| Objective | minimize prediction error | maximize cumulative discounted reward |
| Data | i.i.d. `(x,y)` | trajectory-dependent, non-stationary |

Consequence: backtesting a Q-agent on historical `y` without stepping `env.step()` is invalid — rewards depend on actions' market impact (even if assumed zero) and state evolution [S3 §3.3.2].

## 2.6 Policy-based view (preview of Ch.06)

Value methods learn `Q` and derive `π(s)=argmax Q`. **Policy gradients** learn `π(a|s;θ)` directly: `∇J ≈ E[ G_t ∇log π(a_t|s_t)]` (REINFORCE). Actor-critic combines both: critic estimates value, actor improves policy. Needed when actions are continuous (allocation weights, execution speeds) [S3 Ch.9; S7; S8 §3.2].

## Bridge to finance

All of §2 assumed a perfect simulator (CartPole). Finance violates two assumptions:
1. **Limited data:** one path, not infinite resets [S3 §3.3.1]
2. **No counterfactual:** you can't re-run 2020 to try a different action [S3 §3.3.2, S8 §2.5]

Chapter 04 is the patch kit. Chapter 03 reframes the market as an MDP anyway.

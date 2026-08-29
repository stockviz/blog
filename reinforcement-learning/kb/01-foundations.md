# 01 — Foundations (Level 1: Intro / Easy)

> Prereq: Python, NumPy, pandas basics. No finance or RL assumed. Sources: S1, S3-Ch1.

## 1. What RL is (and isn't)

RL = **learning through interaction**. Agent ⇄ Environment loop: state → action → reward → next state. No supervisor labels actions as right/wrong; the agent discovers them by trial and error to maximize cumulative reward [S1 Intro; S3 §1.3].

Contrast:
- **Supervised learning:** learn from `(x, y)` pairs provided by a teacher.
- **Unsupervised:** find structure in `x`.
- **RL:** learn from evaluative feedback (`r`) that can be delayed by many steps.

Financial intuition [S3 §1.2]: Bayesian coin-toss/di e examples are the warm-up — estimate a hidden bias from repeated bets, update beliefs. Finance generalizes: the "bias" is the market's drift/vol regime, and you update positions.

## 2. Core vocabulary (master these 6 terms)

- **State `s_t ∈ S`:** summary of all information available to the agent at time `t` [S2 §2]. For CartPole: cart position/velocity + pole angle; for trading: recent returns, position, volatility.
- **Action `a_t ∈ A(s_t)`:** choice. Can be discrete (e.g., {-1 sell, 0 flat, 1 buy} [S2]) or continuous (e.g., target position in [-1,1] [S6 Eq.8], or execution speed [S8]).
- **Reward `r_t`:** scalar feedback after acting. Finance rewards: next-bar PnL `r_t = a_{t-1}·(p_t-p_{t-1})`, log return, or risk-adjusted Sharpe/Sortino [S4, S6].
- **Policy `π(a|s)`:** rule mapping states → actions (deterministic `a=π(s)` or stochastic distribution).
- **Episode:** trajectory `s0,a0,r1,s1,...,sT` until terminal state (game over) or horizon. Hilpisch uses fixed-horizon episodes for trading [S3 §3.1].
- **Discount `γ ∈ [0,1]`:** weight on future rewards; `G_t = Σ γ^k r_{t+k+1}`.

## 3. The explore–exploit dilemma

First game: **multi-armed bandit / slot machines** [S1 "Multi-Armed Bandit"]. You must balance exploring (trying unknown machines) vs exploiting (playing the best known). Finance analogue: sampling new signals vs trading the current best strategy. UCB1, ε-greedy, Bayesian posterior sampling are the classic fixes [S1].

## 4. Markov Decision Process (MDP) — the formal frame

An MDP is tuple `(S, A, P, R, γ)` where `P(s'|s,a)` is transition dynamics [S1 MDP ch; S8 §2]. Key assumption: **Markov property** — next state depends only on current state and action, not full history. S8 proves most RL theory requires this; S2 explicitly assumes it for price histories by embedding lags (5 past returns) into the state to *make* it Markovian.

Partial observability (POMDP) [S8 §2.4]: markets hide information (other traders' intent, latent regime). Then `s_t` is not the true market state — the agent sees an observation `o_t`. Recurrent nets (LSTM) are the standard patch [S6, S7].

## 5. Why "interaction" is special for finance

S3 §1.3 lists breakthroughs (Atari, Go, CartPole) that all share a **simulator** generating infinite data. Finance lacks one — you get one historical path per asset [S3 §3.3]. This data-scarcity gap motivates all of Ch.04 (simulation/GAN). Keep it in mind: every easy idea below gets harder once you leave games for markets.

## 6. First code mental model (CartPole → Finance)

Hilpisch Ch.2–3 shows the pattern you'll reuse everywhere:

```python
# [BOTH] — same loop for CartPole and trading, only env changes
env = GymEnvironment()      # CartPole-v1 or FinanceEnvironment
agent = DQLAgent(env)
for episode in range(n_episodes):
    s = env.reset()
    while not done:
        a = agent.act(s)           # ε-greedy over Q(s,·)
        s_next, r, done, info = env.step(a)
        agent.learn(s, a, r, s_next, done)
        s = s_next
```

The Gym API (`reset/step`) is the contract [S3 §2.4.1, §3.1]. Master it here and Chapters 05–07 are just new envs.

## Check yourself (Level 1 → 2 gate)

- Can you state the MDP tuple and why FTS embeds 5 lags to approximate Markov?
- Can you write ε-greedy and explain why `γ` near 1 matters for Sharpe vs PnL rewards?
- Can you run CartPole random agent and beat it with DQL [S3 §2.4.3]?

If yes → 02.

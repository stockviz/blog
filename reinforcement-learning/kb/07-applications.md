# 07 — Applications (Level 4–6)

> End-to-end recipes from book + papers. Each has frequency tag and minimal spec.

## 7.1 Algorithmic trading — single asset

### 7.1a Daily prediction/trading (Hilpisch Ch.6, Théate 2021)

**Tag: [CLOSE]**

- **State:** 25-bar z-scored log returns + position flag [S3 §6.3]; TDQN adds broader market indicators [S4 §2.3].
- **Action:** `{-1,0,1}`.
- **Reward:** next-bar PnL `a_{t-1}·r_t - c·|Δa|` (Hilpisch) or differential Sharpe (TDQN). Théate proves Sharpe reward improves broad-market generalization.
- **Agent:** DQN/DDQN (Hilpisch) or Dueling-DDQN called TDQN (Théate).
- **Training:** synthetic or artificial trajectories (Ch.04), 100–500 episodes, ε-greedy; testing on held-out later years with transaction-cost sweep.
- **Result pattern:** beats random & buy&hold on most but not all regimes; thin markets deteriorate faster due to costs [S4 §4].

```python
# [CLOSE] trading loop — Hilpisch Ch.6 TradingEnvironment
env = TradingEnvironment(data=spy_daily, lookback=25, commission=0.0005)
agent = DQLAgent(env, hidden=128, layers=3)
train(agent, env, episodes=500)
evaluate(agent, data=spy_daily_ohlc_test)  # report Sharpe, turnover, CAGR
```

### 7.1b Multi-modal crypto trading (Avramelou 2024) [CLOSE]

- **State:** `embed(price OHLC) ⊕ embed(daily sentiment)` — sentiment from tweets/news. Embeddings pre-trained, attention reweights without retraining [S5 §4].
- **Reward:** PnL (daily bar).
- **Agent:** PPO / DQN variant over joint embedding; outperforms price-only and buy&hold in Table 6 (PnL metric).
- **Insight:** naive concatenation of sentiment features overfits; embedding fusion + ability to down-weight sentiment at inference is the win.
- **Tag: [CLOSE]** — sentiment pipeline is daily; no intraday validation.

### 7.1c Intraday index-futures trading (Si 2017) **[INTRADAY]**

- **Data:** stock-index futures, intraday bars (minute-level) [S6 §4].
- **Pipeline:** DNN feature learner (3× FC layers) → LSTM policy `a_t = tanh(W h_t + b)` ∈ [-1,1] continuous [S6 Fig.1, Eq.8].
- **Reward:** multi-objective `U = α·mean(DR) - β·std(DR)` where `DR` = daily cumulative reward (one episode = one day). This balances intraday profit vs stability.
- **Optimization:** LSTM weights via BPTT + RMSProp; batch over days, not bars.
- **Costs:** transaction cost inside `R_t`.
- **Takeaway:** profit-only objective amplified intraday swings; multi-objective mandatory for intraday reliability.

### 7.1d RTS futures A3C trading (Ponomarev 2019) **[INTRADAY]**

- **Instrument:** RTS Index futures (MOEX:RTSI), real anonymized bids, 60-second decisions.
- **State:** aggregated bid features + position + history (vector).
- **Architecture search:** several ANNs; **LSTM variant wins** — adding LSTM + deeper FC raised net profit.
- **Agent:** A3C (async advantage actor-critic) [S7 §2–3]; training/testing via two global processes sharing TensorFlow checkpoint.
- **Result:** best net **66% p.a. net of 2.5-ruble commission** (110% gross) — but venue-specific; authors warn of overfitting to RTS microstructure.

## 7.2 Dynamic hedging (Hilpisch Ch.7) — [BOTH] but daily-demos

- **Problem:** learn delta-hedge for European option in Black-Scholes-Merton (BSM) world, minimizing replication error — replaces analytic delta [S3 Ch.7].
- **State:** spot `S_t`, time-to-maturity `τ`, moneyness `m=S/K`, current holdings `h_t`.
- **Action:** hedge ratio `Δh ∈ [0,1]` (position in underlying).
- **Reward:** `- (portfolio - payoff)²` at maturity and intermediate transaction-cost penalties; BSM formula derived in §7.6 for benchmark.
- **Env:** simulates GBM under BSM parameters; GBM paths are the data source (no history needed).
- **Agent:** DQN over discrete hedge buckets; Hilpisch reports RL hedge closes on analytic delta when costs are zero and deviates optimally when costs >0.
- **Extension:** S8 §6 notes deeper hedging (Buehler et al.) and RL pricing of American/Bermudan options — expert tier.

**Tag:** [BOTH] — continuous hedging theory is frequency-agnostic; demo steps daily under GBM, but intraday hedging would just shrink Δt and magnify costs.

## 7.3 Dynamic asset allocation (Hilpisch Ch.8) — [CLOSE] (S8 has [BOTH])

- **2-fund separation (1 risky + 1 risk-free)** → optimal is classic Kelly/Merton; RL should recover it when trained under same GBM — sanity-check [S3 §8.1].
- **2-asset and 3-asset risky:** action = weight vector on simplex; reward = Sharpe or terminal utility. Hilpisch shows DQN/DDPG can discover diversification without Markowitz inputs; equally weighted portfolio (EW) is the benchmark that is surprisingly hard to beat out-of-sample [S3 §8.5].
- **Beyond 3 assets:** combinatorial explosion; S8 §5 surveys multi-asset portfolio RL (Liang et al. adversarial PPO on 5 stocks [CLOSE]).

**Tag:** [CLOSE] in Hilpisch (daily rebalancing); S8 notes intraday allocation possible for multi-strategy pods but rarely reported (turnover penalty dominates).

## 7.4 Optimal execution (Hilpisch Ch.9, Hambly §4) — **[INTRADAY] conceptual**

- **Almgren-Chriss style:** liquidate `X` shares over `T` steps; price impact = permanent + temporary [S3 §9.2].
- **State:** remaining inventory `x_t`, elapsed time `t/T`, mid-price, spread.
- **Action:** shares to trade `a_t ∈ [0, x_t]` (continuous).
- **Reward:** `-(execution_cost + λ·risk)` where risk = variance of cost due to price moves — S8 §4 derives efficient frontier.
- **Agent:** Actor-critic [S3 §9.5] because action is continuous and constrained. Random agent baseline first (§9.4), then actor-critic beats TWAP/VWAP-like heuristics.
- **Tag: [INTRADAY]** intrinsically — execution horizon is minutes/hours; daily execution is a special case.

## 7.5 Mapping: pick your entry

| Your goal | Start with | Frequency |
|-----------|------------|-----------|
| Learn RL → finance | Ch.7.1a daily trading [CLOSE] (S3 Ch.6) | [CLOSE] |
| Add alternative data | 7.1b multi-modal [CLOSE] (S5) | [CLOSE] |
| Go intraday on futures | 7.1c Si pipeline or 7.1d A3C RTS recipe | **[INTRADAY]** |
| Hedge options desk | 7.2 hedging [BOTH] (S3 Ch.7) | [BOTH] |
| Allocate capital | 7.3 allocation [CLOSE] (S3 Ch.8) | [CLOSE] |
| Minimize execution slippage | 7.4 execution [INTRADAY] (S3 Ch.9) | [INTRADAY] |

**Progression advice:** master 7.1a → then branch to either intraday (7.1c/d) or derivatives (7.2/7.4). Don't jump to intraday before passing daily Sharpe-based evaluation (Ch.08) — intraday noise will mask bugs.

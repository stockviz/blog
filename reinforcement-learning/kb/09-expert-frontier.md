# 09 — Expert Frontier & Open Problems (Level 6–7: Expert)

> What the survey says is next, and what none of the sources fully solve. Sources: S8 §6–8, S3 Ch.10, S4 §5, S5 §5, S7 §5.

## 9.1 Theory gap: RL vs stochastic control

Hambly [S8 §2, §7] frames it: control theory assumes known `P` and derives HJB; RL is model-free and shines when `P` is unknown but demands more data. Convergence rates for deep RL in continuous finance remain largely open — DQN's tabular guarantees (Sutton & Barto) don't extend to MLP + replay on non-stationary returns. Current practice is empirical: if it generalizes across 30 stocks [S4] or across days [S6], it is deemed evidence.

## 9.2 Portfolio at scale

- **Solved (in folder):** 2–3 assets with DQN/DDPG recover diversification [S3 Ch.8]; 5 stocks with PPO+adversarial [S8 §5.2].
- **Open:** hundreds of assets (long/short, leverage, constraints), cross-asset dependencies, non-convex costs. Scaling the simplex action and learning joint dynamics remains research-grade — S8 §5 surveys multi-agent and constrained MDP approaches.

## 9.3 Derivative pricing & deeper hedging

S8 §6 beyond Hilpisch Ch.7:
- RL hedging under **market frictions** (spread, impact, short constraints) beats BSM delta — but calibration to real implied vol surface is hard.
- American/Bermudan exercise → optimal stopping as RL (S8 §6.2).
- Recent "deep hedging" (Buehler et al., cited in S8) reframes super-hedging as RL with risk measures (CVaR). No source in folder implements it end-to-end — frontier to explore.

## 9.4 Market making & multi-agent

S8 §7: market making = queue position + inventory control → RL state includes order-book depth, action is quote placement. Requires **LOB simulator**; none of S1–S7 provide one. Intraday RL for market making is the natural extension of S7's 60-sec trader, but needs exchange-grade feed and impact model.

## 9.5 Intraday frontier (specific to [INTRADAY])

- **Feature discovery:** Si's DNN feature learner [S6] is shallow. Modern successor: temporal convolutions / transformers over tick streams (not in folder — predictable next step).
- **Multi-objective tuning:** Si's `α,β` are fixed — dynamic risk-aversion conditioned on volatility regime is open [S8 §3.4].
- **Async vs off-policy:** Ponomarev's A3C [S7] has been superseded by PPO/IMPALA for stability; re-implementing S7 with PPO + LSTM is a direct uplift experiment.
- **Generalization across venues:** S7's RTS result (66% net) likely overfit to RTS microstructure; S8 warns cross-venue generalization is the intraday litmus test (barely attempted).

## 9.6 Sentiment & multi-modality (S5 frontier)

Avramelou [S5 §5] lists:
- Time-decay of sentiment impact (old tweets worth less).
- Intraday sentiment (requires real-time NLP pipeline — not validated).
- Adversarial sentiment (pump tweets) → robust embeddings needed.
- Modality dropping: their post-hoc reweighting without retraining is a step toward robust fusion, but more modalities (macro, order-flow) remain to be fused.

## 9.7 Simulation realism

Hilpisch's MCS/GBM [S3 Ch.4] and GAN [Ch.5] are baselines. Frontier simulators (beyond folder):
- Market simulators with **impact** (Almgren-Chriss) for execution [S3 Ch.9],
- Adversarial market generators (train RL agent against a GAN adversary [S8 §5.2 cites Liang's adversarial PPO]),
- Regime-switching simulators that produce the very tail events the agent must survive (S8 §7: non-stationarity).

## 9.8 Practical deployment view (Hilpisch Ch.10)

Hilpisch Ch.10 + Bertoluzzo & S8 converge:

- RL is **not** a replacement for financial theory — it is a numerical solver for sequential decisions where closed forms fail (Théate's Sharpe-maximizing policy has no analytic form).
- Success criteria are operational: net Sharpe after realistic `c` and latency, drawdown under regime shift, turnover under exchange limits.
- Data augmentation (Ch.04) is *the* enabler; without it every expert idea overfits. Treat augmentation pipeline as part of infrastructure, not an afterthought.

## 9.9 Exercises at expert level

1. Replicate Théate TDQN on NIFTY daily [CLOSE] with 0.05% bps costs; sweep Sharpe vs PnL reward. Extend Δt→5-min and report breakage.
2. Port Si's pipeline [S6] from index futures bars to your broker's minute bars; ablate `α,β` and LSTM.
3. Implement Hilpisch Ch.9 execution env under Almgren-Chriss with `η` permanent impact; train actor-critic vs TWAP; evaluate implementation shortfall.
4. Add a third modality to Avramelou [S5] (e.g., funding rate or vol surface) reusing their embedding-reweight trick.

If you can complete 1+4 with out-of-sample net Sharpe >1 on second venue, you are past the folder and into publishable territory.

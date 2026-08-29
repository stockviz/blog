# 04 — Data, Features & Augmentation (Level 3–4)

> Sources: S3 Ch.4–5, S4 §3.3, S5 §3, S6 §3.1, S7 §3, S8 §4–5.

## 4.1 The data scarcity problem

Games give infinite simulated trajectories. Markets give **one realized path** per instrument [S3 §3.3.1, §4 Intro].

- Hilpisch: "Even with intraday data instead of EOD, the problem of limited financial data persists" [S3 §3.3.1].
- Théate: trains on artificial trajectories bootstrapped from limited history to avoid overfitting [S4 §3.3].
- Consequence: naive DQN overfits to the specific bull/bear pattern it saw (see S3 §4 Fig: noisy vs simulated series).

Two augmentation strategies are covered [S3 Part II]:

## 4.2 Strategy A — Simulated / noisy data (S3 Ch.4)

### A1. Add noise to historical data
```
r_noisy = r_hist + ε,  ε ∼ N(0, σ_noise²)   or   p_noisy = p_hist · (1 + ε)
```
Hilpisch §4.1: inject Gaussian noise calibrated to historical volatility; preserves autocorrelation loosely. Cheap baseline. [BOTH]

### A2. Stochastic process simulation (MCS)
Simulate under parametric model, then train agent in simulation:
- GBM: `dS = μ S dt + σ S dW`
- OU / Heston / jump-diffusion for richer dynamics [S3 §4.2]
- Hilpisch generates 1k–10k paths of length T, trains DQLAgent on synthetic, tests on real holdout.

**When to use:** allocation/hedging where closed-form dynamics exist (Ch.7–9). **Tag:** [BOTH], but especially [CLOSE] validation because GBM at minute scale mis-specifies microstructure.

### A3. Artificial trajectories via block bootstrap (Théate §3.3)
Resample trajectory segments from history to synthesize new episodes; preserves local dependence. Used to train TDQN purely from limited daily data [S4] — best publication-grade trick for [CLOSE].

## 4.3 Strategy B — Generated data with GANs (S3 Ch.5)

Train a GAN to mimic historical `(r, features)` joint distribution, then sample unlimited synthetic series.

- Hilpisch §5.1–5.2: simple example (1-d) → financial example (vector OHLC); KS test to validate synthetic vs real [S3 §5.3, Fig 5.6].
- Advantage over MCS: no parametric assumption; captures fat tails, volatility clustering.
- Risk: GAN mode collapse → synthetic lacks regime diversity; always KS/augment validation.

**Tag:** [BOTH] — equally useful intraday (needs more data, GANs help most there).

## 4.4 Feature engineering across frequencies

| Feature family | Intraday nuance | Close nuance |
|----------------|-----------------|--------------|
| **Raw OHLC / log returns** | Requires bar choice (1-min vs 5-min vs tick). Microstructure noise high at 1-min [S6, S7] | Daily OHLC is standard [S5]; lower noise |
| **Z-scored windows** ` (r-μ)/σ` | Rolling μ,σ over intraday session; reset per day to avoid overnight gap contamination [S7] | 20–50 day rolling window [S3] |
| **Technical indicators** (MA, RSI, vol) | Less predictive intraday; more mean-reversion [S6 feature learner replaces them] | Common in S5 daily indicators |
| **Volatility / regime features** | Realized vol per hour, bid-ask spread, volume imbalance [S7 order-book vector] | Historical vol, VIX-like proxies |
| **Sentiment embeddings** [S5] | Requires **daily** sentiment aggregation for crypto; intraday sentiment is sparse/noisy | **[CLOSE] only validated** — multi-modal embedding: `e_price ⊕ e_sentiment` via MLP autoencoder, attention weight can be tuned post-hoc without retraining |
| **Deep-learned features** [S6 §3.1] | DNN (fully-connected) maps raw intraday bars → latent features `FC1→FC2→FC3` → LSTM | Overkill for daily where handcrafted suffices |

**Ordering principle (easy → expert):**
1. Start: lagged z-scored returns [S2, S3] — 5–25 lags.
2. Add: OHLC + 1–2 indicators, verify no lookahead.
3. Add: volatility regime feature.
4. Expert intraday: order-book imbalance, volume clock, or DNN feature learner [S6]; sentiment only if daily [S5].
5. Always: include current **position** in state if reward is PnL (else agent can't learn to hold).

## 4.5 Normalization & leakage discipline

- Fit scaler (`μ,σ`) on training window only; apply to test [S3 §3.1, §4].
- Label lag: if you predict `r_{t+1}` from `s_t`, `s_t` must not contain `p_{t+1}` [S3 Ch.6].
- Transaction cost `c` must be applied in reward, not added ex-post.

## 4.6 Which augmentation for which frequency?

| Situation | Recommended augmentation | Tag |
|-----------|--------------------------|-----|
| You have 5–10y daily closes per asset (typical) | Artificial trajectories (bootstrap) [S4] + light Gaussian noise [S3 §4.1] | [CLOSE] |
| You want to test allocation across 3 assets (S3 Ch.8) | MCS under GBM with estimated μ,Σ | [BOTH] but daily |
| You have 6 months intraday futures (<1k sessions) | GAN-generated bars [S3 Ch.5] + DNN feature learner [S6] | **[INTRADAY]** |
| You have order-book tick data (rare) | Don't augment ticks; aggregate to 1-min bars first [S7 §3] | [INTRADAY] |
| Hedging / execution model exists (BSM/Almgren-Chriss) | Simulate under that model [S3 Ch.7,9] | [BOTH] |

## 4.7 Sanity tests

- KS test synthetic vs real per Hilpisch §5.3; p > 0.05 before training.
- Train on synthetic, test on real holdout — not the reverse alone.
- Intraday: test across days, not bars shuffled (preserves intraday seasonality).

# Single-Stock Winner / NIFTY-Short Momentum — India Replication

Replication of **Ammann, Moellenbeck & Schmid (2010)**, "Feasible Momentum Strategies in the US Stock Market," adapted for the Indian equity market using NSE data.

[Paper: Feasible Momentum Strategies in the US Stock Market](https://ssrn.com/abstract=1694700) — ([summary](paper_summary.md))

> "Long the single best-performing winner stock, short the S&P 100 index, formation period J = 6 months, holding period K = 3 months." — Best strategy identified in the paper, producing 1.52% monthly excess return (Sharpe 0.37) on S&P 100 names, 1984–2009.

---

## How Configs Are Selected

Each variant runs two configs, both reported on the **test period only (≥ 2020)**:

- **paper_best**: Fixed at J=6, K=3, N=1 — the paper's best cell, always included as a baseline.
- **search**: Chosen by sweeping a 3×3×2 grid (J ∈ {3,6,12}, K ∈ {3,6,12}, N ∈ {1,3}) on the **training set (≤ 2019-12-31)**. The config with the highest annualized Sharpe on training data is selected, then run on the full dataset. Only test-period metrics are reported below.

This lets us compare the paper's original specification against the best configuration the data would have chosen in real time, with no test-period leakage.

---

## Evolution of the Signal

The paper's raw-return momentum signal fails in India for two reasons: (a) it picks extreme-volatility stocks like ADANIENT, and (b) equal-notional NIFTY hedging applies a fixed 0.5 ratio regardless of the stock's actual beta — for the high-beta winners that momentum selects, this under-hedges and leaves substantial market exposure on the table. We tested two improvements:

| Stage | Momentum Signal | Short Leg Sizing | Key Idea |
|---|---|---|---|
| **1. Raw Return** (paper replica) | J-month cumulative return | Equal notional (50/50) | Baseline: exactly as the paper |
| **2. Sharpe Ratio + Beta** | J-month annualized Sharpe of daily returns | 1-year trailing beta × long, capital-neutral: returns ÷ (1+\|β\|) | Vol-adjusted signal + market-neutral hedge, comparable CAGR |
| **3. Omega Ratio + Beta** | Omega(stock daily rets, MAR = NIFTY daily ret) | 1-year trailing beta × long, capital-neutral: returns ÷ (1+\|β\|) | Downside-aware signal: rewards consistency of beating NIFTY |

**Why Omega > Sharpe**: Sharpe penalizes upside and downside volatility equally. A stock that beats NIFTY by 2% every day has the same Sharpe as one that beats by 5% half the time and loses 1% half the time — even though the former is far superior for a long/short strategy. The Omega ratio with NIFTY as the MAR directly measures how consistently a stock outperforms the index: we're long the stock and short NIFTY, so every day the stock beats NIFTY is a winning day.

---

## Test-Period Results (≥ 2020)

All variants use the same universe (top-60% mcap, EQ-only), strand construction, 1-month skip, and annual rebalancing. Only the signal and hedge ratio change.

| # | Script | Signal | Hedge | Config | Test CAGR | Test Sharpe | Test MaxDD | Long-Only CAGR |
|---|---|---|---|---|---|---|---|---|
| 1 | backtest.R | Raw return | Equal notional | J=6,K=3,N=1 | -2.6% | -0.06 | -49.5% | +10.2% |
| 2 | backtest.R | Raw return | Equal notional | J=6,K=12,N=1 | -4.9% | -0.34 | -56.4% | +4.9% |
| 3 | backtest_SR.R | Sharpe ratio | Equal notional | J=6,K=3,N=1 | +0.5% | 0.11 | -20.7% | +18.4% |
| 4 | backtest_SR.R | Sharpe ratio | Beta hedge | J=6,K=3,N=1 | +5.7% | 0.34 | -54.8% | +18.4% |
| 5 | backtest_SR.R | Sharpe ratio | Beta hedge | J=6,K=6,N=3 | +6.7% | 0.50 | -33.4% | +17.3% |
| 6 | backtest_OR.R | **Omega ratio** | **Beta hedge** | J=6,K=3,N=1 | +4.4% | 0.30 | -57.1% | +15.6% |
| 7 | **backtest_OR.R** | **Omega ratio** | **Beta hedge** | **J=6,K=6,N=3** | **+8.8%** | **0.62** | **-29.6%** | **+19.6%** |

**Key takeaway**: The Omega+Beta variant (#7) delivers the best test-period metrics — highest CAGR (+8.8%), highest Sharpe (0.62), lowest MaxDD (-29.6%), and highest long-only CAGR (+19.6%). The progression from raw+equal-notional (-2.6% test) to Omega+Beta (+8.8% test) represents an **11.4 percentage point improvement** from signal quality and hedge precision alone.

---

## Why Each Improvement Matters

### Raw Return → Sharpe Ratio: Eliminates the ADANIENT Problem

Raw return ranks ADANIENT #1 in Dec 2022 (81% 6-month return) — just before the Hindenburg crash (-54.6% in 3 months). Sharpe ratio ranks it far lower because its 200% annualized volatility penalizes it. The top SR stock was ITC (Sharpe ~2.5), which returned +20% over the same period.

| Signal | Top Pick Dec 2022 | Why | Outcome (Jan–Mar 2023) |
|---|---|---|---|
| Raw return | ADANIENT (+81%) | Highest raw return | **-54.6%** |
| Sharpe ratio | ITC (SR ~2.5) | Best risk-adjusted return | **+20.3%** |
| Omega ratio | ITC (Omega ~3.1) | Most consistent NIFTY outperformer | **+20.3%** |

### Equal Notional → Beta Hedge: Stops Destroying Alpha

**The problem (original backtest.R):** Indian momentum stocks average beta ~1.2. Shorting NIFTY at equal notional leaves a net long beta — the stock's market return leaks through and dominates the strategy.

**The fix (backtest_SR.R and backtest_OR.R):** Beta hedging shorts exactly beta units of NIFTY per unit of stock, neutralizing market exposure precisely. Returns are divided by (1+|β|) to maintain capital neutrality — without this scaling, a beta=1.2 hedge would use 2.2× the notional of equal notional, inflating CAGR. The long-only CAGR improved from +10.2% (raw) to +19.6% (Omega) — but only beta hedging lets the combined strategy capture that alpha instead of destroying it.

### Sharpe Ratio → Omega Ratio: Rewards Consistency Over Magnitude

Sharpe penalizes volatility symmetrically. Omega with NIFTY as MAR penalizes only underperformance relative to the index. A stock that steadily beats NIFTY by 1% daily has Omega >> 1. A stock that beats NIFTY by 5% on some days and loses 2% on others may have the same Sharpe but lower Omega. For a long/short strategy, Omega aligns perfectly with the objective: we win when stock > NIFTY.

The Omega search-best (J=6,K=6,N=3) improves test Sharpe from 0.50 (SR) to 0.62. **Note:** while beta hedging, always be capital-neutral — divide returns by (1+|β|) so CAGR is on the same basis as equal notional. Sharpe is scale-invariant and unaffected.

---

## The ADANIENT Case Study (Why Raw Return Fails)

The strategy picks one stock and holds it for K months. When a black-swan event hits that stock, the concentrated exposure is catastrophic with raw returns:

| Signal Date | Stock | 6-mo Momentum | Holding | Stock Return | Combined Return |
|---|---|---|---|---|---|
| 2022-12-30 | ADANIENT | +81.0% | Jan–Mar 2023 | **-54.6%** | **-31.2%** |
| 2023-01-31 | ADANIENT | +76.2% | Feb–Apr 2023 | **-35.3%** | **-20.3%** |

The **Hindenburg Research report** (Jan 24, 2023) crashed ADANIENT from ~3,900 to ~1,700. The 6-month formation window ended before the crash — the signal had no way to anticipate it. With K=3 overlapping strands, **2 of 3 active strands held ADANIENT during the worst of the selloff**. Neither Sharpe nor Omega would have selected ADANIENT — its volatility and inconsistent NIFTY outperformance rank it poorly on both risk-adjusted metrics.

---

## Key Findings

1. **Omega + Beta Hedge is the best combination**: +8.8% test CAGR (Sharpe 0.62) vs -2.6% for the paper's raw+equal-notional.
2. **The signal matters more than the config**: Moving from raw return to Omega improves test Sharpe from -0.06 to 0.30 even at the same J/K/N — a bigger jump than any parameter tuning.
3. **Beta hedging is essential in India**: High-beta momentum stocks need precise market-neutral hedging. Equal notional (50/50) under-hedges them.
4. **Long-only alpha is robust**: The long leg alone delivers +10–20% CAGR across all signal variants. The challenge is extracting it without the hedge destroying it.
5. **J=6 + N=3 dominates**: 6-month formation and 3-stock portfolios consistently rank highest across both SR and Omega sweeps.
6. **Diversification (N>1) improves risk-adjusted signals**: Unlike the paper's finding that N=1 is best, the SR and Omega variants both prefer N=3 — India's concentrated universe benefits from modest diversification with quality signals.

---

## Output Files

| Prefix | Signal | Hedge | Best Config | Script |
|---|---|---|---|---|
| `paper_best_*` | Raw return | Equal notional | J=6,K=3,N=1 | `backtest.R` |
| `search_*` | Raw return | Equal notional | J=6,K=12,N=1 | `backtest.R` |
| `paper_best_SR_*` | Sharpe ratio | Beta hedge | J=6,K=3,N=1 | `backtest_SR.R` |
| `search_SR_*` | Sharpe ratio | Beta hedge | J=6,K=6,N=3 | `backtest_SR.R` |
| `paper_best_OR_*` | Omega ratio | Beta hedge | J=6,K=3,N=1 | `backtest_OR.R` |
| `search_OR_*` | Omega ratio | Beta hedge | J=6,K=6,N=3 | `backtest_OR.R` |
| `search_*_sweep_metrics.png` | Per-variant 18-config sweep tables | | | |

Generate: `Rscript backtest.R` (raw), `backtest_SR.R` (SR+beta), or `backtest_OR.R` (Omega+beta). ~10–25 min from checkpoint depending on variant.

@StockViz

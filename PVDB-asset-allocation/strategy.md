# ABA — Adaptive Breadth Allocation

## Design Rationale

After backtesting PAA, VAA, DAA, and BAA across 2008–2026 using only real ETFs
and walk-forward parameter selection, four findings stand out:

1. **BAA's dual-filter wins.** SMA for offensive rotation + 13612W for
   defensive signaling produced the highest OS Sharpe (0.47) and lowest
   drawdown (−11.2%) in the family.

2. **Canary universes are fragile.** DAA's separate canary was abandoned
   immediately in walk-forward when the EEM pool was used. Even when SPHB
   worked, it added complexity without beating VAA's simpler self-breadth.

3. **Smooth breadth beats binary triggers.** PAA's gradual bond blending
   avoids the all-or-nothing whipsaw of VAA's discrete cash fraction, but
   PAA's SMA(12) filter is too slow. The answer is smooth breadth with a
   fast filter.

4. **Simple cash sleeves work.** BAA's 6-asset defensive universe was
   underutilized — SHY, BIL, and IEF did all the work. Three cash assets
   with momentum selection is sufficient.

ABA combines these lessons: BAA's dual-filter architecture with VAA's
self-breadth (no canary), smoothed defensive fraction, and a simple cash
sleeve.

## Strategy Description

### Universe

| Sleeve | Assets | Purpose |
|---|---|---|
| Offensive | SPY, IWM, EFA, EEM, VNQ, DBC, GLD, TLT, HYG, LQD | Return generation via momentum rotation |
| Defensive | BIL, IEF, SHY | Capital preservation during risk-off periods |

No canary universe. The breadth signal comes from the offensive universe itself.

### Dual Momentum Filter

ABA uses two different momentum filters on the same universe for two different
purposes — the key innovation over prior strategies.

**Offensive filter — SMA(L):** A slow, smoothed SMA lookback ranks assets for
the return-seeking sleeve. This is borrowed from PAA and BAA. The SMA smooths
out short-term noise, producing stable top-N selections with low turnover.
Lookback L is walk-forward selected from [6, 12] months.

**Defensive filter — 13612W:** A fast, recency-weighted blend of 1/3/6/12-month
returns measures breadth for crash protection. This is borrowed from VAA and
DAA. The 13612W formula is:

```
13612W = (12·r₁ + 4·r₃ + 2·r₆ + 1·r₁₂) / 4
```

where rₖ = (P₀ / P₋ₖ) − 1. Positive = above the weighted trend, negative = below.

The asymmetry is intentional: slow selection reduces turnover in normal markets;
fast protection responds quickly to deterioration.

### Breadth-Based Defensive Fraction

At each month-end, compute 13612W momentum for every asset in the offensive
universe. Convert each score to a continuous signal via sigmoid smoothing:

```
score(i) = sigmoid(13612W(i) / σ)
breadth   = mean(score)           ∈ [0, 1]
```

where σ controls the steepness of the transition. A momentum value of zero
(neutral) maps to breadth = 0.5. Strongly positive momentum maps to breadth
near 1.0. Strongly negative maps to breadth near 0.0.

The defensive fraction is then:

```
DF = 1 − breadth^B
```

where B is the breadth-threshold parameter (walk-forward selected from [1, 2, 3]).

- B=1: linear de-risking — 50% breadth → 50% defensive
- B=2: moderate — 50% breadth → 25% defensive (stays invested longer)
- B=3: aggressive — 50% breadth → 12.5% defensive (only de-risks in severe stress)

Unlike VAA's discrete CF = b/B (integer bad-asset count), ABA's DF varies
continuously with the *magnitude* of deterioration, not just the sign. A
breadth score of −0.001 (barely negative) triggers a different response than
−0.20 (deeply negative) — something the original VAA/DAA binary "bad" count
cannot distinguish.

Hysteresis is applied to the defensive fraction to prevent whipsaw:

```
DF_effective = DF_prev + clamp(DF_new − DF_prev, ±0.25)
```

The defensive fraction cannot change by more than 25 percentage points in a
single month.

### Portfolio Construction

1. Rank all offensive assets by SMA(L) momentum (log(price / SMA)).
2. Select the top TopN assets (walk-forward selected from [3, 5]).
3. Compute the defensive fraction DF via 13612W breadth.
4. Select the best defensive asset from {BIL, IEF, SHY} by 13612W momentum
   (no absolute filter — always pick the best, even if all are negative).
5. Allocate:
   - Each offensive asset: (1 − DF) / TopN
   - Defensive asset: DF

Rebalance monthly. Signal from prior month-end, trade at next available price.

### Transaction Costs

Tiered per the backtest methodology:
- 5 bps one-way: SPY, IWM, EFA, EEM, GLD, TLT, IEF, SHY, BIL
- 15 bps one-way: HYG, LQD, VNQ, DBC

### Walk-Forward Parameter Selection

Each year, the parameter set (L, TopN, B) is re-selected using only data
strictly prior to the test year. The pre-registered grid is small:

| Parameter | Values | Meaning |
|---|---|---|
| L | [6, 12] | SMA lookback months |
| TopN | [3, 5] | Number of offensive assets |
| B | [1, 2, 3] | Breadth-threshold exponent |

Total: 2 × 2 × 3 = 12 combinations evaluated per year.

The selection criterion is RAD (Returns Adjusted for Drawdowns):
```
RAD = R · (1 − D/(1−D))   if R ≥ 0 and D ≤ 50%
      0                    otherwise
```
where R is annualized return and D is maximum drawdown on the training data.

### Why This Should Beat the Family

| Weakness in Prior Strategy | How ABA Fixes It |
|---|---|
| VAA's 86% cash drag | Smoother breadth → less time fully in cash |
| DAA's fragile canary | Self-breadth from risky universe |
| PAA's slow SMA crash response | 13612W for defense, SMA only for offense |
| BAA's underutilized defensive sleeve | Three cash assets, momentum-selected |
| VAA/DAA binary "bad" count | Continuous sigmoid breadth |
| All: no hysteresis → whipsaw | ±25% monthly DF change cap |

### Caveats

- Real-ETF history is limited (~2017–2026 common window). The strategy has
  not been tested through a 2008-style crash with real ETF data.
- The sigmoid steepness parameter σ is fixed at 0.05 (not grid-searched).
  Sensitivity to this choice should be tested.
- The hysteresis cap of 0.25 is fixed. Larger caps may allow faster response
  in genuine crashes; smaller caps may reduce false exits.
- Like all breadth-momentum strategies, ABA underperforms buy-and-hold in
  strong bull markets by design. It is a risk-management tool, not a
  return-maximization tool.

# A More Robust Canary-Based Defensive Allocation Strategy
### Design notes + pseudocode addressing weaknesses in Keller & Keuning's DAA (2018)

This document proposes a revised backtest design — call it **DAA-R ("Robust")** — that keeps the useful
core idea of DAA (a separate breadth-momentum "canary" universe for crash signaling, decoupled from the
return-seeking universe) while directly targeting the methodological weaknesses identified in the original
paper.

---

## 1. Mapping fixes to the original paper's problems

| # | Problem in original DAA paper | Fix in DAA-R |
|---|---|---|
| 1 | Canary universe (VWO/BND) chosen from only 4 candidate assets over one 44-year window | Select canary assets via **walk-forward, expanding-window re-selection** each year, from a broader macro-proxy candidate set, not a single fixed historical pick |
| 2 | Same out-of-sample period reused for many rounds of parameter tuning (T, B, cash universe, leverage) | **Nested walk-forward validation**: parameters are only ever fit on data strictly prior to the period they're applied to; no parameter is chosen by looking at the full OS record |
| 3 | Heavy use of non-tradable index proxies pre-1970/2000s | Backtest **only over the period real, currently-tradable ETFs actually existed** (with a documented inception-date table); no synthetic proxy history |
| 4 | No economic rationale for canary choice | Candidate canary assets are restricted to instruments with a **stated macro transmission mechanism** (credit spreads, term structure, currency stress), not chosen purely by backtested fit |
| 5 | DAA underperforms on the small G4 universe; fix (G6) applied post hoc | Test canary logic **simultaneously across all universes with one shared, walk-forward-selected canary set** — no per-universe patching after seeing results |
| 6 | Many nested free parameters (T, B, C3, leverage) tuned per case | **Small pre-registered parameter grid**, fixed before backtesting, plus multiple-testing correction (deflated Sharpe ratio) on the final result |
| 7 | Leveraged-ETF section uses simulated proxies, ignores regime-dependent borrowing costs | Leverage tested **only** over the actual trading history of real leveraged ETFs, with explicit disclosure of realistic borrowing-cost drag |
| 8 | Flat 0.1% cost assumption | **Tiered transaction cost model**: bid-ask + slippage estimated per instrument from realized liquidity, higher for less liquid ETFs |
| 9 | No forward/live validation beyond paper's end date | Explicit **train / validate / holdout** split, with the holdout never touched until the strategy is fully specified |
| 10 | Universe construction inherited without fresh justification | **Explicit, documented universe construction rules** (liquidity, AUM, inception date thresholds) |

---

## 2. Universe definitions (using real, currently tradable ETFs)

**Risky universe (R) — example "Global 10":**
SPY (US equity), IWM (US small cap), EFA (developed ex-US), EEM (emerging equity),
VNQ (US REITs), DBC (commodities), GLD (gold), TLT (long treasuries),
HYG (high yield credit), LQD (investment grade credit)

**Cash universe (C):**
BIL (T-bills), IEF (7-10y treasuries), SHY (1-3y treasuries)

**Canary candidate pool (P-candidates) — chosen for a *stated* macro transmission channel, not backtest fit:**
- HYG (credit-spread stress channel)
- EEM (EM / risk-appetite channel)
- TLT or IEF (rate-shock channel)
- UUP (USD strength channel, currency stress)
- VIXY or a volatility-proxy substitute *(optional, if liquidity acceptable)*

Only assets with **≥ 10 years of live trading history** as of the backtest start are eligible, and only the
period *after* each asset's inception is used — no pre-inception proxy substitution.

---

## 3. Walk-forward design (replacing the single fixed IS/OS split)

Instead of "fit once on 1926–1970, apply forever after," DAA-R re-selects everything on a rolling basis:

```
for each year Y in backtest_range (starting after a minimum 5-year warm-up):
    train_window = all data from start_date .. (Y - 1)        # strictly prior data only
    test_window  = data in year Y                              # never touched during selection

    canary_set   = select_canary(train_window, candidate_pool)
    params       = select_params(train_window, canary_set)      # T, B, cash universe

    run_portfolio(test_window, canary_set, params)               # apply out-of-sample, once
```

This guarantees no parameter or asset choice is ever selected using data from the period it is scored on —
closing the "reused OS window" and "post-hoc patching" problems.

---

## 4. Pseudocode

### 4.1 Data preparation

```
FUNCTION load_data(tickers, start_date, end_date):
    prices = {}
    FOR ticker IN tickers:
        inception = get_inception_date(ticker)
        effective_start = max(start_date, inception)
        prices[ticker] = fetch_monthly_total_return(ticker, effective_start, end_date)
    RETURN prices   # dict of ticker -> monthly TR series, no synthetic pre-inception fill
```

### 4.2 Momentum filter (kept from original paper — a legitimately useful, well-tested component)

```
FUNCTION momentum_13612W(price_series, t):
    r1  = price_series[t] / price_series[t-1]   - 1
    r3  = price_series[t] / price_series[t-3]   - 1
    r6  = price_series[t] / price_series[t-6]   - 1
    r12 = price_series[t] / price_series[t-12]  - 1
    RETURN (12*r1 + 4*r3 + 2*r6 + 1*r12) / 4
```

### 4.3 Canary selection (walk-forward, from a macro-justified candidate pool, with multi-asset ensembling)

```
FUNCTION select_canary(train_data, candidate_pool, max_size=2):
    best_score = -infinity
    best_set   = None

    FOR each combination C of size 1..max_size FROM candidate_pool:
        score = evaluate_canary_candidate(C, train_data)
        # score = risk-adjusted metric (e.g. K25 or Calmar) of a SIMPLE
        # reference portfolio (e.g. equal-weight risky universe) gated by
        # candidate C's breadth signal, computed ONLY on train_data
        IF score > best_score:
            best_score = score
            best_set   = C

    RETURN best_set
```

Key difference from the original paper: this runs **every year on an expanding window**, so the canary
set can (and should) be checked for stability. If the selection flips wildly year to year, that itself is
useful diagnostic evidence that the "canary" signal is unstable / overfit — something the original paper
never tested for, since it picked once and never revisited.

### 4.4 Parameter selection (small, pre-registered grid — not per-universe free tuning)

```
GRID = {
    T: [MIN(3, N_risky), CEIL(N_risky/2)],   # only 2 candidate top-T values, pre-registered
    B: [1, 2]                                 # only two breadth settings, pre-registered
}

FUNCTION select_params(train_data, canary_set):
    best_score = -infinity
    FOR T IN GRID.T:
        FOR B IN GRID.B:
            score = backtest(train_data, canary_set, T, B)   # in-sample only
            IF score > best_score:
                best_score = score
                best_params = (T, B)
    RETURN best_params
```

### 4.5 Portfolio construction (per month)

```
FUNCTION allocate(month, risky_universe, cash_universe, canary_set, T, B):
    bad_count = COUNT(asset IN canary_set WHERE momentum_13612W(asset, month) <= 0)
    cash_frac = MIN(bad_count / B, 1.0)

    top_risky = TOP_T(risky_universe, key=momentum_13612W, T=T)
    best_cash = ARGMAX(cash_universe, key=momentum_13612W)

    weights = {}
    FOR asset IN top_risky:
        weights[asset] = (1 - cash_frac) / T
    weights[best_cash] = cash_frac

    RETURN weights
```

### 4.6 Realistic transaction costs

```
FUNCTION apply_costs(old_weights, new_weights, liquidity_tier_costs):
    turnover = SUM(ABS(new_weights[a] - old_weights.get(a, 0)) for a in new_weights ∪ old_weights)
    cost = 0
    FOR asset IN (new_weights ∪ old_weights):
        delta = ABS(new_weights.get(asset,0) - old_weights.get(asset,0))
        cost += delta * liquidity_tier_costs[asset]   # e.g. 0.03% for SPY, 0.15% for thinner ETFs
    RETURN cost
```

### 4.7 Full walk-forward backtest loop

```
portfolio_value = 1.0
old_weights = {}
results = []

FOR year Y in test_years:
    train_data = data[start_date : Y-1]
    canary_set = select_canary(train_data, candidate_pool)
    (T, B)     = select_params(train_data, canary_set)

    FOR month IN year Y:
        new_weights = allocate(month, risky_universe, cash_universe, canary_set, T, B)
        cost        = apply_costs(old_weights, new_weights, liquidity_tier_costs)
        monthly_ret = PORTFOLIO_RETURN(new_weights, month) - cost
        portfolio_value *= (1 + monthly_ret)
        results.append((month, portfolio_value, canary_set, T, B, cost))
        old_weights = new_weights

RETURN results
```

### 4.8 Statistical robustness check (correcting for multiple testing / overfitting risk)

```
FUNCTION deflated_sharpe_ratio(observed_sharpe, n_trials, n_obs, skew, kurtosis):
    # Bailey & López de Prado (2014) style correction:
    # penalizes the Sharpe ratio for the number of parameter/asset combinations
    # implicitly searched over (n_trials = size of candidate_pool combos x GRID size)
    expected_max_sharpe = estimate_expected_max_under_noise(n_trials, n_obs)
    RETURN probability_true_sharpe_exceeds_zero(observed_sharpe, expected_max_sharpe, skew, kurtosis, n_obs)
```

This step directly targets the original paper's lack of any correction for having searched over many
canary/parameter combinations across many universes.

### 4.9 Final validation protocol

```
1. Fit everything (canary pool, grid) using only walk-forward data up to a chosen holdout date (e.g., 3 years before present).
2. Freeze the resulting rule set completely.
3. Run the frozen rule set, untouched, on the final holdout period.
4. Report holdout performance as the primary result — not the in-sample walk-forward average.
5. Report the deflated Sharpe ratio alongside raw performance.
```

---

## 5. What this buys you (and what it doesn't)

**Addressed:**
- No synthetic/proxy price history — only real ETF returns.
- No single-shot in-sample canary pick reused across a 45-year OS window.
- No post-hoc universe patching (e.g., quietly swapping G4 for G6 after seeing bad results).
- Multiple-testing exposure is explicitly measured (deflated Sharpe) rather than ignored.
- A true untouched holdout period exists for a final honest read of performance.
- Transaction costs vary by instrument liquidity instead of one flat assumption.

**Still not solved (be upfront about this):**
- Even a walk-forward selection process can be *unstable* — if the canary set changes every year, that's a
  sign the signal isn't very robust, and this design will reveal that rather than hide it, but it won't
  automatically fix an unstable strategy.
- Real ETFs still only provide ~20–25 years of live history for many of the more interesting instruments
  (EEM, VNQ, DBC, HYG), which limits statistical power regardless of methodology.
- An economic rationale for *why* a channel like "credit spread stress" should predict crashes is more
  defensible than a purely empirical VWO/BND pick, but it is still a hypothesis, not a proof — the strategy
  should be monitored, not treated as guaranteed to keep working.

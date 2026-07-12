# ABA — Adaptive Breadth Allocation: Backtest Plan

## 0. Universe (Real ETFs Only, No Proxies)

```
OFFENSIVE_UNIVERSE = {
    "SPY":  "US Large Cap",       # inception 1993
    "IWM":  "US Small Cap",       # inception 2000
    "EFA":  "Developed ex-US",    # inception 2001
    "EEM":  "Emerging Markets",   # inception 2003
    "VNQ":  "US REIT",            # inception 2004
    "DBC":  "Commodities",        # inception 2006
    "GLD":  "Gold",               # inception 2004
    "TLT":  "Long Treasuries",    # inception 2002
    "HYG":  "High Yield Bonds",   # inception 2007
    "LQD":  "Investment Grade",   # inception 2002
}

DEFENSIVE_UNIVERSE = {
    "BIL":  "1-3mo T-Bill",       # inception 2007
    "IEF":  "7-10y Treasury",     # inception 2002
    "SHY":  "1-3y Treasury",      # inception 2002
}

ALL_TICKERS = OFFENSIVE_UNIVERSE | DEFENSIVE_UNIVERSE
DATA_START  = max(inception_date(t) for t in ALL_TICKERS)   # honest common start
DATA_END    = today()

FUNCTION load_price_history(tickers, start, end):
    FOR ticker IN tickers:
        raw = fetch_daily_adjusted_close(ticker, start, end)
        validate_no_gaps(raw)
        store(ticker, raw)
    RETURN price_panel
```

**Data source:** `bhav_eq_td` on StockVizUs2 (SQL Server). Adjusted close
column `C`. Daily frequency, aggregated to monthly end-of-month.

## 1. Monthly Price and Return Preparation

```
FUNCTION prepare_monthly_data(price_panel):
    monthly_prices = {}
    FOR ticker IN price_panel:
        monthly_prices[ticker] = resample_to_month_end(price_panel[ticker])

    merged_prices = align_on_common_dates(monthly_prices)
    merged_returns = merged_prices / lag(merged_prices, 1) - 1
    merged_returns = drop_first_row(merged_returns)    # no prior price for return

    RETURN merged_prices, merged_returns
```

## 2. SMA Momentum (Offensive Filter)

```
FUNCTION compute_sma_momentum(prices, L):
    # L = lookback months, walk-forward selected from [6, 12]
    FOR ticker IN prices.columns:
        sma = SMA(prices[ticker], L)
        sma[is_na(sma)] = prices[ticker][is_na(sma)]   # fill early NA with price
        momentum[ticker] = log(prices[ticker] / sma)    # positive = above trend
    RETURN momentum
```

The SMA momentum score is log(price / SMA). Positive means the asset is
trading above its L-month moving average — a simple, stable relative-strength
measure with low turnover.

## 3. 13612W Momentum (Defensive/Breadth Filter)

```
FUNCTION compute_13612w(prices):
    # Requires 12 months of trailing history
    FOR t IN range(12, len(prices)):
        p0  = prices[t]
        p1  = prices[t-1]
        p3  = prices[t-3]
        p6  = prices[t-6]
        p12 = prices[t-12]

        r1  = p0/p1  - 1
        r3  = p0/p3  - 1
        r6  = p0/p6  - 1
        r12 = p0/p12 - 1

        momentum[t] = (12*r1 + 4*r3 + 2*r6 + 1*r12) / 4
    RETURN momentum
```

The 13612W filter uses only the offensive universe — no separate canary. The
filter responds quickly because 40% of the weight is on the most recent month.

## 4. Smoothed Breadth and Defensive Fraction

```
SIGMA = 0.05   # sigmoid steepness (fixed, not grid-searched)

FUNCTION compute_defensive_fraction(momentum_13612w, B):
    # momentum_13612w: vector of 13612W scores for all offensive assets
    FOR i IN range(len(momentum_13612w)):
        score[i] = sigmoid(momentum_13612w[i] / SIGMA)
                  = 1 / (1 + exp(-momentum_13612w[i] / SIGMA))

    breadth = mean(score)           # continuous, ∈ [0, 1]
    df_new  = 1 - breadth^B         # smoothed defensive fraction

    RETURN df_new
```

Key difference from VAA/DAA: `breadth` is the *mean sigmoid score*, not an
integer count of "bad" assets. This means:
- An asset with 13612W = −0.001 contributes almost the same as one at +0.001
  (both near 0.5 after sigmoid with σ=0.05).
- An asset with 13612W = −0.20 contributes near 0.02 — deeply bearish.
- The breadth score degrades *gradually* as assets weaken, not in discrete
  steps of 1/N.

Key difference from PAA: the breadth input is 13612W (fast), not SMA (slow).
This means the defensive fraction responds in weeks, not months.

## 5. Hysteresis

```
MAX_MONTHLY_DF_CHANGE = 0.25

FUNCTION apply_hysteresis(df_new, df_prev):
    delta = df_new - df_prev
    delta = clamp(delta, -MAX_MONTHLY_DF_CHANGE, MAX_MONTHLY_DF_CHANGE)
    RETURN df_prev + delta
```

Prevents the defensive fraction from jumping more than 25 percentage points
in a single month. A full rotation from 0% to 100% defensive takes at least
4 months — enough to filter out single-month whipsaws while still responding
to genuine multi-month deterioration.

## 6. Defensive Asset Selection

```
FUNCTION select_defensive_asset(momentum_13612w):
    # momentum_13612w: vector of 13612W scores for {BIL, IEF, SHY}
    best = argmax(momentum_13612w)    # no absolute filter — always pick best
    RETURN best
```

Three cash-like ETFs, single best selected. No absolute momentum filter —
during genuine crashes when all three may be negative, we still want to be
in the least-bad cash asset.

## 7. Portfolio Construction (Per Month)

```
FUNCTION allocate(month, prices, returns_xts, L, TopN, B):
    # Offensive selection: SMA(L) momentum
    sma_mom = compute_sma_momentum(prices, L)
    ranked  = sort_descending(sma_mom)
    top_off = ranked[0 : TopN]

    # Defensive fraction: 13612W breadth
    mom_13612w = compute_13612w(prices)
    df_new     = compute_defensive_fraction(mom_13612w, B)
    df         = apply_hysteresis(df_new, prev_df)

    # Defensive asset
    best_def = select_defensive_asset(mom_13612w[DEFENSIVE_UNIVERSE])

    # Weights
    weights = {}
    FOR asset IN top_off:
        weights[asset] = (1 - df) / TopN
    weights[best_def] = df

    # Apply tiered transaction costs
    turnover = sum(abs(weights[a] - prev_weights.get(a, 0)) for a in all_assets)
    cost = sum(abs(weights[a] - prev_weights.get(a,0)) * COST_MAP[a] for a in all_assets)

    prev_weights = weights
    prev_df      = df

    RETURN weights, cost
```

## 8. Walk-Forward Backtest Loop

```
WALK_FORWARD_STEP_MONTHS = 12    # re-optimize annually
MIN_TRAIN_WINDOW_MONTHS  = 60    # 5-year warm-up

PARAMETER_GRID = {
    "L":    [6, 12],       # SMA lookback
    "TopN": [3, 5],        # offensive assets
    "B":    [1, 2, 3],     # breadth exponent
}
# Total: 12 combinations

FUNCTION select_best_params(train_prices, train_returns, grid):
    best_score = -infinity
    best_params = None

    FOR L IN grid.L:
        FOR TopN IN grid.TopN:
            FOR B IN grid.B:
                portfolio_returns = simulate(train_returns, train_prices, L, TopN, B)
                portfolio_returns = drop_na(portfolio_returns)

                IF len(portfolio_returns) < 36: CONTINUE

                R = annualized_return(portfolio_returns)
                D = max_drawdown(portfolio_returns)
                score = RAD(R, D)   # R * (1 - D/(1-D)) if R>=0 and D<=0.50

                IF score > best_score:
                    best_score = score
                    best_params = (L, TopN, B)

    RETURN best_params

FUNCTION walk_forward_backtest(price_panel, returns_panel, grid):
    oos_results = []
    all_dates = returns_panel.index
    first_date = all_dates[0]

    warmup_end_year = year(first_date) + MIN_TRAIN_WINDOW_MONTHS // 12
    test_years = unique_years_in_range(warmup_end_year, year(last(all_dates)))

    FOR year IN test_years:
        train_end = date(year - 1, 12, 31)
        test_dates = all_dates[year(all_dates) == year]

        train_prices  = price_panel[ : train_end]
        train_returns = returns_panel[ : train_end]

        IF len(train_returns) < MIN_TRAIN_WINDOW_MONTHS: CONTINUE

        # Select parameters on training data only
        (L, TopN, B) = select_best_params(train_prices, train_returns, grid)

        # Run on test year (out-of-sample)
        test_prices  = price_panel[test_dates]
        test_returns = returns_panel[test_dates]

        FOR month IN test_dates:
            weights, cost = allocate(month, test_prices, test_returns, L, TopN, B)
            monthly_return = portfolio_return(weights, month) - cost
            oos_results.append((month, monthly_return, L, TopN, B, weights))

    RETURN oos_results
```

## 9. Benchmarks (Computed on Same Date Range)

```
FUNCTION compute_benchmarks(returns_panel, common_dates):
    # Equal-weight offensive
    ew_returns = row_means(returns_panel[OFFENSIVE_UNIVERSE])

    # Equal-weight defensive
    ewc_returns = row_means(returns_panel[DEFENSIVE_UNIVERSE])

    # 60/40 SPY/IEF
    sixty_forty = 0.6 * returns_panel["SPY"] + 0.4 * returns_panel["IEF"]

    # SPY buy-and-hold
    spy_returns = returns_panel["SPY"]

    RETURN ew_returns, ewc_returns, sixty_forty, spy_returns
```

## 10. Performance Metrics

```
FUNCTION compute_metrics(returns, ewc_returns):
    R      = annualized_return(returns)
    V      = annualized_volatility(returns)
    D      = max_drawdown(returns)       # positive convention
    Sharpe = (R - annualized_return(ewc_returns)) / V   # excess over bonds
    MAR    = R / D
    RAD    = R * (1 - D/(1-D)) if (R >= 0 and D <= 0.50) else 0

    RETURN {CAGR: R, Vol: V, MaxDD: D, Sharpe: Sharpe, MAR: MAR, RAD: RAD}
```

Report metrics separately for In-Sample, Out-of-Sample, and Full Sample
periods. IS/OS split at the midpoint of the common date range.

## 11. Reporting Output

```
FUNCTION main():
    prices  = load_price_history(ALL_TICKERS, DATA_START, DATA_END)
    monthly_prices, monthly_returns = prepare_monthly_data(prices)

    # Walk-forward backtest
    oos_results = walk_forward_backtest(monthly_prices, monthly_returns, PARAMETER_GRID)

    # Extract returns
    aba_returns = extract_returns(oos_results)

    # Benchmarks on common date range
    ew, ewc, sixty_forty, spy = compute_benchmarks(monthly_returns, aba_returns.index)

    # Align and compute metrics
    all_strats = align_and_merge(aba_returns, ew, ewc, sixty_forty, spy)
    is_end_date = midpoint(all_strats.index)
    metrics_is = compute_metrics(all_strats[ : is_end_date])
    metrics_os = compute_metrics(all_strats[is_end_date : ])
    metrics_fs = compute_metrics(all_strats)

    # Charts
    plot_cumulative_returns(all_strats, title="ABA — Adaptive Breadth Allocation")
    plot_drawdowns(all_strats)
    plot_os_cumulative(all_strats[is_end_date : ])

    # Parameter stability
    report_parameter_history(oos_results)   # how often each (L,TopN,B) was selected
    plot_parameter_heatmap(PARAMETER_GRID, oos_results)

    # Defensive fraction over time
    plot_defensive_fraction(oos_results)

    # Report
    print_metrics_table(metrics_is, metrics_os, metrics_fs)

    # Save
    save(all_strats, oos_results, metrics, PARAMETER_GRID)
```

## 12. Implementation Notes

- **Language:** R, following existing project conventions (xts, PerformanceAnalytics, gt).
- **Database:** `bhav_eq_td` on StockVizUs2, column `C` for adjusted close.
- **Date handling:** All dates in ISO 8601. Month-end aggregation via `to.monthly()`.
- **No look-ahead:** Momentum signals use prior month-end data. Hysteresis uses
  prior month's DF. Parameter selection uses only training data through year-1.
- **Transaction costs:** Tiered per the COST_MAP. Applied as a drag on monthly
  portfolio return proportional to weight changes.
- **Minimum training:** 60 months (5 years) before first test year.
- **Walk-forward step:** Annual. Parameters frozen for the entire test year.
- **Charts:** Cumulative returns (full + OS), drawdowns, gt metrics table with
  IS/OS/FS group rows, conditional formatting per project conventions.

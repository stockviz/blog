# A Robustness-First Backtest Plan for Protective/Breadth-Momentum Strategies

This plan re-tests the PAA/VAA/DAA family of breadth-momentum strategies using **real, publicly-tradeable ETFs** (no synthetic proxies) and a methodology designed to directly close the gaps identified in the original paper: in-sample overfitting, too few independent stress episodes, circular selection criteria, unrealistic costs, single-bond dependency, and discrete allocation jumps.

Each section states **which fault it fixes**, then gives pseudocode.

---

## 0. Universe (real ETFs only, no proxies)

**Fault fixed:** #4 (synthetic proxy data pre-dates actual ETFs and carries construction/model risk).

Use only ETFs with genuine daily price history — accept the shorter live history as the honest cost of using real data, rather than splicing in synthetic pre-inception prices.

```
RISKY_UNIVERSE = {
    "SPY":  "US Large Cap",      # inception 1993
    "IWM":  "US Small Cap",      # inception 2000
    "EFA":  "Developed ex-US",   # inception 2001
    "EEM":  "Emerging Markets",  # inception 2003
    "VNQ":  "US REIT",           # inception 2004
    "DBC" or "GSG": "Commodities", # inception 2006/2007
    "GLD":  "Gold",              # inception 2004
    "HYG":  "High Yield Bonds",  # inception 2007
    "LQD":  "Investment Grade Corp", # inception 2002
    "TLT":  "Long Treasuries",   # inception 2002
}

CANARY_UNIVERSE = { "EEM": "Emerging Markets", "AGG": "Aggregate Bonds" }  # per DAA design

BOND_CANDIDATES = { "IEF": "7-10y Treasury", "SHY": "1-3y Treasury",
                     "BIL": "1-3mo T-Bill",   "AGG": "Aggregate Bond" }

DATA_START = max(inception_date(ticker) for ticker in RISKY_UNIVERSE)  # honest common start
DATA_END   = today()
```

**Data source plan:** pull adjusted (dividend- and split-adjusted) daily closes directly from a vetted market-data API (e.g., a paid provider with point-in-time data), not Yahoo-scraped monthly closes, to avoid restatement/adjustment drift. Log the retrieval date and data vendor for reproducibility.

```
FUNCTION load_price_history(tickers, start, end):
    for ticker in tickers:
        raw = data_vendor_api.get_daily_adjusted(ticker, start, end)
        validate_no_gaps(raw)            # flag missing days, holidays handled explicitly
        validate_adjustment_factors(raw) # confirm split/div adjustment matches vendor docs
        store(ticker, raw)
    return price_panel
```

---

## 1. Avoid in-sample/out-of-sample circularity

**Fault fixed:** #1 and #3 (parameters chosen on IS period using the same Win0/Win5 bar used to declare success; single static IS/OS split).

Replace the single 1970–1992 vs. 1993–2015 split with a **walk-forward (rolling or expanding window) optimization**, so every out-of-sample month is genuinely unseen at the time its parameters were chosen.

```
PARAMETER_GRID = {
    "L":    [3, 6, 9, 12],       # lookback months
    "a":    [0, 1, 2],           # protection factor
    "Top":  [1,2,3,4,5,6],       # rotation size
    "filter_type": ["SMA", "13612W"]   # test both slow and fast filters
}

WALK_FORWARD_STEP_MONTHS = 12          # re-optimize once a year
MIN_TRAIN_WINDOW_MONTHS  = 60          # require minimum history before first OS test

FUNCTION walk_forward_backtest(price_panel, grid):
    oos_returns = []
    t = MIN_TRAIN_WINDOW_MONTHS
    while t < len(price_panel.months) - 1:
        train = price_panel.months[0 : t]              # expanding window (or t-window for rolling)
        test_month = price_panel.months[t]

        best_params = select_best_params(train, grid)   # selection criterion below (not circular)
        oos_return  = simulate_one_month(test_month, best_params)

        oos_returns.append(oos_return)
        t += 1   # step forward one month; re-optimize every WALK_FORWARD_STEP_MONTHS
    return oos_returns
```

Selection criterion during training must NOT be the same absolute-return bar used to report success. Use a **separate, symmetric, pre-registered utility function** (e.g., CVaR-adjusted Sharpe) fixed *before* running any tests:

```
FUNCTION select_best_params(train_window, grid):
    scores = {}
    for params in grid.all_combinations():
        sim = simulate(train_window, params)
        # Pre-registered, not chosen post-hoc:
        scores[params] = sharpe_ratio(sim) - LAMBDA * conditional_value_at_risk(sim, level=0.05)
    return argmax(scores)
```

Report Win0/Win5/drawdown *after the fact*, as descriptive statistics of the OOS walk-forward result — never as the criterion that picked the winning parameters.

---

## 2. Correct for data-snooping statistically

**Fault fixed:** #1, #3 (72-scenario search inflates Sharpe ratios; the paper applies an ad hoc haircut).

Use a formal multiple-testing correction rather than an assumed haircut.

```
FUNCTION deflated_sharpe_ratio(sharpe_observed, n_trials, skew, kurtosis, track_record_length):
    # Bailey & Lopez de Prado (2014) style correction
    expected_max_sharpe_under_null = estimate_expected_max_sharpe(n_trials, skew, kurtosis)
    dsr = probability_that(sharpe_observed > expected_max_sharpe_under_null)
    return dsr

# After grid search:
n_trials = len(PARAMETER_GRID.all_combinations())
dsr = deflated_sharpe_ratio(best_sharpe, n_trials, skew(returns), kurtosis(returns), track_record_length)
ASSERT dsr > 0.95   # require statistical significance net of search-space size, not an assumed 25% haircut
```

Also run **White's Reality Check / Hansen's SPA test** by block-bootstrapping the return series and comparing the best-of-grid strategy against the bootstrap distribution of the best-of-grid from randomly permuted/resampled histories:

```
FUNCTION reality_check(price_panel, grid, n_bootstrap=5000):
    observed_best_sharpe = best_sharpe_from_grid(price_panel, grid)
    bootstrap_best_sharpes = []
    for i in range(n_bootstrap):
        resampled = stationary_block_bootstrap(price_panel, block_length=12)  # preserve autocorrelation
        bootstrap_best_sharpes.append(best_sharpe_from_grid(resampled, grid))
    p_value = fraction(bootstrap_best_sharpes >= observed_best_sharpe)
    return p_value
```

---

## 3. Increase the number of independent stress episodes

**Fault fixed:** #2 (only a handful of real crash episodes in 45 years of monthly data; conclusions rest on few independent events).

### 3a. Block bootstrap / Monte Carlo resampling of regimes
```
FUNCTION regime_stress_test(price_panel, n_paths=10000, block_length_months=6):
    synthetic_paths = []
    for i in range(n_paths):
        path = stationary_block_bootstrap(price_panel, block_length_months)
        synthetic_paths.append(path)

    results = [simulate(strategy, path) for path in synthetic_paths]
    report_distribution(results.Win0, results.Win5, results.max_drawdown, results.CAGR)
```
This generates thousands of pseudo-independent "alternate histories" built from real historical blocks, giving a distribution of Win0/Win5/drawdown instead of one lucky/unlucky historical path.

### 3b. Explicit named-crisis holdout tests (no re-optimization allowed inside these windows)
```
CRISIS_WINDOWS = [
    ("2000-03", "2002-10"),   # dot-com bust
    ("2007-10", "2009-03"),   # GFC
    ("2011-05", "2011-10"),   # Euro debt crisis / US downgrade
    ("2015-08", "2016-02"),   # China slowdown / oil crash
    ("2018-10", "2018-12"),   # Q4 2018 selloff
    ("2020-02", "2020-03"),   # COVID crash
    ("2022-01", "2022-10"),   # 2022 stock+bond simultaneous selloff  <-- critical addition
]

FOR window IN CRISIS_WINDOWS:
    freeze_parameters_as_of(window.start)   # use only params known BEFORE the crisis began
    result = simulate(strategy, window)
    report(window, result.return, result.max_drawdown, result.bond_fraction_path)
```

The 2022 window is deliberately included because it is the clearest real-world test of Fault #6 (bonds and stocks selling off together) — something the original paper's 1970–2015 sample under-represents.

---

## 4. Remove circularity in the absolute-return criterion

**Fault fixed:** #3 (Win0/Win5 used both to pick the winning model and to prove the model works).

```
# Pre-register thresholds and hold them fixed BEFORE seeing any results:
PRE_REGISTERED_WIN0_TARGET = 0.95
PRE_REGISTERED_WIN5_TARGET = 0.99

# Compute only on true out-of-sample / walk-forward output, never on training data:
win0 = fraction(oos_rolling_12m_returns >= 0.0)
win5 = fraction(oos_rolling_12m_returns >= -0.05)

REPORT(win0, win5)   # descriptive only — do not re-tune parameters to hit these numbers
```

---

## 5. Realistic, asset-specific transaction costs and liquidity constraints

**Fault fixed:** #5 (flat 0.1% one-way fee assumption regardless of era or asset liquidity).

```
FUNCTION realistic_cost_model(ticker, trade_date, trade_notional):
    bid_ask_spread   = lookup_historical_spread(ticker, trade_date)      # from vendor quote data
    market_impact    = estimate_impact(trade_notional, avg_daily_volume(ticker, trade_date))
    commission       = broker_commission_schedule(trade_date)             # varies pre/post ~2019 zero-commission era
    total_cost_bps   = bid_ask_spread/2 + market_impact + commission
    return total_cost_bps

FUNCTION simulate_month_with_costs(portfolio_before, portfolio_after, trade_date):
    total_cost = 0
    for ticker in changed_positions(portfolio_before, portfolio_after):
        notional = abs(portfolio_after[ticker] - portfolio_before[ticker])
        cost_bps = realistic_cost_model(ticker, trade_date, notional)
        total_cost += notional * cost_bps / 10000
    return total_cost

# Sensitivity band instead of one flat number:
FOR cost_scenario IN ["optimistic_5bps", "base_case_vendor_estimated", "pessimistic_2x_spread"]:
    run_full_backtest(cost_scenario)
    report_CAGR_and_drawdown_under(cost_scenario)
```

Report performance **under a range** of cost assumptions rather than one static 0.1% figure, and separately report **portfolio turnover** so the cost sensitivity is transparent.

---

## 6. Test robustness to the "safe bond" choice and stock/bond correlation regime

**Fault fixed:** #6 (protective benefit depends on IEF's historical negative correlation with equities, which is not guaranteed, as 2022 showed).

```
FOR bond_choice IN BOND_CANDIDATES:
    run_backtest(strategy, safe_asset=bond_choice)
    report(bond_choice, CAGR, drawdown, Win0, Win5)

# Explicitly test performance conditioned on rolling stock/bond correlation regime:
FUNCTION correlation_regime_analysis(price_panel, window_months=36):
    rolling_corr = rolling_correlation(SPY_returns, IEF_returns, window_months)
    high_corr_periods = periods_where(rolling_corr > 0.3)     # stocks & bonds moving together
    low_corr_periods  = periods_where(rolling_corr < 0.0)     # normal hedge regime

    report_strategy_performance(strategy, during=high_corr_periods)
    report_strategy_performance(strategy, during=low_corr_periods)
```

Also test a **dynamic bond sleeve** (momentum-selected between IEF/SHY/BIL/TIPS, as DAA-style logic does) versus a **fixed single bond**, to see how much of PAA's edge is attributable to IEF specifically.

---

## 7. Smooth out discrete allocation jumps

**Fault fixed:** #7 (bond fraction and asset count move in coarse steps, causing outsized allocation swings near thresholds).

```
# Original: BF = (N-n)/(N-n1), a step function of integer n
# Replacement: smooth continuous breadth score instead of integer good/bad count

FUNCTION continuous_breadth_score(universe_momentum_values):
    # Instead of counting assets above/below zero, use a smoothed logistic weighting
    # so a momentum value of +0.01% isn't treated identically to +10%
    scores = [sigmoid(mom / SCALE_FACTOR) for mom in universe_momentum_values]
    breadth = mean(scores)     # continuous in [0,1], not a step function of integer n
    return breadth

FUNCTION smoothed_bond_fraction(breadth, protection_factor_a):
    bf = 1 - breadth ** (1 + protection_factor_a)   # smooth analogue of eq. (2) in the original paper
    return clip(bf, 0, 1)

# Additionally apply allocation smoothing/hysteresis to reduce whipsaw:
FUNCTION apply_hysteresis(new_bf, prev_bf, max_monthly_change=0.20):
    delta = new_bf - prev_bf
    if abs(delta) > max_monthly_change:
        new_bf = prev_bf + sign(delta) * max_monthly_change
    return new_bf
```

Compare the smoothed/hysteresis version against the original discrete-step version to quantify how much of the reported "protection" was a real signal versus a threshold artifact.

---

## 8. Parameter stability / sensitivity reporting (not just the single best combo)

**Fault fixed:** #1, #3 (only the single "winning" parameter set is reported; no visibility into how fragile that choice is).

```
FUNCTION parameter_stability_heatmap(grid, price_panel):
    results = {}
    for params in grid.all_combinations():
        results[params] = walk_forward_backtest(price_panel, [params])
    plot_heatmap(results, axes=["L", "Top"], metric="Sharpe", facet_by="a")
    # A robust strategy should show a broad plateau of good performance,
    # not an isolated spike at one specific (L, a, Top) combination.
    report_flat_region_size(results)
```

---

## 9. Full pipeline (end-to-end pseudocode)

```
FUNCTION main():
    prices = load_price_history(RISKY_UNIVERSE + CANARY_UNIVERSE + BOND_CANDIDATES,
                                 DATA_START, DATA_END)

    # 1-2: walk-forward optimization with pre-registered, non-circular selection + DSR check
    oos_results = walk_forward_backtest(prices, PARAMETER_GRID)
    dsr = deflated_sharpe_ratio(oos_results.sharpe, n_trials=len(PARAMETER_GRID.all_combinations()),
                                 skew=skew(oos_results.returns), kurtosis=kurtosis(oos_results.returns),
                                 track_record_length=len(oos_results.returns))
    reality_check_pvalue = reality_check(prices, PARAMETER_GRID)

    # 3: stress test on bootstrapped regimes + named crises incl. 2022
    stress_distribution = regime_stress_test(prices)
    crisis_results = [simulate(strategy, window) for window in CRISIS_WINDOWS]

    # 4: descriptive-only absolute-return scoring
    win0, win5 = score_absolute_return(oos_results)

    # 5: cost sensitivity band
    cost_sensitivity = [run_full_backtest(prices, cost_scenario) for cost_scenario in COST_SCENARIOS]

    # 6: bond-choice and correlation-regime robustness
    bond_robustness = [run_backtest(prices, bond) for bond in BOND_CANDIDATES]
    correlation_breakdown = correlation_regime_analysis(prices)

    # 7: discrete vs smoothed allocation comparison
    smoothed_vs_discrete = compare(discrete_strategy, smoothed_strategy_with_hysteresis)

    # 8: parameter stability heatmap
    stability_map = parameter_stability_heatmap(PARAMETER_GRID, prices)

    compile_final_report(oos_results, dsr, reality_check_pvalue, stress_distribution,
                          crisis_results, win0, win5, cost_sensitivity,
                          bond_robustness, correlation_breakdown,
                          smoothed_vs_discrete, stability_map)
```

---

## 10. Reporting checklist (what the final writeup must include)

- [ ] Walk-forward OOS equity curve (never restate parameters using future data)
- [ ] Deflated Sharpe Ratio and Reality Check p-value, explicitly stated alongside raw Sharpe
- [ ] Distribution (not single point estimate) of Win0/Win5/CAGR/drawdown across bootstrapped regimes
- [ ] Performance broken out separately for each named crisis window, including 2022
- [ ] Turnover and cost-sensitivity band (optimistic / base / pessimistic), not a single flat fee
- [ ] Comparison across all four candidate "safe bond" choices, and performance conditioned on stock/bond correlation regime
- [ ] Discrete-threshold vs. smoothed/hysteresis allocation comparison
- [ ] Parameter stability heatmap showing whether good performance is a broad plateau or an isolated spike
- [ ] Full data provenance (vendor, retrieval date, adjustment methodology) — no synthetic pre-inception proxies

## Caveats of this plan itself

- Real ETF histories are shorter than the original paper's 45-year synthetic sample, so statistical power for tail-risk claims is inherently lower — this is an honest trade-off for authenticity, not a flaw to be masked.
- Block bootstrap and Monte Carlo resampling assume the historical block structure is representative of future regimes; they mitigate but do not eliminate regime-change risk (e.g., a genuinely unprecedented macro environment).
- Transaction cost models still rely on historical spread/volume data and broker fee schedules, which is an approximation of real-world execution, especially for less liquid ETFs in early years.

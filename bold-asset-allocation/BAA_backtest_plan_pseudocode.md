# A Robustness-First Backtest Plan for Bold Asset Allocation (BAA)

This plan re-tests BAA using **real, publicly-tradeable ETFs** and a methodology built to directly close the specific gaps found in the BAA paper: individually-tuned discrete design choices (QQQ swap, DBC addition, filter combo), the B=1 binary-cliff breadth trigger, an untested/never-varied canary universe, a proprietary drawdown-cliff performance metric (K), higher turnover under a flat cost assumption, and an in-sample window hand-picked to match the two regimes the strategy is later praised for handling.

Each section names the fault it fixes, then gives pseudocode. Where the earlier PAA backtest plan already solved a shared fault (proxy-free data, walk-forward optimization, deflated Sharpe, cost sensitivity, crisis stress-testing), that machinery is reused and extended rather than rebuilt from scratch.

---

## 0. Universe (real ETFs, all BAA-relevant asset classes, no proxies)

**Fault fixed:** inherited PAA-style synthetic proxy data risk, plus BAA's larger 17-ETF asset list.

```
OFFENSIVE_CANDIDATES = {
    "SPY":"US Large Cap","QQQ":"US Nasdaq","IWM":"US Small Cap",
    "VGK":"Europe","EWJ":"Japan","VWO":"Emerging Markets","VEA":"Developed ex-US",
    "VNQ":"US REIT","DBC":"Commodities","GLD":"Gold","TLT":"Long Treasuries",
    "HYG":"High Yield","LQD":"Inv Grade Corp"
}

DEFENSIVE_CANDIDATES = {
    "BIL":"1-3mo T-Bill","IEF":"7-10y Treasury","TLT":"20y+ Treasury",
    "LQD":"Inv Grade Corp","TIP":"Inflation-Protected Treasury",
    "BND":"Total Bond Market","DBC":"Commodities"
}

CANARY_CANDIDATE_POOL = {
    "SPY","QQQ","IWM","VGK","EWJ","VWO","VEA","VNQ","BND","TLT","IEF"
}   # any subset can be tested as canary — see Section 5

DATA_START = max(inception_date(t) for t in (OFFENSIVE_CANDIDATES | DEFENSIVE_CANDIDATES | CANARY_CANDIDATE_POOL))
DATA_END   = today()

FUNCTION load_price_history(tickers, start, end):
    for ticker in tickers:
        raw = data_vendor_api.get_daily_adjusted(ticker, start, end)
        validate_no_gaps(raw)
        validate_adjustment_factors(raw)
        store(ticker, raw)
    return price_panel
```

Note: TIP (inception 2003) and DBC (inception 2006) meaningfully shorten the honestly-testable history relative to the paper's 1970-start synthetic series — this is treated as a real constraint on claims about the inflation-hedge defensive sleeve, not something to paper over with proxy data.

---

## 1. Pre-register every discrete design choice — don't cherry-pick after the fact

**Fault fixed:** the paper's own footnotes admit that the QQQ-for-SPY swap, the DBC addition to the defensive sleeve, and the SMA(12)/13612W filter pairing were each individually chosen because they were "in-sample optimal on K/IS" — i.e., several separate post-hoc decisions layered on top of each other.

Treat every such choice as a **grid dimension to search jointly**, not a discretionary edit made after peeking at results.

```
DESIGN_GRID = {
    "offensive_universe":   ["G12_full", "G4_QQQ_variant", "G4_SPY_variant", "SPY_only"],
    "defensive_universe":   ["cash_only_BIL_IEF", "bonds_only_no_DBC_no_TIP",
                              "bonds_plus_TIP", "bonds_plus_DBC", "bonds_plus_TIP_plus_DBC"],
    "offensive_filter_L":   ["13612W","SMA3","SMA6","SMA9","SMA12","RET12"],
    "canary_filter_LP":     ["13612W","SMA3","SMA6","SMA9","SMA12","RET12"],
    "breadth_B":            [1,2,3,4],            # scaled to canary universe size
    "TO_offensive_topN":    [1,2,3,6],
    "TD_defensive_topN":    [1,2,3]
}

FUNCTION select_best_design(train_window, grid):
    # Single pre-registered utility function, fixed BEFORE any results are seen —
    # never the same metric used later to declare the model a success.
    scores = {}
    for combo in grid.all_combinations():
        sim = simulate(train_window, combo)
        scores[combo] = cvar_adjusted_sharpe(sim)   # symmetric, standard, not custom-built
    return argmax(scores)
```

This forces the QQQ/SPY choice, the DBC/TIP inclusion, and the filter pairing to compete against every other combination on equal footing, inside the same walk-forward loop — rather than being individually justified one at a time against the same in-sample number.

---

## 2. Walk-forward optimization (no static two-part in-sample window)

**Fault fixed:** BAA's IS1 (1970–80) / IS2 (2012–22) split was deliberately built from the two regimes the model is later praised for handling — a form of hindsight-driven regime selection baked into the training data itself.

```
WALK_FORWARD_STEP_MONTHS = 12
MIN_TRAIN_WINDOW_MONTHS  = 60

FUNCTION walk_forward_backtest(price_panel, grid):
    oos_results = []
    t = MIN_TRAIN_WINDOW_MONTHS
    while t < len(price_panel.months) - 1:
        train = price_panel.months[0:t]          # expanding window; no cherry-picked regime split
        test_month = price_panel.months[t]
        best_design = select_best_design(train, grid)
        oos_results.append(simulate_one_month(test_month, best_design))
        t += 1
    return oos_results
```

Report performance separately by *decade*, purely descriptively, after the fact — never used to pick the training window in the first place.

```
FUNCTION report_by_decade(oos_results):
    FOR decade IN ["1970s","1980s","1990s","2000s","2010s","2020s"]:
        report(decade, subset_metrics(oos_results, decade))
```

---

## 3. Replace the B=1 binary cliff with a continuous, tunable breadth function

**Fault fixed:** with a 4-asset canary and B=1, a single bad asset flips 100% of the portfolio to defensive — reintroducing the discrete, all-or-nothing threshold PAA's breadth mechanism was meant to avoid.

```
FUNCTION smooth_canary_breadth(canary_momentum_values, steepness=SCALE):
    # Continuous alternative to the raw #bad/B step function
    scores = [sigmoid(mom / steepness) for mom in canary_momentum_values]
    breadth = mean(scores)          # in [0,1]; degrades gracefully instead of flipping on 1 asset
    return breadth

FUNCTION defensive_fraction(breadth, B_equivalent):
    df = clip(1 - breadth ** B_equivalent, 0, 1)
    return df

FUNCTION apply_hysteresis(new_df, prev_df, max_monthly_change=0.25):
    delta = new_df - prev_df
    if abs(delta) > max_monthly_change:
        new_df = prev_df + sign(delta) * max_monthly_change
    return new_df
```

Run the full walk-forward pipeline under **both** the original step-function trigger and this smoothed version, and report how much of BAA's headline drawdown protection survives once the binary cliff is removed. If protection quality collapses without the hard cliff, that is itself informative about how much of the reported benefit is trigger-shape artifact versus genuine signal.

---

## 4. Vary the canary universe itself — it was never varied in the original paper

**Fault fixed:** all four BAA variants (G12, G4, G12/T3, G4/T2) in the paper share the identical 4-asset canary (SPY, VWO, VEA, BND); the "robustness across variants" table never tests whether results are sensitive to that specific canary choice.

```
CANARY_VARIANTS = [
    {"SPY","VWO","VEA","BND"},                 # original
    {"SPY","EEM","EFA","AGG"},                  # near-equivalent substitute tickers
    {"SPY","QQQ","IWM","TLT"},                  # US-centric alternative
    {"SPY","VWO","VEA","VNQ","BND"},             # 5-asset expanded canary
    {"IWM","VWO","VEA","BND"},                   # small-cap-led alternative
]

FOR canary IN CANARY_VARIANTS:
    FOR B IN [1, 2, ceil(len(canary)/2)]:
        result = walk_forward_backtest(price_panel, grid_fixed_except_canary(canary, B))
        report(canary, B, result.CAGR, result.max_drawdown, result.defensive_fraction)
```

A canary/B combination that only works well for the one specific 4-asset set used in the paper — but degrades sharply under near-equivalent substitutes — is evidence the original result was tuned to that particular universe rather than reflecting a general breadth-of-market effect.

---

## 5. Statistical correction for the expanded search space

**Fault fixed:** BAA searches over more discrete design axes than PAA (universe choice, defensive composition, two separate filters, breadth parameter, canary composition) — the multiple-comparisons problem is larger here, not smaller, so the correction needs to be at least as rigorous.

```
n_trials = len(DESIGN_GRID.all_combinations()) * len(CANARY_VARIANTS) * len(B_VALUES_TESTED)

dsr = deflated_sharpe_ratio(best_sharpe_observed, n_trials,
                             skew(oos_returns), kurtosis(oos_returns),
                             track_record_length=len(oos_returns))
ASSERT dsr > 0.95

p_value = reality_check(price_panel, DESIGN_GRID, n_bootstrap=5000)  # stationary block bootstrap, block=12mo
```

---

## 6. Replace/benchmark the proprietary "K" metric

**Fault fixed:** K = R·(1 − 2D/(1−2D)) hard-zeroes at D=25%, creating a cliff that can make two strategies with meaningfully different risk (e.g., 24% vs. 40% drawdown) look identical (both ≈0), while overweighting small differences near the cliff (15% vs. 24%).

```
STANDARD_METRICS = ["CAGR","Sharpe","Sortino","Calmar","UlcerPerformanceIndex","CVaR_5pct","MaxDrawdown"]

FUNCTION compute_standard_metrics(returns):
    return { m: compute(m, returns) for m in STANDARD_METRICS }

# If K is reported at all, always report it ALONGSIDE the standard set, and explicitly
# plot K vs. max_drawdown across the full grid to visualize/disclose the cliff behavior:
FUNCTION plot_K_cliff_sensitivity(grid_results):
    scatter(x=grid_results.max_drawdown, y=grid_results.K)
    annotate("Discontinuity at D=25%")
```

---

## 7. Realistic transaction costs scaled to BAA's higher turnover

**Fault fixed:** BAA's turnover (472–523%/year) is nearly double PAA's, yet the paper uses the same flat 0.1% one-way cost assumption — an even larger potential understatement of real-world drag than in PAA.

```
FUNCTION realistic_cost_model(ticker, trade_date, notional):
    spread   = lookup_historical_spread(ticker, trade_date)
    impact   = estimate_impact(notional, avg_daily_volume(ticker, trade_date))
    commission = broker_commission_schedule(trade_date)
    return spread/2 + impact + commission

FOR cost_scenario IN ["optimistic_5bps","vendor_estimated_spread","pessimistic_2x_spread","stress_illiquid_2008_2020"]:
    result = run_full_backtest(price_panel, cost_scenario)
    report(cost_scenario, result.CAGR_after_costs, result.turnover)
```

Include a specific stressed-liquidity scenario for episodes like Mar 2020 and the 2008 crisis, when bid/ask spreads on some ETFs (especially HYG, EEM, DBC) widened materially — directly relevant since BAA's defensive sleeve leans on several of these during exactly the periods it is meant to protect against.

---

## 8. Stress-test the inflation/rising-rate hedge claim specifically

**Fault fixed:** the enhanced defensive universe (adding DBC and TIP) is explicitly motivated by, and tuned against, the two in-sample regimes (1970s stagflation and 2021-22 inflation spike) — a defensive sleeve that "happens" to work in exactly the two periods used to select it deserves a dedicated out-of-training test.

```
INFLATION_STRESS_WINDOWS = [
    ("1973-01","1974-12"),   # 1970s oil shock inflation
    ("1979-01","1981-12"),   # Volcker-era rate hikes
    ("2021-06","2022-12"),   # post-COVID inflation spike (partially in-sample for BAA — flag explicitly)
    ("2004-01","2006-12"),   # rising-rate period without high inflation, as a contrast case
]

FOR window IN INFLATION_STRESS_WINDOWS:
    freeze_parameters_as_of(window.start)     # no re-optimization inside the window
    result = simulate(BAA_strategy, window)
    report(window, result.defensive_sleeve_return, result.DBC_TIP_contribution,
           result.correlation_DBC_vs_equities_during_window)

# Explicitly test: does DBC behave as a hedge, or does it sell off WITH equities
# in a liquidity-crisis-driven (rather than inflation-driven) drawdown?
FUNCTION hedge_quality_check(window):
    corr = rolling_correlation(DBC_returns, SPY_returns, window)
    report("DBC/SPY correlation during", window, corr)
```

---

## 9. Regime and crisis stress testing (reused/extended from the PAA plan)

**Fault fixed:** as with PAA, a 45-70 year sample still contains only a handful of truly independent stress episodes; BAA's added complexity (more assets, more filters) makes this even more important to address directly.

```
CRISIS_WINDOWS = [
    ("1973-01","1974-12"), ("2000-03","2002-10"), ("2007-10","2009-03"),
    ("2011-05","2011-10"), ("2015-08","2016-02"), ("2018-10","2018-12"),
    ("2020-02","2020-03"), ("2022-01","2022-10"),
]

FUNCTION regime_stress_test(price_panel, n_paths=10000, block_length_months=6):
    results = []
    for i in range(n_paths):
        path = stationary_block_bootstrap(price_panel, block_length_months)
        results.append(simulate(BAA_strategy, path))
    report_distribution(results.CAGR, results.max_drawdown, results.defensive_fraction)
```

---

## 10. Parameter stability heatmap across BAA's full design space

**Fault fixed:** the paper's own Fig 13/14-style robustness tables show a genuine cliff (K/FS collapsing to 0.0% for BAA-G4 across a whole block of filter combinations) — this needs to be surfaced as a headline finding, not a footnote.

```
FUNCTION parameter_stability_heatmap(grid, price_panel):
    results = {}
    for combo in grid.all_combinations():
        results[combo] = walk_forward_backtest(price_panel, [combo])
    plot_heatmap(results, axes=["offensive_filter_L","canary_filter_LP"],
                 metric="Sharpe", facet_by=["offensive_universe","defensive_universe"])
    flat_region_fraction = fraction_of_grid_within_X_pct_of_best(results, X=10)
    report(flat_region_fraction)
    # A truly robust variant should show a broad plateau; report explicitly
    # if any variant (esp. the concentrated G4-style ones) shows cliff/discontinuous behavior.
```

---

## 11. Full pipeline (end-to-end pseudocode)

```
FUNCTION main():
    prices = load_price_history(OFFENSIVE_CANDIDATES | DEFENSIVE_CANDIDATES | CANARY_CANDIDATE_POOL,
                                 DATA_START, DATA_END)

    # 1-2: joint design-choice search inside walk-forward loop, no static hindsight-picked IS window
    oos_results = walk_forward_backtest(prices, DESIGN_GRID)
    report_by_decade(oos_results)

    # 3: smoothed vs. binary breadth-trigger comparison
    smoothed_results = walk_forward_backtest(prices, DESIGN_GRID, breadth_fn=smooth_canary_breadth)
    compare(oos_results, smoothed_results)

    # 4: canary universe sensitivity
    canary_sensitivity = [walk_forward_backtest(prices, grid_fixed_except_canary(c, b))
                           for c in CANARY_VARIANTS for b in B_VALUES_TESTED]

    # 5: statistical correction for the (larger) search space
    dsr = deflated_sharpe_ratio(oos_results.best_sharpe, n_trials_total(), skew(oos_results.returns),
                                 kurtosis(oos_results.returns), len(oos_results.returns))
    reality_check_pvalue = reality_check(prices, DESIGN_GRID)

    # 6: standard metrics alongside/instead of proprietary K
    standard_metrics = compute_standard_metrics(oos_results.returns)
    plot_K_cliff_sensitivity(canary_sensitivity)

    # 7: cost sensitivity scaled to actual (higher) turnover
    cost_sensitivity = [run_full_backtest(prices, scenario) for scenario in COST_SCENARIOS]

    # 8: inflation/hedge-specific stress tests
    inflation_stress = [simulate(BAA_strategy, w) for w in INFLATION_STRESS_WINDOWS]
    hedge_quality = [hedge_quality_check(w) for w in INFLATION_STRESS_WINDOWS]

    # 9: broader regime/crisis stress testing
    stress_distribution = regime_stress_test(prices)
    crisis_results = [simulate(BAA_strategy, w) for w in CRISIS_WINDOWS]

    # 10: full parameter stability heatmap, explicitly surfacing cliffs
    stability_map = parameter_stability_heatmap(DESIGN_GRID, prices)

    compile_final_report(oos_results, dsr, reality_check_pvalue, canary_sensitivity,
                          standard_metrics, cost_sensitivity, inflation_stress,
                          hedge_quality, stress_distribution, crisis_results, stability_map)
```

---

## 12. Reporting checklist

- [ ] Walk-forward OOS results with every discrete design choice (universe, DBC/TIP inclusion, filter pairing, QQQ-vs-SPY) selected *inside* the same search, never hand-picked post-hoc
- [ ] Performance reported by decade, descriptively, with no training window chosen to match known-favorable regimes
- [ ] Binary (B=1 step-function) vs. smoothed/hysteresis breadth trigger compared directly
- [ ] Canary universe sensitivity analysis across ≥5 plausible alternative compositions, not just the one used in the paper
- [ ] Deflated Sharpe Ratio / Reality Check p-value reflecting the full, larger search space (universe × filters × breadth × canary)
- [ ] Standard metrics (Sharpe, Sortino, Calmar, UPI, CVaR) reported alongside any proprietary K-style measure, with the K-vs-drawdown cliff explicitly plotted
- [ ] Cost sensitivity band scaled to BAA's actual (higher) turnover, including a stressed-illiquidity scenario
- [ ] Dedicated inflation/rising-rate stress windows testing whether DBC/TIP behave as true hedges or co-move with equities in liquidity-driven (non-inflation) crises
- [ ] Full crisis-window and block-bootstrap regime testing
- [ ] Parameter stability heatmap that explicitly flags cliff/discontinuous regions rather than only reporting the single best combination

## Caveats of this plan itself

- Real-ETF history for TIP (2003–) and DBC (2006–) is much shorter than BAA's claimed 1970-start synthetic sample, so any conclusions about the inflation-hedge defensive sleeve necessarily rest on fewer independent inflationary episodes than the original paper implies — this is an honest limitation, not something to be smoothed over with proxy data.
- Expanding the search grid to include canary-universe composition, while necessary to test Fault #4/#9, further increases the multiple-comparisons burden — the deflated Sharpe/Reality Check correction must scale accordingly, and results should be interpreted more conservatively than a single-universe backtest would suggest.
- Smoothing the B=1 trigger changes the strategy's fundamental character (a core design decision in the original paper), so results under the smoothed version should be reported as a **separate robustness variant**, not as a replacement claim about "the" BAA strategy.

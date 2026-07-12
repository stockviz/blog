# VAA (Vigilant Asset Allocation) Backtest Plan

Based on: Keller & Keuning (2017), "Breadth Momentum and Vigilant Asset Allocation (VAA); Winning More by Losing Less"

This plan covers implementation using your historical ETF price database, for the **VAA-G12** universe (12 risky global assets + 3-asset cash universe), which is the most practical version to replicate with real, currently-tradable ETFs.

---

## 1. Universe Definition

### Risky universe (N=12)

| # | Asset class | ETF |
|---|---|---|
| 1 | US large cap | SPY |
| 2 | US small cap | IWM |
| 3 | US tech/growth | QQQ |
| 4 | European equities | VGK |
| 5 | Japanese equities | EWJ |
| 6 | Emerging market equities | VWO |
| 7 | US/global REITs | VNQ |
| 8 | Commodities | GSG |
| 9 | Gold | GLD |
| 10 | Long-term US Treasuries | TLT |
| 11 | Investment-grade corporate bonds | LQD |
| 12 | High-yield corporate bonds | HYG |

### Cash universe (3 assets — always fully allocated to single best performer, no absolute-momentum filter applied)

| Asset | ETF |
|---|---|
| Short-term Treasuries | SHY |
| Intermediate Treasuries | IEF |
| Investment-grade corporates | LQD |

Note: LQD appears in both universes. This mirrors the original paper's design — if LQD is the top performer in the risky universe, it can also be the fallback in the cash universe.

---

## 2. Determining the Backtest Start Date

**Rule: start date = first month-end at which *all 15 tickers* (12 risky + SHY, IEF; LQD already counted) have valid closing prices in your database, plus a 12-month lookback buffer for the momentum filter.**

### Step 2.1 — Query inception/first-available dates
For each of the 13 unique tickers (SPY, IWM, QQQ, VGK, EWJ, VWO, VNQ, GSG, GLD, TLT, LQD, HYG, SHY, IEF), pull `MIN(date)` from your price table.

### Step 2.2 — Known constraints (approximate — verify against your own data)
The later-inception ETFs will be the binding constraint. Based on public inception dates:
- **HYG**: ~April 2007 (this is very likely your binding constraint)
- **GSG**: ~July 2006
- **VNQ**: ~2004
- **VGK, VWO**: ~2005
- **GLD**: ~2004
- **TLT, IEF, LQD, SHY**: ~2002
- **EWJ**: 1996
- **IWM**: 2000
- **QQQ**: 1999
- **SPY**: 1993

Expect the practical common start to land around **mid-to-late 2007** (driven by HYG), unless your database substitutes a different high-yield proxy with longer history.

### Step 2.3 — Apply the lookback buffer
The 13612W filter needs 12 trailing months of prices to compute its first reading (see Section 3). So:

```
first_valid_common_month = MAX(first_available_month across all 13 tickers)
backtest_start = first_valid_common_month + 12 months
```

### Step 2.4 — Document the actual dates found
Before proceeding, write down:
- The binding ticker and its first available date in your DB
- The resulting `backtest_start`
- The resulting `backtest_end` (last common date across all tickers, typically "today" or your DB's last update)

### Step 2.5 (optional) — Handle a too-short sample
If the common start date leaves too few years for a meaningful in-sample/out-of-sample split (see Section 7), consider:
- Dropping HYG and using an older high-yield proxy or substituting a corporate bond fund with longer history, **or**
- Running a second, longer backtest on the smaller **VAA-G4** universe (SPY, VEA or EFA, VWO or EEM, BND or AGG — likely common start ~2007-2008 as well, or earlier if using EFA/EEM/AGG which predate VEA/VWO/BND), **or**
- Sourcing index-based (non-ETF) proxy series for the pre-inception periods, as the original paper does.

---

## 3. Momentum Filter: 13612W

For each risky asset *i* and each month-end *t*, using **total return** (dividend-adjusted) prices `p`:

```
Momentum(i, t) = 12 * (p_t/p_{t-1} - 1) + 4 * (p_t/p_{t-3} - 1) + 2 * (p_t/p_{t-6} - 1) + 1 * (p_t/p_{t-12} - 1)
```

Equivalently, in the paper's price-ratio notation:
```
13612W = 12*(p0/p1) + 4*(p0/p3) + 2*(p0/p6) + 1*(p0/p12) - 19
```
where `p0` = current month-end price, `p_k` = price *k* months ago.

- **Requires 12 months of trailing price history** — hence the lookback buffer in Section 2.
- Apply the **same filter** to both:
  - **Relative momentum** (ranking assets within the risky universe to pick the Top T), and
  - **Absolute momentum** (classifying an asset as "bad" if its 13612W value ≤ 0), and
  - **Cash universe selection** (ranking SHY/IEF/LQD — but *without* applying the absolute/sign filter; always pick the single best-ranked cash asset regardless of sign).

**Implementation note:** ensure you're using total-return (adjusted for dividends/distributions) price series from your database, not raw close prices — this materially affects momentum values for bond ETFs with significant yield.

---

## 4. Breadth-Based Crash Protection (Cash Fraction)

At each month-end *t*:

1. Compute `b` = number of "bad" assets in the risky universe (13612W ≤ 0), out of N = 12.
2. Compute the raw cash fraction:
   ```
   CF = b / B      if b < B
   CF = 1          if b >= B
   ```
   where `B` is the breadth threshold parameter (see Section 6 for optimization).
3. Apply **Easy Trading (ET) rounding** to avoid excessive rebalancing of all Top-T positions simultaneously:
   ```
   CF_ET = (1/T) * floor(b * T / B),  capped at 1
   ```
   This rounds the cash fraction down to the nearest multiple of `1/T`, so that instead of shaving a bit off every Top-T holding, you fully replace the *worst* asset(s) in the Top-T with cash.

---

## 5. Portfolio Construction Each Month

1. Rank all 12 risky assets by 13612W momentum (highest to lowest).
2. Select the **Top T** assets (equal weight 1/T each, before cash adjustment).
3. Compute `CF_ET` per Section 4.
4. Starting from the worst-ranked asset within the Top T, replace assets with cash **in order from worst to best** until the cumulative cash weight matches `CF_ET` (in multiples of 1/T).
5. For the "cash" portion: rank SHY, IEF, LQD by 13612W momentum (sign ignored) and allocate the entire cash fraction to the single best-ranked cash asset that month.
6. Resulting portfolio = weighted combination of surviving Top-T risky assets (each 1/T) + cash-fraction allocation to the winning cash asset.
7. Rebalance monthly (end-of-month signal, trade at next available price — typically next day's open or same month-end close, be consistent).

---

## 6. Parameter Optimization (In-Sample / Out-of-Sample)

1. Split your backtest period into two roughly equal halves:
   - **In-Sample (IS)**: first half — used only to select T and B.
   - **Out-of-Sample (OS)**: second half — used only to validate, no further tuning.
2. Grid-search over `T = 1..6` and `B = 1..6` (36 combinations) on the IS period.
3. For each (T, B) combination, compute the full IS equity curve and the RAD metric (Section 8).
4. Select the (T, B) that **maximizes RAD** over IS. (The original paper found T=2, B=4 for its version of this universe — do not assume this transfers to your specific date range or ETF substitutions; re-optimize on your own data.)
5. Lock in the chosen (T, B) and run the strategy unchanged over the OS period.
6. Report performance separately for IS, OS, and Full Sample (FS = IS + OS).

---

## 7. Benchmark / Comparison Strategies to Compute Alongside VAA

Compute these on the same data and date range for context:

- **EW**: equal-weight buy & hold of the 12 risky assets, monthly rebalanced.
- **EWC**: equal-weight buy & hold of the 3 cash assets, monthly rebalanced.
- **60/40**: 60% SPY / 40% IEF, monthly rebalanced.
- **SPY**: buy & hold.
- **Dual (traditional dual momentum)**: same 13612W filter, but replace only individual bad assets within the Top T by cash (no breadth mechanism) — optimize T separately (this is the paper's "Dual" comparison strategy).

---

## 8. Performance Metrics to Report

For VAA and each benchmark, over IS, OS, and FS:

- **R**: CAGR (annualized, compounded)
- **V**: annualized volatility of monthly returns
- **D**: maximum drawdown (based on end-of-month prices)
- **Sharpe**: (R − R_EWC) / V — i.e., excess return over the cash-universe return, divided by volatility (the paper uses EWC's return as the "risk-free" benchmark rather than T-bills, since EWC is often meaningfully above the true risk-free rate)
- **MAR**: R / D
- **RAD** (Returns Adjusted for Drawdowns):
  ```
  RAD = R * (1 - D/(1-D))   if R >= 0% and D <= 50%
  RAD = 0%                  otherwise
  ```
- **TTC**: Total Transaction Costs per year, assuming 0.10% one-way cost per trade (2.4%/year = full monthly turnover baseline)
- **CF**: average cash fraction over the period (% of months / % of portfolio weight in cash assets)

---

## 9. Transaction Cost Assumption

Apply a **0.10% one-way transaction cost** to any change in position weight each month (both for entering/exiting risky assets and switching the cash asset). Compute turnover as the sum of absolute weight changes divided by 2, consistent with the paper's Appendix B formula.

---

## 10. Suggested Build Order (Practical Steps)

1. **Data pull**: extract monthly (end-of-month) total-return price series for all 13 tickers from your database; compute the common start date per Section 2.
2. **Momentum engine**: compute 13612W for every ticker, every month, from the start date forward.
3. **Signal engine**: compute `b`, `CF`, `CF_ET`, Top-T ranking, and cash-asset ranking for every month.
4. **Portfolio engine**: build monthly weights per Section 5; compute monthly portfolio returns net of transaction costs.
5. **Grid search**: run steps 3–4 for all 36 (T, B) combinations over IS only; select the RAD-optimal pair.
6. **Validation run**: apply the locked-in (T, B) over OS and FS; compute all metrics in Section 8.
7. **Benchmarks**: compute EW, EWC, 60/40, SPY, and Dual over the same IS/OS/FS windows.
8. **Reporting**: equity curve (log scale) for VAA vs. EW; rolling 3-year returns; drawdown chart; summary tables for IS/OS/FS (mirroring the paper's Tables 2–4 and Figures 7–9).
9. **Sanity checks**:
   - Confirm CF and TTC are in a plausible range (paper's VAA-G12 analog: CF ≈ 55-60%, TTC ≈ 1.2-1.5%/year).
   - Confirm no look-ahead bias: momentum signal computed using only data available *as of* month-end t, portfolio formed using that signal, returns realized over month t+1.
   - Confirm the optimized (T, B) is not sitting at a grid boundary (T=6 or B=6) in a way that suggests the grid should be widened.

---

## 11. Known Limitations to Flag in Your Results

- Common ETF history is short (~starting 2006-2008 depending on exact tickers), so IS/OS split will cover far fewer years and market regimes than the original paper's 1970-2016 and 1926-2016 tests. Results will be far more exposed to in-sample overfitting (fewer independent decades = the Harvey (2013)/Paulsen (2016) "haircut" caveat in the paper's Section 5 is even more relevant here).
- A single sample period cannot separate "skill" from "this particular 15-20 year window" — treat results as illustrative, not as strong statistical evidence.
- If your database contains delisted or substituted ETFs (e.g., an older commodity or high-yield fund that later merged into GSG/HYG), consider splicing series to extend history — but document any splices clearly since this reintroduces the same proxy-construction judgment calls the original paper made explicit in its Appendix A.

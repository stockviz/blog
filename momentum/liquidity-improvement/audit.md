# Backtest Audit Report: Common Errors & Biases

An analysis of the R backtesting codebase reveals several **critical errors** spanning lookahead/survivorship bias, portfolio construction math, execution cost modeling, and return alignment.

---

## Executive Summary of Backtest Errors

| Issue | Severity | Affected File(s) | Impact |
| :--- | :--- | :--- | :--- |
| **Dropped Day-1 Holding Returns** | **Critical** | `liqim-common.R` | Erases the first trading day of every holding period across all strategies. |
| **20x Transaction Cost Underestimation** | **Critical** | `liqim-common.R` | Divides total portfolio drag by $N$, applying 2.5 bps per trade instead of 50 bps. |
| **Survivorship Bias via Full-History Intersect** | **High** | `build.R` | Filters universe using global table intersections, excluding historic delisted stocks. |
| **Alignment Drop of Benchmark Days** | **High** | `backtest.R`, `consolidated.R` | `na.omit(merge.xts(...))` drops benchmark performance on days where portfolio returns are `NA`. |
| **Zombie Stock Filtering in Universe Creation** | **Medium** | `build.R` | `findInterval(...) >= 1L` treats delisted stocks as active indefinitely. |
| **IPO Lookback Distortion in Momentum** | **Medium** | `liqim-common.R` | Compares 5–6 month returns of recent IPOs directly against 12-month returns. |
| **ILLIQ Price/Volume Adjustment Mismatch** | **Medium** | `build.R` | Mixing split-adjusted prices with unadjusted volume distorts Amihud ILLIQ for split stocks. |

---

## 1. Critical Execution & Return Calculation Bugs

### A. Dropped Day-1 Holding Returns (First Day Return Omitted)
In `liqim-common.R`, `stockReturns()` filters prices starting from `fromDate = sigDate + 1`:

```r
# liqim-common.R (Lines 23-31)
stockReturns <- function(df, fromDate, toDate) {
  sub <- df[df$date_stamp >= fromDate & df$date_stamp <= toDate, , drop = FALSE]
  pXts <- xts(sub$c, sub$date_stamp)
  rets <- na.omit(dailyReturn(pXts, type = "arithmetic"))
  xts(coredata(rets), as.Date(index(rets)))
}

```

* **The Flaw:** `dailyReturn(pXts)` calculates $P_t / P_{t-1} - 1$. Because `pXts` begins on `fromDate` (`sigDate + 1`), row 1 has no $P_{t-1}$ and evaluates to `NA`. Calling `na.omit()` deletes row 1.
* **Impact:** The return from $P_{\text{sigDate}}$ to $P_{\text{sigDate}+1}$ is completely deleted. Your backtest misses the entry day return for every stock in every rebalance cycle.
* **Fix:** Include `sigDate` (or the previous close) inside `sub` so `dailyReturn` can calculate day 1's return, then trim `sigDate` from the output.

---

### B. 20x Transaction Cost Underestimation (Drag Dilution)

In `liqim-common.R`, trading drag is applied to the portfolio return vector:

```r
# liqim-common.R (Line 61)
if (drag > 0) vec[1] <- vec[1] - drag / nStocks

```

* **The Flaw:** `vec` is generated via `rowMeans(coredata(mat))`, which represents equal-weighted portfolio return $R_p = \frac{1}{N} \sum_{i=1}^N R_i$. If each stock incurs a trade drag $d$, net portfolio return is $R_p - d$. Dividing `drag` by `nStocks` applies $0.005 / 20 = 0.00025$ (2.5 bps) instead of 50 bps.
* **Impact:** Transaction costs and slippage are underestimated by a factor of 20, artificially boosting strategy returns.
* **Fix:** Subtract `drag` directly without dividing by `nStocks`: `if (drag > 0) vec[1] <- vec[1] - drag`

---

## 2. Survivorship Bias & Universe Filtering Issues

### A. Global Intersection introducing Survivorship Bias

In `build.R`:

```r
# build.R (Line 47)
allSymbols <- sort(Reduce(intersect, list(unique(pxDf$ticker), unique(eqDf$SYMBOL), unique(mcapDf$SYMBOL))))

```

* **The Flaw:** `Reduce(intersect, ...)` forces a stock to exist across all three database tables spanning the entire multi-year dataset. If historical delisted or bankrupt companies are absent from any table or recent market cap query, they are permanently removed from historical selection universe.
* **Impact:** Classic **survivorship bias**. The backtest only trades companies that survived to the database snapshot date.

---

### B. Ineffective Inactive/Delisting Check in `getUniverse()`

In `build.R`:

```r
# build.R (Lines 77-79)
eqOk <- vapply(allSymbols, function(sym){
  df <- eqBySym[[sym]]; if(is.null(df)) return(FALSE); findInterval(sigNum, as.numeric(df$TIME_STAMP))>=1L
}, logical(1))

```

* **The Flaw:** `findInterval(sigNum, as.numeric(df$TIME_STAMP)) >= 1L` tests whether a stock had at least one traded record *on or prior to* `sigNum`. It does **not** check whether the stock was active around `sigNum`.
* **Impact:** A stock delisted in 2012 will evaluate to `eqOk == TRUE` for all subsequent dates in 2015, 2020, and 2025.

---

## 3. Data Integrity & Alignment Bugs

### A. Benchmark Alignment & Return Cancellation

In `backtest.R` and `consolidated.R`:

```r
# backtest.R (Line 27)
combined <- na.omit(do.call(merge.xts, list(benchRets, q1Rets)))

```

* **The Flaw:** Due to Issue 1A, `q1Rets` contains `NA` or missing rows on the first trading day after each month-end. When merging `benchRets` and `q1Rets`, `na.omit()` drops those dates across **all** columns.
* **Impact:** The benchmark series loses the 1st trading day of every month, corrupting performance comparisons, Sharpe ratios, and annual return distributions.

---

### B. IPO Lookback Distortion in Momentum Ranking

In `liqim-common.R`:

```r
# liqim-common.R (Lines 118-125)
sub <- df[df$date_stamp >= momStart & df$date_stamp <= momEnd, , drop = FALSE]
if (nrow(sub) < 100) return(NA_real_)
as.numeric(tail(sub$c, 1)) / as.numeric(sub$c[1]) - 1

```

* **The Flaw:** If a stock was listed 6 months prior, `nrow(sub)` can exceed 100 days. `sub$c[1]` selects its first available trading price 6 months ago.
* **Impact:** A 6-month return of +30% is ranked side-by-side against a 12-month return of +30%, artificially favoring recent IPOs with short price histories.
* **Fix:** Require strict trading history duration or enforce minimum trading day thresholds corresponding to the full lookback window (e.g., `nrow(sub) >= 230` for 12 months).

---

### C. Amihud ILLIQ Price vs. Volume Adjustment Mismatch

In `build.R`:

```r
# build.R (Lines 102-105)
rets <- diff(df$c)/df$c[-n]
dollarVol <- df$c[-n]*df$v[-n]
mean(1e6*abs(rets[ok])/dollarVol[ok], na.rm=TRUE)

```

* **The Flaw:** If `c` comes from split-adjusted price data (`eod_adjusted_nse`), but `v` is unadjusted historical volume, `df$c * df$v` computes a distorted dollar volume for dates preceding stock splits.
* **Impact:** Amihud ILLIQ values ($\frac{|R|}{\text{Dollar Volume}}$) become artificially inflated or deflated across split/bonus event histories.

---

## Suggested Action Plan

1. **Fix `stockReturns()**` to pass `sigDate` as a baseline reference so day-1 returns are retained.
2. **Remove `/ nStocks**` in `buildStrand()` drag deduction.
3. **Audit data sources**: Use unadjusted close with unadjusted volume for ILLIQ calculations, or fully adjusted volume paired with adjusted close.
4. **Enforce minimum window coverage** (e.g., 230+ trading days) in `buildMomentumCache()`.

```



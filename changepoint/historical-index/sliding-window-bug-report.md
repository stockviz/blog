# Bug Report: Inflated Returns in Sliding-Window Regime Backtest

## Summary

The sliding-window test in `script.R` reported implausible performance for
the changepoint-regime ("CP") strategy — annualized returns of 45–54% and
Sharpe ratios above 2.0–3.0, roughly 4x buy-and-hold. This is caused by a
date-misalignment bug in `compute_strategies()`, not a genuine trading edge.
Once fixed, the CP strategy actually **underperforms** buy-and-hold on all
three indices tested.

## The problem

`compute_strategies()` builds several xts objects that are meant to line up
day-for-day by date:

```r
retL1       <- stats::lag(ret_xts[date_range], -1)   # full history through test_end (~1,740 rows)
classSubset <- regime_xts[date_range]                 # only the current test-year (~252 rows)

cpGross <- ifelse(classSubset == 1, retL1, 0)
```

`ifelse()` and `&` in base R do **not** align xts/zoo objects by date — they
operate positionally and silently recycle the shorter vector. Because
`regime_xts` was built to cover only the test year (~252 rows) while
`retL1`/`pxSubset` cover the entire price history up to `test_end` (~1,740
rows), `ifelse()` paired the test year's regime flags with the *dates and
returns from the first ~252 rows of history* (i.e., 2005–2006), not the
actual test-period returns.

This affected the **CP** and **SMA+CP** columns (anything using
`classSubset`). The **SMA** column was unaffected, because `pxSubset`,
`smaPx`, and `retL1` are all built over the same full `date_range` and are
therefore the same length — the recycling bug never triggers there.

## Evidence

Reproduced directly against the cached data
(`prices_index.Rdata`, `window-class-cache.Rdata`).

For the test window **2011-03-28 → 2012-03-29**, the regime cache shows
NIFTY 50 TR was labeled `STABLE` (i.e., "in the market") on every single day
of that window. A strategy that is in the market 100% of the time should
return essentially the same as buy-and-hold for that period, minus a sliver
of drag for the single entry.

| | SMA | **CP** | SMA+CP | B&H |
|---|---|---|---|---|
| Reported (buggy) | -5.0% | **+72.0%** | +21.4% | -8.75% |
| Corrected (date-aligned) | -4.2% | **-8.1%** | -4.4% | -7.9% |

The reported +72% for CP was purely an artifact of pairing 2011–2012
regime flags with 2005–2006 returns. The corrected value (-8.1%) is what it
should be: essentially the same as buy-and-hold.

## Full corrected results (sliding window, mean across 15 test windows)

| Index | SMA | **CP (corrected)** | SMA+CP | B&H |
|---|---|---|---|---|
| NIFTY 50 TR | 5.6% | **5.5%** *(was 53.6%)* | 3.7% | 12.5% |
| NIFTY MIDCAP 150 TR | 18.1% | **4.8%** *(was 50.1%)* | 8.0% | 19.9% |
| NIFTY SMALLCAP 250 TR | 20.1% | **3.0%** *(was 45.0%)* | 7.6% | 19.4% |

Once correctly aligned, the CP strategy underperforms buy-and-hold on all
three indices — consistent with a volatility-avoidance filter that gives up
upside during choppy-but-rising markets in exchange for (real, but more
modest) drawdown protection. Sharpe ratios also fall out of the
implausible 2–3+ range into a realistic ~0.2–1.0 range.

## Scope of impact

- **Not affected:** the expanding-window analysis. There, `regime_xts` is
  built over the entire history (`all_dates`), the same length as
  `retL1`/`pxSubset`, so the length-mismatch recycling bug never triggers.
- **Affected:** every sliding-window output — `sliding-window-sharpe.html/png`,
  the per-index drawdown tables and cumulative-return charts, and the
  "Combined Metrics" table (for the sliding-window rows only) — since these
  are all built from the same corrupted `sliding_strats` objects.

## Fix

Explicitly date-merge `regime_xts` onto `retL1`'s index before using it,
rather than relying on positional recycling:

```r
compute_strategies <- function(price_xts, regime_xts, date_range,
                               sma_lb = 50, drag = 0.2/100,
                               ret_xts = NULL) {
  if (is.null(ret_xts)) {
    retL1 <- stats::lag(dailyReturn(price_xts[date_range]), -1)
  } else {
    retL1 <- stats::lag(ret_xts[date_range], -1)
  }
  pxSubset <- price_xts[date_range]
  smaPx <- SMA(pxSubset, sma_lb)

  # IMPORTANT: ifelse() and `&` on xts objects do NOT align by date --
  # they recycle positionally. regime_xts may cover a shorter span than
  # retL1/pxSubset (e.g. just one test-year window while retL1 spans the
  # full history), so it must be explicitly date-merged onto retL1's
  # index here. Without this, regime flags silently get paired with the
  # wrong dates' returns (whatever falls at the same row position),
  # which previously inflated the CP and SMA_CP results.
  classSubset <- merge(retL1, regime_xts, join = "left")[, 2]

  smaGross <- ifelse(pxSubset > smaPx, retL1, 0)
  trd <- ifelse(pxSubset > smaPx, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  smaNet <- ifelse(trd != 0, smaGross - drag, smaGross)

  cpGross <- ifelse(classSubset == 1, retL1, 0)
  trd <- ifelse(classSubset == 1, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  cpNet <- ifelse(trd != 0, cpGross - drag, cpGross)

  smaCpGross <- ifelse(pxSubset > smaPx & classSubset == 1, retL1, 0)
  trd <- ifelse(pxSubset > smaPx & classSubset == 1, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  smaCpNet <- ifelse(trd != 0, smaCpGross - drag, smaCpGross)

  toPlot <- na.omit(merge(smaNet, cpNet, smaCpNet, retL1))
  names(toPlot) <- c("SMA", "CP", "SMA_CP", "B&H")
  toPlot
}
```

Days outside `regime_xts`'s actual coverage become `NA` after the merge
(rather than being silently filled from the wrong dates), and are correctly
dropped by the existing `na.omit()` at the end of the function — preserving
the original intent of restricting output to dates where a regime signal
actually exists.

## Follow-up items

- **Re-run and regenerate** all sliding-window outputs (`sliding-window-sharpe.html/png`,
  per-index drawdown tables, cumulative-return charts, and the sliding-window
  rows of the combined-metrics table) with the patched function.
- **NaN Sharpe edge case:** one test window (NIFTY 50 TR, ending 2021-05-31)
  had the CP strategy flat (0% position) for the entire year, giving
  `sd(returns) == 0` and a `Sharpe = NaN`, which silently drops out of
  `mean(..., na.rm = TRUE)` when aggregating. Consider guarding the Sharpe
  calculation (e.g., return `NA` explicitly when `sd == 0`) so this is
  handled deliberately rather than by accident.
- Independently of this bug, recall the earlier finding that the
  "sliding window (train/test)" methodology doesn't actually freeze the
  regime label at `train_end` — it re-derives regime daily throughout the
  test year using a window ending on that day. That's still worth revisiting
  if the intent was to test a genuinely frozen, once-a-year model.

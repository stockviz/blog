"""Strategy metrics — exact port of common/returns.R strategy_metrics.

CAGR  = (prod(1+r))^(252/N) - 1            (PerformanceAnalytics geometric)
Vol   = sd(r, ddof=1) * sqrt(252)
Sharpe= mean(r)/sd(r, ddof=1) * sqrt(252)  (rf = 0)
MaxDD = positive max peak-to-trough        (PerformanceAnalytics maxDrawdown)

Windows (house): pre <= 2019-12-31, post >= 2020-05-01, full = everything.
Metrics AND charts are ALWAYS split pre/post/full.
"""

import numpy as np
import pandas as pd

PRE_END = "2019-12-31"
POST_START = "2020-05-01"
PERIODS = {"pre": (None, PRE_END), "post": (POST_START, None), "full": (None, None)}
PERIOD_LAB = {"pre": "PRE <= 2019-12-31", "post": "POST >= 2020-05-01", "full": "FULL"}


def strategy_metrics(rets, annualization=252):
    """Named metrics dict from a return Series (N, CAGR, Vol, Sharpe, MaxDD)."""
    r = np.asarray(pd.Series(rets).dropna(), dtype=float)
    if len(r) == 0:
        return {"N": 0, "CAGR": np.nan, "Vol": np.nan, "Sharpe": np.nan, "MaxDD": np.nan}
    n = len(r)
    total = np.prod(1.0 + r) - 1.0
    cagr = (1.0 + total) ** (annualization / n) - 1.0
    sd = np.std(r, ddof=1)
    vol = sd * np.sqrt(annualization)
    sharpe = (np.mean(r) / sd * np.sqrt(annualization)) if sd > 1e-16 else np.nan
    eq = np.cumprod(1.0 + r)
    dd = eq / np.maximum.accumulate(eq) - 1.0
    maxdd = -float(np.min(dd)) if len(dd) else np.nan
    return {"N": int(n), "CAGR": float(cagr), "Vol": float(vol),
            "Sharpe": float(sharpe), "MaxDD": float(maxdd)}


def slice_returns(rets, start=None, end=None):
    """Port of returns.R slice_returns (NA bounds tolerated)."""
    s = pd.Series(rets).dropna()
    if start is not None:
        s = s[s.index >= pd.Timestamp(start)]
    if end is not None:
        s = s[s.index <= pd.Timestamp(end)]
    return s


def metrics_by_period(rets):
    """pre/post/full metrics rows -> DataFrame (signal/period columns added by caller)."""
    rows = []
    for pn, (s, e) in PERIODS.items():
        m = strategy_metrics(slice_returns(rets, s, e))
        rows.append({"period": pn, **m})
    return pd.DataFrame(rows)


def pct(x, digits=1):
    return f"{100 * x:.{digits}f}%"


def fmt_sharpe(x):
    return f"{x:.2f}"

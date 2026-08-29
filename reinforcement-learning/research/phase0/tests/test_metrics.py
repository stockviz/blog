"""Metrics tests — strategy_metrics port vs hand-computed values + windows."""

import numpy as np
import pandas as pd

from rl.eval.metrics import strategy_metrics, slice_returns, metrics_by_period, PERIODS


def test_metrics_known_values():
    r = pd.Series(np.full(252, 0.001))
    m = strategy_metrics(r)
    assert m["N"] == 252
    # 0.1%/day for exactly one year (252 bars) -> total = annualized
    assert abs(m["CAGR"] - (1.001 ** 252 - 1)) < 1e-12
    assert abs(m["Vol"] - 0.0 * np.sqrt(252)) < 1e-12  # sd=0
    assert np.isnan(m["Sharpe"])  # zero vol
    # MaxDD: monotonic up -> 0
    assert m["MaxDD"] == 0.0


def test_metrics_drawdown_positive():
    r = pd.Series([0.01] * 100 + [-0.5] + [0.01] * 50)
    m = strategy_metrics(r)
    assert abs(m["MaxDD"] - 0.5) < 1e-9  # positive peak-to-trough (R maxDrawdown)


def test_metrics_sharpe_hand():
    rng = np.random.default_rng(0)
    r = pd.Series(rng.normal(0.0005, 0.01, 1000))
    m = strategy_metrics(r)
    sd = r.std(ddof=1)
    expect = r.mean() / sd * np.sqrt(252)
    assert abs(m["Sharpe"] - expect) < 1e-9


def test_windows():
    idx = pd.date_range("2018-01-01", "2022-12-31", freq="B")
    r = pd.Series(0.001, index=idx)
    pre = slice_returns(r, *PERIODS["pre"])
    post = slice_returns(r, *PERIODS["post"])
    assert pre.index.max().year == 2019
    assert post.index.min().year == 2020
    assert post.index.min().month == 5
    m = metrics_by_period(r)
    assert list(m["period"]) == ["pre", "post", "full"]
    assert m["N"].sum() > len(r) * 0.95  # pre+post ~ full (drop the 2020-01..04 gap)

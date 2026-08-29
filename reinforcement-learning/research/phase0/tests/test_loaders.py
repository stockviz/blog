"""Loader tests — real DB data, parquet-cached. Requires the estate (NORWAY/SWEDEN).

Bounded loads: NIFTY 50 daily, VIX, NIFTY futures roll, one 1-min index,
one MCX contract, one zd_option_bars token (discovered at runtime).
"""

import numpy as np
import pandas as pd
import pytest

from rl.data.loaders import (load_bhav_index, load_vix_history, load_bhav_eq_fut,
                             build_futures_calendar, roll_continuous_futures,
                             load_zd_index_bars, load_zd_bars_mcx, load_zd_option_bars,
                             load_px_history, load_momentum, daily_returns)


def test_bhav_index_nifty50():
    df = load_bhav_index("NIFTY 50")
    assert {"o", "h", "l", "close"}.issubset(df.columns)
    assert len(df) > 7000  # OHLC-valid rows from 1995-11 (pre-1995 rows carry zero OHLC)
    assert df.index.min().year <= 1995
    assert df.index.max().year >= 2025
    assert (df[["o", "h", "l", "close"]] > 0).all().all()  # zero-OHLC rows filtered


def test_bhav_index_slice():
    df = load_bhav_index("NIFTY 50", start="2015-01-01", end="2019-12-31")
    assert df.index.min().year == 2015
    assert df.index.max().year == 2019


def test_vix_history():
    df = load_vix_history()
    assert len(df) > 4000
    assert df["close"].min() > 0
    assert df.index.min().year == 2009


def test_futures_calendar_nifty():
    fut = load_bhav_eq_fut("NIFTY", start="2015-01-01")  # calendar starts at the data start (R convention)
    assert len(fut) > 8000  # NIFTY futures rows 2015+ (~8.6k)
    cal = build_futures_calendar(fut, "2015-01-01")
    assert len(cal["monthly_expiries"]) > 100  # ~120 monthly expiries 2015-2026
    # roll dates are ~5 trading days before expiry
    td = cal["trading_dates"]
    for r, e in zip(cal["roll_dates"][:10], cal["monthly_expiries"][:10]):
        gap = np.searchsorted(td, np.datetime64(e)) - np.searchsorted(td, np.datetime64(r))
        assert 1 <= gap <= 8
    assert sum(h is not None for h in cal["held_after"]) > 0.9 * len(cal["trading_dates"])


def test_roll_continuous_no_cross_contract_jump():
    fut = load_bhav_eq_fut("NIFTY", start="2015-01-01")
    rets = roll_continuous_futures(fut, "2015-01-01")
    assert len(rets) > 2000
    assert np.isfinite(rets).all()
    assert np.abs(rets).max() < 0.3  # no new/old price ratio jumps at rolls
    # same-contract check: compare against B&H of the index for a sanity range
    idx = load_bhav_index("NIFTY 50", start="2015-01-01")
    r_idx = daily_returns(idx["close"]).reindex(rets.index).dropna()
    common = rets.index.intersection(r_idx.index)
    corr = rets.loc[common].corr(r_idx.loc[common])
    assert corr > 0.9  # futures returns track the index closely


def test_zd_index_bars_1min():
    df = load_zd_index_bars("NIFTY 50")
    assert len(df) > 900_000
    assert df.index.tz is not None  # UTC tz-aware
    assert df.index.min().year == 2015
    assert df.index.max().year >= 2025
    # 375 bars per day, 60s spacing
    day = df.index.max().date()
    one_day = df[df.index.date == day]
    assert 350 <= len(one_day) <= 390
    gaps = np.diff(one_day.index.view("int64")) // 10**9
    assert np.median(gaps) == 60


def test_zd_bars_mcx():
    from rl.config import pg_conn
    conn = pg_conn()
    cur = conn.cursor()
    cur.execute("SELECT inst_token, COUNT(*) FROM zd_bars_mcx GROUP BY inst_token ORDER BY COUNT(*) DESC LIMIT 1")
    tok, n = cur.fetchone()
    conn.close()
    df = load_zd_bars_mcx(tok)
    assert len(df) == n
    assert df.index.tz is not None
    assert df.index.max().year >= 2025
    assert "oi" in df.columns


def test_zd_option_bars_1990_epoch():
    """1990-base decode verified on a recent NFO futures token (discovered
    via zd_master — no table-wide scans on the huge zd_option_bars)."""
    from rl.config import pg_conn
    conn = pg_conn()
    cur = conn.cursor()
    cur.execute("""SELECT inst_token FROM zd_master
                   WHERE time_stamp = (SELECT MAX(time_stamp) FROM zd_master)
                     AND exch = 'NFO' AND inst_type = 'FUT' LIMIT 10""")
    tokens = [r[0] for r in cur.fetchall()]
    conn.close()
    if not tokens:
        pytest.skip("no recent NFO futures in zd_master")
    tok = None
    df = None
    for t in tokens:
        df = load_zd_option_bars(t, start="2026-07-01")  # bounded: last ~2 months
        if df is not None and len(df) > 0:
            tok = t
            break
    if tok is None:
        pytest.skip("zd_option_bars has no recent NFO data — downloader has not run")
    # 1990-base decode: dates must be 2026 (NOT 2006 or 2046)
    assert df.index.max().year == 2026
    assert df.index.min().year >= 2026
    assert len(df) > 1000
    assert "oi" in df.columns
    assert df["o"].min() > 0


def test_px_history_and_momentum():
    df = load_px_history("RELIANCE")
    assert len(df) > 1000
    assert df["close"].min() > 0
    m = load_momentum("MOMENTUM_PROB", 365, symbols=["RELIANCE", "HDFCBANK", "TCS", "INFY", "ICICIBANK"])
    assert len(m.columns) == 5
    assert "RELIANCE" in m.columns
    assert len(m) > 3000

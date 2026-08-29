"""Data loaders for the StockViz estate (research-plan §1).

Daily tables (SQL Server NORWAY/StockViz): bhav_index, VIX_HISTORY,
BHAV_EQ_FUT (with the R2 common/futures.R roll calendar — phantom-expiry
filter included), PX_HISTORY, MOMENTUM_ABS/PROB.
Intraday tables (Postgres SWEDEN/StockVizDyn): zd_index_bars (1970 epoch),
zd_option_bars (1990 epoch!), zd_bars_mcx (1970 epoch), eod_adjusted_nse.

All intraday tick_stamps are decoded through rl.data.epochs — never
hand-rolled arithmetic in experiment code (research-plan §2.8).
Every loader caches to parquet (idempotent; re-fetch on demand).
"""

import datetime as _dt
from pathlib import Path

import numpy as np
import pandas as pd

from ..config import mssql_conn, pg_conn
from .epochs import decode_series, encode_tick

CACHE_DIR = Path("/mnt/data/books/RL/research/phase0/cache")
CACHE_DIR.mkdir(parents=True, exist_ok=True)

PRE_END = "2019-12-31"
POST_START = "2020-05-01"


def assert_columns(df, required):
    """Port of common/runtime.R assert_columns."""
    missing = [c for c in required if c not in df.columns]
    if missing:
        raise ValueError(f"data missing columns: {missing}")


def _cache_path(name):
    safe = name.replace("/", "_").replace(" ", "_").replace("|", "_")
    return CACHE_DIR / f"{safe}.parquet"


def _cached_df(name, builder, force=False):
    """Get-or-build a parquet-cached DataFrame."""
    path = _cache_path(name)
    if path.exists() and not force:
        return pd.read_parquet(path)
    df = builder()
    df.to_parquet(path, index=True)
    return df


def _sql(sql, conn=None, close=True):
    """Run a SQL query and return a DataFrame (all imports at module top)."""
    import pyodbc
    own = conn is None
    c = conn or mssql_conn()
    try:
        return pd.read_sql(sql, c)
    finally:
        if own and close:
            c.close()


def _pg(sql, params=None):
    import psycopg2
    conn = pg_conn()
    try:
        return pd.read_sql(sql, conn, params=params)
    finally:
        conn.close()


# ─────────────────────────── Daily (NORWAY) ───────────────────────────

def load_bhav_index(name, start=None, end=None, use_cache=True, ohlc_filter=True):
    """Daily index OHLC from bhav_index. Zero-OHLC rows (pre-1995 NIFTY)
    are dropped — filter rowSums(ohlc > 0) == 4 (R2 convention).
    ohlc_filter=False keeps every row with close > 0 even when OHLC is
    zero (NIFTY MIDCAP SELECT has closes from 2004 but OHLC only from
    2022 — close-only consumers like the RL envs want the full series)."""
    def build():
        q = f"SELECT TIME_STAMP, PX_OPEN, PX_HIGH, PX_LOW, PX_CLOSE FROM bhav_index WHERE INDEX_NAME = '{name}' ORDER BY TIME_STAMP"
        df = _sql(q)
        df["TIME_STAMP"] = pd.to_datetime(df["TIME_STAMP"])
        df = df.rename(columns={"TIME_STAMP": "date", "PX_OPEN": "o", "PX_HIGH": "h",
                                "PX_LOW": "l", "PX_CLOSE": "close"})
        if ohlc_filter:
            ohlc = df[["o", "h", "l", "close"]]
            df = df[(ohlc > 0).all(axis=1)].copy()
        else:
            df = df[df["close"] > 0].copy()
        df = df.set_index("date")
        return df
    key = f"bhav_index_{name}" + ("" if ohlc_filter else "_closeonly")
    df = _cached_df(key, build) if use_cache else build()
    return _slice(df, start, end)


def load_vix_history(start=None, end=None, use_cache=True):
    """India VIX daily (2009-03-03+)."""
    def build():
        q = "SELECT TIME_STAMP, PX_OPEN, PX_HIGH, PX_LOW, PX_CLOSE FROM VIX_HISTORY ORDER BY TIME_STAMP"
        df = _sql(q)
        df["TIME_STAMP"] = pd.to_datetime(df["TIME_STAMP"])
        df = df.rename(columns={"TIME_STAMP": "date", "PX_OPEN": "o", "PX_HIGH": "h",
                                "PX_LOW": "l", "PX_CLOSE": "close"})
        df = df[df["close"] > 0].set_index("date")
        return df
    df = _cached_df("VIX_HISTORY", build) if use_cache else build()
    return _slice(df, start, end)


def load_bhav_eq_fut(symbol, start=None, end=None, use_cache=True):
    """Daily index/stock futures OHLC (futures rows only: STRIKE_PR = 0)."""
    def build():
        q = (f"SELECT TIME_STAMP, EXPIRY_DT, PX_OPEN, PX_HIGH, PX_LOW, PX_CLOSE "
             f"FROM BHAV_EQ_FUT WHERE SYMBOL = '{symbol}' AND STRIKE_PR = 0 "
             f"ORDER BY TIME_STAMP")
        df = _sql(q)
        df["TIME_STAMP"] = pd.to_datetime(df["TIME_STAMP"])
        df["EXPIRY_DT"] = pd.to_datetime(df["EXPIRY_DT"]).dt.normalize()
        df = df.rename(columns={"TIME_STAMP": "date", "PX_OPEN": "o", "PX_HIGH": "h",
                                "PX_LOW": "l", "PX_CLOSE": "close"})
        return df[["date", "EXPIRY_DT", "o", "h", "l", "close"]]
    df = _cached_df(f"BHAV_EQ_FUT_{symbol}", build) if use_cache else build()
    if start or end:
        mask = pd.Series(True, index=df.index)
        if start:
            mask &= df["date"] >= pd.Timestamp(start)
        if end:
            mask &= df["date"] <= pd.Timestamp(end)
        df = df[mask]
    return df


def last_weekday_of_month(year, month, weekday):
    """Last weekday (4=Thu, 2=Tue) of a month — port of futures.R."""
    first = _dt.date(year, month, 1)
    if month == 12:
        last = _dt.date(year + 1, 1, 1) - _dt.timedelta(days=1)
    else:
        last = _dt.date(year, month + 1, 1) - _dt.timedelta(days=1)
    while last.weekday() != weekday:
        last -= _dt.timedelta(days=1)
    return pd.Timestamp(last)


def build_futures_calendar(fut_df, start_date, entry_offset=5, first_roll=None, last_month=None):
    """Port of common/futures.R build_monthly_futures_calendar.

    Returns dict(trading_dates, monthly_expiries, roll_dates, held_after).
    Phantom-expiry filter: contracts whose last trade is > 7d before their
    own expiry are dropped BEFORE the same-weekday narrowing.
    """
    assert_columns(fut_df, ["date", "EXPIRY_DT"])
    trading_dates = np.array(sorted(fut_df["date"].unique()), dtype="datetime64[D]")
    expiries = pd.to_datetime(fut_df["EXPIRY_DT"]).dt.normalize()
    expiry_counts = fut_df.assign(e=expiries).groupby("e")["date"].nunique().rename("symbols")
    first_month = pd.Timestamp(start_date).normalize().replace(day=1)
    if last_month is None:
        last_month = pd.Timestamp(expiries.max()).normalize().replace(day=1)
    months = pd.date_range(first_month, last_month, freq="MS")
    expected = [
        last_weekday_of_month(d.year, d.month, 4 if d <= pd.Timestamp("2025-08-01") else 2)
        for d in months
    ]
    last_trade_by_expiry = fut_df.assign(e=expiries).groupby("e")["date"].max()

    def snap_expiry(expected_date):
        dist = np.abs((expiry_counts.index - expected_date).to_numpy())
        candidates = expiry_counts[dist <= np.timedelta64(3, "D")]
        if len(candidates) == 0:
            return expected_date
        cand = candidates.index
        last_trade = last_trade_by_expiry.reindex(cand)
        live = cand <= last_trade + pd.Timedelta(days=7)
        if live.any():
            cand = cand[live]
        same_weekday = cand.weekday == expected_date.weekday()
        pool = cand[same_weekday] if same_weekday.any() else cand
        dist = np.abs((pool - expected_date).to_numpy())
        cnt = expiry_counts.reindex(pool)
        pool = pool[np.argsort(np.lexsort((-np.asarray(cnt), np.asarray(dist))))]
        return pool[0]

    monthly_expiries = np.array([snap_expiry(e) for e in expected], dtype="datetime64[D]")
    monthly_expiries = monthly_expiries[
        (monthly_expiries >= np.datetime64(first_month)) &
        (monthly_expiries <= np.datetime64(pd.Timestamp(expiries.max())))
    ]

    def roll_for(expiry_date):
        steps = int(entry_offset) - 1
        ed = pd.Timestamp(expiry_date)
        if ed <= pd.Timestamp(trading_dates[-1]):
            idx = np.searchsorted(trading_dates, np.datetime64(ed), side="right") - 1
            entry_idx = idx - steps
            if entry_idx < 0:
                return None
            return pd.Timestamp(trading_dates[entry_idx])
        target = ed
        while steps > 0:
            target -= _dt.timedelta(days=1)
            if target.weekday() < 5:
                steps -= 1
        cand = trading_dates[trading_dates <= np.datetime64(target)]
        if len(cand) == 0:
            return None
        return pd.Timestamp(cand[-1])

    roll_dates = [roll_for(e) for e in monthly_expiries]
    keep = [r is not None for r in roll_dates]
    if first_roll is not None:
        fr = pd.Timestamp(first_roll)
        keep = [k and (r >= fr) for k, r in zip(keep, roll_dates)]
    monthly_expiries = monthly_expiries[keep]
    roll_dates = [r for r, k in zip(roll_dates, keep) if k]
    if len(monthly_expiries) < 2:
        raise ValueError("futures calendar has fewer than two expiries")

    held_after = np.full(len(trading_dates), None, dtype=object)
    for k in range(len(monthly_expiries) - 1):
        rk = pd.Timestamp(roll_dates[k])
        rk1 = pd.Timestamp(roll_dates[k + 1])
        idx = np.where((trading_dates >= np.datetime64(rk)) & (trading_dates < np.datetime64(rk1)))[0]
        for i in idx:
            held_after[i] = monthly_expiries[k + 1]
    return {
        "trading_dates": pd.DatetimeIndex(trading_dates),
        "monthly_expiries": pd.DatetimeIndex(monthly_expiries),
        "roll_dates": pd.DatetimeIndex(roll_dates),
        "held_after": held_after,
    }


def roll_continuous_futures(fut_df, start_date, entry_offset=5):
    """Continuous front-contract DAILY returns (accounting invariant #1:
    returns never cross an expiry — the roll day's return is the NEW
    contract's own close-to-close, never new/old price ratio)."""
    assert_columns(fut_df, ["date", "EXPIRY_DT", "close"])
    cal = build_futures_calendar(fut_df, start_date, entry_offset=entry_offset)
    # per-contract close series
    per_contract = {}
    for exp, grp in fut_df.groupby(fut_df["EXPIRY_DT"].dt.normalize()):
        s = grp.set_index("date")["close"].sort_index()
        s = s[~s.index.duplicated(keep="last")]
        per_contract[pd.Timestamp(exp)] = s
    tdates = cal["trading_dates"]
    held = cal["held_after"]
    rets = pd.Series(np.nan, index=tdates)
    for i in range(1, len(tdates)):
        contract = held[i]
        if contract is None:
            continue
        s = per_contract.get(pd.Timestamp(contract))
        if s is None:
            continue
        t = pd.Timestamp(tdates[i])
        if t not in s.index:
            continue
        prev = s.index[s.index < t]
        if len(prev) == 0:
            continue
        p0 = s.loc[prev[-1]]
        p1 = s.loc[t]
        if p0 and p0 > 0:
            rets.iloc[i] = p1 / p0 - 1
    rets = rets.dropna()
    return rets


def load_px_history(symbol, series="EQ", start=None, end=None, use_cache=True):
    """Daily stock EOD from PX_HISTORY (SERIES='EQ')."""
    def build():
        q = (f"SELECT TIME_STAMP, PX_OPEN, PX_HIGH, PX_LOW, PX_CLOSE FROM PX_HISTORY "
             f"WHERE SYMBOL = '{symbol}' AND SERIES = '{series}' ORDER BY TIME_STAMP")
        df = _sql(q)
        df["TIME_STAMP"] = pd.to_datetime(df["TIME_STAMP"])
        df = df.rename(columns={"TIME_STAMP": "date", "PX_OPEN": "o", "PX_HIGH": "h",
                                "PX_LOW": "l", "PX_CLOSE": "close"})
        df = df[df["close"] > 0].set_index("date")
        return df
    df = _cached_df(f"PX_HISTORY_{symbol}_{series}", build) if use_cache else build()
    return _slice(df, start, end)


def load_momentum(table, lookback, symbols=None, start=None, end=None, use_cache=True):
    """MOMENTUM_ABS (CUM_RET) or MOMENTUM_PROB (SCORE) for one lookback,
    wide pivot: index=date, columns=SYMBOL. `symbols` bounds the query
    (tests pass a small list; full-universe pivots are Phase-1 work)."""
    if table not in ("MOMENTUM_ABS", "MOMENTUM_PROB"):
        raise ValueError("table must be MOMENTUM_ABS or MOMENTUM_PROB")
    val = "CUM_RET" if table == "MOMENTUM_ABS" else "SCORE"

    def build():
        q = (f"SELECT SYMBOL, TIME_STAMP, {val} FROM {table} "
             f"WHERE LOOK_BACK = {int(lookback)}")
        if symbols:
            q += f" AND SYMBOL IN ({','.join(chr(39) + s + chr(39) for s in symbols)})"
        q += " ORDER BY TIME_STAMP"
        df = _sql(q)
        df["TIME_STAMP"] = pd.to_datetime(df["TIME_STAMP"])
        wide = df.pivot(index="TIME_STAMP", columns="SYMBOL", values=val)
        wide.index.name = "date"
        return wide
    key = f"{table}_{lookback}" + ("_all" if symbols is None
                                   else "_" + "-".join(sorted(symbols)))
    df = _cached_df(key, build) if use_cache else build()
    return _slice(df, start, end)


def load_eod_adjusted(ticker, start=None, end=None, use_cache=True):
    """Adjusted daily equity closes from PG eod_adjusted_nse."""
    def build():
        q = "SELECT date_stamp, o, h, l, c FROM eod_adjusted_nse WHERE ticker = %s ORDER BY date_stamp"
        df = _pg(q, params=(ticker,))
        df["date_stamp"] = pd.to_datetime(df["date_stamp"])
        df = df.rename(columns={"date_stamp": "date", "c": "close"})
        df = df[df["close"] > 0].set_index("date")
        return df
    df = _cached_df(f"eod_adjusted_nse_{ticker}", build) if use_cache else build()
    return _slice(df, start, end)


# ─────────────────────────── Intraday (SWEDEN) ───────────────────────────

def load_zd_index_bars(symbol, start=None, end=None, use_cache=True):
    """1-minute index candles. tick_stamp = Unix epoch (1970)."""
    def build():
        q = "SELECT time_stamp, tick_stamp, o, h, l, c, v FROM zd_index_bars WHERE symbol = %s"
        params = [symbol]
        if start or end:
            q += " AND tick_stamp >= %s AND tick_stamp <= %s"
            params += [encode_tick(pd.Timestamp(start or "1970-01-01"), 1970),
                       encode_tick(pd.Timestamp(end or "2100-01-01"), 1970)]
        q += " ORDER BY tick_stamp"
        df = _pg(q, params=tuple(params))
        df["ts_utc"] = decode_series(df["tick_stamp"], 1970)
        df = df.drop(columns=["tick_stamp"]).set_index("ts_utc")
        df = df.rename(columns={"time_stamp": "date"})
        return df
    df = _cached_df(f"zd_index_bars_{symbol}", build) if use_cache else build()
    return _slice(df, start, end)


def load_zd_option_bars(inst_token, start=None, end=None, use_cache=True):
    """1-minute NFO futures/options bars. tick_stamp = 1990 base — the
    DerivativeBarsDownloader base-date (research-plan §1.3)."""
    def build():
        q = ("SELECT inst_token, time_stamp, tick_stamp, o, h, l, c, v, oi "
             "FROM zd_option_bars WHERE inst_token = %s")
        params = [str(inst_token)]
        if start or end:
            q += " AND tick_stamp >= %s AND tick_stamp <= %s"
            params += [encode_tick(pd.Timestamp(start or "1990-01-01"), 1990),
                       encode_tick(pd.Timestamp(end or "2100-01-01"), 1990)]
        q += " ORDER BY tick_stamp"
        df = _pg(q, params=tuple(params))
        if df.empty:
            return df
        df["ts_utc"] = decode_series(df["tick_stamp"], 1990)
        df = df.drop(columns=["tick_stamp"]).set_index("ts_utc")
        df = df.rename(columns={"time_stamp": "date"})
        return df
    df = _cached_df(f"zd_option_bars_{inst_token}", build) if use_cache else build()
    return _slice(df, start, end)


def load_zd_bars_mcx(inst_token, start=None, end=None, use_cache=True):
    """1-minute MCX commodity futures bars. tick_stamp = Unix epoch (1970)."""
    def build():
        q = ("SELECT inst_token, time_stamp, tick_stamp, o, h, l, c, v, oi "
             "FROM zd_bars_mcx WHERE inst_token = %s")
        params = [int(inst_token)]
        if start or end:
            q += " AND tick_stamp >= %s AND tick_stamp <= %s"
            params += [encode_tick(pd.Timestamp(start or "1970-01-01"), 1970),
                       encode_tick(pd.Timestamp(end or "2100-01-01"), 1970)]
        q += " ORDER BY tick_stamp"
        df = _pg(q, params=tuple(params))
        if df.empty:
            return df
        df["ts_utc"] = decode_series(df["tick_stamp"], 1970)
        df = df.drop(columns=["tick_stamp"]).set_index("ts_utc")
        df = df.rename(columns={"time_stamp": "date"})
        return df
    df = _cached_df(f"zd_bars_mcx_{inst_token}", build) if use_cache else build()
    return _slice(df, start, end)


# ─────────────────────────── helpers ───────────────────────────

def _slice(df, start=None, end=None):
    if df is None or len(df) == 0:
        return df
    lo = df.index.min()
    hi = df.index.max()
    s = start if start else lo
    e = end if end else hi
    return df.loc[(df.index >= s) & (df.index <= e)]


def daily_returns(close_series):
    """Simple daily returns from a close series (house style: simple P&L)."""
    r = close_series.pct_change()
    return r.dropna()


def log_returns(close_series):
    return np.log(close_series / close_series.shift(1)).dropna()

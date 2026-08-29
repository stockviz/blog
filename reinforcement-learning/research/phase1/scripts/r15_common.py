"""R1.5 shared pieces — universe, per-stock feature loading (cached).

Universe: top-50 by free-float mcap (EQUITY_MISC_INFO FF_MKT_CAP_CR) as of
2019-12-31. Per-stock features: adjusted close (eod_adjusted_nse, avoids
split gaps), MOMENTUM_ABS 365 CUM_RET, MOMENTUM_PROB 365 SCORE, with causal
rolling z-scores (mom_z, prob_z, 60d). Cached as parquet under cache/r15/.
"""

import sys
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "phase0"))

from rl.data.loaders import load_eod_adjusted, load_momentum  # noqa: E402
from rl.envs.base import zscore_window  # noqa: E402

PHASE1 = Path(__file__).resolve().parents[1]
CACHE = PHASE1 / "cache" / "r15"
CACHE.mkdir(parents=True, exist_ok=True)

TOP50_UNIVERSE = [
    "HDFCBANK", "RELIANCE", "HDFC", "ICICIBANK", "INFY", "TCS", "KOTAKBANK",
    "ITC", "AXISBANK", "LT", "HINDUNILVR", "SBIN", "BAJFINANCE", "MARUTI",
    "INDUSINDBK", "BHARTIARTL", "ASIANPAINT", "HCLTECH", "BAJAJFINSV",
    "NESTLEIND", "NTPC", "M&M", "TITAN", "TECHM", "SUNPHARMA", "ULTRACEMCO",
    "POWERGRID", "ONGC", "BAJAJ-AUTO", "BPCL", "COALINDIA", "HDFCLIFE",
    "WIPRO", "SBILIFE", "BRITANNIA", "TATASTEEL", "DRREDDY", "TMPV",
    "HEROMOTOCO", "IOC", "UPL", "HINDALCO", "EICHERMOT", "GRASIM",
    "ADANIPORTS", "VEDL", "ICICIGI", "JSWSTEEL", "SHREECEM", "DABUR",
]

TRAIN_START = "2015-09-02"   # MOMENTUM_ABS availability
TRAIN_END = "2019-12-31"     # house train cutoff
TEST_START = "2020-05-01"    # house post window

STATE_RICH_COLS = ["mom_z", "prob_z", "vix_z", "vix_chg_z",
                   "tvt_mid", "tvt_small", "mom_rank", "rel12m"]

_CACHED = {}   # module-level memo of the shared feature parquets


def _shared(mf, rk, ew):
    key = "shared"
    if key not in _CACHED:
        _CACHED[key] = (pd.read_parquet(mf), pd.read_parquet(rk),
                        pd.read_parquet(ew)["ew12m"])
    return _CACHED[key]


def build_market_feats(force=False):
    """Daily market-regime features (causal): VIX z/change z + TVT p_off."""
    path = CACHE / "market_feats_daily.parquet"
    if path.exists() and not force:
        return pd.read_parquet(path)
    from rl.data.loaders import load_vix_history
    vix = load_vix_history()["close"]
    vix_z = zscore_window(vix, 60).rename("vix_z")
    vix_chg_z = zscore_window(vix.pct_change(), 20).rename("vix_chg_z")
    mf = pd.concat([vix_z, vix_chg_z], axis=1)
    for col, fname in [("tvt_mid", "tvt_filt_NIFTY_MIDCAP_150_TR.csv"),
                       ("tvt_small", "tvt_filt_NIFTY_SMALLCAP_250_TR.csv")]:
        df = pd.read_csv(PHASE1 / "cache" / fname, parse_dates=["date"]).set_index("date")
        mf[col] = df["p_off"]
    mf.to_parquet(path)
    return mf


def build_cross_sectional(force=False):
    """Per-date momentum rank percentile + universe EW 12m return (FF60)."""
    rank_path = CACHE / "cs_rank.parquet"
    ew_path = CACHE / "cs_ew12m.parquet"
    if rank_path.exists() and ew_path.exists() and not force:
        return pd.read_parquet(rank_path), pd.read_parquet(ew_path)["ew12m"]
    mom_cols = {}
    for fp in sorted(CACHE.glob("*.parquet")):
        if "market_feats" in fp.name or "cs_" in fp.name:
            continue
        try:
            df = pd.read_parquet(fp)
        except Exception:
            continue
        if "mom_abs" in df.columns:
            mom_cols[fp.stem] = df["mom_abs"]
    mom = pd.DataFrame(mom_cols)
    rank = mom.rank(axis=1, pct=True, method="first")   # 0..1 percentile
    ew12m = mom.mean(axis=1, skipna=True).rename("ew12m")
    rank.to_parquet(rank_path)
    ew12m.to_frame().to_parquet(ew_path)
    return rank, ew12m


def build_ff_universe(pct=0.60, asof="2019-12-31", force=False):
    """Top-`pct` of names by free-float mcap rank (R2 FF-tier convention:
    EQUITY_MISC_INFO.FF_MKT_CAP_CR percent-rank over the full market).
    asof snapshot (train-time, no test lookahead). Cached as CSV."""
    path = CACHE / f"universe_ff{int(pct*100)}.csv"
    if path.exists() and not force:
        return pd.read_csv(path)["SYMBOL"].tolist()
    from rl.config import mssql_conn
    cn = mssql_conn()
    ff = pd.read_sql(f"""
        SELECT SYMBOL, FF_MKT_CAP_CR FROM EQUITY_MISC_INFO
        WHERE TIME_STAMP = '{asof}' AND FF_MKT_CAP_CR > 0
        ORDER BY FF_MKT_CAP_CR DESC""", cn)
    cn.close()
    thr = ff["FF_MKT_CAP_CR"].quantile(1.0 - pct)
    uni = ff[ff["FF_MKT_CAP_CR"] >= thr][["SYMBOL", "FF_MKT_CAP_CR"]]
    uni.to_csv(path, index=False)
    return uni["SYMBOL"].tolist()


def get_universe(which):
    if which == "top50":
        return list(TOP50_UNIVERSE)
    if which == "ff60":
        return build_ff_universe(0.60)
    raise ValueError(f"unknown universe {which}")


def load_stock_features(symbol, force=False):
    """Aligned per-stock frame: close, ret, mom_abs, mom_z, prob, prob_z.
    Cached as parquet (idempotent); R1.7 rich-state columns (VIX z/change,
    TVT p_off, cross-sectional momentum rank, 12m relative strength) are
    joined at load time from the shared caches."""
    path = CACHE / f"{symbol}.parquet"
    if path.exists() and not force:
        df = pd.read_parquet(path)
    else:
        df = None
    if df is None:
        close = load_eod_adjusted(symbol)["close"]
        mom = load_momentum("MOMENTUM_ABS", 365, symbols=[symbol])
        prob = load_momentum("MOMENTUM_PROB", 365, symbols=[symbol])
        df = pd.DataFrame({"close": close})
        df["ret"] = close.pct_change()
        if symbol in mom.columns:
            df["mom_abs"] = mom[symbol]
            df["mom_z"] = zscore_window(mom[symbol], 60)
        if symbol in prob.columns:
            df["prob"] = prob[symbol]
            df["prob_z"] = zscore_window(prob[symbol], 60)
        df = df.dropna(subset=["close"])
        df.to_parquet(path)
    # R1.7 rich state — join shared caches if present (no per-stock rebuild)
    mf = CACHE / "market_feats_daily.parquet"
    rk = CACHE / "cs_rank.parquet"
    ew = CACHE / "cs_ew12m.parquet"
    if mf.exists() and rk.exists() and ew.exists():
        mfv, rank, ew12m = _shared(mf, rk, ew)
        df = df.join(mfv, how="left")
        if symbol in rank.columns:
            df["mom_rank"] = rank[symbol]
        if "mom_abs" in df.columns:
            df["rel12m"] = df["mom_abs"] - ew12m.reindex(df.index).ffill()
    return df


def train_test_split(df):
    """House windows: train <= 2019-12-31, test >= 2020-05-01."""
    tr = df[(df.index >= TRAIN_START) & (df.index <= TRAIN_END)]
    te = df[df.index >= TEST_START]
    return tr, te

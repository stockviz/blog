"""R1.6 shared pieces — FF60 sleeve series + regime features (weekly).

Sleeves (all long-only, EW within sleeve):
  EW    : equal-weight FF60 market book
  Q5    : top-20% by MOMENTUM_ABS 365 CUM_RET (month-end rank, held next month)
  Q1    : bottom-20% by the same rank
  CASH  : 0
Regime features (weekly): VIX z, TVT-HMM p_off (MID150, SMALL250), breadth
(fraction of FF60 names with positive momentum at week end).
"""

import sys
from pathlib import Path

import numpy as np
import pandas as pd

PHASE1 = Path(__file__).resolve().parents[1]
CACHE = PHASE1 / "cache" / "r16"
CACHE.mkdir(parents=True, exist_ok=True)
R15_CACHE = PHASE1 / "cache" / "r15"

SLEEVES = ["EW", "Q5", "Q1", "CASH"]
TRAIN_START = "2015-09-02"   # MOMENTUM_ABS availability
TRAIN_END = "2019-12-31"     # house train cutoff
TEST_START = "2020-05-01"    # house post window
COST = 5 / 10000.0


def _load_all_stocks():
    """Daily returns + monthly momentum matrices from the r15 parquet cache."""
    files = sorted(R15_CACHE.glob("*.parquet"))
    rets_cols, mom_cols = {}, {}
    for fp in files:
        try:
            df = pd.read_parquet(fp)
        except Exception:
            continue
        if "close" not in df.columns:
            continue
        r = df["close"].pct_change()
        rets_cols[fp.stem] = r
        if "mom_abs" in df.columns:
            mom_cols[fp.stem] = df["mom_abs"]
    rets = pd.DataFrame(rets_cols)
    mom = pd.DataFrame(mom_cols)
    return rets, mom


def _weekly(df):
    return (1.0 + df).resample("W-FRI").prod() - 1.0


def build_sleeves(force=False):
    """Daily sleeve returns -> weekly returns + weekly regime feats (cached)."""
    out_rets = CACHE / "sleeves_weekly.parquet"
    out_feats = CACHE / "feats_weekly.parquet"
    if out_rets.exists() and out_feats.exists() and not force:
        return pd.read_parquet(out_rets), pd.read_parquet(out_feats)

    print("loading per-stock cache...")
    rets, mom = _load_all_stocks()
    rets = rets[rets.index >= TRAIN_START]
    mom = mom[mom.index >= TRAIN_START]
    print(f"{rets.shape[1]} stocks, {len(rets)} daily rows")

    # month-end membership: rank by CUM_RET at month end, hold next month
    month_ends = pd.Series(rets.index, index=rets.index).resample("M").last().dropna()
    members = {}
    q = 0.20
    for me in month_ends:
        me = pd.Timestamp(me)
        m = mom.loc[:me].iloc[-1] if (mom.index <= me).any() else None
        if m is None or m.notna().sum() < 30:
            continue
        ranks = m.rank(ascending=False, method="first")
        n = int(np.ceil(ranks.notna().sum() * q))
        members[me] = {
            "Q5": ranks[ranks <= n].index.tolist(),
            "Q1": ranks[ranks > ranks.notna().sum() - n].index.tolist(),
        }

    # daily sleeve returns (membership held until next month-end)
    ew = rets.mean(axis=1, skipna=True)
    q5 = pd.Series(np.nan, index=rets.index)
    q1 = pd.Series(np.nan, index=rets.index)
    sorted_me = sorted(members)
    for i, me in enumerate(sorted_me):
        end = sorted_me[i + 1] if i + 1 < len(sorted_me) else rets.index[-1]
        seg = rets.loc[me:end]
        q5.loc[seg.index] = seg[members[me]["Q5"]].mean(axis=1, skipna=True)
        q1.loc[seg.index] = seg[members[me]["Q1"]].mean(axis=1, skipna=True)
    cash = pd.Series(0.0, index=rets.index)

    daily = pd.DataFrame({"EW": ew, "Q5": q5, "Q1": q1, "CASH": cash})
    weekly = _weekly(daily).dropna()
    weekly.to_parquet(out_rets)

    # regime features (weekly)
    sys.path.insert(0, str(PHASE1.parent / "phase0"))
    from rl.data.loaders import load_vix_history
    vix = load_vix_history(start=TRAIN_START)["close"]
    feats = pd.DataFrame(index=weekly.index)
    vix_w = vix.reindex(weekly.index, method="ffill")
    feats["vix_z"] = ((vix_w - vix_w.rolling(52).mean()) / vix_w.rolling(52).std())
    for col, fname in [("tvt_mid", "tvt_filt_NIFTY_MIDCAP_150_TR.csv"),
                       ("tvt_small", "tvt_filt_NIFTY_SMALLCAP_250_TR.csv")]:
        df = pd.read_csv(PHASE1 / "cache" / fname, parse_dates=["date"]).set_index("date")
        feats[col] = df["p_off"].reindex(weekly.index, method="ffill")
    # breadth: fraction of names with positive momentum at week end
    mom_ok = mom[mom.index.isin(weekly.index)].gt(0).mean(axis=1)
    feats["breadth"] = mom_ok
    feats = feats.ffill().dropna()
    feats.to_parquet(out_feats)

    weekly = weekly.loc[feats.index]
    weekly.to_parquet(out_rets)
    print(f"sleeves: {weekly.shape[0]} weeks x {weekly.shape[1]}, feats: {feats.shape[1]}")
    return weekly, feats


if __name__ == "__main__":
    r, f = build_sleeves()
    print(r.tail(3).round(4).to_string())
    print(f.tail(3).round(3).to_string())

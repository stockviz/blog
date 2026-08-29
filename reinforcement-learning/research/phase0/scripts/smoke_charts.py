"""Phase-0 end-to-end smoke: house-convention artifacts on REAL data.

Loads NIFTY 50 daily, builds three baselines (buy&hold, MA-cross rule,
flat), and emits pre/post/full metrics CSV + stacked cum+drawdown charts
with end-labeled series and @StockViz captions. This is the toolchain's
integration test and the honest "replay" sanity for Phase 1 (R1.1 will
compare TDQN against these exact baselines).

Usage: python scripts/smoke_charts.py [--force-cache]
"""

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import pandas as pd

from rl.data.loaders import load_bhav_index, daily_returns
from rl.eval.metrics import metrics_by_period, PERIOD_LAB
from rl.eval.baselines import ma_cross_rule
from rl.eval.charts import plot_cum_drawdown

ARTIFACTS = Path(__file__).resolve().parents[1] / "artifacts"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--force-cache", action="store_true")
    args = ap.parse_args()

    close = load_bhav_index("NIFTY 50", start="2000-01-01")["close"]
    r_idx = daily_returns(close)

    _, ma = ma_cross_rule(close, fast=20, slow=50)
    bh = r_idx
    flat = pd.Series(0.0, index=r_idx.index)

    series = {"B&H NIFTY 50": bh, "MA20/50 rule": ma.reindex(r_idx.index).fillna(0.0), "Flat": flat}

    # metrics pre/post/full
    rows = []
    for name, s in series.items():
        m = metrics_by_period(s)
        m.insert(0, "series", name)
        rows.append(m)
    out = pd.concat(rows)
    out.to_csv(ARTIFACTS / "smoke_metrics.csv", index=False)
    print(out.to_string(index=False))

    # charts pre/post/full
    for pn in ["pre", "post", "full"]:
        rng = None
        if pn == "pre":
            rng = (pd.Timestamp("2000-01-01"), pd.Timestamp("2019-12-31"))
        elif pn == "post":
            rng = (pd.Timestamp("2020-05-01"), None)
        plot_cum_drawdown(
            {k: v for k, v in series.items()},
            f"NIFTY 50 daily baselines — {PERIOD_LAB[pn]} (Phase 0 smoke)",
            ARTIFACTS / f"smoke_cumulative_{pn}.png",
            date_range=rng,
        )
        print(f"wrote smoke_cumulative_{pn}.png")


if __name__ == "__main__":
    main()

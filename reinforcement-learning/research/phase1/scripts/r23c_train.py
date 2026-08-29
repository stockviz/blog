"""R2.3 producer (plan spec) — risk-aversion arms on NIFTY 50 1-min.

State upgrade: 1-min INDIA VIX (rolling z) + TVT-HMM p_off (daily ->
ffill to 1-min) as extra features in the Si observation; vol-conditioned
beta (R2.3: beta scales with day vol / sigma_ref so high-vol days are
risk-averse). Arms:
  feats_sr      : SR-type reward + VIX/p_off features
  feats_u21_vc  : U(2,1) + features + vol-conditioned beta

Usage: python scripts/r23c_train.py --arm {feats_sr,feats_u21_vc}
"""

import argparse
import json
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd
import torch

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "phase0"))

torch.set_num_threads(1)

from rl.data.loaders import load_zd_index_bars  # noqa: E402
from rl.envs.si_intraday_env import SiIntradayEnv  # noqa: E402
from rl.envs.base import zscore_window  # noqa: E402
from rl.agents.si_policy import SiAgent  # noqa: E402

PHASE1 = Path(__file__).resolve().parents[1]
LOOKBACK = 30
COST = 5 / 10000.0
ARMS = {"feats_sr": {"reward": "sr", "alpha": 1.0, "beta": 1.0, "vol_cond": False},
        "feats_u21_vc": {"reward": "u21", "alpha": 2.0, "beta": 1.0, "vol_cond": True}}


def build_feats(bars):
    """1-min VIX z + TVT p_off aligned to the index bars (causal ffill)."""
    vix = load_zd_index_bars("INDIA VIX", start="2015-01-01")["c"]
    vix_z = zscore_window(vix, 60).rename("vix_z")
    feats = pd.DataFrame({"vix_z": vix_z})
    for col, fname in [("tvt_mid", "tvt_filt_NIFTY_MIDCAP_150_TR.csv"),
                       ("tvt_small", "tvt_filt_NIFTY_SMALLCAP_250_TR.csv")]:
        df = pd.read_csv(PHASE1 / "cache" / fname, parse_dates=["date"]).set_index("date")
        feats[col] = df["p_off"]
    feats.index = pd.to_datetime(feats.index, utc=True)
    return feats.reindex(bars.index).ffill()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--arm", choices=list(ARMS), required=True)
    ap.add_argument("--days", type=int, default=400)
    ap.add_argument("--epochs", type=int, default=4)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()
    cfg = ARMS[args.arm]

    OUT = PHASE1 / "runs" / f"r23c_{args.arm}"
    CKPT = OUT / "checkpoints"
    CKPT.mkdir(parents=True, exist_ok=True)

    bars = load_zd_index_bars("NIFTY 50", start="2015-01-01")
    feats = build_feats(bars)
    env = SiIntradayEnv(bars, lookback=LOOKBACK, cost=COST,
                        alpha=cfg["alpha"], beta=cfg["beta"], extra_feats=feats)
    all_days = sorted({pd.Timestamp(ts).date() for ts in bars.index})
    train_days = [d for d in all_days if d <= pd.Timestamp("2019-12-31").date()]
    test_days = [d for d in all_days if d >= pd.Timestamp("2020-05-01").date()]

    # sigma_ref = median per-day return vol over the train window
    day_sig = []
    for d in np.random.default_rng(0).choice(train_days, 120, replace=False):
        mask = bars.index.date == d
        day_sig.append(float(bars.loc[mask, "c"].pct_change().std(ddof=0)))
    sigma_ref = float(np.median(day_sig))
    print(f"{args.arm}: {len(train_days)} train / {len(test_days)} test days, "
          f"sigma_ref {sigma_ref:.6f}")

    rng = np.random.default_rng(args.seed)
    sample = list(train_days)
    rng.shuffle(sample)
    sample = sample[:args.days]

    reward_mode = "SR" if cfg["reward"] == "sr" else "U"
    agent = SiAgent(env, lr=1e-3, seed=args.seed, reward_mode=reward_mode,
                    vol_cond=cfg["vol_cond"], sigma_ref=sigma_ref)
    u_trace = []
    t0 = time.time()
    for i, d in enumerate(sample):
        try:
            u = agent.train_day(d, epochs=args.epochs)
        except ValueError as exc:
            print(f"  skip {d}: {exc}")
            continue
        u_trace.append(u)
        if (i + 1) % 50 == 0:
            print(f"  day {i+1}/{len(sample)}  mean_obj(last50)={np.mean(u_trace[-50:]):+.6f} "
                  f"({time.time()-t0:.0f}s)")

    torch.save(agent.net.state_dict(), CKPT / "si_policy.pt")
    np.save(CKPT / "obj_trace.npy", np.array(u_trace))
    meta = {
        "algo": "Si (S6) + R2.3 risk-aversion: VIX-1min/p_off state, vol-cond beta",
        "symbol": "NIFTY 50", "arm": args.arm, "reward_mode": reward_mode,
        "alpha": cfg["alpha"], "beta": cfg["beta"], "vol_cond": cfg["vol_cond"],
        "sigma_ref": float(sigma_ref), "feats": list(feats.columns),
        "lookback": LOOKBACK, "cost_bps": int(COST * 10000),
        "train_start": str(train_days[0]), "train_end": str(train_days[-1]),
        "test_start": str(test_days[0]), "test_end": str(test_days[-1]),
        "train_days_sampled": len(sample), "epochs_per_day": args.epochs, "seed": args.seed,
        "mean_obj_last50": float(np.mean(u_trace[-50:]) if u_trace else 0.0),
    }
    (CKPT / "params.json").write_text(json.dumps(meta, indent=2))
    print("saved:", sorted(p.name for p in CKPT.iterdir()))
    print("Train complete.")


if __name__ == "__main__":
    main()

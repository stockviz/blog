"""R2.1 producer — Si et al. (2017, S6) intraday policy on NIFTY 50 1-min.

Episode = one trading day (375 bars, 09:15-15:29 IST = 03:45-09:59 UTC);
policy = FC feature learner -> LSTM -> tanh (action = target position in
[-1,1]); reward = U = alpha*mean(DR) - beta*std(DR) per day (S6 Eq.5),
costs inside DR; trained by BPTT on -U (S6 Eq.8). Train days 2015-01-09 ..
2019-12-31 (house cutoff), test >= 2020-05-01.

Usage: python scripts/r21_train.py [--days 400] [--epochs 4] [--seed 42]
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
from rl.agents.si_policy import SiAgent  # noqa: E402

PHASE1 = Path(__file__).resolve().parents[1]
LOOKBACK = 30
COST = 5 / 10000.0
ALPHA, BETA = 1.0, 1.0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--days", type=int, default=400, help="train days to sample")
    ap.add_argument("--epochs", type=int, default=4, help="BPTT epochs per day")
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--tag", default="r21")
    args = ap.parse_args()

    OUT = PHASE1 / "runs" / args.tag
    CKPT = OUT / "checkpoints"
    CKPT.mkdir(parents=True, exist_ok=True)

    bars = load_zd_index_bars("NIFTY 50", start="2015-01-01")
    env = SiIntradayEnv(bars, lookback=LOOKBACK, cost=COST, alpha=ALPHA, beta=BETA)
    all_days = sorted({pd.Timestamp(ts).date() for ts in bars.index})
    train_days = [d for d in all_days if d <= pd.Timestamp("2019-12-31").date()]
    test_days = [d for d in all_days if d >= pd.Timestamp("2020-05-01").date()]
    print(f"days: {len(all_days)} total | train {len(train_days)} | test {len(test_days)}")

    rng = np.random.default_rng(args.seed)
    sample = list(train_days)
    rng.shuffle(sample)
    sample = sample[:args.days]
    print(f"training on {len(sample)} sampled days x {args.epochs} epochs")

    agent = SiAgent(env, lr=1e-3, seed=args.seed)
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
            print(f"  day {i+1}/{len(sample)}  mean_U(last50)={np.mean(u_trace[-50:]):+.6f} "
                  f"({time.time()-t0:.0f}s)")

    torch.save(agent.net.state_dict(), CKPT / "si_policy.pt")
    np.save(CKPT / "u_trace.npy", np.array(u_trace))
    meta = {
        "algo": "Si et al. (S6): FC learner -> LSTM -> tanh, BPTT on U",
        "symbol": "NIFTY 50", "bars_per_day": 375,
        "lookback": LOOKBACK, "cost_bps": int(COST * 10000),
        "alpha": ALPHA, "beta": BETA,
        "train_start": str(train_days[0]), "train_end": str(train_days[-1]),
        "test_start": str(test_days[0]), "test_end": str(test_days[-1]),
        "train_days": len(sample), "epochs_per_day": args.epochs, "seed": args.seed,
        "mean_U_last50": float(np.mean(u_trace[-50:]) if u_trace else 0.0),
    }
    (CKPT / "params.json").write_text(json.dumps(meta, indent=2))
    print("saved:", sorted(p.name for p in CKPT.iterdir()))
    print("Train complete.")


if __name__ == "__main__":
    main()

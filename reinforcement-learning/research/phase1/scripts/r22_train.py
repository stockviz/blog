"""R2.2 producer — Si intraday policy on NIFTY BANK 1-min, reward redesign.

Arms (identical config, reward differs; R2.1 taught us U(1,1) makes flat
optimal, so we test an earnings-push and a Sharpe-type objective):
  u11 : U = 1*mean(DR) - 1*std(DR)   (R2.1 control)
  u21 : U = 2*mean(DR) - 1*std(DR)   (earnings push)
  sr  : U = mean(DR)/std(DR)         (per-day Sharpe; flat -> 0)

Usage: python scripts/r22_train.py --reward {u11,u21,sr} [--days 400]
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
AB = {"u11": (1.0, 1.0), "u21": (2.0, 1.0), "sr": (1.0, 1.0)}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--reward", choices=["u11", "u21", "sr"], required=True)
    ap.add_argument("--symbol", default="NIFTY BANK")
    ap.add_argument("--days", type=int, default=400)
    ap.add_argument("--epochs", type=int, default=4)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()

    OUT = PHASE1 / "runs" / f"r22_{args.reward}"
    CKPT = OUT / "checkpoints"
    CKPT.mkdir(parents=True, exist_ok=True)
    alpha, beta = AB[args.reward]
    reward_mode = "SR" if args.reward == "sr" else "U"

    bars = load_zd_index_bars(args.symbol, start="2015-01-01")
    env = SiIntradayEnv(bars, lookback=LOOKBACK, cost=COST, alpha=alpha, beta=beta)
    all_days = sorted({pd.Timestamp(ts).date() for ts in bars.index})
    train_days = [d for d in all_days if d <= pd.Timestamp("2019-12-31").date()]
    test_days = [d for d in all_days if d >= pd.Timestamp("2020-05-01").date()]
    print(f"{args.symbol}: {len(all_days)} days | train {len(train_days)} | test {len(test_days)}")

    rng = np.random.default_rng(args.seed)
    sample = list(train_days)
    rng.shuffle(sample)
    sample = sample[:args.days]
    print(f"reward {args.reward} ({reward_mode}, a={alpha}, b={beta}) on "
          f"{len(sample)} days x {args.epochs} epochs")

    agent = SiAgent(env, lr=1e-3, seed=args.seed, reward_mode=reward_mode)
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
        "algo": "Si et al. (S6): FC learner -> LSTM -> tanh, BPTT on objective",
        "symbol": args.symbol, "reward": args.reward, "reward_mode": reward_mode,
        "alpha": alpha, "beta": beta, "bars_per_day": 375,
        "lookback": LOOKBACK, "cost_bps": int(COST * 10000),
        "train_start": str(train_days[0]), "train_end": str(train_days[-1]),
        "test_start": str(test_days[0]), "test_end": str(test_days[-1]),
        "train_days": len(sample), "epochs_per_day": args.epochs, "seed": args.seed,
        "mean_obj_last50": float(np.mean(u_trace[-50:]) if u_trace else 0.0),
    }
    (CKPT / "params.json").write_text(json.dumps(meta, indent=2))
    print("saved:", sorted(p.name for p in CKPT.iterdir()))
    print("Train complete.")


if __name__ == "__main__":
    main()

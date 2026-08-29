"""R2.3b producer — Si intraday policy on the MCX GOLD INDEX (MCXGOLDEX).

Per user directive: use MCX INDICES, not futures (no expiry/roll issues,
no duplicate zd_master tokens). MCXGOLDEX lives in zd_index_bars with
continuous coverage 2020-11-23 .. now (~1,409 real sessions, NSE-session
hours) — pre-2020 is a sparse backfill (1 real session day). House windows
do not exist for this instrument: train = first 60% of days, test = last
40%. Arms: u21 (U = 2*mean - 1*std) and sr (Sharpe-type).

Usage: python scripts/r23b_train.py --reward {u21,sr} [--days 400]
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
SYMBOL = "MCXGOLDEX"
AB = {"u21": (2.0, 1.0), "sr": (1.0, 1.0)}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--reward", choices=["u21", "sr"], required=True)
    ap.add_argument("--days", type=int, default=400, help="train days to sample")
    ap.add_argument("--epochs", type=int, default=4)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()

    OUT = PHASE1 / "runs" / f"r23b_{args.reward}"
    CKPT = OUT / "checkpoints"
    CKPT.mkdir(parents=True, exist_ok=True)
    alpha, beta = AB[args.reward]
    reward_mode = "SR" if args.reward == "sr" else "U"

    bars = load_zd_index_bars(SYMBOL)
    env = SiIntradayEnv(bars, lookback=LOOKBACK, cost=COST, alpha=alpha, beta=beta)
    cnt = bars.groupby(bars.index.date).size()
    all_days = sorted(cnt[cnt >= 200].index.tolist())  # real sessions only
    print(f"{SYMBOL}: {len(all_days)} real sessions (of {len(cnt)} total)")
    split = int(len(all_days) * 0.6)
    train_days = all_days[:split]
    test_days = all_days[split:]
    print(f"train {len(train_days)} ({train_days[0]}..{train_days[-1]}) | "
          f"test {len(test_days)} ({test_days[0]}..{test_days[-1]})")

    rng = np.random.default_rng(args.seed)
    sample = list(train_days)
    rng.shuffle(sample)
    sample = sample[:args.days]
    print(f"reward {args.reward} ({reward_mode}, a={alpha}, b={beta}) on "
          f"{len(sample)} sampled days x {args.epochs} epochs")

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
        "symbol": SYMBOL, "venue": "MCX index (no expiry)",
        "reward": args.reward, "reward_mode": reward_mode,
        "alpha": alpha, "beta": beta,
        "lookback": LOOKBACK, "cost_bps": int(COST * 10000),
        "train_start": str(train_days[0]), "train_end": str(train_days[-1]),
        "test_start": str(test_days[0]), "test_end": str(test_days[-1]),
        "split_note": "no house windows: index coverage starts 2020-11; train=first 60%",
        "train_days_sampled": len(sample), "epochs_per_day": args.epochs, "seed": args.seed,
        "mean_obj_last50": float(np.mean(u_trace[-50:]) if u_trace else 0.0),
    }
    (CKPT / "params.json").write_text(json.dumps(meta, indent=2))
    print("saved:", sorted(p.name for p in CKPT.iterdir()))
    print("Train complete.")


if __name__ == "__main__":
    main()

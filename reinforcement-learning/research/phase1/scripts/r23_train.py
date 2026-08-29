"""R2.3 producer — Si intraday policy on MCX GOLD 1-min (overnight-less venue).

MCX history is short (~2025-11-10 on, one liquid contract GOLD26OCTFUT), so
the house windows do NOT exist for this instrument: train = first 60% of
days, test = last 40% (plan: emit only the windows that exist and note why).
Arms: u21 (U = 2*mean - 1*std) and sr (Sharpe-type) — the two that trade.

Usage: python scripts/r23_train.py --reward {u21,sr} [--epochs 4]
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

from rl.data.loaders import load_zd_bars_mcx  # noqa: E402
from rl.envs.si_intraday_env import SiIntradayEnv  # noqa: E402
from rl.agents.si_policy import SiAgent  # noqa: E402

PHASE1 = Path(__file__).resolve().parents[1]
LOOKBACK = 30
COST = 5 / 10000.0
GOLD_TOKEN = 123668231   # verified: the inst_token that actually holds the bars
GOLD_SYMBOL = "GOLD26OCTFUT"
AB = {"u21": (2.0, 1.0), "sr": (1.0, 1.0)}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--reward", choices=["u21", "sr"], required=True)
    ap.add_argument("--epochs", type=int, default=4)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()

    OUT = PHASE1 / "runs" / f"r23_{args.reward}"
    CKPT = OUT / "checkpoints"
    CKPT.mkdir(parents=True, exist_ok=True)
    alpha, beta = AB[args.reward]
    reward_mode = "SR" if args.reward == "sr" else "U"

    bars = load_zd_bars_mcx(GOLD_TOKEN)
    env = SiIntradayEnv(bars, lookback=LOOKBACK, cost=COST, alpha=alpha, beta=beta)
    cnt = bars.groupby(bars.index.date).size()
    all_days = sorted(cnt[cnt >= 200].index.tolist())  # real sessions only
    print(f"filtered to {len(all_days)} days with >= 200 bars (of {len(cnt)} total)")
    split = int(len(all_days) * 0.6)
    train_days = all_days[:split]
    test_days = all_days[split:]
    print(f"{GOLD_SYMBOL}: {len(all_days)} days | train {len(train_days)} | "
          f"test {len(test_days)} (no house windows: data starts 2025-11)")

    agent = SiAgent(env, lr=1e-3, seed=args.seed, reward_mode=reward_mode)
    u_trace = []
    t0 = time.time()
    for i, d in enumerate(train_days):
        try:
            u = agent.train_day(d, epochs=args.epochs)
        except ValueError as exc:
            print(f"  skip {d}: {exc}")
            continue
        u_trace.append(u)
        if (i + 1) % 20 == 0:
            print(f"  day {i+1}/{len(train_days)}  mean_obj(last20)={np.mean(u_trace[-20:]):+.6f} "
                  f"({time.time()-t0:.0f}s)")

    torch.save(agent.net.state_dict(), CKPT / "si_policy.pt")
    np.save(CKPT / "obj_trace.npy", np.array(u_trace))
    meta = {
        "algo": "Si et al. (S6): FC learner -> LSTM -> tanh, BPTT on objective",
        "symbol": GOLD_SYMBOL, "inst_token": GOLD_TOKEN,
        "reward": args.reward, "reward_mode": reward_mode,
        "alpha": alpha, "beta": beta,
        "lookback": LOOKBACK, "cost_bps": int(COST * 10000),
        "train_start": str(train_days[0]), "train_end": str(train_days[-1]),
        "test_start": str(test_days[0]), "test_end": str(test_days[-1]),
        "split_note": "no house windows: instrument data starts 2025-11; train=first 60%",
        "epochs_per_day": args.epochs, "seed": args.seed,
        "mean_obj_last20": float(np.mean(u_trace[-20:]) if u_trace else 0.0),
    }
    (CKPT / "params.json").write_text(json.dumps(meta, indent=2))
    print("saved:", sorted(p.name for p in CKPT.iterdir()))
    print("Train complete.")


if __name__ == "__main__":
    main()

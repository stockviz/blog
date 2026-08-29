"""R1.6 producer — PPO continuous allocation over FF60 sleeves (weekly).

S4-style regime-neutral training: sleeve returns de-drifted by their
train-window means (the R1.5 drift-betting lesson), state = 12-week
z-scores of the 4 sleeves + regime feats (VIX, TVT-HMM p_off, breadth)
+ current weights. Train 2015-09 .. 2019-12, test >= 2020-05-01.

Usage: python scripts/r16_train.py [--timesteps 20000] [--seed 42]
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
sys.path.insert(0, str(Path(__file__).resolve().parent))

torch.set_num_threads(1)  # deterministic CPU training

from rl.envs.allocation_env import SleeveAllocationEnv  # noqa: E402
from rl.agents.ppo import make_ppo, train_sb3  # noqa: E402
from r16_common import (TRAIN_START, TRAIN_END, COST, build_sleeves,  # noqa: E402
                        SLEEVES, PHASE1)

LOOKBACK = 12
HORIZON = 60  # weeks per episode


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--timesteps", type=int, default=20000)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--tag", default="r16")
    args = ap.parse_args()

    OUT = PHASE1 / "runs" / args.tag
    CKPT = OUT / "checkpoints"
    CKPT.mkdir(parents=True, exist_ok=True)

    weekly, feats = build_sleeves()
    tr_rets = weekly[(weekly.index >= TRAIN_START) & (weekly.index <= TRAIN_END)]
    tr_feats = feats.reindex(tr_rets.index).ffill()

    # train-time regime neutralisation (R1.5 lesson)
    drift = tr_rets.mean()
    print(f"train weeks: {len(tr_rets)}; per-sleeve weekly drift:\n{drift.round(5).to_string()}")

    env = SleeveAllocationEnv(tr_rets, feats=tr_feats, lookback=LOOKBACK,
                              cost=COST, horizon=HORIZON,
                              de_drift=drift.to_dict())
    print(f"obs dim {env.observation_space.shape[0]}, action dim {env.action_space.shape[0]}")

    model, venv = make_ppo(env, seed=args.seed)
    t0 = time.time()
    train_sb3(model, venv, total_timesteps=args.timesteps)
    print(f"trained {args.timesteps} steps in {time.time()-t0:.0f}s")

    model.save(CKPT / "ppo.zip")
    meta = {
        "algo": "SB3 PPO, continuous simplex allocation (R1.6)",
        "sleeves": SLEEVES, "cost_bps": int(COST * 10000),
        "lookback_weeks": LOOKBACK, "horizon_weeks": HORIZON,
        "train_start": str(tr_rets.index[0].date()), "train_end": str(tr_rets.index[-1].date()),
        "de_drift": {k: float(v) for k, v in drift.items()},
        "feats": list(feats.columns), "timesteps": args.timesteps, "seed": args.seed,
    }
    (CKPT / "params.json").write_text(json.dumps(meta, indent=2))
    print("saved:", sorted(p.name for p in CKPT.iterdir()))
    print("Train complete.")


if __name__ == "__main__":
    main()

"""R2.2 producer (plan spec) — algorithm comparison on NIFTY 50 1-min.

PPO+LSTM (KB 09 §9.5 stability uplift, via sb3_contrib RecurrentPPO) vs
PPO-MLP on the SiIntradayEnv
(same state, 1-min actions, per-bar net pnl reward = discounted PnL
objective). Compare sample efficiency (learning curves) and net per-day
Sharpe on the house test window.

NOTE: the plan's A3C+LSTM (S7) arm is NOT available in this stack — SB3
2.9 removed recurrent A2C and sb3_contrib ships only RecurrentPPO.
Documented as a limitation; the A2C-LSTM phase0 wrapper (rl/agents/ppo.py
make_a2c_lstm) fails on SB3 2.9 for the same reason.

Usage: python scripts/r22b_train.py --arm {ppo_mlp,ppo_lstm,a2c_lstm}
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

from stable_baselines3 import PPO  # noqa: E402
from stable_baselines3.common.env_util import make_vec_env  # noqa: E402
from sb3_contrib import RecurrentPPO  # noqa: E402
from rl.data.loaders import load_zd_index_bars  # noqa: E402
from rl.envs.si_intraday_env import SiIntradayEnv  # noqa: E402

PHASE1 = Path(__file__).resolve().parents[1]
LOOKBACK = 30
COST = 5 / 10000.0
ALGO = {"ppo_mlp": ("PPO", "MlpPolicy"), "ppo_lstm": ("RecurrentPPO", "MlpLstmPolicy")}
STEPS = {"ppo_mlp": 200_000, "ppo_lstm": 200_000}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--arm", choices=list(ALGO), required=True)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()

    OUT = PHASE1 / "runs" / f"r22b_{args.arm}"
    CKPT = OUT / "checkpoints"
    CKPT.mkdir(parents=True, exist_ok=True)
    cls_name, policy = ALGO[args.arm]
    steps = STEPS[args.arm]

    bars = load_zd_index_bars("NIFTY 50", start="2015-01-01")
    env = SiIntradayEnv(bars, lookback=LOOKBACK, cost=COST, alpha=1.0, beta=1.0)
    venv = make_vec_env(lambda: SiIntradayEnv(bars, lookback=LOOKBACK, cost=COST,
                                              alpha=1.0, beta=1.0), n_envs=1, seed=args.seed)
    print(f"{args.arm}: {cls_name}/{policy}, {steps} steps, "
          f"obs dim {env.observation_space.shape[0]}")

    cls = {"PPO": PPO, "RecurrentPPO": RecurrentPPO}[cls_name]
    model = cls(policy, venv, seed=args.seed, verbose=0, n_steps=1024,
                batch_size=128, n_epochs=6, gamma=0.99)
    t0 = time.time()
    model.learn(total_timesteps=steps, progress_bar=False)
    print(f"trained {steps} steps in {time.time()-t0:.0f}s")

    model.save(CKPT / "model.zip")
    meta = {
        "algo": f"SB3 {cls_name}/{policy} on SiIntradayEnv (per-bar net pnl reward)",
        "symbol": "NIFTY 50", "lookback": LOOKBACK, "cost_bps": int(COST * 10000),
        "timesteps": steps, "seed": args.seed,
        "train_note": "episodes = random train days (2015-01 .. 2019-12)",
    }
    (CKPT / "params.json").write_text(json.dumps(meta, indent=2))
    print("saved:", sorted(p.name for p in CKPT.iterdir()))
    print("Train complete.")


if __name__ == "__main__":
    main()

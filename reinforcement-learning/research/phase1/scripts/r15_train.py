"""R1.5 producer — shared TDQN agent over a top-FF cross-sectional universe.

S4 broad-market protocol: ONE agent trained on artificial trajectories
block-bootstrapped per stock (returns + momentum/prob features jointly),
then evaluated per-name on the held-out test window (2020-05-01+).
Train window: 2015-09-02 .. 2019-12-31 (MOMENTUM_ABS availability + house
train cutoff). Reward: pnl @ 5bps (the R1.2/R1.3 survivor).

Usage: python scripts/r15_train.py [--episodes 600] [--seed 42]
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

from rl.data.augment import artificial_trajectories_joint, trajectory_stats  # noqa: E402
from rl.envs.tdqn_env import TDQNEnv  # noqa: E402
from rl.agents.tdqn import TDQNAgent  # noqa: E402
from r15_common import (get_universe, TRAIN_START, TRAIN_END,  # noqa: E402
                        load_stock_features, STATE_RICH_COLS)  # noqa: E402

PHASE1 = Path(__file__).resolve().parents[1]

LOOKBACK = 25
COST = 5 / 10000.0
N_TRAJ_PER_STOCK = 8
TRAJ_LEN = 400
BLOCK_LEN = 30


def build_trajectories(close_train, feats_cols, drift_free=False):
    """Joint (return, feature) trajectories for one stock's train window."""
    logrets = np.log(close_train / close_train.shift(1)).dropna()
    sigma = float(logrets.std(ddof=0))
    feats = pd.DataFrame(feats_cols).reindex(logrets.index)
    if drift_free:
        logrets = logrets - logrets.mean()  # S4 regime-neutral paths
    trajs = artificial_trajectories_joint(
        logrets, feats, n_traj=N_TRAJ_PER_STOCK, traj_len=TRAJ_LEN,
        block_len=BLOCK_LEN, seed=42)
    return trajs, sigma


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--episodes", type=int, default=800)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--universe", choices=["top50", "ff60"], default="ff60")
    ap.add_argument("--train-sample", type=int, default=None,
                    help="stratified FF-mcap training slice (S4: train a slice, eval broadly)")
    ap.add_argument("--long-only", action="store_true", help="{flat,long} actions only")
    ap.add_argument("--drift-free", action="store_true",
                    help="center returns before block bootstrap (regime-neutral paths)")
    ap.add_argument("--reward-scale-auto", action="store_true",
                    help="per-stock 1/vol reward normalization")
    ap.add_argument("--state", choices=["base", "rich"], default="base",
                    help="rich = + VIX z/change, TVT p_off, mom rank, 12m rel strength")
    ap.add_argument("--tag", default=None)
    args = ap.parse_args()
    if args.state == "rich":
        from r15_common import build_market_feats, build_cross_sectional
        build_market_feats()
        build_cross_sectional()
    STATE_COLS = STATE_RICH_COLS if args.state == "rich" else ["mom_z", "prob_z"]
    OUT = PHASE1 / "runs" / (args.tag or ("r15" if args.universe == "top50" else "r15_ff60"))
    CKPT = OUT / "checkpoints"
    CKPT.mkdir(parents=True, exist_ok=True)

    # per-stock trajectories from the train window
    pool = []          # list of (symbol, traj) pairs
    stats_rows = []
    n_ok = 0
    loaded = []
    universe = get_universe(args.universe)
    print(f"universe: {args.universe} ({len(universe)} names)")
    if args.train_sample:
        print(f"training slice: {args.train_sample} stocks stratified by FF-mcap decile")
    for sym in universe:
        try:
            df = load_stock_features(sym)
        except Exception as exc:
            print(f"  skip {sym}: {exc}")
            continue
        tr = df[(df.index >= TRAIN_START) & (df.index <= TRAIN_END)]
        if not set(STATE_COLS).issubset(tr.columns):
            print(f"  skip {sym}: missing state cols {[c for c in STATE_COLS if c not in tr.columns]}")
            continue
        tr = tr.dropna(subset=STATE_COLS)
        if len(tr) < 200:
            print(f"  skip {sym}: only {len(tr)} train rows with features")
            continue
        close = tr["close"]
        feats = tr[STATE_COLS]
        trajs, sigma = build_trajectories(close, feats, drift_free=args.drift_free)
        for t in trajs:
            pool.append((sym, t, sigma))
        st = trajectory_stats(np.log(close / close.shift(1)).dropna(),
                              [t["close"] for t in trajs])
        stats_rows.append({"symbol": sym, "train_days": len(tr),
                           "synth_mean": st["synth_mean"], "real_mean": st["real_mean"],
                           "synth_std": st["synth_std"], "real_std": st["real_std"]})
        n_ok += 1
        loaded.append(sym)
        print(f"  {sym}: {len(tr)} train days -> {len(trajs)} trajectories")

    if args.train_sample and args.train_sample < len(loaded):
        # S4: train on an FF-mcap-decile-stratified slice; evaluate broadly.
        ff = pd.read_csv(PHASE1 / "cache" / "r15" / "universe_ff60.csv")
        mc = ff.set_index("SYMBOL")["FF_MKT_CAP_CR"].reindex(loaded).dropna()
        dec = pd.qcut(mc.rank(method="first"), 10, labels=False)
        rng = np.random.default_rng(args.seed)
        chosen = []
        per = int(np.ceil(args.train_sample / 10))
        for d in range(10):
            cands = mc[dec == d].index.tolist()
            rng.shuffle(cands)
            chosen.extend(cands[:per])
        chosen = chosen[:args.train_sample]
        keep = set(chosen)
        pool = [(s, t, sg) for (s, t, sg) in pool if s in keep]
        print(f"train slice: {len(chosen)} stocks -> {len(pool)} trajectories")

    if len(pool) < 100:
        raise SystemExit(f"universe too thin: {len(pool)} trajectories")
    print(f"pool: {len(pool)} trajectories across {n_ok} stocks")

    # shared agent, pnl@5bps
    scale0 = 1.0 / pool[0][2] if args.reward_scale_auto else 1.0
    env0 = TDQNEnv(pool[0][1]["close"], lookback=LOOKBACK, cost=COST, horizon=TRAJ_LEN,
                   reward_mode="pnl", extra_feats=pool[0][1]["feats"],
                   long_only=args.long_only, reward_scale=scale0)
    agent = TDQNAgent(env0, hidden=128, layers=2, lr=1e-3, epsilon_decay=0.995,
                      replay_capacity=50000, target_sync_every=100, seed=args.seed)
    ep_rets = []
    t0 = time.time()
    for ep in range(args.episodes):
        sym, tr, sigma = pool[ep % len(pool)]  # full coverage of the training pool
        scale = 1.0 / sigma if args.reward_scale_auto else 1.0
        env = TDQNEnv(tr["close"], lookback=LOOKBACK, cost=COST, horizon=TRAJ_LEN,
                      reward_mode="pnl", extra_feats=tr["feats"],
                      long_only=args.long_only, reward_scale=scale)
        obs, _ = env.reset(seed=args.seed + ep)
        env.action_space.seed(args.seed + ep)  # gymnasium gotcha
        total = 0.0
        for _ in range(TRAJ_LEN):
            a = agent.act(obs)
            obs2, r, term, trunc, _ = env.step(a)
            agent.replay.add(obs, a, r, obs2, term or trunc)
            agent.learn()
            obs = obs2
            total += float(r)
            if term or trunc:
                break
        agent.epsilon = max(agent.epsilon_min, agent.epsilon * agent.epsilon_decay)
        ep_rets.append(total)
        if (ep + 1) % 100 == 0:
            print(f"  ep {ep+1}/{args.episodes} eps={agent.epsilon:.3f} "
                  f"mean_ep_ret={np.mean(ep_rets[-100:]):+.5f} ({time.time()-t0:.0f}s)")

    torch.save(agent.q.state_dict(), CKPT / "tdqn_shared.pt")
    pd.DataFrame(stats_rows).to_csv(OUT / "trajectory_stats.csv", index=False)
    meta = {
        "algo": "TDQN shared agent (Double+Dueling, pnl reward)",
        "cost_bps": int(COST * 10000), "lookback": LOOKBACK,
        "universe_size": n_ok, "n_trajectories": len(pool),
        "traj_per_stock": N_TRAJ_PER_STOCK, "traj_len": TRAJ_LEN, "block_len": BLOCK_LEN,
        "universe": args.universe, "train_sample": args.train_sample,
        "long_only": args.long_only,
        "drift_free": args.drift_free, "reward_scale_auto": args.reward_scale_auto,
        "state": args.state,
        "train_start": TRAIN_START, "train_end": TRAIN_END,
        "episodes": args.episodes, "seed": args.seed,
        "final_epsilon": agent.epsilon,
        "ep_ret_mean_last100": float(np.mean(ep_rets[-100:])),
    }
    (CKPT / "params.json").write_text(json.dumps(meta, indent=2))
    np.save(CKPT / "ep_rewards.npy", np.array(ep_rets))
    print("saved:", sorted(p.name for p in CKPT.iterdir()))
    print("Train complete.")


if __name__ == "__main__":
    main()

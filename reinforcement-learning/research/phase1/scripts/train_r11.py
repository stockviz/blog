"""R1.1 producer — train TDQN variants on NIFTY 50 daily (S4 replication).

Variants:
  tdqn_art : TDQN trained on S4 artificial trajectories (block bootstrap
             of 2000-2014 returns) — the Théate recipe.
  tdqn_real: TDQN trained on the real 2000-2014 rolling windows (control:
             what does augmentation actually buy?).

Test window (2015+) is NEVER touched here. Checkpoints -> checkpoints/.
Usage: python scripts/train_r11.py [--episodes 500] [--seed 42]
"""

import argparse
import json
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd
import torch

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "phase0"))  # the rl package lives in phase0/

from rl.config import mssql_conn  # noqa: E402  (imports at top; sys.path first)
from rl.data.loaders import load_bhav_index, load_vix_history  # noqa: E402
from rl.data.augment import (artificial_trajectories, artificial_trajectories_joint,  # noqa: E402
                             trajectory_stats)
from rl.envs.base import zscore_window  # noqa: E402
from rl.envs.tdqn_env import TDQNEnv, HOUSE_DRAG  # noqa: E402
from rl.agents.tdqn import TDQNAgent  # noqa: E402

PHASE1 = Path(__file__).resolve().parents[1]

TRAIN_END = "2014-12-31"
LOOKBACK = 25
N_TRAJ = 80
TRAJ_LEN = 500
BLOCK_LEN = 30


def run_episode(agent, env, seed, max_steps=500):
    """One training episode: act/learn through the env (ε-greedy)."""
    obs, _ = env.reset(seed=seed)
    env.action_space.seed(seed)  # gymnasium gotcha: reset() does not seed the action space
    total = 0.0
    for _ in range(max_steps):
        a = agent.act(obs)
        obs2, r, term, trunc, _ = env.step(a)
        agent.replay.add(obs, a, r, obs2, term or trunc)
        agent.learn()
        obs = obs2
        total += float(r)
        if term or trunc:
            break
    agent.epsilon = max(agent.epsilon_min, agent.epsilon * agent.epsilon_decay)
    return total


def train_artificial(close_train, episodes, seed, reward="dsr", cost=HOUSE_DRAG,
                     churn_penalty=0.0, feats=None):
    """S4 recipe: TDQN on block-bootstrapped artificial trajectories
    (with aligned feature columns when feats is given — R1.3 joint bootstrap)."""
    logrets = np.log(close_train / close_train.shift(1)).dropna()
    if feats is not None:
        trajs = artificial_trajectories_joint(logrets, feats, n_traj=N_TRAJ,
                                              traj_len=TRAJ_LEN, block_len=BLOCK_LEN, seed=seed)
        stats = trajectory_stats(logrets, [t["close"] for t in trajs])
        env0 = TDQNEnv(trajs[0]["close"], lookback=LOOKBACK, cost=cost, horizon=TRAJ_LEN,
                       reward_mode=reward, churn_penalty=churn_penalty,
                       extra_feats=trajs[0]["feats"])
    else:
        trajs = artificial_trajectories(logrets, n_traj=N_TRAJ, traj_len=TRAJ_LEN,
                                        block_len=BLOCK_LEN, seed=seed)
        stats = trajectory_stats(logrets, trajs)
        env0 = TDQNEnv(trajs[0], lookback=LOOKBACK, cost=cost, horizon=TRAJ_LEN,
                       reward_mode=reward, churn_penalty=churn_penalty)
    agent = TDQNAgent(env0, hidden=128, layers=2, lr=1e-3, epsilon_decay=0.995,
                      replay_capacity=50000, target_sync_every=100, seed=seed)
    ep_rets = []
    t0 = time.time()
    for ep in range(episodes):
        tr = trajs[ep % len(trajs)]
        env = TDQNEnv(tr if feats is None else tr["close"], lookback=LOOKBACK, cost=cost,
                      horizon=TRAJ_LEN, reward_mode=reward, churn_penalty=churn_penalty,
                      extra_feats=None if feats is None else tr["feats"])
        ep_rets.append(run_episode(agent, env, seed + ep, max_steps=TRAJ_LEN))
        if (ep + 1) % 100 == 0:
            print(f"  [art] ep {ep+1}/{episodes}  eps={agent.epsilon:.3f}  "
                  f"mean_ep_ret={np.mean(ep_rets[-100:]):+.5f}  ({time.time()-t0:.0f}s)")
    return agent, stats, np.array(ep_rets)


def train_real(close_train, episodes, seed, reward="dsr", cost=HOUSE_DRAG,
               churn_penalty=0.0, feats=None):
    """Control: TDQN on real rolling windows of the train period."""
    env = TDQNEnv(close_train, lookback=LOOKBACK, cost=cost, horizon=500,
                  reward_mode=reward, churn_penalty=churn_penalty,
                  extra_feats=feats)
    agent = TDQNAgent(env, hidden=128, layers=2, lr=1e-3, epsilon_decay=0.995,
                      replay_capacity=50000, target_sync_every=100, seed=seed)
    ep_rets = []
    t0 = time.time()
    for ep in range(episodes):
        ep_rets.append(run_episode(agent, env, seed + ep, max_steps=500))
        if (ep + 1) % 100 == 0:
            print(f"  [real] ep {ep+1}/{episodes}  eps={agent.epsilon:.3f}  "
                  f"mean_ep_ret={np.mean(ep_rets[-100:]):+.5f}  ({time.time()-t0:.0f}s)")
    return agent, np.array(ep_rets)


def build_feats(start, end, which, fill_neutral=False):
    """R1.3 state features aligned to the index calendar:
    vix_z (60d z of VIX level), vix_chg_z (20d z of VIX change),
    tvt_mid / tvt_small (pre-fitted TVT-HMM filtered P(risk-off),
    from the hmm/tvt-hmm checkpoint — causal filtered probs).
    fill_neutral=True pads pre-2009 dates with neutral constants
    (vix_z=0, p_off=0.5) so the full 2000-2014 return history can be
    used — NEVER backfill real feature values into the past (lookahead)."""
    feats = pd.DataFrame()
    if which in ("vix", "both"):
        vix = load_vix_history(start=start, end=end)["close"]
        feats["vix_z"] = zscore_window(vix, 60)
        feats["vix_chg_z"] = zscore_window(vix.pct_change(), 20)
    if which in ("tvt", "both"):
        for col, fname in [("tvt_mid", "tvt_filt_NIFTY_MIDCAP_150_TR.csv"),
                           ("tvt_small", "tvt_filt_NIFTY_SMALLCAP_250_TR.csv")]:
            df = pd.read_csv(PHASE1 / "cache" / fname, parse_dates=["date"])
            feats[col] = df.set_index("date")["p_off"]
    feats = feats.dropna(how="all")
    if fill_neutral:
        idx = pd.date_range(pd.Timestamp(start), pd.Timestamp(end), freq="B")
        feats = feats.reindex(idx)
        for c in feats.columns:
            feats[c] = feats[c].fillna(0.0 if c.startswith("vix") else 0.5)
    return feats


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--episodes", type=int, default=500)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--index", default="NIFTY 50",
                    help="bhav_index name (e.g. 'NIFTY MIDCAP SELECT')")
    ap.add_argument("--tag", default="nifty50",
                    help="run folder name under phase1/runs/")
    ap.add_argument("--start", default="2000-01-01",
                    help="data start (select index closes exist from 2004)")
    ap.add_argument("--reward", default="dsr", choices=["dsr", "pnl", "churn"],
                    help="TDQNEnv reward mode (R1.2 sweep)")
    ap.add_argument("--cost-bps", type=int, default=25,
                    help="cost in basis points per flip")
    ap.add_argument("--churn-mult", type=float, default=1.0,
                    help="churn penalty as multiple of cost (churn mode only)")
    ap.add_argument("--feats", default="none", choices=["none", "vix", "tvt", "both"],
                    help="R1.3 state features (VIX z/change, TVT-HMM p_off)")
    ap.add_argument("--train-start", default="2000-01-01",
                    help="training window start (R1.3 uses 2009-01-01 so VIX/p_off exist)")
    ap.add_argument("--feat-fill", default="none", choices=["none", "neutral"],
                    help="fill pre-feature dates with neutral constants (allows 2000-2014 train with feats)")
    args = ap.parse_args()

    ckpt = PHASE1 / "runs" / args.tag / "checkpoints"
    ckpt.mkdir(parents=True, exist_ok=True)

    ohlc_ok = load_bhav_index(args.index, start=args.start)
    close_all = load_bhav_index(args.index, start=args.start, ohlc_filter=False)["close"]
    # prefer OHLC-filtered when it covers the sample (NIFTY 50); fall back to
    # close-only when OHLC starts late (MIDCAP SELECT: closes 2004, OHLC 2022)
    close = close_all if len(ohlc_ok) < len(close_all) * 0.9 else ohlc_ok["close"]
    close_train = close[(close.index >= args.train_start) & (close.index <= TRAIN_END)]
    close_test = close[close.index > TRAIN_END]
    print(f"{args.index}: train {close_train.index.min().date()}..{close_train.index.max().date()} "
          f"({len(close_train)} bars) | test {close_test.index.min().date()}..{close_test.index.max().date()} "
          f"({len(close_test)} bars)")
    cost = args.cost_bps / 10000.0
    churn_penalty = cost * args.churn_mult
    feats = None
    if args.feats != "none":
        feats = build_feats(args.train_start, TRAIN_END, args.feats,
                            fill_neutral=(args.feat_fill == "neutral"))
        print(f"features={list(feats.columns)} rows={len(feats)}")
    print(f"reward={args.reward} cost={args.cost_bps}bps flip_cost={cost} "
          f"churn_penalty={churn_penalty} feats={args.feats}")

    print("== training TDQN-art (S4 artificial trajectories) ==")
    agent_art, traj_stats, ep_art = train_artificial(
        close_train, args.episodes, args.seed, reward=args.reward, cost=cost,
        churn_penalty=churn_penalty, feats=feats)
    torch.save(agent_art.q.state_dict(), ckpt / "tdqn_art.pt")

    print("== training TDQN-real (control) ==")
    agent_real, ep_real = train_real(close_train, args.episodes, args.seed,
                                     reward=args.reward, cost=cost,
                                     churn_penalty=churn_penalty, feats=feats)
    torch.save(agent_real.q.state_dict(), ckpt / "tdqn_real.pt")

    meta = {
        "index": args.index, "train_end": TRAIN_END, "data_start": str(close_train.index.min().date()),
        "train_start": args.train_start, "feats": args.feats,
        "episodes": args.episodes, "seed": args.seed, "lookback": LOOKBACK,
        "cost": cost, "cost_bps": args.cost_bps, "reward": args.reward,
        "churn_penalty": churn_penalty,
        "n_traj": N_TRAJ, "traj_len": TRAJ_LEN, "block_len": BLOCK_LEN,
        "algo": "TDQN (Double+Dueling, differential-Sharpe reward)",
        "traj_stats": traj_stats,
        "final_epsilon_art": agent_art.epsilon, "final_epsilon_real": agent_real.epsilon,
        "ep_ret_art_mean_last100": float(np.mean(ep_art[-100:])),
        "ep_ret_real_mean_last100": float(np.mean(ep_real[-100:])),
    }
    (ckpt / "params.json").write_text(json.dumps(meta, indent=2, default=str))
    np.save(ckpt / "ep_rewards_art.npy", ep_art)
    np.save(ckpt / "ep_rewards_real.npy", ep_real)
    print("saved checkpoints:", sorted(p.name for p in ckpt.iterdir()))
    print("Train complete.")


if __name__ == "__main__":
    main()

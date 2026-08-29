"""S4-style data augmentation — artificial trajectories (research-plan R1.4,
Théate & Ernst 2021 §3.3; KB 04 §4.2 A3).

Block bootstrap: resample contiguous blocks of daily returns from limited
history and concatenate into synthetic trajectories. Preserves local
dependence (autocorrelation, vol clustering) while multiplying the training
sample — the publication-grade trick for [CLOSE] data.
"""

import numpy as np
import pandas as pd


def artificial_trajectories(rets, n_traj=80, traj_len=500, block_len=30,
                            seed=42, base_date="2000-01-01"):
    """Returns list of synthetic close-price Series (traj_len B-days each).

    rets      : source daily return Series (the limited history)
    n_traj    : number of trajectories to synthesize
    traj_len  : bars per trajectory (default matches TDQNEnv horizon 500)
    block_len : bootstrap block length (local dependence preserved)
    """
    r = pd.Series(rets).dropna().to_numpy(dtype=float)
    n = len(r)
    if n < block_len + 10:
        raise ValueError(f"history too short for block bootstrap (n={n}, block={block_len})")
    rng = np.random.default_rng(seed)
    idx = pd.date_range(base_date, periods=traj_len, freq="B")
    out = []
    for _ in range(int(n_traj)):
        blocks = []
        pos = 0
        while pos < traj_len:
            start = int(rng.integers(0, n - block_len))
            blocks.append(r[start:start + block_len])
            pos += block_len
        traj_r = np.concatenate(blocks)[:traj_len]
        px = 100.0 * np.exp(np.cumsum(traj_r))
        out.append(pd.Series(px, index=idx, name="close"))
    return out


def trajectory_stats(rets, trajs):
    """Sanity: synthetic vs real first/second moments (KS-adjacent check)."""
    r = pd.Series(rets).dropna()
    synth = np.concatenate([np.log(t / t.shift(1)).dropna().to_numpy() for t in trajs])
    return {
        "real_mean": float(r.mean()), "real_std": float(r.std(ddof=0)),
        "synth_mean": float(synth.mean()), "synth_std": float(synth.std(ddof=0)),
        "n_synth": int(len(synth)),
    }


def artificial_trajectories_joint(rets, feats, n_traj=80, traj_len=500,
                                  block_len=30, seed=42, base_date="2000-01-01"):
    """R1.3: block-bootstrap a RETURN series and ALIGNED feature columns
    jointly, so the synthetic trajectories preserve the joint
    (return, VIX, regime) distribution. Returns list of dicts:
      {"close": Series, "feats": DataFrame (same synthetic index)}.
    rets/feats must share dates; the common window is used."""
    r = pd.Series(rets).dropna()
    f = pd.DataFrame(feats)
    common = r.index.intersection(f.index)
    if len(common) < block_len + 10:
        raise ValueError(f"joint sample too short (n={len(common)}, block={block_len})")
    r = r.loc[common].to_numpy(dtype=float)
    f = f.loc[common]
    n = len(r)
    rng = np.random.default_rng(seed)
    idx = pd.date_range(base_date, periods=traj_len, freq="B")
    out = []
    for _ in range(int(n_traj)):
        blocks_r, blocks_f = [], []
        pos = 0
        while pos < traj_len:
            start = int(rng.integers(0, n - block_len))
            blocks_r.append(r[start:start + block_len])
            blocks_f.append(f.iloc[start:start + block_len])
            pos += block_len
        traj_r = np.concatenate(blocks_r)[:traj_len]
        traj_f = pd.concat(blocks_f, axis=0).iloc[:traj_len].reset_index(drop=True)
        traj_f.index = idx
        px = 100.0 * np.exp(np.cumsum(traj_r))
        out.append({"close": pd.Series(px, index=idx, name="close"),
                    "feats": traj_f})
    return out

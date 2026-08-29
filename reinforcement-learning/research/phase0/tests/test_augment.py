"""Artificial-trajectory tests (S4 block bootstrap)."""

import numpy as np
import pandas as pd
import pytest

from rl.data.augment import artificial_trajectories, trajectory_stats


@pytest.fixture
def rets():
    rng = np.random.default_rng(0)
    r = rng.normal(0.0004, 0.01, 2000)
    return pd.Series(r, index=pd.date_range("2000-01-01", periods=2000, freq="B"))


def test_trajectory_shape_and_determinism(rets):
    a = artificial_trajectories(rets, n_traj=5, traj_len=300, block_len=30, seed=7)
    b = artificial_trajectories(rets, n_traj=5, traj_len=300, block_len=30, seed=7)
    assert len(a) == 5
    assert all(len(t) == 300 for t in a)
    assert all(np.isfinite(t).all() for t in a)
    assert all(t.iloc[0] == pytest.approx(100.0, abs=10.0) for t in a)  # 100 * exp(r0)
    assert all(float(t.iloc[-1]) == float(b[i].iloc[-1]) for i, t in enumerate(a))  # deterministic


def test_trajectory_stats_close_to_real(rets):
    trajs = artificial_trajectories(rets, n_traj=20, traj_len=500, block_len=30, seed=1)
    st = trajectory_stats(rets, trajs)
    assert abs(st["synth_mean"] - st["real_mean"]) < 3e-4
    assert abs(st["synth_std"] - st["real_std"]) < 0.25 * st["real_std"]


def test_short_history_raises(rets):
    with pytest.raises(ValueError):
        artificial_trajectories(rets.iloc[:20], n_traj=1, traj_len=100, block_len=30)

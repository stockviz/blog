"""AllocationEnv — Hilpisch Ch.8 (S3): simplex weights over n risky assets.

State   : last L returns of each asset + current weights
Action  : weight vector on the (n-1)-simplex; cash = 1 - sum(w)
Reward  : portfolio return - rebalance cost * turnover
Benchmark: equal weight (S3 §8.5: surprisingly hard to beat).
"""

import numpy as np
import pandas as pd
import gymnasium as gym
from gymnasium import spaces

from .base import zscore_window


class AllocationEnv(gym.Env):
    metadata = {"render_modes": []}

    def __init__(self, close_df, lookback=25, cost=0.0025, horizon=500, long_only=True):
        super().__init__()
        self._close = pd.DataFrame(close_df).dropna()
        self.lookback = int(lookback)
        self.cost = float(cost)
        self.horizon = int(horizon)
        self.long_only = long_only
        self.n = self._close.shape[1]

        self._rets = self._close.pct_change()
        self._z = pd.DataFrame({c: zscore_window(self._rets[c], self.lookback)
                                for c in self._rets.columns})
        self._feat = self._z.dropna()
        self._rets_causal = self._rets.reindex(self._feat.index)

        self.action_space = spaces.Box(low=0.0, high=1.0, shape=(self.n,), dtype=np.float32)
        dim = self.lookback * self.n + self.n
        self.observation_space = spaces.Box(low=-10.0, high=10.0, shape=(dim,), dtype=np.float32)
        self._t = None
        self._w = None

    def _obs(self, t):
        f = self._feat.iloc[max(0, t - self.lookback + 1):t + 1]
        cols = []
        for c in self._feat.columns:
            v = np.atleast_1d(np.nan_to_num(np.asarray(f[c], dtype=np.float32), nan=0.0))
            if len(v) < self.lookback:
                v = np.pad(v, (self.lookback - len(v), 0), constant_values=0.0)
            cols.append(v)
        vec = np.concatenate(cols + [self._w.astype(np.float32)])
        return vec

    def _simplex(self, action):
        a = np.clip(np.asarray(action, dtype=float), 0.0 if self.long_only else -1.0, 1.0)
        s = a.sum()
        return a / s if s > 0 else np.full(self.n, 1.0 / self.n)

    def reset(self, seed=None, options=None):
        super().reset(seed=seed)
        n = len(self._feat)
        lo, hi = self.lookback - 1, n - 2
        if hi < lo:
            raise ValueError(f"not enough data for lookback {self.lookback} (n={n})")
        self._t = int(self.np_random.integers(lo, hi))
        self._w = np.full(self.n, 1.0 / self.n)
        self._steps = 0
        return self._obs(self._t), {}

    def step(self, action):
        w_new = self._simplex(action)
        t = self._t
        r_next = self._rets_causal.iloc[t + 1].to_numpy(dtype=float)
        port_ret = float(np.dot(w_new, r_next))
        turnover = float(np.abs(w_new - self._w).sum())
        reward = port_ret - self.cost * turnover
        self._w = w_new
        self._t = t + 1
        self._steps += 1
        truncated = self._t >= len(self._feat) - 1 or self._steps >= self.horizon
        obs = self._obs(min(self._t, len(self._feat) - 1))
        return obs, reward, False, truncated, {"weights": w_new, "turnover": turnover}


class SleeveAllocationEnv(gym.Env):
    """R1.6 — continuous allocation over K sleeves (weekly steps).

    State   : L-week z-scores of each sleeve + regime feats (VIX z, p_off,
              breadth) + current weights
    Action  : Box(0,1)^K -> normalized to the simplex (cash = last sleeve)
    Reward  : portfolio return - cost * turnover; optional per-sleeve
              de-drift (train-time regime neutralisation, R1.5 lesson)
    """

    metadata = {"render_modes": []}

    def __init__(self, rets, feats=None, lookback=12, cost=0.0005, horizon=100,
                 de_drift=None, reward_scale=1.0):
        super().__init__()
        self._rets_raw = pd.DataFrame(rets).dropna()
        self.lookback = int(lookback)
        self.cost = float(cost)
        self.horizon = int(horizon)
        self.n = self._rets_raw.shape[1]
        self.reward_scale = float(reward_scale)

        # train-time regime neutralisation: subtract per-sleeve train means
        if de_drift is not None:
            self._rets = self._rets_raw - pd.Series(de_drift, index=self._rets_raw.columns)
        else:
            self._rets = self._rets_raw
        z = pd.DataFrame({c: zscore_window(self._rets[c], self.lookback)
                          for c in self._rets.columns})
        self._z = z.replace([np.inf, -np.inf], np.nan).fillna(0.0)  # constant sleeves -> 0
        self._rets_causal = self._rets.reindex(self._z.index)

        self._feats = None
        self.n_feats = 0
        if feats is not None and len(feats.columns):
            self._feats = pd.DataFrame(feats).reindex(self._z.index).ffill()
            self.n_feats = self._feats.shape[1]

        self.action_space = spaces.Box(low=0.0, high=1.0, shape=(self.n,), dtype=np.float32)
        dim = self.lookback * self.n + self.n_feats + self.n
        self.observation_space = spaces.Box(low=-10.0, high=10.0, shape=(dim,), dtype=np.float32)
        self._t = None
        self._w = None

    def _obs(self, t):
        f = self._z.iloc[max(0, t - self.lookback + 1):t + 1]
        cols = []
        for c in self._z.columns:
            v = np.atleast_1d(np.nan_to_num(np.asarray(f[c], dtype=np.float32), nan=0.0))
            if len(v) < self.lookback:
                v = np.pad(v, (self.lookback - len(v), 0), constant_values=0.0)
            cols.append(v)
        vec = np.concatenate(cols + [self._w.astype(np.float32)])
        if self.n_feats:
            fv = np.nan_to_num(self._feats.iloc[t].to_numpy(dtype=np.float32), nan=0.0)
            vec = np.concatenate([vec, fv])
        return vec

    def _simplex(self, action):
        a = np.clip(np.asarray(action, dtype=float), 0.0, 1.0)
        s = a.sum()
        return a / s if s > 0 else np.full(self.n, 1.0 / self.n)

    def reset(self, seed=None, options=None):
        super().reset(seed=seed)
        n = len(self._z)
        lo, hi = self.lookback - 1, n - 2
        if hi < lo:
            raise ValueError(f"not enough data for lookback {self.lookback} (n={n})")
        if options and options.get("start") is not None:
            self._t = int(options["start"])
        else:
            self._t = int(self.np_random.integers(lo, hi))
        self._w = np.full(self.n, 1.0 / self.n)
        self._steps = 0
        return self._obs(self._t), {}

    def step(self, action):
        w_new = self._simplex(action)
        t = self._t
        r_next = self._rets_causal.iloc[t + 1].to_numpy(dtype=float)
        port_ret = float(np.dot(w_new, r_next))
        turnover = float(np.abs(w_new - self._w).sum())
        reward = (port_ret - self.cost * turnover) * self.reward_scale
        self._w = w_new
        self._t = t + 1
        self._steps += 1
        truncated = self._t >= len(self._z) - 1 or self._steps >= self.horizon
        obs = self._obs(min(self._t, len(self._z) - 1))
        return obs, reward, False, truncated, {"weights": w_new, "turnover": turnover}

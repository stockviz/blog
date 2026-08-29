"""TradingEnv — KB 05 (S3 Ch.6): window + position + vol, costs, next-bar PnL.

State   : last L z-scored returns (window ending at t) + realized vol + position
Action  : {-1, 0, +1}
Reward  : a_t * r_{t+1} - c * |a_t - a_{t-1}|   (cost-in-reward)
"""

import numpy as np
import pandas as pd
import gymnasium as gym
from gymnasium import spaces

from .base import zscore_window, charge_cost
from .finance_env import HOUSE_DRAG


class TradingEnv(gym.Env):
    metadata = {"render_modes": []}

    def __init__(self, close, lookback=25, vol_window=20, cost=HOUSE_DRAG, horizon=500):
        super().__init__()
        self._close = pd.Series(close).dropna()
        self.lookback = int(lookback)
        self.vol_window = int(vol_window)
        self.cost = float(cost)
        self.horizon = int(horizon)

        self._rets = self._close.pct_change().dropna()
        z = zscore_window(self._rets, self.lookback)
        vol = self._rets.rolling(self.vol_window).std(ddof=0)
        self._z = z
        self._vol = vol
        self._feat = pd.DataFrame({"z": z, "vol": vol}).dropna()
        self._rets_causal = self._rets.reindex(self._feat.index)

        self.action_space = spaces.Discrete(3)
        dim = self.lookback + 2
        self.observation_space = spaces.Box(low=-10.0, high=10.0, shape=(dim,), dtype=np.float32)
        self._t = None
        self._pos = 0.0
        self._steps = 0

    def _obs(self, t):
        f = self._feat.iloc[max(0, t - self.lookback + 1):t + 1]
        z = np.atleast_1d(np.nan_to_num(np.asarray(f["z"], dtype=np.float32), nan=0.0))
        if len(z) < self.lookback:
            z = np.pad(z, (self.lookback - len(z), 0), constant_values=0.0)
        vol = float(np.asarray(f["vol"]).ravel()[-1]) if len(f) else 0.0
        return np.concatenate([z, [vol], [self._pos]]).astype(np.float32)

    def reset(self, seed=None, options=None):
        super().reset(seed=seed)
        n = len(self._feat)
        lo, hi = self.lookback - 1, n - 2
        if hi < lo:
            raise ValueError(f"not enough data for lookback {self.lookback} (n={n})")
        self._t = int(self.np_random.integers(lo, hi))
        self._pos = 0.0
        self._steps = 0
        return self._obs(self._t), {}

    def step(self, action):
        a = float(action) - 1.0
        t = self._t
        r_next = float(self._rets_causal.iloc[t + 1])
        reward = a * r_next - charge_cost(a, self._pos, self.cost)
        self._pos = a
        self._t = t + 1
        self._steps += 1
        truncated = self._t >= len(self._feat) - 1 or self._steps >= self.horizon
        return self._obs(self._t), float(reward), False, truncated, {"position": self._pos}

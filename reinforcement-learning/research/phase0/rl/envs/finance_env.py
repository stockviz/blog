"""FinanceEnv — KB 05: z-scored log-return window, {-1,0,1}, next-bar PnL.

Timing (house lag k=1): state at step t = window ENDING at t (data <= t);
reward = a_t * r_{t+1} - c * |a_t - a_{t-1}| — r_{t+1} is never in s_t.
Episode: fixed horizon, random start.
"""

import numpy as np
import pandas as pd
import gymnasium as gym
from gymnasium import spaces

from .base import zscore_window, charge_cost

HOUSE_DRAG = 0.0025  # 25bps per unit position change (house convention)


class FinanceEnv(gym.Env):
    metadata = {"render_modes": []}

    def __init__(self, close, lookback=25, cost=HOUSE_DRAG, horizon=500, log_returns=True):
        super().__init__()
        self._close = pd.Series(close).dropna()
        self.lookback = int(lookback)
        self.cost = float(cost)
        self.horizon = int(horizon)

        if log_returns:
            self._rets = np.log(self._close / self._close.shift(1)).dropna()
        else:
            self._rets = self._close.pct_change().dropna()
        self._z = zscore_window(self._rets, self.lookback).dropna()
        self._rets_causal = self._rets.reindex(self._z.index)

        self.action_space = spaces.Discrete(3)  # 0=sell, 1=flat, 2=buy
        self.observation_space = spaces.Box(low=-10.0, high=10.0, shape=(self.lookback,), dtype=np.float32)
        self._t = None
        self._pos = 0.0
        self._steps = 0

    def _obs_window(self, t):
        s = self._z.iloc[max(0, t - self.lookback + 1):t + 1].to_numpy(dtype=np.float32)
        if len(s) < self.lookback:
            s = np.pad(s, (self.lookback - len(s), 0), constant_values=0.0)
        return s

    def reset(self, seed=None, options=None):
        super().reset(seed=seed)
        n = len(self._z)
        if options and options.get("start") is not None:
            self._t = int(options["start"])
        else:
            lo, hi = self.lookback - 1, n - 2  # need window start >= 0 and r[t+1] to exist
            if hi < lo:
                raise ValueError(f"not enough data for lookback {self.lookback} (n={n})")
            self._t = int(self.np_random.integers(lo, hi))
        self._pos = 0.0
        self._steps = 0
        return self._obs_window(self._t), {}

    def step(self, action):
        a = float(action) - 1.0  # {0,1,2} -> {-1,0,1}
        t = self._t
        r_next = float(self._rets_causal.iloc[t + 1])
        reward = a * r_next - charge_cost(a, self._pos, self.cost)
        self._pos = a
        self._t = t + 1
        self._steps += 1
        truncated = self._t >= len(self._z) - 1 or self._steps >= self.horizon
        return self._obs_window(self._t), float(reward), False, truncated, {"position": self._pos}

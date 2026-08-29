"""ExecutionEnv — Almgren-Chriss optimal execution (S3 Ch.9 / S8 §4).

State   : remaining inventory, elapsed time fraction, mid price
Action  : shares to trade in [0, remaining]  (continuous)
Reward  : -(execution cost + lambda * risk); execution price = mid - h*a
          (temporary impact), mid = S_{t-1} + sigma*Z - g*shares (permanent).
Benchmark: TWAP.
"""

import numpy as np
import pandas as pd
import gymnasium as gym
from gymnasium import spaces


class ExecutionEnv(gym.Env):
    metadata = {"render_modes": []}

    def __init__(self, total_shares=1000.0, n_steps=50, sigma=0.01, mu=0.0,
                 temp_impact=0.001, perm_impact=0.0005, lam=0.5, arrival=100.0, seed=0):
        super().__init__()
        self.total = float(total_shares)
        self.n_steps = int(n_steps)
        self.sigma = float(sigma)
        self.mu = float(mu)
        self.h = float(temp_impact)    # temporary impact per share
        self.g = float(perm_impact)    # permanent impact per share
        self.lam = float(lam)
        self.arrival = float(arrival)
        self._seed0 = int(seed)

        self.action_space = spaces.Box(low=0.0, high=1.0, shape=(1,), dtype=np.float32)
        self.observation_space = spaces.Box(low=-np.inf, high=np.inf, shape=(3,), dtype=np.float32)
        self._rng = np.random.default_rng(seed)
        self._inv = None
        self._t = None
        self._mid = None
        self._px = []

    def reset(self, seed=None, options=None):
        super().reset(seed=seed)
        self._rng = np.random.default_rng(self._seed0 if seed is None else seed)
        self._inv = self.total
        self._t = 0
        self._mid = self.arrival
        self._px = []
        return self._obs(), {}

    def _obs(self):
        return np.array([self._inv / self.total, self._t / self.n_steps, self._mid],
                        dtype=np.float32)

    def step(self, action):
        a = float(np.clip(np.asarray(action).reshape(-1)[0], 0.0, 1.0)) * self._inv
        a = min(a, self._inv)
        exec_px = self._mid - self.h * a  # temporary impact (buyer pays more -> for SELLING, px - impact)
        self._px.append(exec_px * a)
        self._inv -= a
        # mid evolves: permanent impact + noise + drift
        shock = self._rng.normal(0.0, self.sigma)
        self._mid = self._mid + self.mu + shock - self.g * a
        self._t += 1
        done = self._t >= self.n_steps
        # reward: negative cost of executed shares vs arrival + risk penalty on remaining
        cost = (self.arrival - exec_px) * a if a > 0 else 0.0
        risk = self.lam * self.sigma * self.sigma * (self._inv ** 2)
        reward = -cost - risk
        if done and self._inv > 1e-9:
            # liquidate remainder at mid (haircut h)
            reward -= (self.arrival - (self._mid - self.h * self._inv)) * self._inv
            self._inv = 0.0
        info = {"inventory": self._inv, "shortfall": self.arrival * self.total - sum(self._px)}
        return self._obs(), float(reward), done, False, info

"""TDQNEnv — Théate & Ernst (2021, S4): daily trading with Sharpe-family reward.

State   : z-scored log-return window + holding + market features (vol)
Action  : {-1, 0, +1}
Reward  : differential Sharpe ratio (Moody & Saffell) of the strategy's
          returns, minus costs — the S4 recipe (Sharpe reward beats pure
          PnL on broad-market generalization; KB 03 §3.4).
"""

import numpy as np
import pandas as pd
import gymnasium as gym
from gymnasium import spaces

from .base import zscore_window, charge_cost
from .finance_env import HOUSE_DRAG


class TDQNEnv(gym.Env):
    metadata = {"render_modes": []}

    def __init__(self, close, lookback=25, vol_window=20, cost=HOUSE_DRAG,
                 horizon=500, eta=0.01, reward_mode="dsr", churn_penalty=0.0,
                 extra_feats=None, long_only=False, reward_scale=1.0):
        super().__init__()
        self._close = pd.Series(close).dropna()
        self.lookback = int(lookback)
        self.vol_window = int(vol_window)
        self.cost = float(cost)
        self.horizon = int(horizon)
        self.eta = float(eta)  # differential-Sharpe adaptation rate
        self.reward_mode = reward_mode  # "dsr" | "pnl" | "churn" (R1.2 sweep)
        self.churn_penalty = float(churn_penalty)  # extra per-flip penalty for "churn"
        self.long_only = bool(long_only)  # {flat, long} only — no short corner
        self.reward_scale = float(reward_scale)  # per-stock training normalization
        if self.reward_mode not in ("dsr", "pnl", "churn"):
            raise ValueError(f"unknown reward_mode {self.reward_mode}")
        # R1.3 state design: aligned causal covariate columns (VIX z/change,
        # TVT-HMM p_off). Values at date t are known at close t — reindex to
        # the feature frame index and carry forward (NO t+1 lookahead).
        self._feats_extra = None
        self.n_feats = 0
        if extra_feats is not None and len(extra_feats.columns):
            self._feats_extra = pd.DataFrame(extra_feats).reindex(self._close.index).ffill()
            self.n_feats = self._feats_extra.shape[1]

        self._logrets = np.log(self._close / self._close.shift(1)).dropna()
        z = zscore_window(self._logrets, self.lookback)
        vol = self._logrets.rolling(self.vol_window).std(ddof=0)
        self._feat = pd.DataFrame({"z": z, "vol": vol}).dropna()
        self._rets_causal = self._logrets.reindex(self._feat.index)

        self.action_space = spaces.Discrete(2 if self.long_only else 3)
        dim = self.lookback + 3 + self.n_feats
        self.observation_space = spaces.Box(low=-10.0, high=10.0, shape=(dim,), dtype=np.float32)
        self._t = None
        self._pos = 0.0
        self._A = 0.0  # differential Sharpe online stats
        self._B = 0.0

    def _obs(self, t):
        f = self._feat.iloc[max(0, t - self.lookback + 1):t + 1]
        z = np.atleast_1d(np.nan_to_num(np.asarray(f["z"], dtype=np.float32), nan=0.0))
        if len(z) < self.lookback:
            z = np.pad(z, (self.lookback - len(z), 0), constant_values=0.0)
        vol = float(np.asarray(f["vol"]).ravel()[-1]) if len(f) else 0.0
        vec = np.concatenate([z, [vol], [self._pos], [self._A]]).astype(np.float32)
        if self.n_feats:
            fv = np.nan_to_num(self._feats_extra.iloc[t].to_numpy(dtype=np.float32), nan=0.0)
            vec = np.concatenate([vec, fv])
        return vec

    def _differential_sharpe(self, r):
        """DSR_t = (B·dA - 0.5·A·dB) / (B - A^2)^(3/2) with online A,B."""
        dA = r - self._A
        dB = r * r - self._B
        denom = (self._B - self._A ** 2) ** 1.5
        dsr = (self._B * dA - 0.5 * self._A * dB) / denom if denom > 1e-12 else 0.0
        self._A += self.eta * dA
        self._B += self.eta * dB
        return float(np.clip(dsr, -1.0, 1.0))

    def reset(self, seed=None, options=None):
        super().reset(seed=seed)
        n = len(self._feat)
        if options and options.get("start") is not None:
            self._t = int(options["start"])
        else:
            lo, hi = self.lookback - 1, n - 2
            if hi < lo:
                raise ValueError(f"not enough data for lookback {self.lookback} (n={n})")
            self._t = int(self.np_random.integers(lo, hi))
        self._pos = 0.0
        self._A = 0.0
        self._B = 0.0
        self._steps = 0
        return self._obs(self._t), {}

    def step(self, action):
        a = float(action) if self.long_only else float(action) - 1.0
        t = self._t
        r_next = float(self._rets_causal.iloc[t + 1])
        strat_ret = a * r_next - charge_cost(a, self._pos, self.cost)
        if self.reward_mode == "dsr":
            reward = self._differential_sharpe(strat_ret)
        elif self.reward_mode == "pnl":
            reward = strat_ret * self.reward_scale
        elif self.reward_mode == "churn":
            reward = (strat_ret - self.churn_penalty * abs(a - self._pos)) * self.reward_scale
        else:
            raise ValueError(f"unknown reward_mode {self.reward_mode}")
        self._pos = a
        self._t = t + 1
        self._steps += 1
        truncated = self._t >= len(self._feat) - 1 or self._steps >= self.horizon
        obs = self._obs(min(self._t, len(self._feat) - 1))
        return obs, float(reward), False, truncated, {"position": self._pos, "strat_ret": strat_ret}

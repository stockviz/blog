"""SiIntradayEnv — Si et al. (2017, S6): episode = one trading day, 1-min bars.

State   : last L feature rows (z-scored returns + rolling vol + volume z),
          reset per day; continuous action in [-1, 1] (target position)
Action  : continuous a_t in [-1, 1] (tanh head)
Reward  : per-bar PnL minus costs (dense); info["U"] at day end carries the
          multi-objective value U = alpha*mean(DR) - beta*std(DR) that the
          S6 trainer maximizes (KB 06 §6.5).
"""

import numpy as np
import pandas as pd
import gymnasium as gym
from gymnasium import spaces

from .base import returns_from, zscore_window, charge_cost


class SiIntradayEnv(gym.Env):
    metadata = {"render_modes": []}

    def __init__(self, bars, lookback=30, cost=0.0002, alpha=1.0, beta=1.0,
                 price_col="c", extra_feats=None):
        super().__init__()
        self.bars = pd.DataFrame(bars).sort_index()
        self.lookback = int(lookback)
        self.cost = float(cost)
        self.alpha = float(alpha)
        self.beta = float(beta)

        r = self.bars[price_col].pct_change()
        z = zscore_window(r, self.lookback)
        vol = r.rolling(20).std(ddof=0)
        v = self.bars["v"].astype(float) if "v" in self.bars.columns else pd.Series(1.0, index=self.bars.index)
        vz = zscore_window(v, self.lookback)
        self._feat = pd.DataFrame({"r": r, "z": z, "vol": vol, "vz": vz})
        self._rets = r
        self._feats_extra = None
        self.n_feats = 0
        if extra_feats is not None and len(extra_feats.columns):
            self._feats_extra = pd.DataFrame(extra_feats).reindex(self.bars.index).ffill()
            self.n_feats = self._feats_extra.shape[1]
        self._days = sorted({pd.Timestamp(ts).date() for ts in self.bars.index})

        self.action_space = spaces.Box(low=-1.0, high=1.0, shape=(1,), dtype=np.float32)
        dim = self.lookback * 3 + 1 + self.n_feats  # z + vol + vz + pos + feats
        self.observation_space = spaces.Box(low=-10.0, high=10.0, shape=(dim,), dtype=np.float32)
        self._day = None
        self._t = None
        self._pos = 0.0
        self._day_pnls = []

    def _obs(self, t):
        f = self._feat.iloc[max(0, t - self.lookback + 1):t + 1]
        cols = []
        for c in ["z", "vol", "vz"]:
            v = np.atleast_1d(np.nan_to_num(np.asarray(f[c], dtype=np.float32), nan=0.0))
            if len(v) < self.lookback:
                v = np.pad(v, (self.lookback - len(v), 0), constant_values=0.0)
            cols.append(v)
        vec = np.concatenate(cols).astype(np.float32)
        vec = np.concatenate([vec, [self._pos]]).astype(np.float32)
        if self.n_feats:
            fv = np.nan_to_num(self._feats_extra.iloc[t].to_numpy(dtype=np.float32), nan=0.0)
            vec = np.concatenate([vec, fv])
        return vec

    def reset(self, seed=None, options=None, day=None):
        super().reset(seed=seed)
        if day is not None and pd.Timestamp(day).date() in self._days:
            self._day = pd.Timestamp(day).date()
        else:
            self._day = self._days[int(self.np_random.integers(0, len(self._days)))]
        self._idx = np.where(self.bars.index.date == self._day)[0]  # O(n) once per reset
        if len(self._idx) < self.lookback + 2:
            raise ValueError(f"day {self._day} too short ({len(self._idx)} bars)")
        self._last = int(self._idx[-1])
        self._t = int(self._idx[0])
        self._pos = 0.0
        self._day_pnls = []
        return self._obs(self._t), {}

    def step(self, action):
        a = float(np.clip(np.asarray(action).reshape(-1)[0], -1.0, 1.0))
        t = self._t
        # intraday-only: the day's last action earns no overnight return (S6)
        r_next = float(self._rets.iloc[t + 1]) if t + 1 <= self._last else 0.0
        pnl = a * r_next - charge_cost(a, self._pos, self.cost)
        self._day_pnls.append(pnl)
        self._pos = a
        self._t = t + 1
        done = self._t >= self._last
        obs = self._obs(min(self._t, len(self._feat) - 1))
        info = {"position": self._pos, "r_next": r_next}
        if done:
            dr = np.array(self._day_pnls)
            info["U"] = float(self.alpha * dr.mean() - self.beta * dr.std(ddof=0)) if len(dr) > 1 else 0.0
            info["day"] = str(self._day)
            info["day_pnls"] = dr
        return obs, float(pnl), done, False, info

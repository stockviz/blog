"""SiPolicy — Si et al. (S6): FC feature learner -> LSTM policy -> tanh head.

Continuous action a_t = tanh(W h_t + b) in [-1, 1]. Trained by BPTT on the
multi-objective objective U = alpha*mean(DR) - beta*std(DR) per day
(episode = one trading day), exactly per S6 Eq.5/8: forward through a day's
bars with the current policy, collect daily returns DR, backprop -U.
"""

import numpy as np
import torch
import torch.nn as nn

from ..envs.si_intraday_env import SiIntradayEnv
from .dqn import DEVICE

torch.set_num_threads(1)  # deterministic CPU training


class SiPolicyNet(nn.Module):
    def __init__(self, obs_dim, feat_dim=64, hidden=128):
        super().__init__()
        # FC feature learner on the raw observation window
        self.learner = nn.Sequential(
            nn.Linear(obs_dim, feat_dim), nn.ReLU(),
            nn.Linear(feat_dim, feat_dim), nn.ReLU(),
            nn.Linear(feat_dim, feat_dim), nn.ReLU(),
        )
        self.lstm = nn.LSTM(feat_dim, hidden, batch_first=True)
        self.head = nn.Linear(hidden, 1)  # tanh applied in forward

    def forward(self, x, h=None):
        # x: (B, T, obs_dim) -> features per bar -> LSTM -> tanh actions
        B, T, _ = x.shape
        f = self.learner(x.reshape(B * T, -1)).reshape(B, T, -1)
        out, h = self.lstm(f, h)
        a = torch.tanh(self.head(out)).squeeze(-1)  # (B, T)
        return a, h


class SiAgent:
    """Trains the S6 policy on a SiIntradayEnv by maximizing U per day."""

    def __init__(self, env, lr=1e-3, seed=42, reward_mode="U",
                 vol_cond=False, sigma_ref=None, **net_kwargs):
        torch.manual_seed(seed)
        np.random.seed(seed)
        self.env = env
        self.reward_mode = reward_mode  # "U": alpha*mean-beta*std | "SR": mean/std
        self.vol_cond = bool(vol_cond)   # R2.3: beta scales with day vol / sigma_ref
        self.sigma_ref = float(sigma_ref) if sigma_ref else 1.0
        obs_dim = int(np.prod(env.observation_space.shape))
        self.net = SiPolicyNet(obs_dim, **net_kwargs).to(DEVICE)
        self.opt = torch.optim.RMSprop(self.net.parameters(), lr=lr)

    def _objective(self, pnls):
        """Day objective (S6 U or Sharpe-type; flat -> 0 so inaction is not
        trivially optimal under SR)."""
        m = pnls.mean()
        s = pnls.std(ddof=0)
        if self.reward_mode == "SR":
            return float(m / s) if s > 1e-12 else 0.0
        beta = self.env.beta * (s / self.sigma_ref) if self.vol_cond else self.env.beta
        return float(self.env.alpha * m - beta * s)

    @torch.no_grad()
    def act(self, obs, h=None):
        x = torch.as_tensor(np.asarray(obs, dtype=np.float32), device=DEVICE).unsqueeze(0).unsqueeze(0)
        a, h = self.net(x, h)
        return float(a[0, -1].item()), h

    def _rollout(self, day):
        """Forward through one day; returns (day_pnls, U, hidden traces)."""
        obs, _ = self.env.reset(day=day)
        done = False
        h = None
        pnls = []
        obs_seq = []
        while not done:
            obs_seq.append(np.asarray(obs, dtype=np.float32))
            a, h = self.act(obs, h)
            obs, r, term, trunc, info = self.env.step(a)
            pnls.append(float(r))
            done = term or trunc
        pnls = np.array(pnls)
        U = self._objective(pnls) if len(pnls) > 1 else 0.0
        return pnls, U

    def train_day(self, day, epochs=5):
        """BPTT on -U for one trading day (S6 training step).

        Actions are computed WITH the graph (no no_grad), pnls are rebuilt
        as a differentiable function of the policy's actions, and
        loss = -U = -(alpha*mean(DR) - beta*std(DR)) is backpropagated
        through the LSTM hidden-state chain (S6 Eq.5/8). Each epoch is a
        fresh rollout + one backward (a second backward on the same graph
        is illegal in torch)."""
        last_U = 0.0
        for _ in range(epochs):
            obs, _ = self.env.reset(day=day)
            done = False
            h = None
            a_list = []
            r_list = []
            cost_list = []
            while not done:
                x = torch.as_tensor(np.asarray(obs, dtype=np.float32), device=DEVICE).unsqueeze(0).unsqueeze(0)
                a, h = self.net(x, h)          # WITH grad (BPTT path)
                a_scalar = float(a[0, -1].item())
                obs, r, term, trunc, info = self.env.step(a_scalar)
                a_list.append(a[0, -1])
                r_list.append(float(info["r_next"]))
                cost_list.append(self.env.cost)
                done = term or trunc
            # differentiable daily PnL: DR_t = a_t * r_{t+1} - c*|a_t - a_{t-1}|
            a_vec = torch.stack(a_list)
            r_vec = torch.tensor(r_list, device=DEVICE)
            a_prev = torch.cat([torch.zeros(1, device=DEVICE), a_vec[:-1]])
            costs = torch.tensor(cost_list, device=DEVICE) * (a_vec - a_prev).abs()
            pnls = a_vec * r_vec - costs
            if self.reward_mode == "SR":
                s = pnls.std(correction=0)
                U = (pnls.mean() / s) if s > 1e-12 else torch.zeros((), device=DEVICE)
            else:
                s = pnls.std(correction=0)
                beta = self.env.beta * (s / self.sigma_ref) if self.vol_cond else self.env.beta
                U = self.env.alpha * pnls.mean() - beta * s
            loss = -U
            self.opt.zero_grad()
            loss.backward()
            torch.nn.utils.clip_grad_norm_(self.net.parameters(), 5.0)
            self.opt.step()
            last_U = float(U.item())
        return last_U

    def evaluate(self, days, seed=42):
        """Mean U across days (per-day aggregation — S6 §4, never per-bar)."""
        us = []
        for d in days:
            _, u = self._rollout(d)
            us.append(u)
        return float(np.mean(us)), np.array(us)

"""TDQN — Théate & Ernst (S4): Double DQN + Dueling network.

Dueling: Q(s,a) = V(s) + A(s,a) - mean_a A(s,a)
Double : action selected by the ONLINE net, evaluated by the TARGET net
         (reduces max-bias on noisy financial rewards — S4 §2.4.1).
"""

import numpy as np
import torch
import torch.nn as nn

from .dqn import ReplayBuffer, DEVICE


class DuelingMLP(nn.Module):
    def __init__(self, in_dim, out_dim, hidden=128, layers=2):
        super().__init__()
        self.features = nn.Sequential(*(
            [nn.Linear(in_dim, hidden), nn.ReLU()] +
            [nn.Linear(hidden, hidden), nn.ReLU()] * (layers - 1)
        ))
        self.v = nn.Linear(hidden, 1)
        self.a = nn.Linear(hidden, out_dim)

    def forward(self, x):
        f = self.features(x)
        v = self.v(f)
        adv = self.a(f)
        return v + adv - adv.mean(dim=1, keepdim=True)


class TDQNAgent:
    """Double-DQN with dueling heads. Same training loop shape as DQLAgent."""

    def __init__(self, env, hidden=128, layers=2, lr=1e-3, gamma=0.99,
                 epsilon=1.0, epsilon_min=0.05, epsilon_decay=0.998,
                 replay_capacity=10000, batch=32, tau=1e-3, seed=42,
                 target_sync_every=1000):
        torch.manual_seed(seed)
        np.random.seed(seed)
        self.env = env
        obs_dim = int(np.prod(env.observation_space.shape))
        n_actions = int(env.action_space.n)
        self.q = DuelingMLP(obs_dim, n_actions, hidden, layers).to(DEVICE)
        self.target = DuelingMLP(obs_dim, n_actions, hidden, layers).to(DEVICE)
        self.target.load_state_dict(self.q.state_dict())
        self.opt = torch.optim.Adam(self.q.parameters(), lr=lr)
        self.gamma = gamma
        self.epsilon = epsilon
        self.epsilon_min = epsilon_min
        self.epsilon_decay = epsilon_decay
        self.replay = ReplayBuffer(replay_capacity)
        self.batch = batch
        self.tau = tau
        self.target_sync_every = int(target_sync_every)
        self._learn_steps = 0

    def act(self, obs, eval_mode=False):
        if not eval_mode and np.random.rand() < self.epsilon:
            return int(self.env.action_space.sample())
        with torch.no_grad():
            x = torch.as_tensor(np.asarray(obs, dtype=np.float32), device=DEVICE).unsqueeze(0)
            return int(self.q(x).argmax(dim=1).item())

    def learn(self):
        if len(self.replay) < self.batch:
            return None
        batch = self.replay.sample(self.batch)
        s = torch.as_tensor(np.array([b[0] for b in batch], dtype=np.float32), device=DEVICE)
        a = torch.as_tensor(np.array([b[1] for b in batch], dtype=np.int64), device=DEVICE)
        r = torch.as_tensor(np.array([b[2] for b in batch], dtype=np.float32), device=DEVICE)
        s2 = torch.as_tensor(np.array([b[3] for b in batch], dtype=np.float32), device=DEVICE)
        d = torch.as_tensor(np.array([b[4] for b in batch], dtype=np.float32), device=DEVICE)
        qv = self.q(s).gather(1, a.unsqueeze(1)).squeeze(1)
        with torch.no_grad():
            # Double-DQN: pick with online net, evaluate with target net
            a2 = self.q(s2).argmax(dim=1, keepdim=True)
            qmax = self.target(s2).gather(1, a2).squeeze(1)
            y = r + self.gamma * qmax * (1.0 - d)
        loss = nn.functional.mse_loss(qv, y)
        self.opt.zero_grad()
        loss.backward()
        torch.nn.utils.clip_grad_norm_(self.q.parameters(), 5.0)
        self.opt.step()
        self._learn_steps += 1
        if self._learn_steps % self.target_sync_every == 0:
            self.target.load_state_dict(self.q.state_dict())
        return float(loss.item())

    def train(self, env, episodes=200, max_steps=500, seed=42):
        ep_rewards = []
        for ep in range(episodes):
            obs, _ = env.reset(seed=seed + ep)
            env.action_space.seed(seed + ep)  # gymnasium gotcha: reset() does not seed the action space
            total = 0.0
            for _ in range(max_steps):
                a = self.act(obs)
                obs2, r, term, trunc, _ = env.step(a)
                self.replay.add(obs, a, r, obs2, term or trunc)
                self.learn()
                obs = obs2
                total += float(r)
                if term or trunc:
                    break
            ep_rewards.append(total)
            self.epsilon = max(self.epsilon_min, self.epsilon * self.epsilon_decay)
        return ep_rewards

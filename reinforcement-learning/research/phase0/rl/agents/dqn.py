"""DQLAgent — Hilpisch baseline (S3 §2.4.3): MLP Q-network, replay buffer,
target network (soft update), epsilon-greedy. Reusable for any discrete
Gym env; the CartPole sanity test lives in tests/test_agents.py.
"""

import numpy as np
import torch
import torch.nn as nn
from collections import deque

DEVICE = torch.device("cuda" if torch.cuda.is_available() else "cpu")
torch.set_num_threads(1)  # deterministic CPU training (multithreaded GEMM is not reproducible)


class MLP(nn.Module):
    def __init__(self, in_dim, out_dim, hidden=128, layers=2):
        super().__init__()
        dims = [in_dim] + [hidden] * layers + [out_dim]
        mods = []
        for i in range(len(dims) - 1):
            mods.append(nn.Linear(dims[i], dims[i + 1]))
            if i < len(dims) - 2:
                mods.append(nn.ReLU())
        self.net = nn.Sequential(*mods)

    def forward(self, x):
        return self.net(x)


class ReplayBuffer:
    def __init__(self, capacity=10000):
        self.buf = deque(maxlen=capacity)

    def add(self, s, a, r, s2, done):
        self.buf.append((s, a, r, s2, done))

    def sample(self, batch=32):
        idx = np.random.choice(len(self.buf), min(batch, len(self.buf)), replace=False)
        return [self.buf[i] for i in idx]

    def __len__(self):
        return len(self.buf)


class DQLAgent:
    def __init__(self, env, hidden=128, layers=2, lr=1e-3, gamma=0.99,
                 epsilon=1.0, epsilon_min=0.05, epsilon_decay=0.998,
                 replay_capacity=10000, batch=32, tau=1e-3, seed=42,
                 target_sync_every=1000):
        torch.manual_seed(seed)
        np.random.seed(seed)
        self.env = env
        obs_dim = int(np.prod(env.observation_space.shape))
        n_actions = int(env.action_space.n)
        self.q = MLP(obs_dim, n_actions, hidden, layers).to(DEVICE)
        self.target = MLP(obs_dim, n_actions, hidden, layers).to(DEVICE)
        self.target.load_state_dict(self.q.state_dict())
        self.opt = torch.optim.Adam(self.q.parameters(), lr=lr)
        self.gamma = gamma
        self.epsilon = epsilon
        self.epsilon_min = epsilon_min
        self.epsilon_decay = epsilon_decay
        self.replay = ReplayBuffer(replay_capacity)
        self.batch = batch
        self.tau = tau
        self.target_sync_every = int(target_sync_every)  # hard sync (Mnih-style)
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
            qmax = self.target(s2).max(dim=1).values
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
        """Standard DQN loop; returns per-episode rewards (for the floor test)."""
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

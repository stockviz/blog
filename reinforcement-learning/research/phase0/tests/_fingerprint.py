import numpy as np
import torch
import gymnasium as gym
from rl.agents.dqn import DQLAgent

print("threads:", torch.get_num_threads())
print("cuda:", torch.cuda.is_available())
env = gym.make("CartPole-v1")
agent = DQLAgent(env, hidden=128, layers=2, lr=1e-3, epsilon_decay=0.995,
                 replay_capacity=50000, target_sync_every=100, seed=42)
# fingerprint initial weights
w0 = agent.q.net[0].weight.detach().numpy().flatten()[:4]
print("init w:", w0)
rewards = agent.train(env, episodes=400, max_steps=500, seed=42)
r = np.array(rewards)
for lo in range(0, 400, 50):
    print(f"eps {lo:3d}-{lo+50:3d}: mean {r[lo:lo+50].mean():7.1f}  max {r[lo:lo+50].max():6.1f}")
print("last50:", r[-50:].mean())

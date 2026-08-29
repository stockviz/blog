import numpy as np
import gymnasium as gym
from rl.agents.dqn import DQLAgent

env = gym.make("CartPole-v1")
agent = DQLAgent(env, hidden=128, layers=2, lr=1e-3, epsilon_decay=0.995,
                 replay_capacity=50000, target_sync_every=100, seed=42)
rewards = agent.train(env, episodes=400, max_steps=500, seed=42)
r = np.array(rewards)
print("last50:", r[-50:].mean())
print("blocks:", ["%.0f" % b for b in [r[i:i+50].mean() for i in range(0, 400, 50)]])
print("torch threads:", __import__("torch").get_num_threads())

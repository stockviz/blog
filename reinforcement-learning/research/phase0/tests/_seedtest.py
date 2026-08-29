import gymnasium as gym
import numpy as np

env = gym.make("CartPole-v1")
obs, _ = env.reset(seed=42)
acts = [int(env.action_space.sample()) for _ in range(10)]
print("acts:", acts)
# also run a few steps with fixed actions
obs2, _ = env.reset(seed=42)
tot = 0
for _ in range(50):
    a = env.action_space.sample()
    obs2, r, t, tr, _ = env.step(int(a))
    tot += r
print("random-step total(50):", tot)

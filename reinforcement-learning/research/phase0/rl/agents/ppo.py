"""PPO / A2C-LSTM — stable-baselines3 wrappers (off-the-shelf where the
recipe is standard; SB3 is the external library of record for policy
gradients). PPO: KB 06 §6.3 (S5, S8); A2C-LSTM: synchronous stand-in for
Ponomarev's A3C+LSTM (S7) — same LSTM policy, async workers omitted.
"""

from stable_baselines3 import PPO, A2C
from stable_baselines3.common.env_util import make_vec_env


def make_ppo(env, policy="MlpPolicy", seed=42, **kwargs):
    """PPO with MLP policy on a single env (vectorized internally)."""
    venv = make_vec_env(lambda: env, n_envs=1, seed=seed)
    model = PPO(policy, venv, seed=seed, verbose=0, **kwargs)
    return model, venv


def make_a2c_lstm(env, seed=42, **kwargs):
    """A2C with LSTM policy (recurrent) — the S7 architecture, synchronous."""
    venv = make_vec_env(lambda: env, n_envs=1, seed=seed)
    model = A2C("MlpLstmPolicy", venv, seed=seed, verbose=0, **kwargs)
    return model, venv


def train_sb3(model, venv, total_timesteps=50_000):
    model.learn(total_timesteps=total_timesteps)
    return model


def evaluate_sb3(model, env, episodes=10, seed=42):
    """Mean episodic reward of a trained SB3 model (deterministic)."""
    import numpy as np
    rng = np.random.default_rng(seed)
    rets = []
    for _ in range(episodes):
        obs, _ = env.reset(seed=int(rng.integers(0, 2**31)))
        done = False
        total = 0.0
        while not done:
            a, _ = model.predict(obs, deterministic=True)
            obs, r, term, trunc, _ = env.step(a)
            total += float(r)
            done = term or trunc
        rets.append(total)
    return float(np.mean(rets))

"""Agent tests — CartPole sanity (DQLAgent must solve), TDQN mechanics,
SB3 smoke, Si-policy smoke (S6). Runtime-bounded."""

import numpy as np
import pandas as pd
import pytest

gym = pytest.importorskip("gymnasium")

from rl.agents.dqn import DQLAgent
from rl.agents.tdqn import TDQNAgent
from rl.agents.si_policy import SiAgent
from rl.envs.finance_env import FinanceEnv


def _cartpole():
    import gymnasium as gym
    return gym.make("CartPole-v1")


@pytest.mark.timeout(900)
def test_cartpole_dqn_solves():
    env = _cartpole()
    agent = DQLAgent(env, hidden=128, layers=2, lr=1e-3, epsilon_decay=0.995,
                     replay_capacity=50000, target_sync_every=100, seed=42)
    rewards = agent.train(env, episodes=500, max_steps=500, seed=42)
    last50 = np.mean(rewards[-50:])
    assert last50 >= 195.0, f"DQLAgent failed CartPole sanity: last-50 mean {last50:.1f}"


def test_tdqn_mechanics_and_loss():
    rng = np.random.default_rng(0)
    px = 100 * np.exp(np.cumsum(rng.normal(0.0004, 0.01, 1500)))
    close = pd.Series(px, index=pd.date_range("2015-01-01", periods=1500, freq="B"))
    env = FinanceEnv(close, lookback=25)
    agent = TDQNAgent(env, seed=1)
    losses = []
    obs, _ = env.reset(seed=0)
    for _ in range(500):
        a = agent.act(obs)
        obs2, r, term, trunc, _ = env.step(a)
        agent.replay.add(obs, a, r, obs2, term or trunc)
        loss = agent.learn()
        if loss is not None:
            losses.append(loss)
        obs = obs2
        if term or trunc:
            obs, _ = env.reset(seed=1)
    assert len(losses) > 100
    assert np.isfinite(losses).all()
    # losses should shrink materially from the early values
    assert np.mean(losses[-50:]) < np.mean(losses[:50])


def test_sb3_ppo_smoke():
    sb3 = pytest.importorskip("stable_baselines3")
    rng = np.random.default_rng(1)
    px = 100 * np.exp(np.cumsum(rng.normal(0.0003, 0.008, 1200)))
    close = pd.Series(px, index=pd.date_range("2016-01-01", periods=1200, freq="B"))
    env = FinanceEnv(close, lookback=25)
    from rl.agents.ppo import make_ppo, train_sb3
    model, venv = make_ppo(env, seed=0, n_steps=128, batch_size=32)
    train_sb3(model, venv, total_timesteps=1000)
    assert model.num_timesteps > 0
    venv.close()


def test_si_agent_smoke():
    rng = np.random.default_rng(7)
    days = pd.date_range("2026-01-05", "2026-01-09", freq="B")
    idx = []
    for d in days:
        idx.extend(pd.date_range(d + pd.Timedelta(hours=3, minutes=45),
                                 d + pd.Timedelta(hours=9, minutes=59), freq="1min"))
    n = len(idx)
    px = 100 * np.exp(np.cumsum(rng.normal(0.0, 0.0004, n)))
    bars = pd.DataFrame({"c": px, "v": rng.integers(100, 10000, n)}, index=pd.DatetimeIndex(idx))
    from rl.envs.si_intraday_env import SiIntradayEnv
    env = SiIntradayEnv(bars, lookback=10)
    agent = SiAgent(env, lr=1e-3, seed=0)
    day = pd.Timestamp(bars.index[0]).date()
    u0 = agent.train_day(day, epochs=2)
    assert np.isfinite(u0)
    us = agent.evaluate([day])[1]
    assert np.isfinite(us).all()


def test_si_agent_objective_modes():
    import numpy as np
    from rl.agents.si_policy import SiAgent
    a_u = SiAgent.__new__(SiAgent)
    a_u.reward_mode = "U"
    a_u.vol_cond = False
    a_u.sigma_ref = 1.0
    a_u.env = type("E", (), {"alpha": 2.0, "beta": 1.0})()
    a_sr = SiAgent.__new__(SiAgent)
    a_sr.reward_mode = "SR"
    a_sr.vol_cond = False
    a_sr.sigma_ref = 1.0
    p = np.array([1.0, 0.0, 0.0])
    u = a_u._objective(p)
    sr = a_sr._objective(p)
    assert 0.0 < u < 2.0
    assert sr > 0.0
    assert a_sr._objective(np.zeros(3)) == 0.0             # flat -> 0 under SR
    assert a_sr._objective(np.array([1.0, -1.0])) == 0.0   # zero mean -> 0

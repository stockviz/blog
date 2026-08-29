"""Random-agent floor (KB 08 §8.5 / exit gate M0).

1. Every env runs random episodes without error (finite rewards).
2. On a strongly trending series, buy&hold beats random in FinanceEnv —
   validates reward mechanics end-to-end.
3. A briefly-trained DQLAgent beats the random floor on the same env —
   "if the agent cannot beat random, it is a bug, not a signal."
"""

import numpy as np
import pandas as pd
import pytest

from rl.envs.finance_env import FinanceEnv, HOUSE_DRAG
from rl.envs.trading_env import TradingEnv
from rl.envs.tdqn_env import TDQNEnv
from rl.envs.si_intraday_env import SiIntradayEnv
from rl.envs.allocation_env import AllocationEnv
from rl.envs.execution_env import ExecutionEnv
from rl.eval.baselines import random_policy, buy_and_hold_policy, flat_policy, evaluate_policy


def _trending_close(n=2000, drift=0.0012, vol=0.008, seed=3):
    rng = np.random.default_rng(seed)
    r = rng.normal(drift, vol, n)
    px = 100 * np.exp(np.cumsum(r))
    return pd.Series(px, index=pd.date_range("2015-01-01", periods=n, freq="B"))


@pytest.fixture(scope="module")
def trending():
    return _trending_close()


def test_random_floor_all_envs(trending):
    envs = [
        FinanceEnv(trending, lookback=25),
        TradingEnv(trending, lookback=25),
        TDQNEnv(trending, lookback=25),
    ]
    rng = np.random.default_rng(0)
    idx = pd.date_range("2026-01-05", "2026-01-16", freq="B")
    bars_idx = []
    for d in idx:
        bars_idx.extend(pd.date_range(d + pd.Timedelta(hours=3, minutes=45),
                                      d + pd.Timedelta(hours=9, minutes=59), freq="1min"))
    n = len(bars_idx)
    px = 100 * np.exp(np.cumsum(rng.normal(0.0, 0.0004, n)))
    bars = pd.DataFrame({"c": px, "v": rng.integers(100, 10000, n)}, index=pd.DatetimeIndex(bars_idx))
    envs.append(SiIntradayEnv(bars, lookback=10))
    px2 = trending.iloc[:200]
    envs.append(AllocationEnv(pd.DataFrame({"A": px2, "B": px2 * 1.05}), lookback=10))
    envs.append(ExecutionEnv(seed=0))

    for env in envs:
        rets = evaluate_policy(env, random_policy(env), n_episodes=5, seed=0)
        assert np.isfinite(rets).all(), f"random floor crashed on {type(env).__name__}"


def test_buy_and_hold_beats_random_on_trend(trending):
    env = FinanceEnv(trending, lookback=25, cost=HOUSE_DRAG)
    rnd = evaluate_policy(env, random_policy(env), n_episodes=15, seed=0)
    bh = evaluate_policy(env, buy_and_hold_policy(env), n_episodes=15, seed=0)
    assert bh.mean() > rnd.mean(), f"B&H {bh.mean():.3f} should beat random {rnd.mean():.3f} on a trending series"


def test_dqn_beats_random_floor(trending):
    from rl.agents.dqn import DQLAgent
    env = FinanceEnv(trending, lookback=25, cost=HOUSE_DRAG)
    rnd = evaluate_policy(env, random_policy(env), n_episodes=15, seed=0)
    agent = DQLAgent(env, hidden=64, layers=2, lr=5e-4, epsilon_decay=0.97, seed=42)
    agent.train(env, episodes=80, max_steps=400, seed=42)
    eval_rets = evaluate_policy(env, lambda o: agent.act(o, eval_mode=True), n_episodes=15, seed=1)
    assert eval_rets.mean() > rnd.mean(), (
        f"DQLAgent {eval_rets.mean():.3f} must beat random {rnd.mean():.3f} (bug, not signal)")

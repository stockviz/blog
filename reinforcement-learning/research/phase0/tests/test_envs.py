"""Env tests: reward accounting (oracle recompute), cost charging,
no-lookahead (state at t has no t+1 info), episode mechanics.

House lag k=1: reward for action a_t == a_t * r_{t+1} - c*|a_t - a_{t-1}|,
with r_{t+1} NOT present in the state observed at t.
"""

import numpy as np
import pandas as pd
import pytest

from rl.envs.finance_env import FinanceEnv, HOUSE_DRAG
from rl.envs.trading_env import TradingEnv
from rl.envs.tdqn_env import TDQNEnv
from rl.envs.si_intraday_env import SiIntradayEnv
from rl.envs.allocation_env import AllocationEnv, SleeveAllocationEnv
from rl.envs.execution_env import ExecutionEnv
from rl.envs.base import charge_cost


@pytest.fixture(scope="module")
def daily_close():
    rng = np.random.default_rng(42)
    steps = 3000
    r = rng.normal(0.0004, 0.01, steps) + 0.0002 * np.sin(np.arange(steps) / 40)
    px = 100 * np.exp(np.cumsum(r))
    return pd.Series(px, index=pd.date_range("2015-01-01", periods=steps, freq="B"))


@pytest.fixture(scope="module")
def minute_bars():
    rng = np.random.default_rng(7)
    days = pd.date_range("2026-01-05", "2026-01-16", freq="B")
    idx = []
    for d in days:
        idx.extend(pd.date_range(d + pd.Timedelta(hours=3, minutes=45),
                                 d + pd.Timedelta(hours=9, minutes=59), freq="1min"))
    n = len(idx)
    r = rng.normal(0.0, 0.0004, n)
    px = 100 * np.exp(np.cumsum(r))
    return pd.DataFrame({"c": px, "v": rng.integers(100, 10000, n)}, index=pd.DatetimeIndex(idx))


def test_charge_cost():
    assert charge_cost(1.0, 0.0, 0.0025) == 0.0025
    assert charge_cost(1.0, 1.0, 0.0025) == 0.0
    assert charge_cost(-1.0, 1.0, 0.0025) == 0.005  # 2-unit flip


def test_finance_env_reward_oracle(daily_close):
    env = FinanceEnv(daily_close, lookback=25, cost=HOUSE_DRAG)
    obs, _ = env.reset(seed=0)
    t0 = env._t
    a = 1.0  # buy
    _, r, _, _, _ = env.step(2)
    r_next = float(env._rets_causal.iloc[t0 + 1])
    assert abs(r - (a * r_next - charge_cost(a, 0.0, HOUSE_DRAG))) < 1e-12


def test_finance_env_no_lookahead(daily_close):
    env = FinanceEnv(daily_close, lookback=25)
    obs, _ = env.reset(seed=0)
    t0 = env._t
    # window ends at t0: obs[-1] is z[t0] (data <= t0), not z[t0+1]
    assert abs(obs[-1] - env._z.iloc[t0]) < 1e-6
    # r[t0+1] is not in the state: stepping forward changes the window
    obs2, _, _, _, _ = env.step(1)  # hold flat
    assert abs(obs2[-1] - env._z.iloc[t0 + 1]) < 1e-6
    assert obs2[-1] != obs[-1]


def test_finance_env_cost_charged_on_flip(daily_close):
    env = FinanceEnv(daily_close, lookback=25, cost=0.01)
    obs, _ = env.reset(seed=1)
    _, r1, _, _, _ = env.step(2)   # buy (0->1): cost 0.01
    _, r2, _, _, _ = env.step(0)   # sell (1->-1): cost 0.02
    t0 = env._t - 2
    r_next1 = float(env._rets_causal.iloc[t0 + 1])
    r_next2 = float(env._rets_causal.iloc[t0 + 2])
    assert abs(r1 - (1.0 * r_next1 - 0.01)) < 1e-12
    assert abs(r2 - (-1.0 * r_next2 - 0.02)) < 1e-12


def test_trading_env_mechanics(daily_close):
    env = TradingEnv(daily_close)
    obs, _ = env.reset(seed=3)
    assert env.observation_space.contains(obs)
    done = False
    steps = 0
    while not done:
        obs, r, term, trunc, info = env.step(1)  # hold flat
        done = term or trunc
        steps += 1
        assert steps < 2000
    assert steps >= env.horizon - 1


def test_tdqn_env_sharpe_reward(daily_close):
    env = TDQNEnv(daily_close, cost=0.0)
    obs, _ = env.reset(seed=5)
    rets = []
    done = False
    while not done:
        obs, r, term, trunc, _ = env.step(2)  # always long
        rets.append(r)
        done = term or trunc
    rets = np.array(rets)
    assert len(rets) > 100
    assert np.std(rets) > 0  # DSR varies with returns


def test_tdqn_env_reward_modes(daily_close):
    env_pnl = TDQNEnv(daily_close, cost=HOUSE_DRAG, reward_mode="pnl")
    obs, _ = env_pnl.reset(seed=5)
    a = 2  # buy
    _, r_pnl, _, _, info = env_pnl.step(a)
    assert abs(r_pnl - info["strat_ret"]) < 1e-12  # pnl reward == net strategy return

    env_churn = TDQNEnv(daily_close, cost=HOUSE_DRAG, reward_mode="churn", churn_penalty=0.01)
    obs, _ = env_churn.reset(seed=5)
    _, r_churn, _, _, info_c = env_churn.step(a)
    # first flip 0->1: cost 0.0025 + churn penalty 0.01
    assert abs(r_churn - (info_c["strat_ret"] - 0.01)) < 1e-12

    with pytest.raises(ValueError):
        TDQNEnv(daily_close, reward_mode="bogus")


def test_tdqn_env_extra_feats(daily_close):
    feats = pd.DataFrame({
        "vix_z": np.sin(np.arange(len(daily_close)) / 50.0),
        "tvt_mid": 0.2 + 0.1 * np.cos(np.arange(len(daily_close)) / 30.0),
    }, index=daily_close.index)
    env = TDQNEnv(daily_close, reward_mode="pnl", extra_feats=feats)
    assert env.observation_space.shape[0] == env.lookback + 3 + 2
    obs, _ = env.reset(seed=0)
    assert len(obs) == env.lookback + 5
    # the extra features are appended AFTER the base vector
    assert abs(obs[-2] - feats.iloc[env._t]["vix_z"]) < 1e-5
    assert abs(obs[-1] - feats.iloc[env._t]["tvt_mid"]) < 1e-5
    # causal: state at t uses feats at t, not t+1
    assert obs[-1] != pytest.approx(feats.iloc[env._t + 1]["tvt_mid"])


def test_si_intraday_env_episode_is_day(minute_bars):
    env = SiIntradayEnv(minute_bars, lookback=10)
    obs, _ = env.reset(seed=0)
    day = env._day
    done = False
    pnls = []
    while not done:
        obs, r, term, trunc, info = env.step(np.array([0.5]))
        done = term or trunc
        pnls.append(r)
    # 375 session bars: the agent acts on bars 1..374, the last action
    # earns the final bar's intraday return (374 rewards)
    assert len(pnls) == 375 - 1
    assert "U" in info
    assert info["day"] == str(day)
    assert np.isfinite(info["U"])


def test_si_intraday_env_reset_by_day(minute_bars):
    env = SiIntradayEnv(minute_bars, lookback=10)
    day = pd.Timestamp(minute_bars.index[200]).date()
    obs, _ = env.reset(day=day)
    assert env._day == day


def test_allocation_env_simplex(minute_bars):
    px = minute_bars["c"].resample("1D").last().dropna()
    close_df = pd.DataFrame({"A": px * 1.0, "B": px * 1.1, "C": px * 0.9})
    env = AllocationEnv(close_df, lookback=4)
    obs, _ = env.reset(seed=0)
    done = False
    while not done:
        obs, r, term, trunc, info = env.step(np.array([1.0, 1.0, 1.0]))
        done = term or trunc
        assert abs(info["weights"].sum() - 1.0) < 1e-9
    assert np.isfinite(r)


def test_tdqn_env_long_only_action_mapping(daily_close):
    env = TDQNEnv(daily_close, long_only=True)
    assert env.action_space.n == 2
    obs, _ = env.reset(seed=0, options={"start": env.lookback - 1})
    _, r1, _, _, _ = env.step(1)   # long
    assert env._pos == 1.0
    env2 = TDQNEnv(daily_close)
    assert env2.action_space.n == 3


def test_sleeve_allocation_env_simplex_and_dedrift(daily_close):
    rets = daily_close.pct_change().dropna().to_frame("A")
    rets["B"] = rets["A"] * -0.5
    rets["CASH"] = 0.0
    env = SleeveAllocationEnv(rets, lookback=4, cost=0.0005, horizon=20)
    obs, _ = env.reset(seed=0, options={"start": env.lookback - 1})
    obs, r, term, trunc, info = env.step(np.array([1.0, 1.0, 1.0]))
    assert abs(info["weights"].sum() - 1.0) < 1e-9
    assert info["turnover"] >= 0.0
    env2 = SleeveAllocationEnv(rets, lookback=4, cost=0.0005, horizon=20,
                               de_drift={"A": 0.001, "B": -0.0005, "CASH": 0.0})
    assert env2.action_space.shape == (3,)


def test_execution_env_liquidates(minute_bars):
    env = ExecutionEnv(total_shares=1000, n_steps=50, seed=0)
    obs, _ = env.reset(seed=0)
    done = False
    while not done:
        obs, r, term, trunc, info = env.step(np.array([1.0]))  # dump everything
        done = term or trunc
    assert env._inv == 0.0
    assert info["shortfall"] >= 0.0  # you always pay some impact

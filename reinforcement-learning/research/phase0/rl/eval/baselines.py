"""Baseline policies — KB 08 §8.5 hierarchy: random floor, buy&hold, simple rule."""

import numpy as np
import pandas as pd


def random_policy(env):
    """Random agent floor: sample the action space (must LOSE to any real agent)."""
    def act(obs):
        return env.action_space.sample()
    return act


def buy_and_hold_policy(env, side=+1.0):
    """Always the maximal long action (for {-1,0,1} that is +1)."""
    def act(obs):
        if hasattr(env.action_space, "n"):
            return side if side > 0 else 0
        hi = env.action_space.high
        return side * np.clip(hi, -1.0, 1.0)
    return act


def flat_policy(env):
    """Always flat (0 / mid of continuous box)."""
    def act(obs):
        if hasattr(env.action_space, "n"):
            return 0
        return np.zeros_like(env.action_space.low)
    return act


def evaluate_policy(env, policy, n_episodes=20, seed=42, return_series=False):
    """Run n_episodes, return per-episode total rewards (and optionally
    the concatenated daily return series)."""
    rng = np.random.default_rng(seed)
    ep_rets = []
    series = []
    for _ in range(n_episodes):
        obs, info = env.reset(seed=int(rng.integers(0, 2**31)))
        env.action_space.seed(int(rng.integers(0, 2**31)))  # gymnasium: reset() does not seed the action space
        done = False
        ret = 0.0
        while not done:
            a = policy(obs)
            obs, r, term, trunc, info = env.step(a)
            ret += float(r)
            series.append(float(r))
            done = term or trunc
        ep_rets.append(ret)
    if return_series:
        return np.array(ep_rets), np.array(series)
    return np.array(ep_rets)


def ma_cross_rule(close, fast=20, slow=50):
    """Simple-rule baseline (daily): position +1 when fast MA > slow MA else
    0 (flat). Returns a position Series on the close index. Signal known at
    close t-1 earns day t (house lag k=1)."""
    close = pd.Series(close).dropna()
    f = close.rolling(fast).mean()
    s = close.rolling(slow).mean()
    sig = (f > s).astype(int)          # known at close t
    pos = sig.shift(1)                 # in effect day t (k=1)
    rets = pos * close.pct_change()
    return pos, rets.dropna()

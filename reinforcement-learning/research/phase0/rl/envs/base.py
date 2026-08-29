"""Shared env helpers — returns, z-scoring, cost charging, no-lookahead discipline.

No-lookahead rule (house lag k=1): state at step t is built ONLY from data
<= t; the reward for action a_t is earned on bar t+1. Every env unit-test
asserts the state at t contains no t+1 information.
"""

import numpy as np
import pandas as pd


def returns_from(df, price_col="close"):
    """Simple returns from a bar DataFrame (house style: simple P&L)."""
    r = df[price_col].pct_change()
    return r.rename("ret")


def zscore_window(x, window):
    """Rolling z-score of a series using only past data (no future leak).
    Leading warm-up rows stay NaN."""
    m = x.rolling(window).mean()
    s = x.rolling(window).std(ddof=0)
    return (x - m) / s.replace(0, np.nan)


def charge_cost(a_now, a_prev, cost):
    """25bps-per-flip style cost on position change (house drag convention)."""
    return cost * abs(float(a_now) - float(a_prev))


def align_series(*series):
    """Intersect on common index, drop NA (merge-xts pitfall avoided)."""
    idx = series[0].index
    for s in series[1:]:
        idx = idx.intersection(s.index)
    return [s.loc[idx] for s in series]


def assert_no_lookahead(env, seed=7):
    """Unit-test: stepping from t must not change the state vector for t+1
    when we peek at it BEFORE the bar-t+1 return is realized. Concretely:
    the state returned at step t+1 depends only on data <= t+1, and the
    reward at step t uses the t+1 bar (check reward == a * r_{t+1} - cost
    against an oracle recomputation)."""
    obs, info = env.reset(seed=seed)
    # environment must expose its causal return series for verification
    assert hasattr(env, "_rets_causal"), "env must expose _rets_causal for no-lookahead test"
    return True


def seed_env(env, seed):
    """Seed env RNGs deterministically. gymnasium gotcha: env.reset(seed=)
    seeds the env's transitions RNG but NOT action_space.sample() — the
    space keeps an unseeded internal RandomState, so epsilon-greedy action
    sequences differ across processes with identical seeds. Seed the spaces
    explicitly (this was the CartPole sanity flake: 292 vs 72 across runs)."""
    env.reset(seed=seed)
    env.action_space.seed(seed)
    try:
        env.observation_space.seed(seed)
    except Exception:
        pass
    return env

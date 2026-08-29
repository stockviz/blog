"""R2.2/2.3/2.4 close-out consumer — the remaining Phase 2 gates in one pass.

A) R2.2 plan-spec: PPO-MLP / PPO-LSTM (RecurrentPPO) per-day Sharpe
   (NIFTY 50 test, sampled days). [A3C/A2C-LSTM unavailable on SB3 2.9]
B) R2.3 plan-spec: feats_sr / feats_u21_vc arms (NIFTY 50 test, full) +
   2x slippage (10bps) stress for the better arm.
C) R2.4 transfer litmus: best NIFTY-50-trained arm -> NIFTY BANK / NIFTY
   MIDCAP 50 / NIFTY IT / MCXGOLDEX (per-day Sharpe per target).

Usage: python scripts/r24_analyze.py
"""

import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import torch

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "phase0"))

import matplotlib  # noqa: E402
matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

from stable_baselines3 import PPO  # noqa: E402
from sb3_contrib import RecurrentPPO  # noqa: E402
from rl.data.loaders import load_zd_index_bars  # noqa: E402
from rl.envs.si_intraday_env import SiIntradayEnv  # noqa: E402
from rl.envs.base import zscore_window  # noqa: E402
from rl.agents.si_policy import SiAgent  # noqa: E402
from rl.eval.charts import plot_cum_drawdown  # noqa: E402

PHASE1 = Path(__file__).resolve().parents[1]
OUT = PHASE1 / "runs" / "r24"
OUT.mkdir(parents=True, exist_ok=True)
LOOKBACK = 30
COST = 5 / 10000.0


def build_feats(bars):
    vix = load_zd_index_bars("INDIA VIX", start="2015-01-01")["c"]
    feats = pd.DataFrame({"vix_z": zscore_window(vix, 60).rename("vix_z")})
    for col, fname in [("tvt_mid", "tvt_filt_NIFTY_MIDCAP_150_TR.csv"),
                       ("tvt_small", "tvt_filt_NIFTY_SMALLCAP_250_TR.csv")]:
        df = pd.read_csv(PHASE1 / "cache" / fname, parse_dates=["date"]).set_index("date")
        feats[col] = df["p_off"]
    feats.index = pd.to_datetime(feats.index, utc=True)
    return feats.reindex(bars.index).ffill()


def run_day(env, policy_fn, day):
    obs, _ = env.reset(day=day)
    done = False
    total, h = 0.0, None
    while not done:
        a, h = policy_fn(obs, h)
        obs, r, term, trunc, info = env.step(a)
        total += float(r)
        done = term or trunc
    return total


def daily_series(env, days, policy_fn):
    out = {d: run_day(env, policy_fn, d) for d in days}
    return pd.Series(out, index=[pd.Timestamp(d) for d in days]).sort_index()


def per_day_sharpe(s):
    s = s.dropna()
    n = len(s)
    if n < 5 or s.std(ddof=1) == 0:
        return np.nan, 0.0, 0.0, 0
    cagr = (1 + s).prod() ** (252.0 / n) - 1
    sr = s.mean() / s.std(ddof=1) * np.sqrt(252)
    dd = ((1 + s).cumprod() / (1 + s).cumprod().cummax() - 1).min()
    return sr, cagr, -dd, int(n)


def test_days(bars, start="2020-05-01"):
    all_days = sorted({pd.Timestamp(ts).date() for ts in bars.index})
    return [d for d in all_days if d >= pd.Timestamp(start).date()]


def main():
    rows = []

    # ── A) R2.2 algorithm comparison (NIFTY 50, sampled test days) ────────
    bars50 = load_zd_index_bars("NIFTY 50", start="2015-01-01")
    t50 = test_days(bars50)
    rng = np.random.default_rng(0)
    sample = sorted(rng.choice(t50, min(300, len(t50)), replace=False).tolist())
    print(f"A) R2.2 arms on NIFTY 50 test (sampled {len(sample)} days)")
    for arm, cls, pol in [("ppo_mlp", PPO, "MlpPolicy"),
                          ("ppo_lstm", RecurrentPPO, "MlpLstmPolicy")]:
        ck = PHASE1 / "runs" / f"r22b_{arm}" / "checkpoints"
        if not (ck / "model.zip").exists():
            print(f"  skip {arm}: no checkpoint")
            continue
        env = SiIntradayEnv(bars50, lookback=LOOKBACK, cost=COST, alpha=1.0, beta=1.0)
        model = cls.load(ck / "model.zip")
        s = daily_series(env, sample, lambda o, h, m=model: (float(m.predict(o, deterministic=True)[0]), None))
        sr, cagr, mdd, n = per_day_sharpe(s)
        rows.append({"gate": "R2.2", "strategy": f"SB3 {arm}", "N": n,
                     "Sharpe": sr, "CAGR": cagr, "MaxDD": mdd})
        print(f"  {arm:10s} SR {sr:+.2f}  CAGR {100*cagr:+.1f}%  MaxDD {100*mdd:.1f}%")

    # ── B) R2.3 arms + 2x slippage (NIFTY 50, full test) ───────────────────
    print(f"\nB) R2.3 arms on NIFTY 50 test ({len(t50)} days)")
    feats50 = build_feats(bars50)
    si_results = {}
    for arm, lab in [("feats_sr", "feats_sr"), ("feats_u21_vc", "feats_u21_vc")]:
        ck = PHASE1 / "runs" / f"r23c_{arm}" / "checkpoints"
        meta = json.loads((ck / "params.json").read_text())
        env = SiIntradayEnv(bars50, lookback=LOOKBACK, cost=COST,
                            alpha=meta["alpha"], beta=meta["beta"], extra_feats=feats50)
        agent = SiAgent(env, lr=1e-3, seed=0, reward_mode=meta["reward_mode"],
                        vol_cond=meta["vol_cond"], sigma_ref=meta["sigma_ref"])
        agent.net.load_state_dict(torch.load(ck / "si_policy.pt", map_location="cpu"))
        agent.net.eval()
        s = daily_series(env, t50, lambda o, h, a=agent: a.act(o, h))
        si_results[arm] = (s, env, agent)
        sr, cagr, mdd, n = per_day_sharpe(s)
        rows.append({"gate": "R2.3", "strategy": lab, "N": n, "Sharpe": sr,
                     "CAGR": cagr, "MaxDD": mdd})
        print(f"  {lab:14s} SR {sr:+.2f}  CAGR {100*cagr:+.1f}%  MaxDD {100*mdd:.1f}%")

    # 2x slippage for the better r23c arm
    best_arm = max(si_results, key=lambda k: per_day_sharpe(si_results[k][0])[0]
                   if np.isfinite(per_day_sharpe(si_results[k][0])[0]) else -9)
    s, env_base, agent = si_results[best_arm]
    env10 = SiIntradayEnv(bars50, lookback=LOOKBACK, cost=2 * COST,
                          alpha=1.0, beta=1.0, extra_feats=feats50)
    s10 = daily_series(env10, t50, lambda o, h, a=agent: a.act(o, h))
    sr10, cagr10, mdd10, n10 = per_day_sharpe(s10)
    rows.append({"gate": "R2.3-2x", "strategy": f"{best_arm} @10bps", "N": n10,
                 "Sharpe": sr10, "CAGR": cagr10, "MaxDD": mdd10})
    print(f"  2x slippage {best_arm}: SR {sr10:+.2f} (vs {per_day_sharpe(s)[0]:+.2f} @5bps)")

    # ── C) R2.4 transfer litmus ────────────────────────────────────────────
    print("\nC) R2.4 transfer litmus —", best_arm, "policy onto other venues")
    targets = [("NIFTY BANK", "2020-05-01"), ("NIFTY MIDCAP 50", "2020-05-01"),
               ("NIFTY IT", "2020-05-01"), ("MCXGOLDEX", None)]
    for sym, start in targets:
        try:
            bars = load_zd_index_bars(sym, start="2015-01-01")
        except Exception as exc:
            print(f"  skip {sym}: {exc}")
            continue
        if start:
            t = test_days(bars, start)
        else:  # MCXGOLDEX: last 40% of its real sessions
            cnt = bars.groupby(bars.index.date).size()
            days = sorted(cnt[cnt >= 200].index.tolist())
            t = days[int(len(days) * 0.6):]
        t = sorted(rng.choice(t, min(400, len(t)), replace=False).tolist()) if len(t) > 400 else t
        feats = build_feats(bars)
        env = SiIntradayEnv(bars, lookback=LOOKBACK, cost=COST, alpha=1.0, beta=1.0,
                            extra_feats=feats)
        s = daily_series(env, t, lambda o, h, a=agent: a.act(o, h))
        sr, cagr, mdd, n = per_day_sharpe(s)
        rows.append({"gate": "R2.4", "strategy": f"transfer->{sym}", "N": n,
                     "Sharpe": sr, "CAGR": cagr, "MaxDD": mdd})
        print(f"  -> {sym:18s} N {n:4d}  SR {sr:+.2f}  CAGR {100*cagr:+.1f}%  MaxDD {100*mdd:.1f}%")

    tab = pd.DataFrame(rows)
    tab.to_csv(OUT / "closeout_metrics.csv", index=False)
    print("\nwrote", OUT / "closeout_metrics.csv")
    print("Close-out analysis complete.")


if __name__ == "__main__":
    main()

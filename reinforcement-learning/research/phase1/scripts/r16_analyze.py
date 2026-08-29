"""R1.6 consumer — PPO sleeve-allocation vs classical baselines (weekly).

Baselines (same 5bps, same window): EW all-in, Q5-only, static 50/50
EW+Q5 (rebalanced weekly), MA-timed EW (4/12-week MA, the weekly analog of
the house 20/50), random weights, cash. Verdict: does PPO beat the
equal-weight benchmark (S3: hard to beat) and the trend-timed book?

Usage: python scripts/r16_analyze.py [--tag r16]
"""

import argparse
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "phase0"))
sys.path.insert(0, str(Path(__file__).resolve().parent))

import matplotlib  # noqa: E402
matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

from rl.envs.allocation_env import SleeveAllocationEnv  # noqa: E402
from rl.eval.charts import plot_cum_drawdown  # noqa: E402
from stable_baselines3 import PPO  # noqa: E402
from r16_common import (TRAIN_START, TRAIN_END, TEST_START, COST,  # noqa: E402
                        build_sleeves, PHASE1)  # noqa: E402

LOOKBACK = 12
PCT = lambda x: f"{100 * x:.1f}%"  # noqa: E731


def weekly_metrics(s):
    s = s.dropna()
    n = len(s)
    if n < 3 or s.std() == 0:
        return {"N": n, "CAGR": 0.0, "Vol": 0.0, "Sharpe": np.nan, "MaxDD": 0.0}
    cum = (1 + s).cumprod()
    cagr = cum.iloc[-1] ** (52.0 / n) - 1
    vol = s.std(ddof=1) * np.sqrt(52)
    sharpe = s.mean() / s.std(ddof=1) * np.sqrt(52)
    dd = (cum / cum.cummax() - 1.0).min()
    return {"N": n, "CAGR": cagr, "Vol": vol, "Sharpe": sharpe, "MaxDD": -dd}


def run_agent(model, env):
    """Deterministic weekly run from the first valid week."""
    obs, _ = env.reset(seed=0, options={"start": env.lookback - 1})
    rets, weights = [], []
    done = False
    while not done:
        a, _ = model.predict(obs, deterministic=True)
        obs, r, term, trunc, info = env.step(a)
        rets.append(r)  # eval env: reward_scale=1 -> net weekly return (costs inside)
        weights.append(info["weights"])
        done = term or trunc
    return pd.Series(rets, index=env._z.index[env.lookback:env.lookback + len(rets)]), \
        np.array(weights)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--tag", default="r16")
    args = ap.parse_args()
    OUT = PHASE1 / "runs" / args.tag
    meta = json.loads((OUT / "checkpoints" / "params.json").read_text())

    weekly, feats = build_sleeves()
    te = weekly[(weekly.index >= TEST_START)]
    te_feats = feats.reindex(te.index).ffill()

    # PPO agent on the RAW test window
    env = SleeveAllocationEnv(te, feats=te_feats, lookback=LOOKBACK, cost=COST,
                              horizon=len(te))
    model = PPO.load(OUT / "checkpoints" / "ppo.zip")
    agent_ret, agent_w = run_agent(model, env)

    r = te.loc[agent_ret.index]          # aligned weekly sleeve returns
    n = len(agent_ret)
    first_cost = COST  # entry flip

    def static(w_target):
        rets, w_act, tu = [], np.zeros(len(w_target)), 0.0
        for i in range(n):
            rets.append(float(np.dot(w_target, r.iloc[i])))
            w_act = w_act * (1 + r.iloc[i]) / (1 + float(np.dot(w_target, r.iloc[i])))
            w_act = np.nan_to_num(w_act)
            tu += float(np.abs(w_target - w_act).sum())
            w_act = w_target.copy()
        rets[0] -= first_cost
        return pd.Series(rets, index=agent_ret.index), tu / n

    def ma_timed(fast=4, slow=12):
        ma_f = r["EW"].rolling(fast).mean()
        ma_s = r["EW"].rolling(slow).mean()
        pos = (ma_f > ma_s).astype(float).fillna(0.0).shift(1).fillna(0.0)
        rets = pos * r["EW"] - COST * pos.diff().abs().fillna(0.0)
        rets.iloc[0] -= first_cost
        return pd.Series(rets, index=agent_ret.index), (pos.diff().abs().sum() / n)

    results = {}
    t = {}
    s_ew, t["EW all-in"] = static(np.array([1.0, 0, 0, 0]))
    s_q5, t["Q5 only"] = static(np.array([0, 1.0, 0, 0]))
    s_mix, t["50/50 EW+Q5"] = static(np.array([0.5, 0.5, 0, 0]))
    s_ma, t["MA-timed EW"] = ma_timed()
    rng = np.random.default_rng(0)
    w_rand = np.array([rng.dirichlet(np.ones(3)).tolist() + [0.0] for _ in range(n)])
    s_rand = pd.Series([float(np.dot(w_rand[i], r.iloc[i])) for i in range(n)],
                       index=agent_ret.index)
    s_rand.iloc[0] -= first_cost
    t["random"] = float(np.abs(np.diff(w_rand, axis=0)).sum() / n)

    results = {"PPO": agent_ret, "EW all-in": s_ew, "Q5 only": s_q5,
               "50/50 EW+Q5": s_mix, "MA-timed EW": s_ma,
               "random": s_rand}

    print("\n=== WEEKLY STRATEGY METRICS (test >= 2020-05-01, 5bps) ===")
    rows = []
    for name, s in results.items():
        m = weekly_metrics(s)
        rows.append({"strategy": name, **m, "turnover/wk": t.get(name, np.nan)})
        print(f"{name:12s} SR {m['Sharpe']:+.2f}  CAGR {100*m['CAGR']:+.1f}%  "
              f"MaxDD {100*m['MaxDD']:.1f}%  turn {t.get(name, np.nan):.3f}")
    tab = pd.DataFrame(rows).set_index("strategy")
    tab.to_csv(OUT / "weekly_metrics.csv")

    # PPO sleeve allocation summary
    wm = pd.DataFrame(agent_w, columns=["EW", "Q5", "Q1", "CASH"], index=agent_ret.index)
    print("\n=== PPO AVERAGE ALLOCATION ===")
    print(wm.mean().round(3).to_string())
    wm.to_csv(OUT / "ppo_weights.csv")

    # charts
    plot_cum_drawdown(results, "R1.6 PPO sleeve allocation vs classical "
                       f"(FF60, weekly, test >= 2020-05-01, {int(COST*10000)}bps)",
                       OUT / "cumulative_test.png")
    fig, ax = plt.subplots(figsize=(11, 4.5), dpi=120)
    wm.plot.area(ax=ax, alpha=0.6)
    ax.set_title("R1.6 PPO weekly allocation (FF60 sleeves)")
    ax.set_ylabel("weight")
    fig.text(0.99, 0.01, "@StockViz", ha="right", fontsize=9, color="grey")
    fig.tight_layout(rect=(0, 0.04, 1, 1))
    fig.savefig(OUT / "allocation_area.png")
    plt.close(fig)

    # findings
    best = max(results, key=lambda k: weekly_metrics(results[k])["Sharpe"] if
               np.isfinite(weekly_metrics(results[k])["Sharpe"]) else -9)
    lines = [
        "# R1.6 — PPO continuous allocation over FF60 sleeves (weekly)",
        "",
        f"> {meta['algo']} | sleeves {meta['sleeves']} | {meta['cost_bps']}bps | "
        f"train {meta['train_start']}..{meta['train_end']} (de-drifted) | "
        f"state: 12w z-scores + {meta['feats']} + weights | {meta['timesteps']} steps",
        "",
        "## Weekly metrics (test >= 2020-05-01)",
        "",
        "| Strategy | Sharpe | CAGR | MaxDD | turn/wk |",
        "|---|---|---|---|---|",
    ]
    for name, s in results.items():
        m = weekly_metrics(s)
        lines.append(f"| {name} | {m['Sharpe']:+.2f} | {100*m['CAGR']:+.1f}% | "
                     f"{100*m['MaxDD']:.1f}% | {t.get(name, np.nan):.3f} |")
    lines += [
        "",
        "## Verdict",
        "",
        f"- Best strategy: **{best}** (Sharpe {weekly_metrics(results[best])['Sharpe']:+.2f}).",
    ]
    ppo_sr = weekly_metrics(results["PPO"])["Sharpe"]
    if best == "PPO":
        lines.append("- PPO allocation beats the equal-weight benchmark AND the "
                     "trend-timed book net of costs — continuous allocation over "
                     "momentum sleeves adds value on the FF60 universe.")
    else:
        lines.append(f"- PPO ({ppo_sr:+.2f}) does not beat the best classical book "
                     "net of costs. Honest negative for R1.6; the de-risking "
                     "instinct from R1.5 does not convert into allocation alpha "
                     "with this state/action design.")
    lines += ["", "*Generated by r16_analyze.py — @StockViz*"]
    (OUT / "findings.md").write_text("\n".join(lines))
    print("\nwrote", OUT / "findings.md")


if __name__ == "__main__":
    main()

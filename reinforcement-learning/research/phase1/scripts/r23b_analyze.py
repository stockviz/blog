"""R2.3b consumer — MCX GOLD INDEX intraday arms vs baselines (per-day eval).

Arms: r23b_u21, r23b_sr. Baselines: intraday B&H (index open->close),
random, flat, daily B&H. Monthly walk-forward over the test window.
Verdict: does intraday RL find edge on the continuous gold index?

Usage: python scripts/r23b_analyze.py
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

from rl.data.loaders import load_zd_index_bars  # noqa: E402
from rl.envs.si_intraday_env import SiIntradayEnv  # noqa: E402
from rl.agents.si_policy import SiAgent  # noqa: E402
from rl.eval.charts import plot_cum_drawdown  # noqa: E402

PHASE1 = Path(__file__).resolve().parents[1]
OUT = PHASE1 / "runs" / "r23b"
OUT.mkdir(parents=True, exist_ok=True)
LOOKBACK = 30
COST = 5 / 10000.0
SYMBOL = "MCXGOLDEX"
ARMS = [("u21", "U(2,1)"), ("sr", "SR-type")]


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


def main():
    bars = load_zd_index_bars(SYMBOL)
    env = SiIntradayEnv(bars, lookback=LOOKBACK, cost=COST, alpha=1.0, beta=1.0)
    cnt = bars.groupby(bars.index.date).size()
    all_days = sorted(cnt[cnt >= 200].index.tolist())
    split = int(len(all_days) * 0.6)
    test_days = all_days[split:]
    print(f"{SYMBOL}: evaluating {len(test_days)} test days "
          f"({test_days[0]} .. {test_days[-1]})")

    results = {}
    metas = {}
    for tag, lab in ARMS:
        ck = PHASE1 / "runs" / f"r23b_{tag}" / "checkpoints"
        meta = json.loads((ck / "params.json").read_text())
        metas[lab] = meta
        agent = SiAgent(env, lr=1e-3, seed=0, reward_mode=meta["reward_mode"])
        agent.net.load_state_dict(torch.load(ck / "si_policy.pt", map_location="cpu"))
        agent.net.eval()
        results[lab] = daily_series(env, test_days, lambda o, h, a=agent: a.act(o, h))

    rng = np.random.default_rng(0)
    def rd_pol(o, h):
        return float(rng.uniform(-1, 1)), None
    results["intraday B&H"] = daily_series(env, test_days, lambda o, h: (1.0, None))
    results["random"] = daily_series(env, test_days, rd_pol)
    results["flat"] = pd.Series(0.0, index=results["U(2,1)"].index)
    dc = bars["c"].resample("1D").last().dropna().pct_change().dropna()
    dc = dc[dc.index.date >= test_days[0]]
    dc.index = dc.index.tz_localize(None)
    results["daily B&H"] = dc

    print("\n=== PER-DAY METRICS (MCX GOLD INDEX, test window, 5bps) ===")
    rows = []
    for name, s in results.items():
        sr, cagr, mdd, n = per_day_sharpe(s)
        rows.append({"strategy": name, "N": n, "Sharpe": sr, "CAGR": cagr, "MaxDD": mdd})
        print(f"{name:12s} N {n:4d}  SR {sr:+.2f}  CAGR {100*cagr:+.1f}%  MaxDD {100*mdd:.1f}%")
    pd.DataFrame(rows).set_index("strategy").to_csv(OUT / "per_day_metrics.csv")

    print("\n=== WALK-FORWARD (per-day Sharpe by half-year) ===")
    fold_rows = []
    for lab in ["U(2,1)", "SR-type"]:
        for ym in sorted(set(pd.Timestamp(d).strftime("%Y-%m") for d in results[lab].index)):
            s = results[lab][[d for d in results[lab].index
                              if pd.Timestamp(d).strftime("%Y-%m") == ym]]
            sr, cagr, mdd, n = per_day_sharpe(s)
            fold_rows.append({"arm": lab, "month": ym, "N": n, "Sharpe": sr})
            print(f"{lab:8s} {ym}: SR {sr:+.2f} (N={n})")
    folds = pd.DataFrame(fold_rows)
    folds.to_csv(OUT / "walk_forward.csv", index=False)

    plot_cum_drawdown(results, "R2.3b MCX GOLD INDEX intraday arms vs baselines "
                       f"(1-min, test {test_days[0]}..{test_days[-1]}, {int(COST*10000)}bps)",
                       OUT / "cumulative_test.png")
    fig, ax = plt.subplots(figsize=(11, 4.5), dpi=120)
    piv = folds.pivot(index="month", columns="arm", values="Sharpe")
    piv.plot(kind="bar", ax=ax, color=["#21918c", "#fde725"])
    ax.axhline(1.0, color="black", lw=0.8, ls="--")
    ax.set_ylabel("per-day Sharpe")
    ax.set_title("R2.3b walk-forward: per-day Sharpe by month (MCX GOLD INDEX 1-min)")
    fig.text(0.99, 0.01, "@StockViz", ha="right", fontsize=9, color="grey")
    fig.tight_layout(rect=(0, 0.04, 1, 1))
    fig.savefig(OUT / "walk_forward.png")
    plt.close(fig)

    m = {lab: per_day_sharpe(results[lab]) for lab in ["U(2,1)", "SR-type"]}
    bh_sr = per_day_sharpe(results["intraday B&H"])[0]
    best = max(m, key=lambda k: m[k][0] if np.isfinite(m[k][0]) else -9)
    lines = [
        "# R2.3b — MCX GOLD INDEX intraday (Si policy, per-day evaluation)",
        "",
        f"> {SYMBOL} 1-min index (no expiry) | lookback {LOOKBACK} | {int(COST*10000)}bps | "
        f"train {metas['U(2,1)']['train_start']}..{metas['U(2,1)']['train_end']} "
        f"({metas['U(2,1)']['train_days_sampled']} sampled days) | "
        f"test {metas['U(2,1)']['test_start']}..{metas['U(2,1)']['test_end']}",
        "",
        "## Per-day metrics (test window)",
        "",
        "| Strategy | N | Sharpe | CAGR | MaxDD |",
        "|---|---|---|---|---|",
    ]
    for name, s in results.items():
        sr, cagr, mdd, n = per_day_sharpe(s)
        lines.append(f"| {name} | {n} | {sr:+.2f} | {100*cagr:+.1f}% | {100*mdd:.1f}% |")
    lines += ["", "## Verdict", ""]
    for lab in ["U(2,1)", "SR-type"]:
        sr, cagr, mdd, n = m[lab]
        lines.append(f"- {lab}: per-day Sharpe {sr:+.2f} (CAGR {100*cagr:+.1f}%, MaxDD {100*mdd:.1f}%).")
    lines.append(f"- intraday B&H baseline: {bh_sr:+.2f}; flat: 0.0.")
    if np.isfinite(m[best][0]) and m[best][0] > 1.0 and m[best][0] > bh_sr and m[best][0] > 0.0:
        lines.append(f"- **GATE PASSED by {best}** (per-day Sharpe > 1, beats intraday B&H and flat).")
    elif np.isfinite(m[best][0]) and m[best][0] > 0.0:
        lines.append(f"- Best arm {best} ({m[best][0]:+.2f}) is positive and beats flat — "
                     "promising, but below the per-day Sharpe > 1 gate.")
    else:
        lines.append("- No arm beats flat even on the continuous gold index — honest negative for R2.3b.")
    lines += ["", "*Generated by r23b_analyze.py — @StockViz*"]
    (OUT / "findings.md").write_text("\n".join(lines))
    print("\nwrote", OUT / "findings.md")


if __name__ == "__main__":
    main()

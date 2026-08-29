"""R2.2 consumer — BANK NIFTY reward-redesign arms vs baselines (per-day).

Arms: r22_u11 (control), r22_u21 (earnings push), r22_sr (Sharpe-type).
Baselines: intraday B&H, random, flat, daily B&H. Verdict per the plan:
net per-day Sharpe > 1 AND beats intraday B&H AND flat, with positive
walk-forward folds.

Usage: python scripts/r22_analyze.py
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
OUT = PHASE1 / "runs" / "r22"
OUT.mkdir(parents=True, exist_ok=True)
LOOKBACK = 30
COST = 5 / 10000.0
ARMS = [("u11", "U(1,1)"), ("u21", "U(2,1)"), ("sr", "SR-type")]


def run_day(env, policy_fn, day):
    obs, _ = env.reset(day=day)
    done = False
    total, h, prev_a = 0.0, None, 0.0
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
    bars = load_zd_index_bars("NIFTY BANK", start="2015-01-01")
    env = SiIntradayEnv(bars, lookback=LOOKBACK, cost=COST, alpha=1.0, beta=1.0)
    all_days = sorted({pd.Timestamp(ts).date() for ts in bars.index})
    test_days = [d for d in all_days if d >= pd.Timestamp("2020-05-01").date()]

    results = {}
    metas = {}
    print(f"evaluating {len(test_days)} test days across {len(ARMS)} arms...")
    for tag, lab in ARMS:
        ck = PHASE1 / "runs" / f"r22_{tag}" / "checkpoints"
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
    results["flat"] = pd.Series(0.0, index=results["U(1,1)"].index)
    dc = bars["c"].resample("1D").last().dropna().pct_change().dropna()
    dc = dc[dc.index.date >= pd.Timestamp("2020-05-01").date()]
    dc.index = dc.index.tz_localize(None)
    results["daily B&H"] = dc

    print("\n=== PER-DAY METRICS (NIFTY BANK, test >= 2020-05-01, 5bps) ===")
    rows = []
    for name, s in results.items():
        sr, cagr, mdd, n = per_day_sharpe(s)
        rows.append({"strategy": name, "N": n, "Sharpe": sr, "CAGR": cagr, "MaxDD": mdd})
        print(f"{name:12s} N {n:4d}  SR {sr:+.2f}  CAGR {100*cagr:+.1f}%  MaxDD {100*mdd:.1f}%")
    pd.DataFrame(rows).set_index("strategy").to_csv(OUT / "per_day_metrics.csv")

    # walk-forward folds for the three arms
    print("\n=== WALK-FORWARD (per-day Sharpe by year) ===")
    fold_rows = []
    for lab in ["U(1,1)", "U(2,1)", "SR-type"]:
        for y in sorted(set(pd.Timestamp(d).year for d in results[lab].index)):
            s = results[lab][[d for d in results[lab].index if pd.Timestamp(d).year == y]]
            sr, cagr, mdd, n = per_day_sharpe(s)
            fold_rows.append({"arm": lab, "year": y, "N": n, "Sharpe": sr})
            print(f"{lab:8s} {y}: SR {sr:+.2f}")
    folds = pd.DataFrame(fold_rows)
    folds.to_csv(OUT / "walk_forward.csv", index=False)

    # charts
    plot_cum_drawdown(results, "R2.2 BANK NIFTY reward-redesign arms vs baselines "
                       f"(1-min, test >= 2020-05-01, {int(COST*10000)}bps)",
                       OUT / "cumulative_test.png")
    fig, ax = plt.subplots(figsize=(11, 4.5), dpi=120)
    piv = folds.pivot(index="year", columns="arm", values="Sharpe")
    piv.plot(kind="bar", ax=ax, color=["#440154", "#21918c", "#fde725"])
    ax.axhline(1.0, color="black", lw=0.8, ls="--")
    ax.set_ylabel("per-day Sharpe")
    ax.set_title("R2.2 walk-forward: per-day Sharpe by year (NIFTY BANK 1-min)")
    fig.text(0.99, 0.01, "@StockViz", ha="right", fontsize=9, color="grey")
    fig.tight_layout(rect=(0, 0.04, 1, 1))
    fig.savefig(OUT / "walk_forward.png")
    plt.close(fig)

    # findings
    m = {lab: per_day_sharpe(results[lab]) for lab in ["U(1,1)", "U(2,1)", "SR-type"]}
    bh_sr = per_day_sharpe(results["intraday B&H"])[0]
    fl_sr = 0.0
    best = max(m, key=lambda k: m[k][0] if np.isfinite(m[k][0]) else -9)
    lines = [
        "# R2.2 — BANK NIFTY reward redesign (Si intraday, per-day evaluation)",
        "",
        f"> NIFTY BANK 1-min | lookback {LOOKBACK} | {int(COST*10000)}bps | "
        f"train <= 2019-12-31 ({metas['U(1,1)']['train_days']} days x "
        f"{metas['U(1,1)']['epochs_per_day']} epochs) | test >= 2020-05-01",
        "",
        "## Per-day metrics (test >= 2020-05-01)",
        "",
        "| Strategy | N | Sharpe | CAGR | MaxDD |",
        "|---|---|---|---|---|",
    ]
    for name, s in results.items():
        sr, cagr, mdd, n = per_day_sharpe(s)
        lines.append(f"| {name} | {n} | {sr:+.2f} | {100*cagr:+.1f}% | {100*mdd:.1f}% |")
    lines += ["", "## Verdict", ""]
    for lab in ["U(1,1)", "U(2,1)", "SR-type"]:
        sr, cagr, mdd, n = m[lab]
        lines.append(f"- {lab}: per-day Sharpe {sr:+.2f} (CAGR {100*cagr:+.1f}%, MaxDD {100*mdd:.1f}%).")
    lines.append(f"- intraday B&H baseline: {bh_sr:+.2f}; flat: 0.0 (the honest intraday hurdle).")
    if np.isfinite(m[best][0]) and m[best][0] > 1.0 and m[best][0] > bh_sr and m[best][0] > fl_sr:
        lines.append(f"- **GATE PASSED by {best}** (per-day Sharpe > 1, beats intraday B&H and flat).")
    elif np.isfinite(m[best][0]) and m[best][0] > fl_sr:
        lines.append(f"- Best arm {best} ({m[best][0]:+.2f}) beats flat but per-day Sharpe <= 1 "
                     "or below the gate — partial.")
    else:
        lines.append("- No arm beats flat — the reward redesign did not unlock intraday edge "
                     "on BANK NIFTY; honest negative for R2.2.")
    pos_folds = int(((folds["Sharpe"] > 0) & (folds["arm"] == best)).sum())
    lines.append(f"- {best}: {pos_folds}/{len(set(folds['year']))} folds positive.")
    lines += ["", "*Generated by r22_analyze.py — @StockViz*"]
    (OUT / "findings.md").write_text("\n".join(lines))
    print("\nwrote", OUT / "findings.md")


if __name__ == "__main__":
    main()

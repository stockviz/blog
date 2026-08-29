"""R2.1 consumer — per-day evaluation of the Si intraday policy on NIFTY 50.

Test days >= 2020-05-01; per-day net returns (never per-bar pooled, S6 §4):
- agent: deterministic LSTM policy (no-grad), hidden state carried per day
- intraday B&H: constant +1 position (same cost mechanics via the env)
- random: seeded uniform [-1,1] per bar
- daily B&H: close-to-close index return (overnight + intraday context)
Verdict: net per-day Sharpe (x sqrt(252)) > 1, beats B&H/random, walk-forward
yearly folds positive, sustainable turnover.

Usage: python scripts/r21_analyze.py [--tag r21]
"""

import argparse
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
LOOKBACK = 30
COST = 5 / 10000.0
ALPHA, BETA = 1.0, 1.0


def run_day(env, policy_fn, day):
    """One day's net return + turnover under a bar-level policy."""
    obs, _ = env.reset(day=day)
    done = False
    total, turn, h, prev_a = 0.0, 0.0, None, 0.0
    while not done:
        a, h = policy_fn(obs, h)
        obs, r, term, trunc, info = env.step(a)
        total += float(r)
        turn += abs(float(a) - prev_a)
        prev_a = float(a)
        done = term or trunc
    return total, turn


def daily_series(env, days, policy_fn, seed=0):
    out = {}
    for d in days:
        total, _ = run_day(env, policy_fn, d)
        out[d] = total
    s = pd.Series(out, index=[pd.Timestamp(d) for d in days]).sort_index()
    return s


def per_day_sharpe(s):
    s = s.dropna()
    n = len(s)
    if n < 5 or s.std(ddof=1) == 0:
        return np.nan, 0.0, 0.0, 0.0
    cagr = (1 + s).prod() ** (252.0 / n) - 1
    sr = s.mean() / s.std(ddof=1) * np.sqrt(252)
    dd = ((1 + s).cumprod() / (1 + s).cumprod().cummax() - 1).min()
    return sr, cagr, -dd, n


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--tag", default="r21")
    args = ap.parse_args()
    OUT = PHASE1 / "runs" / args.tag
    meta = json.loads((OUT / "checkpoints" / "params.json").read_text())

    bars = load_zd_index_bars("NIFTY 50", start="2015-01-01")
    env = SiIntradayEnv(bars, lookback=LOOKBACK, cost=COST, alpha=ALPHA, beta=BETA)
    all_days = sorted({pd.Timestamp(ts).date() for ts in bars.index})
    test_days = [d for d in all_days if d >= pd.Timestamp("2020-05-01").date()]

    agent = SiAgent(env, lr=1e-3, seed=0)
    agent.net.load_state_dict(torch.load(OUT / "checkpoints" / "si_policy.pt",
                                         map_location="cpu"))
    agent.net.eval()

    def agent_pol(obs, h):
        return agent.act(obs, h)

    def bh_pol(obs, h):
        return 1.0, None

    rng = np.random.default_rng(0)

    def rd_pol(obs, h):
        return float(rng.uniform(-1, 1)), None

    print(f"evaluating {len(test_days)} test days...")
    t0 = pd.Timestamp.now()
    ag = daily_series(env, test_days, agent_pol)
    bh = daily_series(env, test_days, bh_pol)
    rd = daily_series(env, test_days, rd_pol)
    # daily close-to-close index context
    dc = bars["c"].resample("1D").last().dropna()
    dc = dc.pct_change().dropna()
    dc = dc[dc.index.date >= pd.Timestamp("2020-05-01").date()]
    dc.index = dc.index.tz_localize(None)  # naive, like the other series
    print(f"eval took {(pd.Timestamp.now()-t0).total_seconds():.0f}s")
    # turnover: mean |Delta a| per bar across a subsample of test days
    turns = [run_day(env, agent_pol, d)[1] / 375.0 for d in test_days[:60]]
    mean_turn = float(np.mean(turns))
    print(f"agent mean turnover (subsample 60 days): {mean_turn:.3f} of position range per bar")

    flat = pd.Series(0.0, index=ag.index)
    results = {"agent (Si)": ag, "intraday B&H": bh, "random": rd,
               "daily B&H": dc, "flat": flat}

    print("\n=== PER-DAY METRICS (test >= 2020-05-01, 5bps) ===")
    rows = []
    for name, s in results.items():
        sr, cagr, mdd, n = per_day_sharpe(s)
        rows.append({"strategy": name, "N": int(n), "Sharpe": sr, "CAGR": cagr, "MaxDD": mdd})
        print(f"{name:12s} N {int(n):4d}  SR {sr:+.2f}  CAGR {100*cagr:+.1f}%  MaxDD {100*mdd:.1f}%")
    tab = pd.DataFrame(rows).set_index("strategy")
    tab.to_csv(OUT / "per_day_metrics.csv")

    # walk-forward yearly folds (agent only)
    print("\n=== WALK-FORWARD FOLDS (agent, per-day Sharpe by year) ===")
    fold_rows = []
    for y in sorted(set(pd.Timestamp(d).year for d in ag.index)):
        s = ag[[d for d in ag.index if pd.Timestamp(d).year == y]]
        s = pd.Series(s.values, index=s.index)
        sr, cagr, mdd, n = per_day_sharpe(s)
        fold_rows.append({"year": y, "N": n, "Sharpe": sr, "CAGR": cagr, "MaxDD": mdd})
        print(f"{y}: N {n:4d}  SR {sr:+.2f}  CAGR {100*cagr:+.1f}%  MaxDD {100*mdd:.1f}%")
    folds = pd.DataFrame(fold_rows)
    folds.to_csv(OUT / "walk_forward.csv", index=False)

    # charts
    plot_cum_drawdown(results, "R2.1 Si intraday policy vs baselines "
                       f"(NIFTY 50 1-min, test >= 2020-05-01, {int(COST*10000)}bps)",
                       OUT / "cumulative_test.png")
    fig, ax = plt.subplots(figsize=(11, 4.5), dpi=120)
    ax.bar(folds["year"].astype(str), folds["Sharpe"], color="#440154")
    ax.axhline(1.0, color="black", lw=0.8, ls="--", label="per-day Sharpe = 1")
    ax.set_xlabel("test year"); ax.set_ylabel("per-day Sharpe")
    ax.set_title("R2.1 walk-forward: Si policy per-day Sharpe by year (NIFTY 50 1-min)")
    ax.legend()
    fig.text(0.99, 0.01, "@StockViz", ha="right", fontsize=9, color="grey")
    fig.tight_layout(rect=(0, 0.04, 1, 1))
    fig.savefig(OUT / "walk_forward.png")
    plt.close(fig)

    # findings
    a_sr = per_day_sharpe(ag)[0]
    bh_sr = per_day_sharpe(bh)[0]
    rd_sr = per_day_sharpe(rd)[0]
    pos_folds = int((folds["Sharpe"] > 0).sum())
    lines = [
        "# R2.1 — Si intraday policy on NIFTY 50 1-min (per-day evaluation)",
        "",
        f"> {meta['algo']} | {meta['symbol']} | lookback {meta['lookback']} | "
        f"{meta['cost_bps']}bps | alpha {meta['alpha']} beta {meta['beta']} | "
        f"train {meta['train_start']}..{meta['train_end']} ({meta['train_days']} days x "
        f"{meta['epochs_per_day']} epochs) | test {meta['test_start']}..{meta['test_end']}",
        f"agent mean turnover: {mean_turn:.3f} of position range per bar (60-day subsample)",
        "",
        "## Per-day metrics (test >= 2020-05-01)",
        "",
        "| Strategy | N | Sharpe | CAGR | MaxDD |",
        "|---|---|---|---|---|",
    ]
    for name, s in results.items():
        sr, cagr, mdd, n = per_day_sharpe(s)
        lines.append(f"| {name} | {n} | {sr:+.2f} | {100*cagr:+.1f}% | {100*mdd:.1f}% |")
    lines += ["", "## Walk-forward (agent per-day Sharpe by year)", "",
              "| Year | N | Sharpe | CAGR | MaxDD |", "|---|---|---|---|---|"]
    for _, r in folds.iterrows():
        lines.append(f"| {int(r['year'])} | {int(r['N'])} | {r['Sharpe']:+.2f} | "
                     f"{100*r['CAGR']:+.1f}% | {100*r['MaxDD']:.1f}% |")
    lines += ["", "## Verdict", ""]
    verdicts = []
    verdicts.append(f"per-day Sharpe {a_sr:+.2f} vs intraday B&H {bh_sr:+.2f} "
                    f"({100*(a_sr-bh_sr):+.2f}) and random {rd_sr:+.2f}")
    if np.isfinite(a_sr) and a_sr > 1.0 and a_sr > bh_sr and a_sr > rd_sr:
        verdicts.append("**GATE PASSED**: per-day Sharpe > 1 AND beats intraday B&H and random.")
    elif np.isfinite(a_sr) and a_sr > bh_sr and a_sr > rd_sr:
        verdicts.append("Beats baselines but per-day Sharpe <= 1 — positive but below the gate.")
    else:
        verdicts.append("Does NOT beat the baselines — honest negative for R2.1.")
    verdicts.append(f"{pos_folds}/{len(folds)} post-2020 folds have positive per-day Sharpe.")
    lines += [f"- {v}" for v in verdicts]
    lines += ["", "*Generated by r21_analyze.py — @StockViz*"]
    (OUT / "findings.md").write_text("\n".join(lines))
    print("\nwrote", OUT / "findings.md")


if __name__ == "__main__":
    main()

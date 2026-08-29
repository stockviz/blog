"""R1.2 consumer — reward × cost sweep on NIFTY 50 (fix agenda for R1.1).

Arms (each differs ONLY in reward_mode × cost; everything else identical):
  dsr@25  : differential-Sharpe reward @ 25bps (R1.1 reference, runs/nifty50)
  dsr@5   : differential-Sharpe @ 5bps (index-futures realistic cost)
  pnl@25  : plain net-PnL reward @ 25bps
  pnl@5   : plain net-PnL reward @ 5bps
  churn@25: net-PnL + churn penalty (=cost, so 50bps total per flip) @ 25bps
  churn@5 : net-PnL + churn penalty @ 5bps (10bps total per flip)

Gate (KB 08 §8.5): does any arm beat MA-cross net of costs (same cost
level) in the full window? Then R1.1's floor is cleared and R1.3 proceeds.

Usage: python scripts/analyze_r12.py
"""

import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import torch

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "phase0"))

from rl.data.loaders import load_bhav_index, daily_returns  # noqa: E402
from rl.eval.metrics import strategy_metrics, PERIODS, PERIOD_LAB  # noqa: E402
from rl.eval.baselines import ma_cross_rule  # noqa: E402
from rl.eval.charts import plot_cum_drawdown  # noqa: E402
from rl.envs.tdqn_env import TDQNEnv  # noqa: E402
from rl.agents.tdqn import TDQNAgent  # noqa: E402

PHASE1 = Path(__file__).resolve().parents[1]
OUT = PHASE1 / "runs" / "r12"
OUT.mkdir(parents=True, exist_ok=True)

ARMS = ["dsr@25", "dsr@5", "pnl@25", "pnl@5", "churn@25", "churn@5"]
ARM_TAG = {"dsr@25": "nifty50", "dsr@5": "r12_dsr_5", "pnl@25": "r12_pnl_25",
           "pnl@5": "r12_pnl_5", "churn@25": "r12_churn_25", "churn@5": "r12_churn_5"}
ARM_REWARD = {"dsr@25": "dsr", "dsr@5": "dsr", "pnl@25": "pnl", "pnl@5": "pnl",
              "churn@25": "churn", "churn@5": "churn"}
ARM_BPS = {"dsr@25": 25, "dsr@5": 5, "pnl@25": 25, "pnl@5": 5,
           "churn@25": 25, "churn@5": 5}

LOOKBACK = 25
TEST_START = "2015-01-01"
PCT = lambda x: f"{100 * x:.1f}%"  # noqa: E731


def evaluate_returns(policy_fn, env):
    start = env.lookback - 1
    obs, _ = env.reset(seed=0, options={"start": start})
    dates, rets, poss = [], [], []
    done = False
    while not done:
        a = policy_fn(obs)
        obs, r, term, trunc, info = env.step(a)
        dates.append(env._rets_causal.index[env._t])
        rets.append(float(info["strat_ret"]))
        poss.append(float(info["position"]))
        done = term or trunc
    idx = pd.DatetimeIndex(dates)
    return (pd.Series(rets, index=idx, name="ret"),
            pd.Series(poss, index=idx, name="position"))


def load_arm(close, arm, env_factory):
    tag = ARM_TAG[arm]
    ckpt = PHASE1 / "runs" / tag / "checkpoints"
    meta = json.loads((ckpt / "params.json").read_text())
    env = env_factory()
    agent = TDQNAgent(env, hidden=128, layers=2, lr=1e-3, seed=0)
    agent.q.load_state_dict(torch.load(ckpt / "tdqn_art.pt", map_location="cpu"))
    agent.epsilon = 0.0
    rets, pos = evaluate_returns(lambda o, a=agent: a.act(o, eval_mode=True), env)
    return rets, pos, meta


def main():
    close = load_bhav_index("NIFTY 50", start=TEST_START)["close"]
    r_idx = daily_returns(close)
    results, positions, metas = {}, {}, {}

    for arm in ARMS:
        cost = ARM_BPS[arm] / 10000.0
        env = TDQNEnv(close, lookback=LOOKBACK, cost=cost,
                      reward_mode=ARM_REWARD[arm], horizon=len(close))
        results[arm], positions[arm], metas[arm] = load_arm(close, arm, lambda: env)
        print(f"{arm}: {len(results[arm])} days")

    # baselines at each cost level (B&H entry flip + MA drag at that cost)
    for arm in ARMS:
        cost = ARM_BPS[arm] / 10000.0
        bh = r_idx.reindex(results[arm].index).dropna()
        bh.iloc[0] -= cost
        results[f"B&H@{ARM_BPS[arm]}"] = bh
        pos, _ = ma_cross_rule(close, fast=20, slow=50)
        pos = pos.reindex(results[arm].index).fillna(0.0)
        ma = pos * r_idx.reindex(results[arm].index).fillna(0.0) - cost * pos.diff().abs().fillna(0.0)
        results[f"MA@{ARM_BPS[arm]}"] = ma

    # metrics pre/post/full
    rows = []
    for name, s in results.items():
        for pn, (p0, p1) in PERIODS.items():
            sub = s[s.index <= pd.Timestamp(p1)] if p1 else s
            sub = sub[sub.index >= pd.Timestamp(p0)] if p0 else sub
            m = strategy_metrics(sub)
            rows.append({"series": name, "period": pn, **m})
    metrics = pd.DataFrame(rows)
    metrics.to_csv(OUT / "metrics.csv", index=False)

    # turnover per arm
    to_rows = []
    for arm in ARMS:
        p = positions[arm]
        flips = p.diff().abs().sum()
        yrs = len(p) / 252
        to_rows.append({"arm": arm, "flips_per_year": flips / yrs,
                        "pct_long": 100 * (p > 0).mean(), "pct_flat": 100 * (p == 0).mean(),
                        "pct_short": 100 * (p < 0).mean()})
    turnover = pd.DataFrame(to_rows)
    turnover.to_csv(OUT / "turnover.csv", index=False)

    print("\n=== METRICS (full window) ===")
    full = metrics[metrics.period == "full"].set_index("series")
    print(full[["CAGR", "Vol", "Sharpe", "MaxDD"]].to_string(
        formatters={"CAGR": PCT, "Vol": PCT, "MaxDD": PCT, "Sharpe": lambda x: f"{x:.2f}"}))
    print("\n=== TURNOVER ===")
    print(turnover.to_string(index=False, formatters={"flips_per_year": lambda x: f"{x:.1f}",
                                                      "pct_long": lambda x: f"{x:.0f}%",
                                                      "pct_flat": lambda x: f"{x:.0f}%",
                                                      "pct_short": lambda x: f"{x:.0f}%"}))

    # floor gate: each arm vs MA at ITS cost level (full window, aligned)
    print("\n=== FLOOR GATE (arm vs MA@same-cost, full window) ===")
    gate_rows = []
    for arm in ARMS:
        a = results[arm]
        ma = results[f"MA@{ARM_BPS[arm]}"]
        common = a.index.intersection(ma.index)
        sa, sm = strategy_metrics(a.loc[common]), strategy_metrics(ma.loc[common])
        gate_rows.append({"arm": arm, "arm_SR": sa["Sharpe"], "MA_SR": sm["Sharpe"],
                          "dSR": sa["Sharpe"] - sm["Sharpe"], "arm_MaxDD": sa["MaxDD"],
                          "MA_MaxDD": sm["MaxDD"]})
        print(f"{arm:9s} SR {sa['Sharpe']:+.2f} vs MA {sm['Sharpe']:+.2f} "
              f"(d {sa['Sharpe']-sm['Sharpe']:+.2f}) | MaxDD {100*sa['MaxDD']:.0f}% vs {100*sm['MaxDD']:.0f}%")
    pd.DataFrame(gate_rows).to_csv(OUT / "floor_gate.csv", index=False)

    # cost break-even vs B&H
    print("\n=== COST BREAK-EVEN (arm vs B&H@same-cost, full window) ===")
    for arm in ARMS:
        a = results[arm]
        bh = results[f"B&H@{ARM_BPS[arm]}"]
        common = a.index.intersection(bh.index)
        sa, sb = strategy_metrics(a.loc[common]), strategy_metrics(bh.loc[common])
        print(f"{arm:9s} SR {sa['Sharpe']:+.2f} vs B&H {sb['Sharpe']:+.2f} (d {sa['Sharpe']-sb['Sharpe']:+.2f})")

    # charts per window: all arms + B&H@25 + MA@25
    chart_series = {arm: results[arm] for arm in ARMS}
    chart_series["B&H@25"] = results["B&H@25"]
    chart_series["MA@25"] = results["MA@25"]
    for pn in PERIODS:
        rng = None
        if pn == "pre":
            rng = (pd.Timestamp(TEST_START), pd.Timestamp("2019-12-31"))
        elif pn == "post":
            rng = (pd.Timestamp("2020-05-01"), None)
        plot_cum_drawdown(
            chart_series,
            f"R1.2 reward x cost sweep ({PERIOD_LAB[pn]}) | NIFTY 50 | train <= 2014-12-31",
            OUT / f"chart_{pn}.png", date_range=rng)
        print(f"charted {pn}")

    # findings
    lines = [
        "# R1.2 — Reward x cost sweep (NIFTY 50 PR daily)",
        "",
        "Arms (identical except reward x cost; TDQN-art, 500 eps, seed 42):",
        "",
        "| Arm | Reward | Flip cost | churn penalty | Tag |",
        "|---|---|---|---|---|",
    ]
    for arm in ARMS:
        lines.append(f"| {arm} | {ARM_REWARD[arm]} | {ARM_BPS[arm]}bps | "
                     f"{'=cost' if ARM_REWARD[arm] == 'churn' else '-'} | {ARM_TAG[arm]} |")
    lines += ["", "## Full-window metrics", "",
              "| Series | CAGR | Sharpe | MaxDD |", "|---|---|---|---|"]
    for name in full.index:
        r = full.loc[name]
        lines.append(f"| {name} | {100*r['CAGR']:.1f}% | {r['Sharpe']:.2f} | {100*r['MaxDD']:.1f}% |")
    lines += ["", "## Floor gate (arm vs MA-cross at the arm's own cost)", ""]
    for g in gate_rows:
        verdict = "PASS" if g["dSR"] > 0 else "FAIL"
        lines.append(f"- {g['arm']}: SR {g['arm_SR']:.2f} vs MA {g['MA_SR']:.2f} "
                     f"(d {g['dSR']:+.2f}) — **{verdict}**")
    lines += ["", "## Verdict (R1.1 gate)", ""]
    best = max(gate_rows, key=lambda g: g["dSR"])
    lines.append(f"- Best arm vs its MA: {best['arm']} (d Sharpe {best['dSR']:+.2f}).")
    if best["dSR"] > 0:
        lines.append("- The floor is cleared: at least one reward/cost arm beats MA-cross "
                     "net of costs -> proceed to R1.3 (state design) and R1.5 (broad market).")
    else:
        lines.append("- NO arm beats MA-cross: reward/cost alone is insufficient -> next fix: "
                     "action persistence (capped per-day position delta), then re-test.")
    lines += ["", "*Generated by analyze_r12.py — @StockViz*"]
    (OUT / "findings.md").write_text("\n".join(lines))
    print("\nwrote", OUT / "findings.md")


if __name__ == "__main__":
    main()

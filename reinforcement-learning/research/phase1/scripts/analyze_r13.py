"""R1.3 consumer — state-design ablation on NIFTY 50 (pnl@5 reward).

Arms (identical except state features; all pnl@5bps, train 2009-2014 so
VIX/p_off exist in the joint bootstrap window):
  s0    : baseline state (z-window + vol + pos) — the control
  s1_vix: + India VIX (60d-z level, 20d-z change)
  s2_tvt: + TVT-HMM filtered P(risk-off) for MIDCAP 150 TR & SMALLCAP 250 TR
  s3_both: + both
  ref   : r12_pnl_5 (R1.2 best arm, trained 2000-2014) — continuity reference

Questions: (1) does any state arm clear the MA@5 floor? (2) ablation —
does each feature group improve on the s0 control? (3) train/test gap.

Usage: python scripts/analyze_r13.py
"""

import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import torch

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "phase0"))
sys.path.insert(0, str(Path(__file__).resolve().parent))

from rl.data.loaders import load_bhav_index, daily_returns  # noqa: E402
from rl.eval.metrics import strategy_metrics, PERIODS, PERIOD_LAB  # noqa: E402
from rl.eval.baselines import ma_cross_rule  # noqa: E402
from rl.eval.charts import plot_cum_drawdown  # noqa: E402
from rl.envs.tdqn_env import TDQNEnv  # noqa: E402
from rl.agents.tdqn import TDQNAgent  # noqa: E402
from train_r11 import build_feats  # noqa: E402

PHASE1 = Path(__file__).resolve().parents[1]
OUT = PHASE1 / "runs" / "r13"
OUT.mkdir(parents=True, exist_ok=True)

ARMS = ["s0", "s1_vix", "s2_tvt", "s3_both", "s3_full", "ref"]
ARM_TAG = {"s0": "r13_s0", "s1_vix": "r13_s1_vix", "s2_tvt": "r13_s2_tvt",
           "s3_both": "r13_s3_both", "s3_full": "r13_s3_full", "ref": "r12_pnl_5"}
ARM_LAB = {"s0": "S0 control", "s1_vix": "S1 +VIX", "s2_tvt": "S2 +TVT-HMM",
           "s3_both": "S3 +VIX+TVT", "s3_full": "S3-full (2000-14+feats)",
           "ref": "ref pnl@5 (2000-14)"}

LOOKBACK = 25
TEST_START = "2015-01-01"
COST = 5 / 10000.0
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


def main():
    close = load_bhav_index("NIFTY 50", start=TEST_START)["close"]
    r_idx = daily_returns(close)
    results, positions, metas = {}, {}, {}

    for arm in ARMS:
        tag = ARM_TAG[arm]
        ckpt = PHASE1 / "runs" / tag / "checkpoints"
        meta = json.loads((ckpt / "params.json").read_text())
        metas[arm] = meta
        feats = None
        ft = meta.get("feats", "none")
        if ft != "none":
            feats = build_feats(TEST_START, None, ft)
        env = TDQNEnv(close, lookback=LOOKBACK, cost=COST, reward_mode="pnl",
                      horizon=len(close), extra_feats=feats)
        agent = TDQNAgent(env, hidden=128, layers=2, lr=1e-3, seed=0)
        agent.q.load_state_dict(torch.load(ckpt / "tdqn_art.pt", map_location="cpu"))
        agent.epsilon = 0.0
        results[arm], positions[arm] = evaluate_returns(
            lambda o, a=agent: a.act(o, eval_mode=True), env)
        print(f"{arm:8s} ({ARM_LAB[arm]}): {len(results[arm])} days  feats={ft}")

    # baselines at 5bps
    bh = r_idx.reindex(results["s0"].index).dropna()
    bh.iloc[0] -= COST
    results["B&H@5"] = bh
    pos, _ = ma_cross_rule(close, fast=20, slow=50)
    pos = pos.reindex(results["s0"].index).fillna(0.0)
    ma = pos * r_idx.reindex(results["s0"].index).fillna(0.0) - COST * pos.diff().abs().fillna(0.0)
    results["MA@5"] = ma

    # metrics
    rows = []
    for name, s in results.items():
        for pn, (p0, p1) in PERIODS.items():
            sub = s[s.index <= pd.Timestamp(p1)] if p1 else s
            sub = sub[sub.index >= pd.Timestamp(p0)] if p0 else sub
            m = strategy_metrics(sub)
            rows.append({"series": name, "period": pn, **m})
    metrics = pd.DataFrame(rows)
    metrics.to_csv(OUT / "metrics.csv", index=False)

    # turnover
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

    # floor gate vs MA@5
    print("\n=== FLOOR GATE (arm vs MA@5, full window, aligned) ===")
    gate_rows = []
    for arm in ARMS:
        a = results[arm]
        common = a.index.intersection(ma.index)
        sa, sm = strategy_metrics(a.loc[common]), strategy_metrics(ma.loc[common])
        gate_rows.append({"arm": arm, "arm_SR": sa["Sharpe"], "MA_SR": sm["Sharpe"],
                          "dSR": sa["Sharpe"] - sm["Sharpe"], "arm_MaxDD": sa["MaxDD"],
                          "MA_MaxDD": sm["MaxDD"]})
        print(f"{arm:8s} SR {sa['Sharpe']:+.2f} vs MA {sm['Sharpe']:+.2f} "
              f"(d {sa['Sharpe']-sm['Sharpe']:+.2f}) | MaxDD {100*sa['MaxDD']:.0f}% vs {100*sm['MaxDD']:.0f}%")
    pd.DataFrame(gate_rows).to_csv(OUT / "floor_gate.csv", index=False)

    # state ablation: each state arm vs s0 control (aligned, per window)
    print("\n=== STATE ABLATION (vs S0 control, per window) ===")
    abl_rows = []
    s0 = results["s0"]
    for arm in ["s1_vix", "s2_tvt", "s3_both"]:
        b = results[arm]
        common = s0.index.intersection(b.index)
        for pn, (p0, p1) in PERIODS.items():
            c = common
            if p0:
                c = c[c >= pd.Timestamp(p0)]
            if p1:
                c = c[c <= pd.Timestamp(p1)]
            if len(c) < 50:
                continue
            ms, mb = strategy_metrics(s0.loc[c]), strategy_metrics(b.loc[c])
            abl_rows.append({"arm": arm, "period": pn, "n": len(c),
                             "s0_SR": ms["Sharpe"], "arm_SR": mb["Sharpe"],
                             "dSharpe": mb["Sharpe"] - ms["Sharpe"],
                             "s0_MaxDD": ms["MaxDD"], "arm_MaxDD": mb["MaxDD"]})
            print(f"{arm:8s} {pn:4s} SR {mb['Sharpe']:+.2f} vs S0 {ms['Sharpe']:+.2f} "
                  f"(d {mb['Sharpe']-ms['Sharpe']:+.2f}) | MaxDD {100*mb['MaxDD']:.0f}% vs {100*ms['MaxDD']:.0f}%")
    pd.DataFrame(abl_rows).to_csv(OUT / "ablation.csv", index=False)

    # train/test gap: training episode reward vs test Sharpe (per arm)
    print("\n=== TRAIN/TEST GAP ===")
    for arm in ARMS:
        meta = metas[arm]
        ep = meta.get("ep_ret_art_mean_last100")
        sr = full.loc[arm, "Sharpe"]
        print(f"{arm:8s} train ep-ret(last100) {ep:+.2f} | test full Sharpe {sr:+.2f}")

    # charts
    chart_series = {ARM_LAB[arm]: results[arm] for arm in ARMS}
    chart_series["B&H@5"] = results["B&H@5"]
    chart_series["MA@5"] = results["MA@5"]
    for pn in PERIODS:
        rng = None
        if pn == "pre":
            rng = (pd.Timestamp(TEST_START), pd.Timestamp("2019-12-31"))
        elif pn == "post":
            rng = (pd.Timestamp("2020-05-01"), None)
        plot_cum_drawdown(
            chart_series,
            f"R1.3 state design sweep ({PERIOD_LAB[pn]}) | NIFTY 50 | pnl@5bps | train 2009-2014",
            OUT / f"chart_{pn}.png", date_range=rng)
        print(f"charted {pn}")

    # findings
    lines = [
        "# R1.3 — State design sweep (NIFTY 50, pnl@5bps reward)",
        "",
        "| Arm | State features | Train window | Tag |",
        "|---|---|---|---|",
        "| S0 | baseline (z-window+vol+pos) | 2009-2014 | r13_s0 |",
        "| S1 | + India VIX (60d-z level, 20d-z change) | 2009-2014 | r13_s1_vix |",
        "| S2 | + TVT-HMM P(risk-off) MID150 & SMALL250 | 2009-2014 | r13_s2_tvt |",
        "| S3 | + both | 2009-2014 | r13_s3_both |",
        "| S3-full | + both, neutral pre-2009 fill | 2000-2014 | r13_s3_full |",
        "| ref | baseline (R1.2 best arm) | 2000-2014 | r12_pnl_5 |",
        "",
        "## Full-window metrics",
        "",
        "| Series | CAGR | Sharpe | MaxDD |",
        "|---|---|---|---|",
    ]
    for name in full.index:
        r = full.loc[name]
        lines.append(f"| {name} | {100*r['CAGR']:.1f}% | {r['Sharpe']:.2f} | {100*r['MaxDD']:.1f}% |")
    lines += ["", "## Floor gate (vs MA@5)", ""]
    for g in gate_rows:
        v = "PASS" if g["dSR"] > 0 else "FAIL"
        lines.append(f"- {g['arm']}: SR {g['arm_SR']:.2f} vs MA {g['MA_SR']:.2f} "
                     f"(d {g['dSR']:+.2f}) — **{v}**")
    lines += ["", "## State ablation (vs S0, per window)", ""]
    for a in abl_rows:
        lines.append(f"- {a['arm']} {a['period']}: SR {a['arm_SR']:.2f} vs S0 {a['s0_SR']:.2f} "
                     f"(d {a['dSharpe']:+.2f}) | MaxDD {100*a['arm_MaxDD']:.0f}% vs {100*a['s0_MaxDD']:.0f}%")
    lines += ["", "## Verdict (R1.1 gate)", ""]
    best = max(gate_rows, key=lambda g: g["dSR"])
    lines.append(f"- Best arm vs MA@5: {best['arm']} (d Sharpe {best['dSR']:+.2f}).")
    if best["dSR"] > 0:
        lines.append("- FLOOR CLEARED: a state arm beats MA-cross net of costs -> "
                     "proceed to R1.5 (broad market) with the winning state.")
    else:
        lines.append("- FLOOR NOT CLEARED: even with regime features, daily index RL does not "
                     "beat the trend rule on NIFTY 50 — the honest conclusion for the plan's "
                     "sequencing gate; next candidates: action persistence, or accept the "
                     "negative and pivot to intraday (Phase 2).")
    lines += ["", "*Generated by analyze_r13.py — @StockViz*"]
    (OUT / "findings.md").write_text("\n".join(lines))
    print("\nwrote", OUT / "findings.md")


if __name__ == "__main__":
    main()

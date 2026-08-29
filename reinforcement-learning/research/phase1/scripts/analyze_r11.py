"""R1.1 consumer — TDQN vs baseline zoo vs prior agents on NIFTY 50 daily.

Evaluation (house conventions):
- test window 2015-01-01 -> 2026-08-27 (train cutoff 2014-12-31, NEVER touched)
- windows: pre <= 2019-12-31 / post >= 2020-05-01 / full-test
- cost-in-reward: 25bps per position flip (TDQNEnv built-in; MA-cross drag applied)
- metrics AND charts AND tables split pre/post/full
- honesty verdicts (KB 08 §8.5 floor): does TDQN beat B&H net of 25bps?
  beat MA-cross? get within shouting distance of the prior agents?

Usage: python scripts/analyze_r11.py [--index 'NIFTY MIDCAP SELECT'] [--tag midcap_select]
"""

import argparse
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import torch

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "phase0"))  # the rl package lives in phase0/

from rl.data.loaders import load_bhav_index, daily_returns  # noqa: E402
from rl.eval.metrics import strategy_metrics, PERIODS, PERIOD_LAB  # noqa: E402
from rl.eval.baselines import ma_cross_rule, random_policy  # noqa: E402
from rl.eval.charts import plot_cum_drawdown  # noqa: E402
from rl.envs.tdqn_env import TDQNEnv, HOUSE_DRAG  # noqa: E402
from rl.agents.tdqn import TDQNAgent  # noqa: E402

PHASE1 = Path(__file__).resolve().parents[1]
CACHE = PHASE1 / "cache"

LOOKBACK = 25
TEST_START = "2015-01-01"
PCT = lambda x: f"{100 * x:.1f}%"  # noqa: E731


def load_prior(name):
    df = pd.read_csv(CACHE / f"{name}.csv", parse_dates=["date"])
    s = df.set_index("date")["ret"].dropna()
    return s[s.index >= TEST_START]


def evaluate_returns(policy_fn, env):
    """Run one deterministic episode from the first valid index; return the
    strategy's net return series (info['strat_ret'] = a*r_next - cost)
    plus the position series (for turnover reporting — house convention)."""
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
    ap = argparse.ArgumentParser()
    ap.add_argument("--index", default="NIFTY 50")
    ap.add_argument("--tag", default="nifty50")
    args = ap.parse_args()

    ckpt = PHASE1 / "runs" / args.tag / "checkpoints"
    art = PHASE1 / "runs" / args.tag / "artifacts"
    art.mkdir(parents=True, exist_ok=True)
    meta = json.loads((ckpt / "params.json").read_text())

    ohlc_ok = load_bhav_index(args.index, start=TEST_START)
    close_all = load_bhav_index(args.index, start=TEST_START, ohlc_filter=False)["close"]
    close = close_all if len(ohlc_ok) < len(close_all) * 0.9 else ohlc_ok["close"]
    r_idx = daily_returns(close)

    # ── policies on the test TDQNEnv ────────────────────────────────────────
    env = TDQNEnv(close, lookback=LOOKBACK, cost=HOUSE_DRAG, horizon=len(close))
    results = {}
    positions = {}

    def load_agent(name):
        agent = TDQNAgent(env, hidden=128, layers=2, lr=1e-3, seed=0)
        agent.q.load_state_dict(torch.load(ckpt / f"{name}.pt", map_location="cpu"))
        agent.epsilon = 0.0
        return agent

    for name in ["tdqn_art", "tdqn_real"]:
        agent = load_agent(name)
        results[name], positions[name] = evaluate_returns(lambda o, a=agent: a.act(o, eval_mode=True), env)
        print(f"{name}: {len(results[name])} days "
              f"{results[name].index.min().date()}..{results[name].index.max().date()}")

    # random floor (same env, same reward accounting)
    rng = np.random.default_rng(0)
    results["random"], positions["random"] = evaluate_returns(lambda o: int(env.action_space.sample()), env)
    print(f"random floor: {len(results['random'])} days")

    # ── classical baselines (net of 25bps drag) ─────────────────────────────
    bh = r_idx.reindex(results["tdqn_art"].index).dropna()
    bh.iloc[0] -= HOUSE_DRAG  # entry flip
    results["B&H NIFTY 50"] = bh

    pos, _ = ma_cross_rule(close, fast=20, slow=50)
    pos = pos.reindex(results["tdqn_art"].index).fillna(0.0)
    ma = pos * r_idx.reindex(results["tdqn_art"].index).fillna(0.0) - HOUSE_DRAG * pos.diff().abs().fillna(0.0)
    results["MA20/50 rule"] = ma

    # ── prior agents (R2 tree) ──────────────────────────────────────────────
    results["Prior Stop-Cash (ABS@FF60)"] = load_prior("prior_stopcash")
    results["Prior TVT-HMM gate A"] = load_prior("prior_tvt_gateA")

    # ── metrics pre/post/full ───────────────────────────────────────────────
    rows = []
    for name, s in results.items():
        for pn, (p0, p1) in PERIODS.items():
            sub = s[s.index <= pd.Timestamp(p1)] if p1 else s
            sub = sub[sub.index >= pd.Timestamp(p0)] if p0 else sub
            m = strategy_metrics(sub)
            rows.append({"series": name, "period": pn, **m})
    metrics = pd.DataFrame(rows)
    metrics.to_csv(art / "metrics.csv", index=False)
    for pn in PERIODS:
        metrics[metrics.period == pn].to_csv(art / f"metrics_{pn}.csv", index=False)
    print("\n=== METRICS (N / CAGR / Vol / Sharpe / MaxDD) ===")
    print(metrics.to_string(index=False, formatters={
        "CAGR": PCT, "Vol": PCT, "MaxDD": PCT, "Sharpe": lambda x: f"{x:.2f}"}))

    # ── pairwise deltas vs TDQN-art on ALIGNED dates ────────────────────────
    delta_rows = []
    art_series = results["tdqn_art"]
    for name in results:
        if name == "tdqn_art":
            continue
        b = results[name]
        common = art_series.index.intersection(b.index)
        if len(common) < 50:
            continue
        for pn, (p0, p1) in PERIODS.items():
            c = common
            if p0:
                c = c[c >= pd.Timestamp(p0)]
            if p1:
                c = c[c <= pd.Timestamp(p1)]
            if len(c) < 50:
                continue
            ma_, mb_ = strategy_metrics(art_series.loc[c]), strategy_metrics(b.loc[c])
            delta_rows.append({"vs": name, "period": pn, "n": len(c),
                               "dSharpe": ma_["Sharpe"] - mb_["Sharpe"],
                               "dCAGR": ma_["CAGR"] - mb_["CAGR"],
                               "art_MaxDD": ma_["MaxDD"], "other_MaxDD": mb_["MaxDD"]})
    deltas = pd.DataFrame(delta_rows)
    deltas.to_csv(art / "sharpe_deltas.csv", index=False)
    print("\n=== TDQN-art DELTAS (vs baseline, aligned dates) ===")
    print(deltas.to_string(index=False, formatters={"dSharpe": lambda x: f"{x:+.2f}",
                                                    "dCAGR": lambda x: f"{100*x:+.1f}%",
                                                    "art_MaxDD": PCT, "other_MaxDD": PCT}))

    # ── turnover / position diagnostics (house convention: report turnover) ─
    to_rows = []
    for name in ["tdqn_art", "tdqn_real", "random"]:
        p = positions[name]
        flips = p.diff().abs().sum()
        days = len(p)
        years = days / 252
        to_rows.append({"series": name, "days": days,
                        "flips_per_year": flips / years,
                        "pct_long": 100 * (p > 0).mean(),
                        "pct_flat": 100 * (p == 0).mean(),
                        "pct_short": 100 * (p < 0).mean()})
    turnover = pd.DataFrame(to_rows)
    turnover.to_csv(art / "turnover.csv", index=False)
    print("\n=== TURNOVER / POSITION DIAGNOSTICS ===")
    print(turnover.to_string(index=False, formatters={"flips_per_year": lambda x: f"{x:.1f}",
                                                      "pct_long": lambda x: f"{x:.0f}%",
                                                      "pct_flat": lambda x: f"{x:.0f}%",
                                                      "pct_short": lambda x: f"{x:.0f}%"}))

    # ── charts (stacked cum+dd, end labels, @StockViz) ──────────────────────
    tag = " | train <= 2014-12-31 | 25bps/flip"  # @StockViz via the chart helper's caption
    for pn in PERIODS:
        rng = None
        if pn == "pre":
            rng = (pd.Timestamp(TEST_START), pd.Timestamp("2019-12-31"))
        elif pn == "post":
            rng = (pd.Timestamp("2020-05-01"), None)
        plot_cum_drawdown(
            {k: results[k] for k in ["tdqn_art", "tdqn_real", "B&H NIFTY 50", "MA20/50 rule"]},
            f"R1.1 TDQN vs classical baselines ({PERIOD_LAB[pn]}){tag}",
            art / f"chart1_{pn}.png", date_range=rng)
        plot_cum_drawdown(
            {k: results[k] for k in ["tdqn_art", "Prior Stop-Cash (ABS@FF60)", "Prior TVT-HMM gate A", "B&H NIFTY 50"]},
            f"R1.1 TDQN vs R2 prior agents ({PERIOD_LAB[pn]}){tag}",
            art / f"chart2_{pn}.png", date_range=rng)
        print(f"charted {pn}")

    # ── honesty verdicts ────────────────────────────────────────────────────
    def w(series, pn):
        p0, p1 = PERIODS[pn]
        sub = series[series.index <= pd.Timestamp(p1)] if p1 else series
        sub = sub[sub.index >= pd.Timestamp(p0)] if p0 else sub
        return strategy_metrics(sub)

    print("\n=== R1.1 HONESTY VERDICTS ===")
    for pn in PERIODS:
        a = w(results["tdqn_art"], pn)
        bh_ = w(results["B&H NIFTY 50"], pn)
        ma_ = w(results["MA20/50 rule"], pn)
        sc = w(results["Prior Stop-Cash (ABS@FF60)"], pn)
        tv = w(results["Prior TVT-HMM gate A"], pn)
        print(f"[{pn}] TDQN-art SR {a['Sharpe']:.2f} vs B&H {bh_['Sharpe']:.2f} "
              f"(Δ{a['Sharpe']-bh_['Sharpe']:+.2f}) | vs MA {ma_['Sharpe']:.2f} "
              f"(Δ{a['Sharpe']-ma_['Sharpe']:+.2f}) | prior SC {sc['Sharpe']:.2f} "
              f"(Δ{a['Sharpe']-sc['Sharpe']:+.2f}) | prior TVT {tv['Sharpe']:.2f} "
              f"(Δ{a['Sharpe']-tv['Sharpe']:+.2f})")

    # ── findings.md ─────────────────────────────────────────────────────────
    lines = [
        f"# R1.1 — TDQN replication & honesty check ({meta['index']})",
        "",
        f"> {meta['algo']} | cost {meta['cost']} per flip | train <= {meta['train_end']} | "
        f"data from {meta.get('data_start', 'n/a')} | test >= {TEST_START} | episodes {meta['episodes']} | n_traj {meta['n_traj']} "
        f"(block {meta['block_len']}) | seed {meta['seed']}",
        "",
        "## Setup",
        "",
        f"- Data: {meta['index']} daily (bhav_index). Train ..2014-12 (artificial trajectories "
        "block-bootstrapped from it per S4 §3.3); test 2015-01..2026-08 never touched by training.",
        f"- Synthetic vs real first moments: mean {meta['traj_stats']['synth_mean']:.2e} vs "
        f"{meta['traj_stats']['real_mean']:.2e}, sd {meta['traj_stats']['synth_std']:.2e} vs "
        f"{meta['traj_stats']['real_std']:.2e} (log returns).",
        "- Baselines: random floor, B&H, MA20/50 (net of 25bps drag), prior agents from the R2 tree "
        "(ABS@FF60 Stop-Cash; TVT-HMM index gate A).",
        "",
        "## Metrics (CAGR / Sharpe / MaxDD)",
        "",
        "| Series | Window | CAGR | Vol | Sharpe | MaxDD |",
        "|---|---|---|---|---|---|",
    ]
    for _, r in metrics.iterrows():
        lines.append(f"| {r['series']} | {r['period']} | {100*r['CAGR']:.1f}% | "
                     f"{100*r['Vol']:.1f}% | {r['Sharpe']:.2f} | {100*r['MaxDD']:.1f}% |")
    lines += [
        "",
        "## Verdicts (KB 08 §8.5 floor order)",
        "",
    ]
    for pn in PERIODS:
        a = w(results["tdqn_art"], pn)
        bh_ = w(results["B&H NIFTY 50"], pn)
        ma_ = w(results["MA20/50 rule"], pn)
        sc = w(results["Prior Stop-Cash (ABS@FF60)"], pn)
        tv = w(results["Prior TVT-HMM gate A"], pn)
        lines.append(f"- **{pn}**: TDQN-art Sharpe {a['Sharpe']:.2f} — vs B&H {bh_['Sharpe']:.2f} "
                     f"({a['Sharpe']-bh_['Sharpe']:+.2f}), vs MA-cross {ma_['Sharpe']:.2f} "
                     f"({a['Sharpe']-ma_['Sharpe']:+.2f}), vs prior Stop-Cash {sc['Sharpe']:.2f} "
                     f"({a['Sharpe']-sc['Sharpe']:+.2f}), vs prior TVT-HMM {tv['Sharpe']:.2f} "
                     f"({a['Sharpe']-tv['Sharpe']:+.2f}).")
    lines += [
        "",
        "## Files",
        "",
        "- `scripts/train_r11.py` (producer) / `scripts/analyze_r11.py` (consumer).",
        "- `checkpoints/` — tdqn_art.pt, tdqn_real.pt, params.json, episode-reward traces.",
        "- `artifacts/` — metrics{,_pre,_post,_full}.csv, sharpe_deltas.csv, chart1_* / chart2_* PNGs.",
        "- Prior-agent series cached from the R2 checkpoints (cache/prior_*.csv).",
        "",
        "*Generated by analyze_r11.py — @StockViz*",
    ]
    out = PHASE1 / "runs" / args.tag / "findings.md"
    out.write_text("\n".join(lines))
    print(f"\nwrote {out.relative_to(PHASE1)}")


if __name__ == "__main__":
    main()

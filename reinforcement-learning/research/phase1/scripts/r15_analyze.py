"""R1.5 consumer — per-name evaluation of the shared TDQN agent.

S4 broad-market protocol on the house test window (2020-05-01+):
- per-stock net returns from the SHARED agent (deterministic), vs per-stock
  B&H, MA20/50 and random at the same 5bps cost
- distribution of per-stock Sharpe (mean/median/%positive) — not one lucky
  asset
- equal-weight book of each policy vs equal-weight B&H/MA/random books
- verdict: does the learned book beat the classical books net of costs?

Usage: python scripts/r15_analyze.py
"""

import argparse
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import torch

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "phase0"))
sys.path.insert(0, str(Path(__file__).resolve().parent))

from rl.eval.metrics import strategy_metrics  # noqa: E402
from rl.eval.baselines import ma_cross_rule  # noqa: E402
from rl.eval.charts import plot_cum_drawdown  # noqa: E402
from rl.envs.tdqn_env import TDQNEnv  # noqa: E402
from rl.agents.tdqn import TDQNAgent  # noqa: E402
from r15_common import (get_universe, TEST_START, load_stock_features,  # noqa: E402
                        STATE_RICH_COLS)  # noqa: E402

import matplotlib  # noqa: E402
matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

PHASE1 = Path(__file__).resolve().parents[1]
OUT = PHASE1 / "runs" / "r15"
OUT.mkdir(parents=True, exist_ok=True)

LOOKBACK = 25
COST = 5 / 10000.0
PCT = lambda x: f"{100 * x:.1f}%"  # noqa: E731


def evaluate_returns(policy_fn, env):
    start = env.lookback - 1
    obs, _ = env.reset(seed=0, options={"start": start})
    dates, rets = [], []
    done = False
    while not done:
        a = policy_fn(obs)
        obs, r, term, trunc, info = env.step(a)
        dates.append(env._rets_causal.index[env._t])
        rets.append(float(info["strat_ret"]))
        done = term or trunc
    return pd.Series(rets, index=pd.DatetimeIndex(dates), name="ret")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--universe", choices=["top50", "ff60"], default="ff60")
    ap.add_argument("--tag", default=None)
    args = ap.parse_args()
    OUT = PHASE1 / "runs" / (args.tag or ("r15" if args.universe == "top50" else "r15_ff60"))
    meta = json.loads((OUT / "checkpoints" / "params.json").read_text())
    STATE_COLS = STATE_RICH_COLS if meta.get("state", "base") == "rich" else ["mom_z", "prob_z"]
    per_stock = []          # rows for the distribution table
    books = {"agent": {}, "bh": {}, "ma": {}, "random": {}}  # symbol -> Series

    universe = get_universe(args.universe)
    for sym in universe:
        try:
            df = load_stock_features(sym)
        except Exception:
            continue
        te = df[(df.index >= TEST_START) & df["close"].notna()]
        if len(te) < 300:
            continue
        if not set(STATE_COLS).issubset(te.columns):
            print(f"  skip {sym}: missing state cols in test")
            continue
        close = te["close"]
        feats = te[STATE_COLS]
        r_idx = close.pct_change()

        env = TDQNEnv(close, lookback=LOOKBACK, cost=COST, reward_mode="pnl",
                      horizon=len(close), extra_feats=feats,
                      long_only=bool(meta.get("long_only", False)))
        agent = TDQNAgent(env, hidden=128, layers=2, lr=1e-3, seed=0)
        agent.q.load_state_dict(torch.load(OUT / "checkpoints" / "tdqn_shared.pt",
                                           map_location="cpu"))
        agent.epsilon = 0.0
        ag = evaluate_returns(lambda o, a=agent: a.act(o, eval_mode=True), env)

        bh = r_idx.reindex(ag.index).dropna()
        bh.iloc[0] -= COST
        pos, _ = ma_cross_rule(close, fast=20, slow=50)
        pos = pos.reindex(ag.index).fillna(0.0)
        ma = pos * r_idx.reindex(ag.index).fillna(0.0) - COST * pos.diff().abs().fillna(0.0)
        rng = np.random.default_rng(0)
        rd = evaluate_returns(lambda o, e=env, g=rng: int(e.action_space.sample()), env)

        def m(s):
            mm = strategy_metrics(s)
            return mm["Sharpe"], mm["CAGR"], mm["MaxDD"]
        sa, ca, da = m(ag)
        sb, cb, db = m(bh)
        sm, cm, dm = m(ma)
        sr, cr, dr = m(rd)
        per_stock.append({"symbol": sym, "days": len(ag),
                          "agent_SR": sa, "bh_SR": sb, "ma_SR": sm, "rand_SR": sr,
                          "agent_CAGR": ca, "agent_MaxDD": da,
                          "beats_BH": sa > sb, "beats_MA": sa > sm})
        books["agent"][sym] = ag
        books["bh"][sym] = bh
        books["ma"][sym] = ma
        books["random"][sym] = rd

    tab = pd.DataFrame(per_stock)
    tab.to_csv(OUT / "per_stock.csv", index=False)
    print(f"evaluated {len(tab)} stocks, test {TEST_START}+")

    print("\n=== DISTRIBUTION OF PER-STOCK SHARPE ===")
    for col, lab in [("agent_SR", "agent"), ("bh_SR", "B&H"), ("ma_SR", "MA"),
                     ("rand_SR", "random")]:
        v = tab[col]
        print(f"{lab:8s} mean {v.mean():+.2f}  median {v.median():+.2f}  "
              f"pct>0 {100*(v>0).mean():.0f}%  min {v.min():+.2f}  max {v.max():+.2f}")
    print(f"\nagent beats own B&H: {100*tab['beats_BH'].mean():.0f}% of stocks")
    print(f"agent beats own MA : {100*tab['beats_MA'].mean():.0f}% of stocks")

    # equal-weight books (mean of per-stock daily returns)
    def book(which):
        dfb = pd.DataFrame(books[which]).dropna(how="all")
        return dfb.mean(axis=1, skipna=True)
    agg = {"agent book": book("agent"), "B&H book": book("bh"),
           "MA book": book("ma"), "random book": book("random")}
    print("\n=== EQUAL-WEIGHT BOOKS (test window) ===")
    for name, s in agg.items():
        m = strategy_metrics(s)
        print(f"{name:12s} Sharpe {m['Sharpe']:+.2f}  CAGR {100*m['CAGR']:+.1f}%  "
              f"MaxDD {100*m['MaxDD']:.1f}%  N {m['N']}")
    book_df = pd.DataFrame({k: v for k, v in agg.items()})
    book_df.to_csv(OUT / "books.csv")

    # charts
    plot_cum_drawdown(agg, "R1.5 equal-weight books — shared TDQN vs classical "
                       f"({args.universe} FF, test >= 2020-05-01, 5bps)", OUT / "books_full.png")
    fig, ax = plt.subplots(figsize=(11, 6), dpi=120)
    bins = np.linspace(-3, 4, 35)
    for col, lab, c in [("agent_SR", "shared TDQN", "#440154"), ("bh_SR", "B&H", "#21918c"),
                        ("ma_SR", "MA20/50", "#fde725"), ("rand_SR", "random", "#999999")]:
        ax.hist(tab[col], bins=bins, alpha=0.45, label=lab, color=c)
    ax.axvline(0, color="black", lw=0.8)
    ax.set_xlabel("per-stock net Sharpe (test window)")
    ax.set_ylabel("stocks")
    ax.set_title("R1.5 per-stock Sharpe distribution — shared TDQN vs classical baselines")
    ax.legend()
    fig.text(0.99, 0.01, "@StockViz", ha="right", fontsize=9, color="grey")
    fig.tight_layout(rect=(0, 0.03, 1, 1))
    fig.savefig(OUT / "sharpe_distribution.png")
    plt.close(fig)

    # findings
    def w(m, pn="full"):
        return m
    lines = [
        f"# R1.5 — Broad-market cross-sectional (shared TDQN, {args.universe} FF universe)",
        "",
        f"> {meta['algo']} | pnl @ {meta['cost_bps']}bps | train {meta['train_start']}.."
        f"{meta['train_end']} (per-stock joint trajectories: returns + MOMENTUM_ABS/PROB "
        f"z-features) | test >= 2020-05-01 | {meta['universe_size']} stocks, "
        f"{meta['n_trajectories']} trajectories | {meta['episodes']} episodes",
        "",
        "## Per-stock Sharpe distribution",
        "",
        "| Policy | mean | median | % positive | min | max |",
        "|---|---|---|---|---|---|",
    ]
    for col, lab in [("agent_SR", "shared TDQN"), ("bh_SR", "B&H"), ("ma_SR", "MA20/50"),
                     ("rand_SR", "random")]:
        v = tab[col]
        lines.append(f"| {lab} | {v.mean():+.2f} | {v.median():+.2f} | "
                     f"{100*(v>0).mean():.0f}% | {v.min():+.2f} | {v.max():+.2f} |")
    lines += [
        "",
        f"- Agent beats its own B&H on {100*tab['beats_BH'].mean():.0f}% of stocks; "
        f"beats its own MA on {100*tab['beats_MA'].mean():.0f}%.",
        "",
        "## Equal-weight books (test window)",
        "",
        "| Book | Sharpe | CAGR | MaxDD |",
        "|---|---|---|---|",
    ]
    for name, s in agg.items():
        m = strategy_metrics(s)
        lines.append(f"| {name} | {m['Sharpe']:+.2f} | {100*m['CAGR']:+.1f}% | {100*m['MaxDD']:.1f}% |")
    best = max(agg, key=lambda k: strategy_metrics(agg[k])["Sharpe"])
    lines += [
        "",
        "## Verdict",
        "",
        f"- Best equal-weight book: **{best}** (Sharpe {strategy_metrics(agg[best])['Sharpe']:+.2f}).",
    ]
    if best == "agent book":
        lines.append("- The shared TDQN book beats the classical books net of costs — "
                     "cross-sectional RL adds value over B&H/MA on the top-50 FF universe. "
                     "Next: R1.5 follow-ups (cost sweep at 25bps, universe size, feature set).")
    else:
        lines.append("- The classical book still leads — cross-sectional RL does not add value "
                     "over the momentum-ish MA rule at 5bps on this universe. Honest negative "
                     "for the plan; candidates: 25bps stress, larger universe, PPO/continuous "
                     "allocation, or Phase 2 intraday.")
    lines += [
        "",
        "*Generated by r15_analyze.py — @StockViz*",
    ]
    (OUT / "findings.md").write_text("\n".join(lines))
    print("\nwrote", OUT / "findings.md")


if __name__ == "__main__":
    main()

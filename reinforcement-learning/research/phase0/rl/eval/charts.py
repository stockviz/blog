"""Charts — house style port of common/charts.R plotCumDrawdown.

Stacked cumulative-return (top) + drawdown (bottom) panels; every series
end-labeled (name + CAGR + Sharpe; dd panel labels names only); viridis
colors; @StockViz caption bottom-right only (never subtitle — user rule);
6m date breaks for <7y spans, 1y otherwise.
"""

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

from .metrics import strategy_metrics


def _align(rets_dict):
    """Align series on the union index; leading NA warm-up = flat (0)."""
    idx = None
    for s in rets_dict.values():
        idx = s.index if idx is None else idx.union(s.index)
    out = {}
    for name, s in rets_dict.items():
        out[name] = pd.Series(s, index=idx).fillna(0.0)
    return pd.DataFrame(out).sort_index()


def plot_cum_drawdown(rets_dict, title, out_path, date_range=None, log_scale=False, dpi=120):
    """rets_dict: name -> daily return Series. Writes stacked PNG."""
    if date_range is not None:
        rets_dict = {n: s[date_range[0]:date_range[1]] if date_range[1] else s[date_range[0]:]
                     for n, s in rets_dict.items()}
    df = _align(rets_dict)
    names = list(df.columns)
    dates = df.index

    cum = (1.0 + df).cumprod()
    dd = cum / cum.cummax() - 1.0
    ann = {n: strategy_metrics(df[n])["CAGR"] for n in names}
    sr = {n: strategy_metrics(df[n])["Sharpe"] for n in names}

    colors = plt.cm.viridis(np.linspace(0, 0.9, max(len(names), 1)))

    fig, (ax1, ax2) = plt.subplots(
        2, 1, figsize=(13, 8.5), dpi=dpi, sharex=True,
        gridspec_kw={"height_ratios": [2.4, 1.0]})

    for i, n in enumerate(names):
        ax1.plot(dates, cum[n], color=colors[i], linewidth=0.9)
        ax2.plot(dates, dd[n], color=colors[i], linewidth=0.8)
    # end-of-line labels, staggered by final value rank (name + CAGR + Sharpe)
    order = np.argsort(np.argsort([cum[n].iloc[-1] for n in names], kind="stable"))
    for i, n in enumerate(names):
        rk = order[i]
        nudge = (rk - (len(names) - 1) / 2) * 0.015 * max(1.0, float(cum[n].iloc[-1]))
        ax1.annotate(f"{n}\n{100 * ann[n]:.1f}%  SR {sr[n]:.2f}",
                     xy=(dates[-1], cum[n].iloc[-1]), xytext=(dates[-1], cum[n].iloc[-1] + nudge),
                     fontsize=9, fontweight="bold", color=colors[i], va="center")
    order_dd = np.argsort(np.argsort([dd[n].iloc[-1] for n in names], kind="stable"))
    for i, n in enumerate(names):
        rk = order_dd[i]
        nudge = (rk - (len(names) - 1) / 2) * 0.012
        ax2.annotate(n, xy=(dates[-1], dd[n].iloc[-1]),
                     xytext=(dates[-1], dd[n].iloc[-1] + nudge),
                     fontsize=8, fontweight="bold", color=colors[i], va="center")

    span_days = (dates[-1] - dates[0]).days
    if span_days > 7 * 365:
        ax1.xaxis.set_major_locator(mdates.YearLocator())
        ax1.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    else:
        ax1.xaxis.set_major_locator(mdates.MonthLocator(interval=6))
        ax1.xaxis.set_major_formatter(mdates.DateFormatter("%b %Y"))

    ax1.set_title(title, fontsize=12)
    ax1.set_ylabel("Cumulative Return")
    ax1.grid(alpha=0.3)
    ax2.set_ylabel("Drawdown")
    ax2.grid(alpha=0.3)
    ax2.yaxis.set_major_formatter(matplotlib.ticker.PercentFormatter(1.0))
    fig.text(0.99, 0.01, "@StockViz", ha="right", va="bottom", fontsize=9, color="grey")
    fig.autofmt_xdate()
    fig.tight_layout(rect=(0, 0.02, 1, 1))
    fig.savefig(out_path, dpi=dpi)
    plt.close(fig)
    return out_path

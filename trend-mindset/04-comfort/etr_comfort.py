#!/usr/bin/env python3
"""
ETR Comfort Ratio + drawdown-adherence (abandonment) simulation.

Implements Plan items 14 and 15 from /mnt/data/blog/trend-mindset/plan.md:

  * ETR Comfort Ratio (book-inspired, Tom Basso / "The Trend Following Mindset"):
      - drawdown return threshold: primary 10%, robustness 5% and 20%
      - drawdown time threshold:  primary 126 trading days, robustness 63 and 252
      - discomfort accumulates while EITHER the depth or the duration threshold is exceeded
      - comfort accumulates during new-high / surge periods
      - ETR = accumulated comfort / accumulated discomfort
      - sensitivity to daily vs monthly sampling

  * Abandonment simulation (drawdown adherence):
      - an investor redeems to cash (0% thereafter) the first time a drawdown rule fires
      - rules: depth-only, duration-only, depth-AND-duration, depth-OR-duration
      - realized return under abandonment vs the uninterrupted backtest

Reuses daily strategy return streams already produced by sibling folders:
  turbulence/simple/            -> NIFTY and SELECT: B&H, Long/Flat, Long/Short
  turbulence/midcap-indices/    -> MIDCAP 150 TR and SMALLCAP 250 TR: B&H, Trend-filtered reversal Long/Flat

Those streams are daily net returns (25 bps turnover drag already charged by the
producers). No new price data is fetched and nothing outside this folder is written.
"""

import csv
import math
import os

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

HERE = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = "/mnt/data/blog"

# (file-relative-to-repo, instrument label, {strategy label -> csv column})
SOURCES = [
    (
        "turbulence/simple/daily_NIFTY.csv",
        "NIFTY",
        {"B&H": "bh", "Timing Long/Flat": "long_flat", "Trend Long/Short": "ls"},
    ),
    (
        "turbulence/simple/daily_SELECT.csv",
        "SELECT",
        {"B&H": "bh", "Timing Long/Flat": "long_flat", "Trend Long/Short": "ls"},
    ),
    (
        "turbulence/midcap-indices/daily_NIFTY_MIDCAP_150_TR.csv",
        "MIDCAP 150 TR",
        {"B&H": "bh", "Trend-Filtered Reversal Long/Flat": "strategy"},
    ),
    (
        "turbulence/midcap-indices/daily_NIFTY_SMALLCAP_250_TR.csv",
        "SMALLCAP 250 TR",
        {"B&H": "bh", "Trend-Filtered Reversal Long/Flat": "strategy"},
    ),
]

DEPTH_THRESHOLDS = [0.05, 0.10, 0.20]        # drawdown return thresholds
TIME_THRESHOLDS = [63, 126, 252]              # drawdown duration thresholds, trading days
TRADING_DAYS_PER_YEAR = 252
# Monthly sampling: map the day thresholds to month counts (~21 trading days/month).
DAY_TO_MONTH = {63: 3, 126: 6, 252: 12}

# Abandonment rules. depth/time may be None for the one-sided rules.
ABANDON_RULES = []
for d in DEPTH_THRESHOLDS:
    ABANDON_RULES.append(("depth", d, None))
for t in TIME_THRESHOLDS:
    ABANDON_RULES.append(("duration", None, t))
for d, t in zip(DEPTH_THRESHOLDS, TIME_THRESHOLDS):
    ABANDON_RULES.append(("AND", d, t))
    ABANDON_RULES.append(("OR", d, t))


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def read_float(v):
    if v is None or v == "" or v == "NA":
        return None
    try:
        return float(v)
    except ValueError:
        return None


def load_series(path):
    """Return (dates, {strategy -> list of daily returns}) for one source file."""
    full = os.path.join(REPO_ROOT, path)
    with open(full, newline="") as f:
        reader = csv.DictReader(f)
        rows = list(reader)
    dates = [r["date"] for r in rows]
    # strategy columns live in the caller
    return dates, rows


def equity_from_returns(returns):
    """Build an equity curve from daily returns; skip leading NAs (signal warm-up)."""
    eq = 1.0
    equity = []
    for r in returns:
        rv = read_float(r)
        if rv is None:
            equity.append(None)
            continue
        eq *= (1.0 + rv)
        equity.append(eq)
    return equity


def drawdown_series(equity):
    """
    Return aligned lists: dd_magnitude (>=0), duration (periods since peak).
    dd_magnitude = 1 - equity/running_max ; duration = consecutive periods below the
    running max (0 on a new-high period).
    """
    n = len(equity)
    dd_mag = [None] * n
    duration = [None] * n
    running_max = None
    dur = 0
    for i, e in enumerate(equity):
        if e is None:
            continue
        if running_max is None or e >= running_max:
            running_max = e
            dur = 0
        else:
            dur += 1
        dd_mag[i] = 1.0 - e / running_max
        duration[i] = dur
    return dd_mag, duration


def valid_mask(equity):
    return [e is not None for e in equity]


def compute_metrics(returns, equity):
    """Standard diagnostics for one curve (daily returns, equity)."""
    r = [read_float(x) for x in returns]
    r = [x for x in r if x is not None]
    n = len(r)
    if n == 0:
        return {}
    mean = sum(r) / n
    var = sum((x - mean) ** 2 for x in r) / n
    sd = math.sqrt(var)
    ann_vol = sd * math.sqrt(TRADING_DAYS_PER_YEAR)
    sharpe = mean / sd * math.sqrt(TRADING_DAYS_PER_YEAR) if sd > 0 else None
    downside = [min(x - mean, 0.0) ** 2 for x in r]
    dsd = math.sqrt(sum(downside) / n) if n else 0.0
    sortino = mean / dsd * math.sqrt(TRADING_DAYS_PER_YEAR) if dsd > 0 else None
    eq = [e for e in equity if e is not None]
    end = eq[-1]
    total_return = end - 1.0
    cagr = end ** (TRADING_DAYS_PER_YEAR / n) - 1.0 if end > 0 else None
    dd_mag, duration = drawdown_series(equity)
    dd_valid = [dd_mag[i] for i in range(len(equity)) if dd_mag[i] is not None]
    dur_valid = [duration[i] for i in range(len(equity)) if duration[i] is not None]
    max_dd = max(dd_valid) if dd_valid else 0.0
    longest_dd = max(dur_valid) if dur_valid else 0
    in_dd = [d for d in dd_valid if d > 1e-12]
    avg_dd = sum(in_dd) / len(in_dd) if in_dd else 0.0
    time_in_dd_frac = len(in_dd) / len(dd_valid) if dd_valid else 0.0
    mar = cagr / max_dd if (cagr is not None and max_dd > 0) else None
    return {
        "N": n, "CAGR": cagr, "AnnVol": ann_vol, "Sharpe": sharpe,
        "Sortino": sortino, "MaxDD": max_dd, "LongestDD": longest_dd,
        "AvgDD": avg_dd, "TimeInDD": time_in_dd_frac, "MAR": mar,
        "TotalReturn": total_return,
    }


def etr_from_equity(equity, depth_thr, time_thr):
    """
    ETR Comfort Ratio on an equity curve.

    comfort period   : equity at/near a new running high (surge/new-high)
    discomfort period: drawdown magnitude >= depth_thr  OR  time-in-drawdown >= time_thr
    ETR = comfort_periods / discomfort_periods  (None if discomfort == 0)
    """
    dd_mag, duration = drawdown_series(equity)
    comfort = 0
    discomfort = 0
    neutral = 0
    for i, e in enumerate(equity):
        if e is None:
            continue
        m = dd_mag[i]
        d = duration[i]
        at_high = (d == 0)
        deep = m >= depth_thr
        long_ = d >= time_thr
        if deep or long_:
            discomfort += 1
        elif at_high:
            comfort += 1
        else:
            neutral += 1
    ratio = comfort / discomfort if discomfort > 0 else None
    return comfort, discomfort, neutral, ratio


def monthly_equity(dates, equity):
    """Resample an equity curve to month-end (last observed value per calendar month)."""
    out_dates = []
    out_eq = []
    last_key = None
    last_date = None
    last_eq = None
    for d, e in zip(dates, equity):
        if e is None:
            continue
        key = d[:7]  # YYYY-MM
        if last_key is not None and key != last_key:
            out_dates.append(last_date)
            out_eq.append(last_eq)
        last_key = key
        last_date = d
        last_eq = e
    if last_key is not None:
        out_dates.append(last_date)
        out_eq.append(last_eq)
    return out_dates, out_eq


def abandonment_realized(dates, equity, rule_type, depth_thr, time_thr):
    """
    Simulate redemption-to-cash at the first trigger.

    Returns a dict with abandon details and realized vs uninterrupted returns.
    Cash earns 0% after abandonment.
    """
    dd_mag, duration = drawdown_series(equity)
    eq = [e for e in equity if e is not None]
    n_total = len(eq)
    end = eq[-1]
    uninterrupted_total = end - 1.0
    uninterrupted_cagr = end ** (TRADING_DAYS_PER_YEAR / n_total) - 1.0 if end > 0 else None

    abandon_idx = None
    for i, e in enumerate(equity):
        if e is None:
            continue
        m = dd_mag[i]
        d = duration[i]
        deep = m >= depth_thr if depth_thr is not None else False
        long_ = d >= time_thr if time_thr is not None else False
        if rule_type == "depth":
            hit = deep
        elif rule_type == "duration":
            hit = long_
        elif rule_type == "AND":
            hit = deep and long_
        elif rule_type == "OR":
            hit = deep or long_
        else:
            raise ValueError(rule_type)
        if hit:
            abandon_idx = i
            break

    if abandon_idx is None:
        return {
            "abandoned": False, "abandon_date": "", "dd_at_abandon": None,
            "duration_at_abandon": None,
            "realized_total": uninterrupted_total, "realized_cagr": uninterrupted_cagr,
            "uninterrupted_total": uninterrupted_total,
            "uninterrupted_cagr": uninterrupted_cagr, "missed": 0.0,
        }

    abandon_eq = equity[abandon_idx]
    realized_total = abandon_eq - 1.0
    realized_cagr = abandon_eq ** (TRADING_DAYS_PER_YEAR / n_total) - 1.0 if abandon_eq > 0 else None
    missed = uninterrupted_total - realized_total
    return {
        "abandoned": True, "abandon_date": dates[abandon_idx],
        "dd_at_abandon": dd_mag[abandon_idx], "duration_at_abandon": duration[abandon_idx],
        "realized_total": realized_total, "realized_cagr": realized_cagr,
        "uninterrupted_total": uninterrupted_total,
        "uninterrupted_cagr": uninterrupted_cagr, "missed": missed,
    }


def fmt(v, nd=4):
    if v is None:
        return ""
    if isinstance(v, bool):
        return "TRUE" if v else "FALSE"
    if isinstance(v, str):
        return v
    if abs(v - round(v)) < 1e-9 and abs(v) < 1e9:
        return str(round(v))
    return f"{v:.{nd}f}"


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    # Collect all curves
    curves = []  # dicts: instrument, strategy, dates, returns(list), equity(list)
    for rel_path, instrument, strat_map in SOURCES:
        dates, rows = load_series(rel_path)
        for strat, col in strat_map.items():
            returns = [r[col] for r in rows]
            equity = equity_from_returns(returns)
            curves.append({
                "instrument": instrument, "strategy": strat,
                "dates": dates, "returns": returns, "equity": equity,
            })

    # ---- ETR results ----
    etr_rows = []
    for c in curves:
        eq = c["equity"]
        dates = c["dates"]
        # daily sampling
        for dt in DEPTH_THRESHOLDS:
            for tt in TIME_THRESHOLDS:
                comfort, discomfort, neutral, ratio = etr_from_equity(eq, dt, tt)
                m = compute_metrics(c["returns"], eq)
                etr_rows.append({
                    "Instrument": c["instrument"], "Strategy": c["strategy"],
                    "Sampling": "daily", "DepthThr": dt, "TimeThr": tt,
                    "TimeUnits": "days", "Comfort": comfort, "Discomfort": discomfort,
                    "Neutral": neutral, "ETR": ratio,
                    "MaxDD": m["MaxDD"], "LongestDD": m["LongestDD"],
                    "AvgDD": m["AvgDD"], "TimeInDD": m["TimeInDD"],
                    "CAGR": m["CAGR"], "AnnVol": m["AnnVol"], "Sharpe": m["Sharpe"],
                    "Sortino": m["Sortino"], "MAR": m["MAR"],
                    "TotalReturn": m["TotalReturn"], "N": m["N"],
                })
        # monthly sampling
        mdates, meq = monthly_equity(dates, eq)
        for dt in DEPTH_THRESHOLDS:
            for tt in TIME_THRESHOLDS:
                mt = DAY_TO_MONTH[tt]
                comfort, discomfort, neutral, ratio = etr_from_equity(meq, dt, mt)
                m = compute_metrics(c["returns"], eq)  # metrics stay daily-based
                etr_rows.append({
                    "Instrument": c["instrument"], "Strategy": c["strategy"],
                    "Sampling": "monthly", "DepthThr": dt, "TimeThr": mt,
                    "TimeUnits": "months", "Comfort": comfort, "Discomfort": discomfort,
                    "Neutral": neutral, "ETR": ratio,
                    "MaxDD": m["MaxDD"], "LongestDD": m["LongestDD"],
                    "AvgDD": m["AvgDD"], "TimeInDD": m["TimeInDD"],
                    "CAGR": m["CAGR"], "AnnVol": m["AnnVol"], "Sharpe": m["Sharpe"],
                    "Sortino": m["Sortino"], "MAR": m["MAR"],
                    "TotalReturn": m["TotalReturn"], "N": m["N"],
                })

    etr_fields = [
        "Instrument", "Strategy", "Sampling", "DepthThr", "TimeThr", "TimeUnits",
        "Comfort", "Discomfort", "Neutral", "ETR",
        "MaxDD", "LongestDD", "AvgDD", "TimeInDD",
        "CAGR", "AnnVol", "Sharpe", "Sortino", "MAR", "TotalReturn", "N",
    ]
    with open(os.path.join(HERE, "etr_results.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=etr_fields, extrasaction="ignore")
        w.writeheader()
        for row in etr_rows:
            w.writerow({k: fmt(v, 4) if not isinstance(v, str) else v for k, v in row.items()})

    # ---- Abandonment results ----
    ab_rows = []
    for c in curves:
        for rule_type, dt, tt in ABANDON_RULES:
            res = abandonment_realized(c["dates"], c["equity"], rule_type, dt, tt)
            ab_rows.append({
                "Instrument": c["instrument"], "Strategy": c["strategy"],
                "Rule": rule_type, "DepthThr": dt, "TimeThr": tt,
                "Abandoned": res["abandoned"], "AbandonDate": res["abandon_date"],
                "DDAtAbandon": res["dd_at_abandon"], "DurationAtAbandon": res["duration_at_abandon"],
                "RealizedTotalReturn": res["realized_total"],
                "RealizedCAGR": res["realized_cagr"],
                "UninterruptedTotalReturn": res["uninterrupted_total"],
                "UninterruptedCAGR": res["uninterrupted_cagr"],
                "MissedReturn": res["missed"],
            })

    ab_fields = [
        "Instrument", "Strategy", "Rule", "DepthThr", "TimeThr",
        "Abandoned", "AbandonDate", "DDAtAbandon", "DurationAtAbandon",
        "RealizedTotalReturn", "RealizedCAGR",
        "UninterruptedTotalReturn", "UninterruptedCAGR", "MissedReturn",
    ]
    with open(os.path.join(HERE, "abandonment_results.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=ab_fields, extrasaction="ignore")
        w.writeheader()
        for row in ab_rows:
            w.writerow({k: fmt(v, 4) if not isinstance(v, str) else v for k, v in row.items()})

    # ---- Primary-parameter summary (depth 10%, time 126d) ----
    summary_rows = []
    for c in curves:
        eq = c["equity"]
        m = compute_metrics(c["returns"], eq)
        comfort, discomfort, neutral, ratio = etr_from_equity(eq, 0.10, 126)
        mdates, meq = monthly_equity(c["dates"], eq)
        mcomfort, mdiscomfort, mneutral, mratio = etr_from_equity(meq, 0.10, 6)
        summary_rows.append({
            "Instrument": c["instrument"], "Strategy": c["strategy"],
            "CAGR": m["CAGR"], "Sharpe": m["Sharpe"], "Sortino": m["Sortino"],
            "MaxDD": m["MaxDD"], "LongestDD": m["LongestDD"], "AvgDD": m["AvgDD"],
            "TimeInDD": m["TimeInDD"], "MAR": m["MAR"],
            "ETR_daily": ratio, "ETR_monthly": mratio,
        })
    summary_fields = [
        "Instrument", "Strategy", "CAGR", "Sharpe", "Sortino", "MaxDD", "LongestDD",
        "AvgDD", "TimeInDD", "MAR", "ETR_daily", "ETR_monthly",
    ]
    with open(os.path.join(HERE, "etr_summary.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=summary_fields, extrasaction="ignore")
        w.writeheader()
        for row in summary_rows:
            w.writerow({k: fmt(v, 4) if not isinstance(v, str) else v for k, v in row.items()})

    print(f"Curves processed: {len(curves)}")
    print(f"ETR rows:        {len(etr_rows)}")
    print(f"Abandonment rows:{len(ab_rows)}")
    print(f"Summary rows:    {len(summary_rows)}")
    print("\n--- ETR summary (depth 10%, 126d daily / 6m monthly) ---")
    for r in summary_rows:
        print(f"{r['Instrument']:<16} {r['Strategy']:<34} "
              f"CAGR={r['CAGR']:.3f} MaxDD={r['MaxDD']:.3f} "
              f"MAR={r['MAR']:.3f} ETR_d={r['ETR_daily'] if r['ETR_daily'] is not None else float('inf'):.2f} "
              f"ETR_m={r['ETR_monthly'] if r['ETR_monthly'] is not None else float('inf'):.2f}")


if __name__ == "__main__":
    main()

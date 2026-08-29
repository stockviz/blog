"""DataInventory — coverage manifest for the estate (research-plan §1).

Live probe of every table Phase 0+ touches; tick_stamps decoded with the
per-table epoch (§1.3) so the manifest's min/max dates are real. Writes:
  artifacts/inventory.json  (machine-readable)
  artifacts/inventory.md    (report)
"""

import datetime as _dt
import json
from pathlib import Path

import pandas as pd

from ..config import mssql_conn, pg_conn
from .epochs import decode_tick, TICK_EPOCHS

ARTIFACT_DIR = Path("/mnt/data/books/RL/research/phase0/artifacts")
ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)

KEY_INDICES = ["NIFTY 50", "NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR"]
KEY_FUT_SYMBOLS = ["NIFTY", "BANKNIFTY"]
KEY_INDEX_BARS = ["NIFTY 50", "NIFTY BANK", "NIFTY IT", "NIFTY 100", "NIFTY MIDCAP 50", "INDIA VIX"]


def _q(cur, sql):
    cur.execute(sql)
    return cur.fetchall()


def probe_mssql():
    cn = mssql_conn()
    cur = cn.cursor()
    out = {}

    rows = _q(cur, "SELECT INDEX_NAME, MIN(TIME_STAMP), MAX(TIME_STAMP), COUNT(*) FROM bhav_index "
                   f"WHERE INDEX_NAME IN ({','.join(chr(39) + n + chr(39) for n in KEY_INDICES)}) "
                   "GROUP BY INDEX_NAME")
    out["bhav_index"] = [
        {"name": r[0], "start": str(r[1]), "end": str(r[2]), "rows": int(r[3])} for r in rows]

    rows = _q(cur, "SELECT MIN(TIME_STAMP), MAX(TIME_STAMP), COUNT(*) FROM VIX_HISTORY")
    out["VIX_HISTORY"] = [{"start": str(rows[0][0]), "end": str(rows[0][1]), "rows": int(rows[0][2])}]

    rows = _q(cur, "SELECT SYMBOL, MIN(TIME_STAMP), MAX(TIME_STAMP), COUNT(*) FROM BHAV_EQ_FUT "
                   f"WHERE SYMBOL IN ({','.join(chr(39) + n + chr(39) for n in KEY_FUT_SYMBOLS)}) "
                   "AND STRIKE_PR = 0 GROUP BY SYMBOL")
    out["BHAV_EQ_FUT"] = [
        {"symbol": r[0], "start": str(r[1]), "end": str(r[2]), "rows": int(r[3])} for r in rows]

    rows = _q(cur, "SELECT 'MOMENTUM_ABS', MIN(TIME_STAMP), MAX(TIME_STAMP), COUNT(DISTINCT SYMBOL) FROM MOMENTUM_ABS "
                   "UNION ALL SELECT 'MOMENTUM_PROB', MIN(TIME_STAMP), MAX(TIME_STAMP), COUNT(DISTINCT SYMBOL) FROM MOMENTUM_PROB")
    out["MOMENTUM"] = [
        {"table": r[0], "start": str(r[1]), "end": str(r[2]), "symbols": int(r[3])} for r in rows]
    cn.close()
    return out


def probe_pg():
    conn = pg_conn()
    cur = conn.cursor()
    # the bars tables are huge — use pg statistics for row estimates and
    # index-friendly MIN/MAX instead of full scans (zd_option_bars has
    # 100M+ rows; COUNT(*) over it hangs the inventory).
    cur.execute("SELECT relname, reltuples::bigint FROM pg_class WHERE relname IN ('zd_option_bars','zd_bars_mcx')")
    est = dict(cur.fetchall())
    out = {}

    rows = _q(cur, "SELECT symbol, COUNT(*) FROM zd_index_bars "
                   f"WHERE symbol IN ({','.join(chr(39) + n + chr(39) for n in KEY_INDEX_BARS)}) "
                   "GROUP BY symbol ORDER BY symbol")
    out["zd_index_bars"] = []
    for r in rows:
        cur.execute("SELECT MIN(tick_stamp), MAX(tick_stamp), COUNT(DISTINCT time_stamp) FROM zd_index_bars WHERE symbol = %s", (r[0],))
        m = cur.fetchone()
        out["zd_index_bars"].append({
            "symbol": r[0], "rows": int(r[1]),
            "start": str(decode_tick(m[0], 1970)), "end": str(decode_tick(m[1], 1970)),
            "days": int(m[2]), "tick_epoch": 1970,
        })

    # full-table MIN/MAX scan on a 2.36B-row table hangs — probe per-token
    # (inst_token is indexed) and record the row estimate for the table.
    cur.execute("SET statement_timeout = 30000")

    def token_range(table, epoch, token):
        cur.execute(f"SELECT MIN(tick_stamp), MAX(tick_stamp), COUNT(*) FROM {table} WHERE inst_token = %s",
                    (str(token),))
        r = cur.fetchone()
        return {"start": str(decode_tick(r[0], epoch)), "end": str(decode_tick(r[1], epoch)),
                "rows": int(r[2]), "inst_token": str(token)}

    try:
        out["zd_bars_mcx"] = [{"table_rows": int(est.get("zd_bars_mcx", -1)), "tick_epoch": 1970,
                               "sample": token_range("zd_bars_mcx", 1970, 120761863)}]
    except Exception as exc:
        out["zd_bars_mcx"] = [{"table_rows": int(est.get("zd_bars_mcx", -1)), "tick_epoch": 1970,
                               "sample": "n/a", "note": str(exc)}]

    try:
        cur.execute("""SELECT inst_token FROM zd_master
                       WHERE time_stamp = (SELECT MAX(time_stamp) FROM zd_master)
                         AND exch = 'NFO' AND inst_type = 'FUT' LIMIT 1""")
        tok = cur.fetchone()
        if tok:
            out["zd_option_bars"] = [{"table_rows": int(est.get("zd_option_bars", -1)), "tick_epoch": 1990,
                                      "sample": token_range("zd_option_bars", 1990, tok[0])}]
        else:
            out["zd_option_bars"] = [{"table_rows": int(est.get("zd_option_bars", -1)), "tick_epoch": 1990,
                                      "sample": "n/a (no recent NFO token in zd_master)"}]
    except Exception as exc:
        out["zd_option_bars"] = [{"table_rows": int(est.get("zd_option_bars", -1)), "tick_epoch": 1990,
                                  "sample": "n/a", "note": str(exc)}]
    conn.close()
    return out


def build_inventory(force=False):
    """Probe both DBs and write artifacts/inventory.{json,md}."""
    inv = {
        "built": _dt.datetime.now(_dt.timezone.utc).isoformat(),
        "mssql": probe_mssql(),
        "pg": probe_pg(),
    }
    (ARTIFACT_DIR / "inventory.json").write_text(json.dumps(inv, indent=2, default=str))

    lines = ["# DataInventory — verified coverage", "", f"> built {inv['built']}", ""]
    lines.append("## Daily [CLOSE] — SQL Server StockViz")
    lines.append("| Table | Scope | Start | End | Rows |")
    lines.append("|---|---|---|---|---|")
    for row in inv["mssql"].get("bhav_index", []):
        lines.append(f"| bhav_index | {row['name']} | {row['start']} | {row['end']} | {row['rows']:,} |")
    for row in inv["mssql"].get("VIX_HISTORY", []):
        lines.append(f"| VIX_HISTORY | all | {row['start']} | {row['end']} | {row['rows']:,} |")
    for row in inv["mssql"].get("BHAV_EQ_FUT", []):
        lines.append(f"| BHAV_EQ_FUT | {row['symbol']} fut | {row['start']} | {row['end']} | {row['rows']:,} |")
    for row in inv["mssql"].get("MOMENTUM", []):
        lines.append(f"| {row['table']} | {row['symbols']} symbols | {row['start']} | {row['end']} | — |")

    lines.append("")
    lines.append("## Intraday [INTRADAY] — Postgres StockVizDyn")
    lines.append("| Table | Scope | Start (UTC) | End (UTC) | Rows | Days | tick epoch |")
    lines.append("|---|---|---|---|---|---|---|")
    for row in inv["pg"].get("zd_index_bars", []):
        lines.append(f"| zd_index_bars | {row['symbol']} | {row['start']} | {row['end']} | {row['rows']:,} | {row['days']} | {row['tick_epoch']} |")
    for row in inv["pg"].get("zd_option_bars", []):
        s = row.get("sample")
        if isinstance(s, dict):
            lines.append(f"| zd_option_bars | est {row['table_rows']:,} rows | {s['start']} | {s['end']} | sample {s['rows']:,} (tok {s['inst_token']}) | — | {row['tick_epoch']} |")
        else:
            lines.append(f"| zd_option_bars | est {row['table_rows']:,} rows | n/a | n/a | {s} | — | {row['tick_epoch']} |")
    for row in inv["pg"].get("zd_bars_mcx", []):
        s = row.get("sample")
        if isinstance(s, dict):
            lines.append(f"| zd_bars_mcx | est {row['table_rows']:,} rows | {s['start']} | {s['end']} | sample {s['rows']:,} (tok {s['inst_token']}) | — | {row['tick_epoch']} |")
        else:
            lines.append(f"| zd_bars_mcx | est {row['table_rows']:,} rows | n/a | n/a | {s} | — | {row['tick_epoch']} |")

    (ARTIFACT_DIR / "inventory.md").write_text("\n".join(lines))
    return inv


if __name__ == "__main__":
    inv = build_inventory()
    print(json.dumps(inv, indent=2, default=str))

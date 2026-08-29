"""DB configuration — reuses the R2 backtests config.json (single source of truth).

NORWAY  : SQL Server StockViz (daily: bhav_index, VIX_HISTORY, BHAV_EQ_FUT,
          PX_HISTORY, MOMENTUM_ABS/PROB, ...)
SWEDEN  : Postgres StockVizDyn (intraday: zd_index_bars, zd_option_bars,
          zd_bars_mcx, eod_adjusted_nse)

The SWEDEN password is masked ("***") in config.json — the dev password lives
in env var STOCKVIZ_PG_PW if set, else the known dev password is used.
"""

import json
import os
from pathlib import Path

import pyodbc
import psycopg2

_CONFIG_PATH = Path("/mnt/ssd1/stockviz/R2/backtests/config.json")
_DEV_PG_PASSWORD = "*ferrari1"  # StockVizDyn dev (sweden/user01) — masked in config.json


def _load_config():
    with open(_CONFIG_PATH) as fh:
        return json.load(fh)


def mssql_conn(server_override=None):
    """SQL Server StockViz connection (NORWAY)."""
    cfg = _load_config()["NORWAY"]
    parts = dict(p.split("=", 1) for p in cfg.split(";") if "=" in p)
    return pyodbc.connect(
        f"Driver={{ODBC Driver 17 for SQL Server}};"
        f"Server={server_override or parts['Server']};"
        f"Database={parts['Database']};uid={parts['uid']};pwd={parts['pwd']}",
        timeout=30,
    )


def pg_conn():
    """Postgres StockVizDyn connection (SWEDEN)."""
    url = _load_config()["SWEDEN"]
    # postgresql://user01:***@sweden/StockVizDyn?sslmode=allow
    rest = url.split("://", 1)[1]
    creds, hostdb = rest.split("@", 1)
    user, pw = creds.split(":", 1)
    host, db = hostdb.split("/", 1)
    db = db.split("?", 1)[0]
    if pw.strip() == "***" or pw.strip() == "":
        pw = os.environ.get("STOCKVIZ_PG_PW", _DEV_PG_PASSWORD)
    return psycopg2.connect(
        host=host, dbname=db, user=user, password=pw,
        sslmode="allow", connect_timeout=30,
    )


def quote_sql_string(value):
    """Single-quote a SQL literal safely (avoids the R heredoc pitfall)."""
    return "'" + str(value).replace("'", "''") + "'"

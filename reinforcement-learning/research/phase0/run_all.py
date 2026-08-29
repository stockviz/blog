#!/usr/bin/env python
"""Phase 0 run registry — mirrors R2/backtests/common/run-all.R.

Ordered steps, per-step logs under logs/, exit-code + completion-marker
checks. Producers precede consumers (inventory -> tests -> smoke charts).
Usage:
    python run_all.py            # run everything
    python run_all.py --only=loaders   # grepl-style substring filter
"""

import argparse
import subprocess
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent
LOGS = ROOT / "logs"
LOGS.mkdir(exist_ok=True)

PY = "/mnt/ssd1/pyenv/bin/python"

STEPS = [
    ("01_inventory", [PY, "-m", "rl.data.inventory"], "inventory manifest -> artifacts/inventory.json/md"),
    ("02_pytest", [PY, "-m", "pytest", "tests", "-q", "--disable-warnings"], "full test suite (epochs, sessions, loaders, metrics, envs, agents, floor)"),
    ("03_smoke_charts", [PY, "scripts/smoke_charts.py"], "house-convention artifacts on real NIFTY 50 data"),
]


def run_step(name, cmd, desc):
    log = LOGS / f"{name}.log"
    print(f"== {name}: {desc}")
    t0 = time.time()
    with open(log, "w") as fh:
        proc = subprocess.run(cmd, cwd=ROOT, stdout=fh, stderr=subprocess.STDOUT, text=True)
    dt = time.time() - t0
    status = "OK" if proc.returncode == 0 else "FAILED"
    print(f"   -> {status} ({dt:.1f}s) log: {log.relative_to(ROOT)}")
    if proc.returncode != 0:
        tail = "\n".join(log.read_text().splitlines()[-25:])
        print(f"   tail:\n{tail}")
    return proc.returncode


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--only", default=None, help="substring filter on step names (grepl-style)")
    args = ap.parse_args()

    failed = 0
    for name, cmd, desc in STEPS:
        if args.only and args.only not in name:
            continue
        rc = run_step(name, cmd, desc)
        if rc != 0:
            failed += 1
    print(f"\n{'ALL STEPS PASSED' if failed == 0 else f'{failed} STEP(S) FAILED'}")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()

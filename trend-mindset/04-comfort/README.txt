04 ETR comfort ratio and drawdown adherence

Run:

    python3 etr_comfort.py

The script reuses daily net return streams from the completed turbulence study
and writes threshold/sampling diagnostics plus abandonment simulations.

Outputs:
- etr_summary.csv
- etr_results.csv
- abandonment_results.csv
- findings.md

This is a behavioral/process diagnostic. It is not used to select trading
parameters or to claim a return edge.

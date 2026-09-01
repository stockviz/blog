01 EMA timing and risk sizing

Run:

    Rscript run.R
    Rscript walk_forward.R

The first command runs fixed lookbacks; the second performs the rolling
three-year train / one-year test selection using only prior data.

Outputs:
- metrics.csv
- daily_outputs.csv
- walk_forward_metrics.csv
- walk_forward_daily.csv
- walk_forward_selections.csv
- findings.md

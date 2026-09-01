03 MCX liquidity and continuity audit

Run:

    Rscript run.R

This queries BHAV_COM_MCX using the configured SQL Server connection and writes
screening.csv. Credentials are loaded from STOCKVIZ_CONFIG or the standard
StockViz config path and are never printed.

The portfolio stage is gated until the continuity audit removes implausible
contract-to-contract jumps. See findings.md.

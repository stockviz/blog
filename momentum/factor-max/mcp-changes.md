# MCP backtest export changes

Date: 2026-09-27

Source changed: `factor-rotation-ew.R`

Purpose: make the factor-momentum backtest consumable by the mutual-fund MCP server without scraping charts, HTML tables, console output, or log files.

## Changes made

### 1. Added a dedicated MCP artifact directory

The script now writes machine-readable files below `mcp/` by default:

```text
/mnt/data/blog/momentum/factor-max/mcp/
```

The destination can be overridden for staging or upload jobs with:

```bash
MCP_OUTPUT_DIR=/path/to/staging Rscript factor-rotation-ew.R
```

Human-facing reports continue to be written to the existing report directory.

Why: the MCP ingestion job needs a stable directory of versioned artifacts and should not depend on presentation files.

### 2. Added a backtest definition artifact

`backtest_definition.json` records:

- stable strategy ID: `factor-momentum`;
- strategy name: `Indian MF Factor Momentum`;
- signal frequency and return frequency;
- prior-completed-month signal rule;
- following-month holding rule;
- causal lag convention;
- factor-index universe;
- benchmark: `NIFTY 500 TR`;
- switch drag and its convention;
- source table and source script;
- fixed-rule/no-parameter-selection status;
- pre, post, and full window definitions;
- coverage dates and row counts.

Why: an MCP answer needs the strategy assumptions and date conventions alongside the numerical result. A metric row without this context is not safely interpretable.

### 3. Added stable run IDs and fingerprints

The script creates a run ID in this form:

```text
factor-momentum-YYYYMMDD-<12-character-config-hash>
```

The hash is derived from the strategy configuration and source contract. The run ID is attached to all exported rows.

Why: MCP queries need to identify exactly which backtest run produced a metric or observation. The run ID also lets the ingestion layer keep multiple historical runs instead of overwriting them.

### 4. Added MCP metrics export

`backtest_metrics.csv` contains one row per run, window, and series. It includes:

- run ID;
- strategy ID and name;
- benchmark;
- `pre`, `post`, or `full` window;
- actual coverage start and end dates;
- CAGR;
- volatility;
- Sharpe;
- Sortino;
- maximum drawdown;
- Calmar;
- best and worst year;
- positive-month fraction;
- observation count.

Why: this is the primary source for `get_backtest_summary` and `search_backtests`.

### 5. Added long-format backtest observations

`backtest_observations.csv` contains one row per run, series, and monthly period:

- period start and end;
- series name;
- series type (`strategy` or `benchmark`);
- monthly return;
- cumulative value from a starting value of 1;
- drawdown.

Why: this supports date-filtered `get_backtest_series` responses and lets the MCP server return the actual time series behind a summary metric.

### 6. Added annual return observations

`annual_returns.csv` in the MCP directory is long format and includes:

- run ID;
- year end;
- series;
- annual return.

Why: annual returns are useful for questions about consistency, positive years, and specific calendar periods. The existing human-facing annual-return output remains unchanged.

### 7. Added drawdown episode export

`drawdown_episodes.csv` records drawdown episodes for each strategy and benchmark series:

- episode number;
- start date;
- trough date;
- recovery date when available;
- maximum drawdown;
- duration in monthly periods;
- recovery status.

Why: the MCP plan explicitly includes drawdown questions. Returning episode data is more useful than returning only one maximum-drawdown number.

### 8. Added an artifact manifest

`manifest.json` records:

- MCP artifact schema version: `mcp-backtest-v1`;
- run ID and strategy ID;
- generation timestamp;
- source script;
- every artifact filename;
- SHA-256 checksum;
- byte size.

Why: the Cloudflare ingestion pipeline can verify the upload, detect partial or altered files, and preserve reproducibility metadata before publishing a dataset version.

## Intended MCP loading contract

The Cloudflare ingestion job should:

1. read `manifest.json`;
2. verify every listed file exists;
3. verify file sizes and SHA-256 checksums;
4. parse `backtest_definition.json`;
5. load metrics into `backtest_runs`/`backtest_metrics`;
6. load observations into `backtest_observations`;
7. load annual returns and drawdown episodes;
8. preserve the run ID and source metadata on every row;
9. reject the upload if the files disagree on run ID or strategy ID;
10. publish the run only after all read-back checks pass.

The artifact set is intentionally append-friendly. A later rerun should create a new run ID when its configuration or coverage changes, rather than replacing an older published run silently.

## Verification performed

The modified R file passed syntax parsing:

```text
Rscript -e "parse(file='factor-rotation-ew.R')"
```

The exact new export block was exercised with a synthetic monthly fixture. It successfully produced and verified:

- `manifest.json`;
- `backtest_definition.json`;
- `backtest_metrics.csv`;
- `backtest_observations.csv`;
- `annual_returns.csv`;
- `drawdown_episodes.csv`.

The fixture run also verified that the manifest contains five checksummed artifacts and that the generated run ID propagates into the exported files.

## Live-run status

A full live execution was attempted with:

```bash
Rscript factor-rotation-ew.R
```

It was blocked before the database query because the existing shared dependency path is absent in this environment:

```text
/mnt/hollandC/StockViz/R/config.r
```

Therefore no live MCP artifact set is claimed as generated from the production `BHAV_INDEX` data in this run. The source script is syntactically valid, and the new export contract has been exercised independently with synthetic data. A live artifact upload still requires the existing StockViz R configuration and database access to be restored.

## Scope deliberately not changed

- The factor-selection rule was not changed.
- The existing pre/post/full windows were not changed.
- The existing human-facing charts and HTML tables were not removed.
- No credentials or database configuration were copied into the MCP output.
- No Cloudflare upload was attempted.

# PPFAS Flexi Cap Fund — Portfolio Analysis

## Overview

Downloads historical monthly portfolio disclosures from PPFAS AMC, extracts Indian
equity holdings of Parag Parikh Flexi Cap Fund, computes 1-year forward returns
and holding-period CAGR, and generates performance charts.

## Data Flow

```
amc.ppfas.com                     PostgreSQL (StockVizDyn)
     │                                  │
     ▼                                  │
download.py ──► downloads/ (155 .xls)   │
     │                                  │
     ▼                                  ▼
extract_portfolio.py ──► indian_equity_lo_portfolio.csv
                              │
                              ▼
                    compute_returns.R ──► indian_equity_lo_portfolio.csv
                    (adds: symbol, position_return_1y,        (enriched)
                     nifty_midcap150_tr_return_1y)
                              │
              ┌───────────────┼──────────────────┐
              ▼               ▼                   ▼
   holding_period_returns.R  chart_performance.R  chart_entry_vs_exit.R
   chart_new_positions.R     chart_removed_positions.R
        │                         │
        ▼                         ▼
   holding_periods.csv       charts/*.png
        │
        ▼
   table_holding_summary.R ──► charts/holding_period_summary.png
```

## Files

### Data Pipeline

| File | Purpose |
|------|---------|
| `config/config.json` | Download/extract settings (paths, retries, URLs) |
| `urls.txt` | 155 source URLs scraped from amc.ppfas.com |
| `download.py` | Downloads portfolio .xls/.xlsx files to `downloads/` |
| `downloads/` | 155 raw Excel files (Jun 2013 – Apr 2026) |
| `extract_portfolio.py` | Extracts Indian equity holdings into CSV |
| `compute_returns.R` | Looks up NSE symbols, computes 1yr forward returns |
| `indian_equity_lo_portfolio.csv` | Final data: 3272 rows × 8 columns |

### Analysis Scripts

| Script | Output |
|--------|--------|
| `chart_performance.R` | `charts/position_excess_return_bar.png` — avg excess by position |
| | `charts/position_excess_return_range.png` — IQR range plot |
| | `charts/position_excess_return_heatmap.png` — top/bottom 10 heatmap |
| `chart_new_positions.R` | `charts/new_positions_bar.png` — excess return at entry |
| `chart_removed_positions.R` | `charts/removed_positions_bar.png` — excess return at exit |
| `chart_entry_vs_exit.R` | `charts/entry_vs_exit_scatter.png` — entry vs exit scatter |
| `holding_period_returns.R` | `holding_periods.csv` + `charts/holding_period_returns.png` |
| `table_holding_summary.R` | `charts/holding_period_summary.png` — gt summary table |

## CSV Schema

### indian_equity_lo_portfolio.csv

| Column | Type | Description |
|--------|------|-------------|
| `date` | date | Portfolio month-end |
| `name` | string | Stock name from disclosure |
| `isin` | string | ISIN |
| `industry` | string | Industry classification |
| `pct_net_assets` | float | % of net assets (decimal) |
| `position_return_1y` | float | 1-year forward return (decimal) |
| `nifty_midcap150_tr_return_1y` | float | NIFTY MIDCAP 150 TR 1yr return |
| `symbol` | string | Resolved NSE symbol |

### holding_periods.csv

| Column | Type | Description |
|--------|------|-------------|
| `symbol` | string | NSE symbol |
| `start_date` | date | First month of contiguous holding |
| `end_date` | date | Last month of contiguous holding |
| `n_months` | int | Months held |
| `years` | float | Years held |
| `stock_ret` | float | Annualized stock return (CAGR) |
| `index_ret` | float | Annualized index return (CAGR) |
| `excess_ret` | float | Annualized excess return |

## Conventions

- **No pie charts** — use bars, dot plots, heatmaps instead
- **No dual-axis charts** — no `sec.axis` ever
- **No period cutoffs** — include all positions regardless of tenure
- **Titles**: short descriptive title; subtitle starts with "PPFAS Flexi Cap Fund —"
- **Y-axis labels**: 10% apart for return charts (`breaks = seq(-1, 1, 0.1)`)
- **Legend labels**: 45° tilt on range chart; horizontal on heatmap
- **Caption**: `@StockViz` bottom-right (`hjust = 1`, `plot.caption.position = "plot"`)
- **Theme**: `theme_economist()` + `viridis` colors
- **R conventions**: `coredata()` for xts, `is.data.frame()` checks, `format()` for dates
- **Symbol lookup**: ISIN first → name fallback (strips Ltd/Company/(India)/&→and)
- **Holding periods**: gap > 60 days = exit + re-entry (contiguous blocks)
- **Returns normalization**: auto-detect % vs decimal by per-date sum

## Dependencies

### Python
- `requests`, `openpyxl`, `xlrd`

### R
- `tidyverse`, `RODBC`, `RPostgres`, `xts`, `zoo`
- `ggthemes`, `viridis`, `ggrepel`, `scales`
- `gt`, `gtExtras`, `webshot2`

### Databases
- SQL Server (NORWAY): `stockviz` — equity_ticker, RETURN_SERIES_ALL, BHAV_INDEX
- PostgreSQL (SWEDEN): `StockVizDyn` — eod_adjusted_nse
- Credentials: `/mnt/hollandC/StockViz/R/config.r`

## Re-run All

```bash
cd /mnt/data/blog/ppfas2

# Download (skip if already done)
python3 download.py

# Extract portfolio
python3 extract_portfolio.py

# Compute returns
Rscript compute_returns.R

# Generate all charts and tables
Rscript chart_performance.R
Rscript chart_new_positions.R
Rscript chart_removed_positions.R
Rscript chart_entry_vs_exit.R
Rscript holding_period_returns.R
Rscript table_holding_summary.R
```

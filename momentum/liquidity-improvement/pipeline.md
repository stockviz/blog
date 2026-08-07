# LIQIM Pipeline v2

Single-tier pipeline testing the Amihud liquidity improvement factor (LIQC)
on the top 60% FF-mcap NSE universe. Three phases with zero code duplication.

## Quick Start

```bash
cd /mnt/data/blog/momentum/liquidity-improvement

# Full pipeline (each phase saves its outputs for the next)
Rscript build.R && Rscript backtest.R && Rscript momentum.R && Rscript consolidated.R
```

## Architecture

All logic lives in `liqim-common.R`. Each script is a thin orchestrator.

```
liqim-config.R          shared parameters
liqim-common.R          all shared logic (buildStrand, makePortfolio, pickQ,
                        pickMomentum, buildMomentumCache, buildQ5Exclude,
                        makeCumretChart, makeAnnualChart, makeGtTable, etc.)

build.R                 Phase 1 — data fetch, ILLIQ, LIQC, universe, quintile stats
backtest.R              Phase 2 — Q1 LIQC portfolio → q1_liqc.rds
momentum.R              Phase 3 — momentum vs mom-ex-Q5 → momentum.rds
consolidated.R          Phase 4 — load RDS files, plot combined charts
```

## Phase 1 — build.R

Fetches data, computes ILLIQ/LIQC/universeCache, quintile stats, saves checkpoint.

**Outputs:** `checkpoint.rds`, `quintile_stats.csv`

## Phase 2 — backtest.R

Loads checkpoint. Builds Q1 LIQC portfolio (20 stocks, equal-weight, 1-month hold,
50bps drag). Benchmarks against NIFTY500 MOMENTUM 50 TR.

**Outputs:** `cumret.png`, `annual.png`, `metrics.png`, `monthly_liqc.csv`, `q1_liqc.rds`

## Phase 3 — momentum.R

Loads checkpoint. Builds 12-month momentum cache. Computes Q5 exclusion sets
for both 1-month and 12-month LIQC lookbacks. Constructs three portfolios:
raw momentum, momentum ex-Q5 (1m), momentum ex-Q5 (12m).

**Outputs:** `mom_cumret.png`, `mom_metrics.png`, `momentum.rds`

## Phase 4 — consolidated.R

Loads `q1_liqc.rds` and `momentum.rds`. No computation — just combines and plots
all five strategies on one set of axes.

**Outputs:** `consolidated_cumret.png`, `consolidated_annual.png`, `consolidated_metrics.png`

## Parameters

All in `liqim-config.R`:

| Parameter | Value | Description |
|---|---|---|
| MCAP_PCT | 0.60 | Top FF-mcap percentile |
| MIN_PRICE | 30 | Min closing price (INR) |
| MIN_DVOL | 1e7 | Min median daily dollar volume (₹1cr) |
| ILLIQ_LB | 1 | LIQC lookback (months) |
| WINSOR_LO/HI | 0.01/0.99 | Winsorization |
| TOP_N | 20 | Stocks per portfolio |
| HOLDING_K | 1 | Holding period (months) |
| DRAG | 0.005 | 50bps per trade |
| MOM_LB | 12 | Momentum lookback (months) |

## Signal

```
ILLIQ_t  = 1e6 × mean(|simple return| / dollar volume)   per month
LIQC_t   = −(ILLIQ_t − ILLIQ_{t−1})
```

All returns are simple (arithmetic). Monthly: `prod(1 + daily) − 1`.

## Data Sources

| Database | Table | Columns |
|---|---|---|
| StockVizDyn (PG) | `eod_adjusted_nse` | ticker, date_stamp, c, v |
| StockViz (MSSQL) | `equity_misc_info` | SYMBOL, FF_MKT_CAP_CR, TIME_STAMP |
| StockViz (MSSQL) | `px_history` | SYMBOL, SERIES='EQ' |
| StockViz (MSSQL) | `bhav_index` | NIFTY500 MOMENTUM 50 TR, px_close |

## Shared Helpers (liqim-common.R)

| Function | Purpose |
|---|---|
| `winsorize` | Winsorize at given quantiles |
| `compoundReturn` | Compound simple returns: prod(1+r) − 1 |
| `computeMetrics` | CAGR, Vol, Sharpe, MaxDD, Calmar from daily xts |
| `stockReturns` | Daily simple-return xts for a stock over date range |
| `computeLIQC` | LIQC from cached ILLIQ for a given lookback |
| `buildStrand` | Single holding-period equal-weight return strand |
| `makePortfolio` | Full portfolio returns over all months |
| `pickQ` | Stock picker closure for LIQC quintile |
| `buildMomentumCache` | 12-month momentum cache |
| `buildQ5Exclude` | Q5 exclusion sets per month |
| `pickMomentum` | Stock picker closure for momentum (with optional Q5 exclusion) |
| `makeCumretChart` | Cumulative return chart via Common.PlotCumReturns |
| `makeAnnualChart` | Annual return bar chart (viridis, theme_economist) |
| `makeGtTable` | GT metrics table + webshot PNG |
| `makeMonthlyCsv` | Monthly return CSV export |

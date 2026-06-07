# Broker Market Share Charts — Context & Conventions

## Scripts

### broker_marketshare.R — Static 100% Stacked Area
- Reads `People.dbo.broker_check` and `People.dbo.broker_aggregate` (FIN_YEAR >= 201415)
- Top 8 brokers by **average** market share across all 13 FYs, tracked consistently
- Rest + unattributed gap → "Others"
- Output: `broker_marketshare.png` (1440×840, 120 dpi)

### broker_marketshare_anim.R — Animated Bar Chart Race
- Per-year **actual** top 8 — categories change each frame
- 13 frames, 2s each, infinite loop
- Labels padded to fixed width + monospace font so chart doesn't shift
- Stitched with ImageMagick `convert -delay 200 -loop 0`
- Output: `broker_marketshare.gif` (1000×600, 220 KB)

## Data sources

| Table | Database | Key columns |
|---|---|---|
| broker_check | People | FIN_YEAR, BROKER_NAME, NUM_ACTIVE |
| broker_aggregate | People | FIN_YEAR, TOTAL_ACTIVE |

FY encoding: `201415` = FY 2014-15 (YYYYXX format)
Market share = NUM_ACTIVE / TOTAL_ACTIVE

## Name normalization

Brokers that changed names over time are merged via a name_map:
- NextBillion Technology → Groww
- RKSV Securities → Upstox
- Angel Broking → Angel One
- Zerodha / Zerodha Securities → Zerodha
- India Infoline → IIFL Capital
- Motilal Oswal Securities → Motilal Oswal
- Kotak Mahindra Securities → Kotak Securities
- ICICI Securities Primary Dealership → ICICI Securities
- Geojit BNP Paribas → Geojit

## Chart conventions

- Theme: `theme_economist()`
- Colors: `viridis_pal(option = "D")`, Others = `#7CB5D0`
- Caption: `@StockViz`
- Title: left-aligned by default (theme_economist)
- Subtitle: `hjust = 0` (left-aligned)
- X-axis labels (static): 90° rotation, `hjust = 1, vjust = 0.5`
- Y-axis labels (anim): `family = "mono"` for fixed-width
- Static: 12×7", 120 dpi
- Animation frames: 10×6", 100 dpi, 2s delay

## Unattributed gap

The stacked area chart shows 1.2–8.5% "unattributed" at the top —
the difference between the report's declared total (broker_aggregate)
and the sum of individual broker rows (broker_check). These are
exchange-level clients not attributed to specific trading members.

## Database refresh

NSE Report 1C .xls files are downloaded and loaded into People.dbo
by `/mnt/ssd1/stockviz/Admin03/nse_report1c_download.py` (skill: nse-report1c).
Rerun that script before regenerating these charts to pick up new data.

## Boilerplate

```r
library('RODBC')
library('tidyverse')
library('ggthemes')
library('viridis')
library('scales')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "/mnt/data/blog/brokers"
source("/mnt/hollandC/StockViz/R/config.r")

lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=People;Uid=%s;Pwd=%s;",
          ldbserver, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)
```

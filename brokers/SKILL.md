---
name: broker-market-share-charts
description: Generate NSE broker market share charts — static stacked area and animated bar chart race — from People.dbo.broker_check and broker_aggregate.
version: 1.0.0
platforms: [linux]
metadata:
  hermes:
    tags: [nse, brokers, market-share, charts, r, gif]
prerequisites:
  commands: [Rscript, convert]
  packages: [r-base, imagemagick]
---

# Broker Market Share Charts

Two R scripts in `/mnt/data/blog/brokers/` that generate visualizations of
NSE broker market share from `People.dbo.broker_check` and `broker_aggregate`.

## Scripts

### `broker_marketshare.R` — Static 100% Stacked Area
- Top 8 brokers by average market share across all 13 FYs, tracked consistently
- Rest + unattributed gap → "Others"
- Output: `broker_marketshare.png`

### `broker_marketshare_anim.R` — Animated Bar Chart Race
- Per-year actual top 8 — categories shift between frames
- Labels: monospace font + padded to fixed width so chart doesn't shift
- 13 frames, 2s each, stitched with ImageMagick `convert`
- Output: `broker_marketshare.gif`

## Data flow

```
People.dbo.broker_check (NUM_ACTIVE)
People.dbo.broker_aggregate (TOTAL_ACTIVE)
        ↓
market_share = NUM_ACTIVE / TOTAL_ACTIVE
        ↓
name_map (merge old/new entity names)
        ↓
Top N selection → chart
```

## Name normalization

Entity name changes are merged so the same broker isn't split:
- NextBillion → Groww, RKSV → Upstox, Angel Broking → Angel One
- Zerodha variants → Zerodha, India Infoline → IIFL Capital
- Motilal Oswal Securities → Motilal Oswal, Geojit BNP Paribas → Geojit
- Kotak Mahindra Securities → Kotak Securities

## Conventions
- Theme: `theme_economist()`, colors: `viridis_pal(option="D")`, Others: `#7CB5D0`
- Caption: `@StockViz`, subtitle left-aligned (`hjust=0`)
- Credentials: `source("/mnt/hollandC/StockViz/R/config.r")`
- FY filter: `FIN_YEAR >= 201415`
- Context doc: `/mnt/data/blog/brokers/broker-charts-context.md`

## Regeneration

```bash
cd /mnt/data/blog/brokers
Rscript broker_marketshare.R      # static
Rscript broker_marketshare_anim.R # animation
```

Refresh the underlying data first:
```bash
python3 /mnt/ssd1/stockviz/Admin03/nse_report1c_download.py
```

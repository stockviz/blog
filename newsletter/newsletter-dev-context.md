# Newsletter 20260606 — Context & Conventions

## Scripts

### script.R — AUM Analysis
- Reads `dates_with_funds_and_aum.csv`
- Parses index category from URL (`/broad-based-indices/`, `/sectoral-indices/`, etc.)
- Generates charts: top20 AUM, count by category, launch vs AUM scatter, scheme count dist, dashboard
- Pure R (no DB — all data from CSV)

### script2.R — Launch Returns
- Reads `dates_with_funds_and_aum.csv`
- Connects to StockViz via RODBC
- Maps index names → TR index names in `BHAV_INDEX` (e.g., "NIFTY 50" → "NIFTY 50 TR")
- For each index: gets TR returns ±1yr around the index's `LaunchDate`
- For each fund: gets index TR return for year before fund start, and actual NAV return for year after
- Matches index names to TR names: `toupper(name) + " TR"`, falls back to fuzzy grep
- Generates: index-launch-returns.png, fund-launch-returns.png, index-pre-post-scatter.png, fund-pre-post-scatter.png

## Data sources

| Table | Columns used |
|---|---|
| `BHAV_INDEX` | `INDEX_NAME`, `TIME_STAMP`, `PX_CLOSE` |
| `MF_NAV_HISTORY` | `SCHEME_CODE`, `NAV`, `AS_OF` |

TR index naming: `"NIFTY 50 TR"`, `"NIFTY NEXT 50 TR"`, etc. Filter: `INDEX_NAME LIKE '% TR' AND INDEX_NAME NOT LIKE '%TR %'`.

## Critical pitfalls

### xts + dplyr conflicts
- `dplyr::lag()` breaks `xts::lag()` — use `stats::lag()` for xts objects
- `dplyr::first()`/`last()` mask `xts::first()`/`last()` — use `coredata()` to extract raw values:
  ```r
  as.numeric(coredata(last(x))) / as.numeric(coredata(first(x))) - 1
  ```
- `Return.cumulative()` from PerformanceAnalytics returns `Inf` when dplyr is loaded — avoid it

### RODBC sqlQuery date formatting
- `sprintf` with `%s` on R `Date` objects prints the internal numeric, not the date string
- MUST use `format(date, "%Y-%m-%d")` before passing to SQL
- SQL strings must use single quotes `'value'` not double quotes `"value"` (SQL Server treats `"` as identifier quotes)

### RODBC sqlQuery error handling
- `sqlQuery` returns a character vector on error, not a data frame
- Check with `!is.data.frame(px)` before using `nrow()`

### Tibble row subsetting
- `df[i,]` on a tibble returns a 1-row tibble; `df$col[i]` returns a scalar
- Prefer `df$col[i]` over `df[i,]$col` in loops to avoid "arguments imply differing number of rows" errors

### read_csv date parsing
- Don't use `col_types = cols(FundStartDate = col_character())` then `as.Date()` — it breaks for some columns
- Better: `mutate(FundStartDate = as.Date(FundStartDate))` after read_csv with default types
- But FundStartDate might be empty strings — `read_csv` parses them to Date as NA silently
- For character columns like SchemeCode, leave as-is and check with `!= ""` and `!= "0"`

### SchemeCode handling
- In the CSV, SchemeCode is a number like `118581` but empty for unmatched indices
- Filter: `!is.na(SchemeCode) & SchemeCode != "" & SchemeCode != "0"`
- Convert with `as.integer()` before using in SQL queries

## Chart conventions
- Theme: `theme_economist()` from ggthemes
- Colors: `scale_fill_viridis_d()`, `scale_color_viridis_d()`
- Sizes: 10-12" wide, 6-8" tall (units="in")
- Caption: `"@StockViz"`
- Dashboard: `patchwork` with `(p1 + p2) / (p3 + p4)` layout, 14×10"
- Legend: `guides(color = guide_legend(nrow = 1))` for horizontal, `legend.box = "vertical"` for stacked
- X-axis labels: horizontal by default, `element_text(angle = 90, hjust = 1, vjust = 0.5)` for year breaks
- Date breaks: `scale_x_date(date_breaks = "2 years", date_labels = "%Y")`
- Scatter: include geom_abline(slope=1) for reference, geom_smooth(method="lm") for trend
- Subtitle format: `"%d indices | Above diagonal = post > pre | Median: pre=%.1f%%, post=%.1f%%"`

## File structure
```
/mnt/data/blog/newsletter/20260606/
├── dates_with_funds_and_aum.csv   ← copy from /mnt/data/niftyindices/
├── script.R                       ← AUM charts (5 outputs)
├── script2.R                      ← Launch returns (4 outputs)
├── *.png                          ← generated charts
```

## Boilerplate
```r
library('RODBC')
library('tidyverse')
library('xts')
library('ggthemes')
library('viridis')
library('patchwork')
library('scales')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)
```

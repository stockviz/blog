# Newsletter R Script Conventions

Patterns and conventions for R scripts in `/mnt/data/blog/newsletter/`.

## Library imports (standard set)
```r
library('RODBC')          # SQL Server via ODBC
library('RPostgres')      # PostgreSQL
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('data.table')     # rleid
library('ggrepel')        # end-of-line labels
```
Import order: database drivers first, then analytics, then viz.

## Boilerplate header
```r
pdf(NULL)                              # suppress Rplots.pdf
options("scipen" = 100)                # no scientific notation
options(stringsAsFactors = FALSE)

reportPath <- "."                      # output directory
source("/mnt/hollandC/StockViz/R/config.r")   # credentials
source("/mnt/data/blog/common/plot.common.r") # shared plot functions
```

## Database connections

### SQL Server (RODBC)
```r
lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

lconUs2 <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockVizUs2", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)
```

### PostgreSQL (RPostgres)
```r
pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)
```

Credential variables come from `/mnt/hollandC/StockViz/R/config.r`:
- `ldbserver`, `ldbname`, `ldbuser`, `ldbpassword` — SQL Server
- `ldbuser2`, `ldbpassword2` — PostgreSQL

## Database query patterns

### SQL Server
```r
val <- sqlQuery(lcon, "select max(time_stamp) from TABLE")[[1]]          # single scalar
vals <- sqlQuery(lcon, "select symbol from TABLE where ...")[,1]          # column vector
df <- sqlQuery(lcon, sprintf("select * from TABLE where dt = '%s'", dt))  # data frame
```

### PostgreSQL
```r
pxDf <- dbGetQuery(pgCon, "select * from table where coin=$1", params = list(coin))
```

## Plot conventions

### Theme
- `theme_economist()` from ggthemes
- `scale_fill_viridis_d()` / `viridis_pal()` for colors
- Axis text: `element_text(angle = 45, hjust = 1, vjust = 1)` or 90° for bar charts
- **No pie charts** — use bars, dot plots, heatmaps instead
- **No dual-axis charts** — no `sec.axis` ever
- X-axis date breaks: minimum 6 months apart (`date_breaks = "6 months"`)
- No explicit `dpi` in `ggsave` — use the default (300)

### Caption
- `@StockViz` always bottom-right:
```r
plot.caption = element_text(size = 8, color = "grey50", hjust = 1),
plot.caption.position = "plot"
```

### Saving
```r
ggsave(sprintf("%s/filename.%s.png", reportPath, suffix), width = 12, height = 6, units="in")
```

### End-of-line labels (replaces legend)

For line charts with many categories, use `geom_text_repel` at the last data point:

```r
geom_text_repel(
  data = df |> filter(date_col == max(date_col)),
  aes(label = category),
  direction = "y", nudge_x = 30, hjust = 0,
  size = 3.5, fontface = "bold", segment.color = NA
)
```

Pair with:
- `scale_x_date(expand = expansion(mult = c(0.02, 0.25)))` — push axis 25% past last point for label room
- `coord_cartesian(clip = "off")` — prevent clipping text outside panel
- `scale_color_*(guide = "none")` — suppress the legend
- `plot.margin = margin(5, 70, 5, 5)` — extra right margin

### Factor ordering in facets

When faceting by a label column derived from a lookup vector, preserve order by creating a proper factor. Categories with explicit ordering must maintain it across all charts:

```r
short_labels <- c('Long Name' = 'Short', ...)
label_order <- unname(short_labels[original_categories])
df <- df |> mutate(
  cat_label = factor(short_labels[as.character(original_col)], levels = label_order)
)
```

Then use `cat_label` directly in all charts — no inline `mutate()` in ggplot calls.

**Gold ETF** must always be last in category ordering.

### Titles and subtitles
- Title: short, descriptive (e.g. "Monthly Net Fund Flows")
- Subtitle: date range + context; fund/scheme name goes here, not in title
- Format: `labs(title = "Short Title", subtitle = dateLabel, caption = '@StockViz')`

### Time series
- Convert to xts for financial calculations: `xts(df[,1], df[,2])`
- Use `dailyReturn()`, `annualReturn()`, `SharpeRatio.annualized()`
- Merge with `merge.xts()` (left join)

## Data wrangling
- `tidyverse` pipes: `|>` (native R pipe), `mutate()`, `filter()`, `group_by()`, `summarise()`, `pivot_longer()`
- `data.table::rleid()` for run-length encoding
- `lubridate::wday()`, `year()`, `month()`, `hour()`

## Common pitfalls
- Use `na.omit()` before passing to PerformanceAnalytics functions
- Set first return to 0 in cumulative charts: `toPlot[1,] <- 0.0`
- Avoid `select()` conflicts between MASS and dplyr — use explicit `dplyr::select()` if needed
- `sprintf` format for dates: `'%Y-%m-%d'`, `'%4d-%2d-01'` for year-month

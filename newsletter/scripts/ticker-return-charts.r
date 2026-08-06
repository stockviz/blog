#!/usr/bin/env Rscript

# ticker-return-charts.r — Cumulative + annual return charts for given tickers
# Usage:
#   Rscript ticker-return-charts.r --tickers AAPL,MSFT,GOOGL --start 2020-01-01 --end 2025-12-31
#   Rscript ticker-return-charts.r --tickers AAPL,MSFT --start 2020-01-01 --end 2025-12-31 -o /tmp/out
#   Rscript ticker-return-charts.r -t AAPL,MSFT -s 2020-01-01 -e 2025-12-31

suppressMessages({
  library('RODBC')
  library('quantmod')
  library('PerformanceAnalytics')
  library('tidyverse')
  library('ggthemes')
  library('viridis')
  library('xts')
  library('scales')
  library('gt')
  library('webshot2')
})

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

# ------------------------------------------------------------
# CLI parsing via commandArgs
# ------------------------------------------------------------
parsePair <- function(arg) {
  # "--key=value" or "--key value" (handled upstream)
  if (grepl("^--[^=]+=.+$", arg)) {
    parts <- strsplit(sub("^--", "", arg), "=")[[1]]
    return(c(parts[1], parts[2]))
  }
  return(NULL)
}

rawArgs <- commandArgs(trailingOnly = TRUE)
cli     <- list(tickers = NULL, start = NULL, end = NULL, outdir = ".")

i <- 1
while (i <= length(rawArgs)) {
  pair <- parsePair(rawArgs[i])
  if (!is.null(pair)) {
    cli[[pair[1]]] <- pair[2]
    i <- i + 1
    next
  }
  flag <- sub("^--?", "", rawArgs[i])
  if (flag %in% c("t", "tickers")) {
    cli$tickers <- rawArgs[i + 1]; i <- i + 2
  } else if (flag %in% c("s", "start")) {
    cli$start   <- rawArgs[i + 1]; i <- i + 2
  } else if (flag %in% c("e", "end")) {
    cli$end     <- rawArgs[i + 1]; i <- i + 2
  } else if (flag %in% c("o", "outdir")) {
    cli$outdir  <- rawArgs[i + 1]; i <- i + 2
  } else if (flag %in% c("h", "help")) {
    cat("Usage: Rscript ticker-return-charts.r --tickers AAPL,MSFT --start YYYY-MM-DD --end YYYY-MM-DD [-o outdir]\n")
    cat("  -t, --tickers  Comma-separated ticker symbols\n")
    cat("  -s, --start    Start date (YYYY-MM-DD)\n")
    cat("  -e, --end      End date (YYYY-MM-DD)\n")
    cat("  -o, --outdir   Output directory for PNGs [default: .]\n")
    cat("  -h, --help     Show this help\n")
    quit(status = 0)
  } else {
    i <- i + 1
  }
}

if (is.null(cli$tickers) || is.null(cli$start) || is.null(cli$end)) {
  stop("Missing required args. Use --help for usage.")
}

tickers    <- strsplit(cli$tickers, ",\\s*")[[1]]
startDate  <- cli$start
endDate    <- cli$end
reportPath <- cli$outdir

dir.create(reportPath, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("Tickers : %s\n", paste(tickers, collapse = ", ")))
cat(sprintf("Period  : %s → %s\n", startDate, endDate))
cat(sprintf("Output  : %s\n", reportPath))

# ------------------------------------------------------------
# Database connection (TIINGO_DATA in StockVizUs2)
# ------------------------------------------------------------
lconUs2 <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockVizUs2", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

# ------------------------------------------------------------
# Fetch prices
# ------------------------------------------------------------
pxList <- list()
for (tkr in tickers) {
  sql <- sprintf(
    "SELECT time_stamp, c FROM TIINGO_DATA
     WHERE ticker = '%s'
       AND time_stamp >= '%s'
       AND time_stamp <= '%s'
     ORDER BY time_stamp",
    tkr, startDate, endDate)

  pxDf <- sqlQuery(lconUs2, sql)

  if (!is.data.frame(pxDf) || nrow(pxDf) < 2) {
    cat(sprintf("  WARNING: no data for %s, skipping\n", tkr))
    next
  }

  pxXts <- xts(pxDf$c, as.Date(pxDf$time_stamp))
  colnames(pxXts) <- tkr
  pxList[[tkr]] <- pxXts
}

odbcClose(lconUs2)

if (length(pxList) == 0) {
  stop("No data for any of the requested tickers.")
}

tickers <- names(pxList)  # only tickers we actually have data for

# Merge into a single xts
prices <- do.call(merge.xts, pxList)
names(prices) <- tickers

cat(sprintf("Loaded %d tickers, %d rows.\n", length(tickers), nrow(prices)))

# ------------------------------------------------------------
# Daily returns (per-column — dailyReturn on multi-col xts collapses to one)
# ------------------------------------------------------------
dailyRets <- do.call(merge.xts, lapply(tickers, function(tkr) {
  ret <- na.omit(dailyReturn(prices[, tkr], type = "log"))
  colnames(ret) <- tkr
  # dailyReturn produces POSIXct index — rebuild with Date
  xts(coredata(ret), as.Date(index(ret)))
}))
# na.omit in case merge created NAs (inner join)
dailyRets <- na.omit(dailyRets)

# ------------------------------------------------------------
# 1. Cumulative return chart via Common.PlotCumReturns
# ------------------------------------------------------------
toPlot <- na.omit(dailyRets)
toPlot[1, ] <- 0.0

suffix <- paste0(
  paste(tickers[1:min(3, length(tickers))], collapse = "_"),
  if (length(tickers) > 3) "_etal" else "",
  "_", gsub("-", "", startDate), "_", gsub("-", "", endDate)
)

cumretFile <- file.path(reportPath, sprintf("%s.cumret.png", suffix))

cat("Plotting cumulative returns...\n")

# Compute Sharpe ratios for subtitle annotation (legend order, no ticker names)
sr <- SharpeRatio.annualized(toPlot)
srStr <- paste(sprintf("%.2f", sapply(tickers, function(tkr) sr[1, tkr])), collapse = " / ")

Common.PlotCumReturns(
  toPlot,
  chartTitle    = paste(tickers, collapse = " / "),
  chartSubTitle = sprintf("%s → %s  |  SR: %s", startDate, endDate, srStr),
  fileName      = cumretFile,
  NULL
)
cat(sprintf("  → %s\n", cumretFile))

# ------------------------------------------------------------
# 2. Annual return chart via ggplot (viridis + economist)
# ------------------------------------------------------------
# Compute calendar-year returns
annual <- apply.yearly(toPlot, Return.cumulative)
annualDf <- fortify(annual, melt = TRUE)
names(annualDf) <- c("Year", "Ticker", "Return")
annualDf$Year <- as.numeric(format(annualDf$Year, "%Y"))

annualFile <- file.path(reportPath, sprintf("%s.annual.png", suffix))

cat("Plotting annual returns...\n")

p <- ggplot(annualDf, aes(x = factor(Year), y = Return, fill = Ticker)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_viridis_d(end = 0.9) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(-1, 1, 0.1)
  ) +
  labs(
    title    = "Annual Returns",
    subtitle = sprintf("%s → %s", startDate, endDate),
    caption  = "@StockViz",
    x        = NULL,
    y        = NULL
  ) +
  theme_economist() +
  theme(
    plot.caption         = element_text(size = 8, color = "grey50", hjust = 1),
    plot.caption.position = "plot",
    axis.text.x          = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position      = "bottom"
  )

ggsave(annualFile, p, width = 12, height = 7, units = "in")
cat(sprintf("  → %s\n", annualFile))

# ------------------------------------------------------------
# Shared: ticker → viridis color map (matches ggplot scale_fill_viridis_d)
# ------------------------------------------------------------
tickerColors <- setNames(
  viridis_pal(end = 0.9)(length(tickers)),
  tickers
)

# Blend hex color with white (amount 0–1: 0 = original, 1 = white)
lighten <- function(hex, amount = 0.85) {
  rgb <- col2rgb(hex) / 255
  rgb <- rgb + (1 - rgb) * amount
  rgb(rgb[1], rgb[2], rgb[3])
}

# ------------------------------------------------------------
# 3. Top 5 drawdowns table (across all tickers)
# ------------------------------------------------------------
cat("Building top-5 drawdowns table...\n")

ddAll <- tibble()
for (tkr in tickers) {
  tdd <- table.Drawdowns(toPlot[, tkr])
  if (nrow(tdd) == 0) next
  tddDf <- as_tibble(tdd)
  tddDf$Ticker <- tkr
  ddAll <- rbind(ddAll, tddDf)
}

# Top 5 per ticker (table.Drawdowns already sorts by Depth)
ddTop5 <- ddAll |>
  group_by(Ticker) |>
  slice_head(n = 5) |>
  ungroup()

ddHtml <- file.path(reportPath, sprintf("%s.drawdowns.html", suffix))

ddTbl <- ddTop5 |>
  gt() |>
  tab_header(
    title    = "Top 5 Drawdowns",
    subtitle = sprintf("%s → %s", startDate, endDate)
  ) |>
  cols_label(
    Ticker    = "Ticker",
    From      = "From",
    Trough    = "Trough",
    To        = "To",
    Depth     = "Depth (%)",
    Length    = "Days",
    `To Trough` = "To Trough",
    Recovery  = "Recovery"
  ) |>
  fmt_percent(columns = Depth, decimals = 2) |>
  fmt_number(columns = c(Length, `To Trough`, Recovery), decimals = 0) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_source_notes()
  )

# Color Ticker cells and rows to match ggplot viridis palette
for (tkr in tickers) {
  lc <- lighten(tickerColors[tkr])
  ddTbl <- ddTbl |>
    tab_style(
      style = cell_fill(color = lc),
      locations = cells_body(rows = Ticker == tkr)
    ) |>
    tab_style(
      style = list(
        cell_fill(color = tickerColors[tkr]),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_body(columns = Ticker, rows = Ticker == tkr)
    )
}

ddTbl |> gtsave(ddHtml)

ddPng <- file.path(reportPath, sprintf("%s.drawdowns.png", suffix))
webshot2::webshot(ddHtml, ddPng,
  selector = "table.gt_table", expand = c(10, 10, 10, 10))
cat(sprintf("  → %s\n", ddPng))

# ------------------------------------------------------------
# 4. Consolidated metrics table (Max DD, Sharpe, CAGR per ticker)
# ------------------------------------------------------------
cat("Building consolidated metrics table...\n")

metrics <- tibble()
for (tkr in tickers) {
  r <- toPlot[, tkr]
  dd  <- maxDrawdown(r)                    # returns positive
  sr  <- SharpeRatio.annualized(r)[1, 1]    # matrix → scalar
  cagr <- Return.annualized(r)[1, 1]        # matrix → scalar
  metrics <- rbind(metrics, tibble(
    Ticker  = tkr,
    MaxDD   = dd,
    Sharpe  = sr,
    CAGR    = cagr
  ))
}

metricsHtml <- file.path(reportPath, sprintf("%s.metrics.html", suffix))

metricsTbl <- metrics |>
  gt() |>
  tab_header(
    title    = "Risk / Return Summary",
    subtitle = sprintf("%s → %s", startDate, endDate)
  ) |>
  fmt_percent(columns = c(MaxDD, CAGR), decimals = 2) |>
  fmt_number(columns = Sharpe, decimals = 2) |>
  cols_label(
    Ticker = "Ticker",
    MaxDD  = "Max DD",
    Sharpe = "Sharpe",
    CAGR   = "CAGR"
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_source_notes()
  )

# Color Ticker cells and rows to match ggplot viridis palette
for (tkr in tickers) {
  lc <- lighten(tickerColors[tkr])
  metricsTbl <- metricsTbl |>
    tab_style(
      style = cell_fill(color = lc),
      locations = cells_body(rows = Ticker == tkr)
    ) |>
    tab_style(
      style = list(
        cell_fill(color = tickerColors[tkr]),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_body(columns = Ticker, rows = Ticker == tkr)
    )
}

metricsTbl |> gtsave(metricsHtml)

metricsPng <- file.path(reportPath, sprintf("%s.metrics.png", suffix))
webshot2::webshot(metricsHtml, metricsPng,
  selector = "table.gt_table", expand = c(10, 10, 10, 10))
cat(sprintf("  → %s\n", metricsPng))

cat("Done.\n")

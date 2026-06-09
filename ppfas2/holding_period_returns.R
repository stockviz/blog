#!/usr/bin/env Rscript
#
# PPFAS Flexi Cap Fund — Holding Period Returns
#
# Identifies contiguous holding periods, fetches start/end prices,
# computes annualized excess return vs NIFTY MIDCAP 150 TR.
#

suppressPackageStartupMessages({
  library('RODBC')
  library('RPostgres')
  library('tidyverse')
  library('xts')
  library('ggthemes')
  library('viridis')
  library('ggrepel')
  library('scales')
})

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

argv <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", argv, value = TRUE)
SCRIPT_DIR <- if (length(file_arg) > 0) dirname(sub("^--file=", "", file_arg)) else getwd()
DATA_DIR   <- SCRIPT_DIR

CSV_IN    <- file.path(DATA_DIR, "indian_equity_lo_portfolio.csv")
CSV_OUT   <- file.path(DATA_DIR, "holding_periods.csv")
CHART_DIR <- file.path(DATA_DIR, "charts")
dir.create(CHART_DIR, showWarnings = FALSE, recursive = TRUE)

source("/mnt/hollandC/StockViz/R/config.r")

# ---------------------------------------------------------------------------
# Database connections
# ---------------------------------------------------------------------------
mssqlCon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = ldbserver2, user = ldbuser2, password = ldbpassword2,
  dbname = 'StockVizDyn', sslmode = 'allow'
)

# ---------------------------------------------------------------------------
# Load portfolio data
# ---------------------------------------------------------------------------
cat(sprintf("Reading %s ...\n", CSV_IN))
df <- read_csv(CSV_IN, show_col_types = FALSE) |>
  filter(!is.na(position_return_1y) & position_return_1y != "NA") |>
  mutate(
    date = as.Date(date),
    label = if_else(is.na(symbol) | symbol == "NA",
                    str_trunc(name, 30), symbol)
  )

# ---------------------------------------------------------------------------
# Identify contiguous holding periods (gap > 60 days = new period)
# ---------------------------------------------------------------------------
cat("Identifying holding periods ...\n")

periods <- tibble()
for (lbl in unique(df$label)) {
  pos_df <- df |> filter(label == lbl) |> arrange(date)
  if (nrow(pos_df) == 0) next

  pos_df <- pos_df |>
    mutate(
      prev_date  = lag(date),
      gap_days   = as.numeric(date - prev_date),
      new_period = row_number() == 1 | is.na(gap_days) | gap_days > 60
    )

  # Assign period IDs
  pos_df$period_id <- cumsum(pos_df$new_period)

  for (pid in unique(pos_df$period_id)) {
    block <- pos_df |> filter(period_id == pid)
    periods <- bind_rows(periods, tibble(
      symbol     = lbl,
      start_date = min(block$date),
      end_date   = max(block$date),
      n_months   = nrow(block)
    ))
  }
}

# Keep only periods > 12 months
periods_long <- periods |> filter(n_months > 12)
cat(sprintf("%d total periods, %d > 12 months\n", nrow(periods), nrow(periods_long)))

# ---------------------------------------------------------------------------
# Fetch prices and compute annualized excess return
# ---------------------------------------------------------------------------
cat("Computing annualized excess returns ...\n")

#' Fetch adjusted close for a symbol on a date (or nearest after)
fetch_close <- function(sym, target_date) {
  df <- tryCatch(
    dbGetQuery(pgCon,
      "SELECT date_stamp, c FROM eod_adjusted_nse
       WHERE ticker = $1 AND date_stamp >= $2
       ORDER BY date_stamp LIMIT 1",
      params = list(sym, target_date)),
    error = function(e) NULL
  )
  if (!is.data.frame(df) || nrow(df) == 0) return(NA_real_)
  df$c[1]
}

#' Fetch NIFTY MIDCAP 150 TR close on a date
fetch_index_close <- function(target_date) {
  sql <- sprintf(
    "SELECT TOP 1 PX_CLOSE FROM BHAV_INDEX
     WHERE INDEX_NAME = 'NIFTY MIDCAP 150 TR' AND TIME_STAMP >= '%s'
     ORDER BY TIME_STAMP",
    format(target_date, "%Y-%m-%d")
  )
  df <- tryCatch(sqlQuery(mssqlCon, sql), error = function(e) NULL)
  if (!is.data.frame(df) || nrow(df) == 0) return(NA_real_)
  df$PX_CLOSE[1]
}

periods_long <- periods_long |>
  rowwise() |>
  mutate(
    px_start   = fetch_close(symbol, start_date),
    px_end     = fetch_close(symbol, end_date),
    idx_start  = fetch_index_close(start_date),
    idx_end    = fetch_index_close(end_date),
    years      = as.numeric(end_date - start_date) / 365.25,
    stock_ret  = if_else(!is.na(px_start) & !is.na(px_end) & px_start > 0,
                         (px_end / px_start) ^ (1 / years) - 1, NA_real_),
    index_ret  = if_else(!is.na(idx_start) & !is.na(idx_end) & idx_start > 0,
                         (idx_end / idx_start) ^ (1 / years) - 1, NA_real_),
    excess_ret = stock_ret - index_ret
  ) |>
  ungroup() |>
  filter(!is.na(excess_ret))

cat(sprintf("%d periods with valid return data\n", nrow(periods_long)))

# ---------------------------------------------------------------------------
# Write CSV
# ---------------------------------------------------------------------------
periods_out <- periods_long |>
  select(symbol, start_date, end_date, n_months, years,
         stock_ret, index_ret, excess_ret) |>
  arrange(symbol, start_date)

write_csv(periods_out, CSV_OUT)
cat(sprintf("Wrote %s\n", CSV_OUT))

# ---------------------------------------------------------------------------
# Chart
# ---------------------------------------------------------------------------
cat("Charting ...\n")

plot_data <- periods_out |>
  arrange(excess_ret) |>
  mutate(
    label = sprintf("%s\n%s→%s", symbol,
                    format(start_date, "%Y-%m"), format(end_date, "%Y-%m")),
    label = factor(label, levels = label)
  )

p <- ggplot(plot_data, aes(x = label, y = excess_ret)) +
  geom_col(aes(fill = excess_ret), width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  scale_fill_viridis(option = "D", labels = percent_format()) +
  scale_y_continuous(labels = percent_format(), breaks = seq(-1, 1, 0.1)) +
  labs(
    title    = "Annualized Excess Returns",
    subtitle = sprintf(
      "PPFAS Flexi Cap Fund — %d periods > 12 months · vs NIFTY MIDCAP 150 TR",
      nrow(plot_data)
    ),
    x        = NULL,
    y        = "Annualized Excess Return\nvs NIFTY MIDCAP 150 TR",
    fill     = NULL,
    caption  = "@StockViz"
  ) +
  theme_economist() +
  theme(
    axis.text.x          = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    legend.position      = "none",
    plot.caption         = element_text(size = 8, color = "grey50", hjust = 1),
    plot.caption.position = "plot"
  )

ggsave(file.path(CHART_DIR, "holding_period_returns.png"),
       p, width = 14, height = 8, units = "in")

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat(sprintf("\nDone! Saved to %s/\n", CHART_DIR))

cat(sprintf("\nTop 5 by annualized excess:\n"))
top5 <- periods_out |> slice_max(excess_ret, n = 5)
for (i in seq_len(nrow(top5))) {
  cat(sprintf("  %-15s %s → %s  %+5.1f%%/yr  (%.1f yr)\n",
              top5$symbol[i], top5$start_date[i], top5$end_date[i],
              top5$excess_ret[i] * 100, top5$years[i]))
}

cat(sprintf("\nBottom 5:\n"))
bot5 <- periods_out |> slice_min(excess_ret, n = 5)
for (i in seq_len(nrow(bot5))) {
  cat(sprintf("  %-15s %s → %s  %+5.1f%%/yr  (%.1f yr)\n",
              bot5$symbol[i], bot5$start_date[i], bot5$end_date[i],
              bot5$excess_ret[i] * 100, bot5$years[i]))
}

odbcClose(mssqlCon)
dbDisconnect(pgCon)

#!/usr/bin/env Rscript
#
# PPFAS Flexi Cap Fund — Holding Period Summary Table
#

suppressPackageStartupMessages({
  library('tidyverse')
  library('gt')
  library('gtExtras')
  library('webshot2')
})

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

argv <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", argv, value = TRUE)
SCRIPT_DIR <- if (length(file_arg) > 0) dirname(sub("^--file=", "", file_arg)) else getwd()
DATA_DIR   <- SCRIPT_DIR

CSV_IN    <- file.path(DATA_DIR, "holding_periods.csv")
CHART_DIR <- file.path(DATA_DIR, "charts")
dir.create(CHART_DIR, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Load
# ---------------------------------------------------------------------------
d <- read_csv(CSV_IN, show_col_types = FALSE)

df_all <- read_csv(file.path(DATA_DIR, "indian_equity_lo_portfolio.csv"), show_col_types = FALSE) |>
  filter(!is.na(position_return_1y) & position_return_1y != "NA") |>
  mutate(
    date = as.Date(date),
    excess_return = as.numeric(position_return_1y) - as.numeric(nifty_midcap150_tr_return_1y),
    label = if_else(is.na(symbol) | symbol == "NA", str_trunc(name, 30), symbol)
  )

# ---------------------------------------------------------------------------
# Count all periods and collect entry/exit excess returns
# ---------------------------------------------------------------------------
latest_date <- max(df_all$date)
total_periods <- 0
all_durations <- c()
entry_rets <- c()
exit_rets  <- c()

for (lbl in unique(df_all$label)) {
  pos_df <- df_all |> filter(label == lbl) |> arrange(date)
  if (nrow(pos_df) == 0) next

  pos_df <- pos_df |> mutate(
    prev_date = lag(date),
    next_date = lead(date),
    gap_prev  = as.numeric(date - prev_date),
    gap_next  = as.numeric(next_date - date),
    is_entry  = row_number() == 1 | is.na(gap_prev) | gap_prev > 60,
    is_exit   = is.na(gap_next) | gap_next > 60
  )

  # Assign period IDs and count durations
  pos_df$period_id <- cumsum(pos_df$is_entry)
  period_counts <- pos_df |> group_by(period_id) |> summarise(n = n(), .groups = "drop")
  all_durations <- c(all_durations, period_counts$n)
  total_periods <- total_periods + nrow(period_counts)

  # Collect entry returns
  entry_rets <- c(entry_rets, pos_df$excess_return[pos_df$is_entry])

  # Collect exit returns (exclude still-held periods)
  exits <- pos_df |> filter(is_exit, date < latest_date)
  exit_rets <- c(exit_rets, exits$excess_return)
}

# ---------------------------------------------------------------------------
# Build table
# ---------------------------------------------------------------------------
count_rows <- tibble(
  Statistic = c("Symbols", "All Periods", ">12 Months"),
  Value     = c(n_distinct(d$symbol), total_periods, nrow(d))
)

duration_rows <- tibble(
  Statistic = c("Maximum", "Minimum", "Average"),
  Value     = c(max(all_durations), min(all_durations), round(mean(all_durations)))
)

excess_rows <- tibble(
  Statistic = c("Maximum", "Minimum", "Average"),
  Value     = c(max(d$excess_ret), min(d$excess_ret), mean(d$excess_ret))
)

entry_rows <- tibble(
  Statistic = c("Maximum", "Minimum", "Average"),
  Value     = c(max(entry_rets), min(entry_rets), mean(entry_rets))
)

exit_rows <- tibble(
  Statistic = c("Maximum", "Minimum", "Average"),
  Value     = c(max(exit_rets), min(exit_rets), mean(exit_rets))
)

tbl <- bind_rows(
  count_rows    |> mutate(Group = "Count"),
  duration_rows |> mutate(Group = "Holding Period (months)"),
  excess_rows   |> mutate(Group = "Excess Return (>12 mo holding)"),
  entry_rows    |> mutate(Group = "Excess Return After Entry (1-year)"),
  exit_rows     |> mutate(Group = "Excess Return After Exit (1-year)")
) |>
  select(Group, Statistic, Value) |>
  gt(groupname_col = "Group") |>
  tab_header(
    title    = "PPFAS Flexi Cap Fund — Holding Period Returns",
    subtitle = "contiguous holding periods · excess vs NIFTY MIDCAP 150 TR"
  ) |>
  fmt_number(
    columns  = Value,
    rows     = Group %in% c("Excess Return (>12 mo holding)",
                            "Excess Return After Entry (1-year)",
                            "Excess Return After Exit (1-year)"),
    decimals = 1,
    pattern  = "{x}%",
    scale_by = 100
  ) |>
  fmt_number(
    columns  = Value,
    rows     = !Group %in% c("Excess Return (>12 mo holding)",
                              "Excess Return After Entry (1-year)",
                              "Excess Return After Exit (1-year)"),
    decimals = 0
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "#FFFFFF"),
      cell_fill(color = "#4E79A7")
    ),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = list(
      cell_text(color = "#333333"),
      cell_fill(color = "#F2F2F2")
    ),
    locations = cells_body(rows = seq_len(nrow(count_rows) + nrow(duration_rows) + nrow(excess_rows) + nrow(entry_rows) + nrow(exit_rows)) %% 2 == 0)
  ) |>
  tab_options(
    table.font.size       = '130%',
    column_labels.hidden  = TRUE,
    row_group.padding     = px(10),
    table.border.top.style   = "none",
    table.border.bottom.style = "none"
  ) |>
  tab_footnote(footnote = md("<sub><sub>*@StockViz*</sub></sub>"))

# Save
html_path <- file.path(CHART_DIR, "holding_period_summary.html")
png_path  <- file.path(CHART_DIR, "holding_period_summary.png")

gtsave(tbl, html_path)

webshot2::webshot(html_path, png_path, zoom = 2, selector = "table.gt_table", expand = c(20, 20, 20, 20))

cat(sprintf("Saved %s\n", png_path))

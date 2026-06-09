#!/usr/bin/env Rscript
#
# PPFAS Flexi Cap Fund — Entry vs Exit Performance
#
# Detects contiguous holding periods (gap > 60 days = exit + re-entry).
# Pairs each entry with its corresponding exit and plots scatter.
#

suppressPackageStartupMessages({
  library('tidyverse')
  library('ggthemes')
  library('viridis')
  library('ggrepel')
  library('scales')
})

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
argv <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", argv, value = TRUE)
SCRIPT_DIR <- if (length(file_arg) > 0) dirname(sub("^--file=", "", file_arg)) else getwd()
DATA_DIR   <- SCRIPT_DIR

CSV_IN    <- file.path(DATA_DIR, "indian_equity_lo_portfolio.csv")
CHART_DIR <- file.path(DATA_DIR, "charts")
dir.create(CHART_DIR, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------
cat(sprintf("Reading %s ...\n", CSV_IN))
df <- read_csv(CSV_IN, show_col_types = FALSE) |>
  filter(!is.na(position_return_1y) & position_return_1y != "NA") |>
  mutate(
    date            = as.Date(date),
    position_return = as.numeric(position_return_1y),
    index_return    = as.numeric(nifty_midcap150_tr_return_1y),
    excess_return   = position_return - index_return,
    label           = if_else(is.na(symbol) | symbol == "NA",
                              str_trunc(name, 30), symbol)
  )

latest_date <- max(df$date)

# ---------------------------------------------------------------------------
# Pair each entry with its corresponding exit
# ---------------------------------------------------------------------------
pairs <- tibble()

for (lbl in unique(df$label)) {
  pos_df <- df |> filter(label == lbl) |> arrange(date)
  if (nrow(pos_df) == 0) next

  pos_df <- pos_df |>
    mutate(
      prev_date = lag(date),
      next_date = lead(date),
      gap_prev  = as.numeric(date - prev_date),
      gap_next  = as.numeric(next_date - date),
      is_entry  = row_number() == 1 | is.na(gap_prev) | gap_prev > 60,
      is_exit   = is.na(gap_next) | gap_next > 60
    )

  entries <- pos_df |> filter(is_entry)
  exits   <- pos_df |> filter(is_exit)

  # Pair nth entry with nth exit
  n_periods <- min(nrow(entries), nrow(exits))
  if (n_periods == 0) next

  for (p in seq_len(n_periods)) {
    # Skip if exit is the latest date (still held in final period)
    if (exits$date[p] >= latest_date) next

    pairs <- bind_rows(pairs, tibble(
      label        = lbl,
      entry_date   = entries$date[p],
      exit_date    = exits$date[p],
      entry_excess = entries$excess_return[p],
      exit_excess  = exits$excess_return[p],
      held_months  = as.numeric(exits$date[p] - entries$date[p]) / 30.44
    ))
  }
}

# Filter out INTELLECT outlier
pairs <- pairs |> filter(label != "INTELLECT")

cat(sprintf("%d entry-exit pairs across %d symbols\n", nrow(pairs), n_distinct(pairs$label)))

# ---------------------------------------------------------------------------
# Chart
# ---------------------------------------------------------------------------
cat("Chart: Entry vs Exit scatter ...\n")

p <- ggplot(pairs, aes(x = entry_excess, y = exit_excess)) +
  geom_vline(xintercept = 0, linewidth = 0.3, color = "grey60", linetype = "dashed") +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.3, color = "grey70") +
  geom_point(aes(color = held_months), size = 3, alpha = 0.85) +
  geom_text_repel(
    aes(label = label),
    size = 3.2,
    max.overlaps = 20,
    segment.color = "grey70",
    segment.size = 0.3
  ) +
  scale_color_viridis(option = "D", name = "Months\nHeld") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Excess Return — Entry vs Exit",
    subtitle = sprintf(
      "PPFAS Flexi Cap Fund — %d holding periods · diagonal = same return at entry and exit",
      nrow(pairs)
    ),
    x        = "1-Year Excess Return After Entry\nvs NIFTY MIDCAP 150 TR",
    y        = "1-Year Excess Return After Exit\nvs NIFTY MIDCAP 150 TR",
    caption  = "@StockViz"
  ) +
  theme_economist() +
  theme(
    legend.text          = element_text(size = 7),
    plot.caption         = element_text(size = 8, color = "grey50", hjust = 1),
    plot.caption.position = "plot"
  )

ggsave(file.path(CHART_DIR, "entry_vs_exit_scatter.png"),
       p, width = 11, height = 9, units = "in")

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat(sprintf("\nDone! Saved to %s/\n", CHART_DIR))

q_both_pos <- sum(pairs$entry_excess > 0 & pairs$exit_excess > 0)
q_both_neg <- sum(pairs$entry_excess < 0 & pairs$exit_excess < 0)
q_e_pos_x_neg <- sum(pairs$entry_excess > 0 & pairs$exit_excess < 0)
q_e_neg_x_pos <- sum(pairs$entry_excess < 0 & pairs$exit_excess > 0)

cat(sprintf("\nQuadrant breakdown:\n"))
cat(sprintf("  Good entry, good exit:    %d\n", q_both_pos))
cat(sprintf("  Bad entry, bad exit:      %d\n", q_both_neg))
cat(sprintf("  Good entry, bad exit:     %d\n", q_e_pos_x_neg))
cat(sprintf("  Bad entry, good exit:     %d\n", q_e_neg_x_pos))

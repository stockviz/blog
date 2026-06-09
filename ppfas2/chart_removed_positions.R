#!/usr/bin/env Rscript
#
# PPFAS Flexi Cap Fund — Performance of Removed Positions
#
# Detects contiguous holding periods. A gap > 2 months counts as exit.
# Charts final-year excess return for every exit event.
#

suppressPackageStartupMessages({
  library('tidyverse')
  library('ggthemes')
  library('viridis')
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

# ---------------------------------------------------------------------------
# Detect exits (last date of each contiguous period, except current holders)
# ---------------------------------------------------------------------------
latest_date <- max(df$date)

find_exits <- function(df) {
  exits <- tibble()
  for (lbl in unique(df$label)) {
    pos_df <- df |> filter(label == lbl) |> arrange(date)
    if (nrow(pos_df) == 0) next

    pos_df <- pos_df |>
      mutate(
        next_date  = lead(date),
        gap_days   = as.numeric(next_date - date),
        period_end = is.na(gap_days) | gap_days > 60  # last row of contiguous block
      )

    period_ends <- pos_df |> filter(period_end)
    exits <- bind_rows(exits, period_ends)
  }
  exits
}

exits <- find_exits(df) |>
  # Exclude exits that are still in the latest month (still held)
  filter(date < latest_date) |>
  arrange(excess_return) |>
  mutate(label = factor(label, levels = unique(label)))

cat(sprintf("%d rows, %d exit events\n", nrow(df), nrow(exits)))

# ---------------------------------------------------------------------------
# Chart: Excess return at each exit
# ---------------------------------------------------------------------------
cat("Chart: Excess return at exit ...\n")

p <- ggplot(exits, aes(x = label, y = excess_return)) +
  geom_col(aes(fill = excess_return), width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  scale_fill_viridis(option = "D", labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Excess Return After Exit",
    subtitle = sprintf(
      "PPFAS Flexi Cap Fund — %d exit events · 1-year forward vs NIFTY MIDCAP 150 TR",
      nrow(exits)
    ),
    x        = NULL,
    y        = "Excess Return vs NIFTY MIDCAP 150 TR\n(1-year forward from exit)",
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

ggsave(file.path(CHART_DIR, "removed_positions_bar.png"),
       p, width = 14, height = 8, units = "in")

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat(sprintf("\nDone! Saved to %s/\n", CHART_DIR))
cat(sprintf("\nTotal exit events: %d across %d symbols\n",
            nrow(exits), n_distinct(exits$label)))

cat("\nWorst exits:\n")
worst <- exits |> slice_min(excess_return, n = 5)
for (i in seq_len(nrow(worst))) {
  cat(sprintf("  %-15s %s  %+5.1f%%\n",
              worst$label[i], worst$date[i], worst$excess_return[i] * 100))
}

cat("\nBest exits:\n")
best <- exits |> slice_max(excess_return, n = 5)
for (i in seq_len(nrow(best))) {
  cat(sprintf("  %-15s %s  %+5.1f%%\n",
              best$label[i], best$date[i], best$excess_return[i] * 100))
}
